require "set"
require "./hygiene"
require "./macro_footprint"

module Facet
  module Compiler
    alias MacroValue = Int64 | String | Bool | Nil | Array(MacroValue) | Hash(String, MacroValue)
    class MacroExpander
      getter diagnostics : Array(Diagnostic)
      getter cache_hits : Int32
      getter last_footprint : MacroFootprint?

      def initialize(@index : ProgramIndex? = nil, @max_passes : Int32 = 8)
        @diagnostics = [] of Diagnostic
        @env_stack = [] of Hash(String, MacroValue)
        @cache = {} of String => String
        @cache_hits = 0
        @hygiene = Hygiene.new
        @file_cache = {} of UInt64 => AstFile
        @last_footprint = nil
      end

      def expand_all(asts : Array(AstFile), index : ProgramIndex? = nil) : Array(AstFile)
        idx = index || @index || ProgramIndex.new
        if index.nil? && @index.nil?
          Indexer.index_macros(asts, idx)
        end
        asts.map { |file| expand(file, idx) }
      end

      def expand(ast : AstFile, index : ProgramIndex? = nil, footprint : MacroFootprint? = nil) : AstFile
        idx = index || @index
        current_ast = ast
        base_fp = fingerprint_text(ast.source, Span.new(0, ast.source.size)).hash.to_u64
        idx_fp = idx ? idx.fingerprint : 0_u64
        cache_key = base_fp ^ idx_fp
        if cached = @file_cache[cache_key]?
          @cache_hits += 1
          return cached
        end
        passes = 0
        seen_texts = Set(String).new
        caller_idents = collect_idents(current_ast.root, current_ast)
        footprint ||= MacroFootprint.new

        loop do
          text_sig = current_ast.source.text
          if seen_texts.includes?(text_sig)
            @diagnostics << Diagnostic.new(Span.new(0, current_ast.source.size), "macro expansion cycle detected")
            break
          end
          seen_texts << text_sig

          macros = [] of NodeId
          collect_macros(current_ast.root, current_ast, macros)
          break if macros.empty?

          if passes >= @max_passes
            @diagnostics << Diagnostic.new(Span.new(0, current_ast.source.size), "macro expansion exceeded max passes (#{@max_passes})")
            break
          end

          expanded_text = expand_text(current_ast, macros, idx, nil, caller_idents, footprint)
          site = ExpansionSite.new(current_ast.source, Span.new(0, current_ast.source.size))
          new_source = Source.new(expanded_text, current_ast.source.filename, SourceKind::Virtual, site)
          parser = Parser.new(new_source)
          next_ast = parser.parse_file
          parser.diagnostics.each { |d| @diagnostics << d }

          current_ast = next_ast
          passes += 1
        end

        footprint.merge_macro_uses
        @last_footprint = footprint
        @file_cache[cache_key] = current_ast
        current_ast
      end

      private def expand_text(
        ast : AstFile,
        macros : Array(NodeId),
        index : ProgramIndex?,
        slice_span : Span? = nil,
        caller_idents : Hash(String, Bool)? = nil,
        footprint : MacroFootprint? = nil,
        replacements : Array(Tuple(Span, String)) = [] of Tuple(Span, String)
      ) : String
        macros.sort_by! { |id| ast.node(id).span.start }
        start_pos = slice_span ? slice_span.start : 0
        end_pos = slice_span ? slice_span.finish : ast.source.size

        src = ast.source
        bytes = src.bytes
        builder = String::Builder.new
        last = start_pos
        replacements_index = 0

        macros.each do |id|
          span = ast.node(id).span
          next if span.start < start_pos || span.start >= end_pos
          last = write_with_replacements(bytes, last, span.start, replacements, builder, replacements_index)
          expansion = expand_macro(id, ast, index, caller_idents || {} of String => Bool, footprint) || ""
          builder << expansion
          last = span.finish
        end

        if last < end_pos
          write_with_replacements(bytes, last, end_pos, replacements, builder, replacements_index)
        end
        builder.to_s
      end

      private def collect_macros(node_id : NodeId, ast : AstFile, acc : Array(NodeId))
        node = ast.node(node_id)
        if node.kind == NodeKind::MacroExpr || node.kind == NodeKind::MacroControl
          acc << node_id
          return
        end
        ast.children(node_id).each do |child|
          collect_macros(child, ast, acc)
        end
      end

      private def expand_macro(node_id : NodeId, ast : AstFile, index : ProgramIndex?, caller_idents : Hash(String, Bool), footprint : MacroFootprint?) : String?
        node = ast.node(node_id)
        case node.kind
        when NodeKind::MacroExpr
          expand_macro_expr(node_id, ast, index, caller_idents, footprint)
        when NodeKind::MacroControl
          expand_macro_control(node_id, ast)
        when NodeKind::MacroVar
          expand_macro_var(node_id, ast)
        else
          nil
        end
      end

      private def expand_macro_expr(node_id : NodeId, ast : AstFile, index : ProgramIndex?, caller_idents : Hash(String, Bool), footprint : MacroFootprint?) : String?
        body_id = ast.children(node_id)[0]
        exprs = ast.children(body_id)
        return "" if exprs.empty?
        expr = exprs.first
        if index && (name = macro_call_name(expr, ast))
          footprint.try &.macro_use(name)
          refs = index.macros_for(name)
          if refs && !refs.empty?
            call_args = macro_call_args(expr, ast)
            key = cache_key(refs.first, call_args)
            if cached = @cache[key]?
              @cache_hits += 1
              return cached
            end
            result = expand_macro_def(refs.first, call_args, caller_idents, footprint)
            @cache[key] = result if result
            return result
          end
        end
        eval_to_text(expr, ast)
      end

      private def macro_call_name(node_id : NodeId, ast : AstFile) : String?
        node = ast.node(node_id)
        case node.kind
        when NodeKind::Ident
          ast.arena.symbols[node.payload_index]
        when NodeKind::Call
          callee_id = ast.children(node_id)[0]
          callee = ast.node(callee_id)
          return nil unless callee.kind == NodeKind::Ident
          ast.arena.symbols[callee.payload_index]
        else
          nil
        end
      end

      private def macro_call_args(node_id : NodeId, ast : AstFile) : Tuple(Array(MacroValue), Hash(String, MacroValue))
        node = ast.node(node_id)
        positional = [] of MacroValue
        named = {} of String => MacroValue
        return {positional, named} unless node.kind == NodeKind::Call
        args_id = ast.children(node_id)[1]?
        return {positional, named} unless args_id
        ast.children(args_id).each do |arg_id|
          arg_node = ast.node(arg_id)
          if arg_node.kind == NodeKind::NamedArg
            value_id = ast.children(arg_id)[0]
            if value = eval_value(value_id, ast)
              name = ast.arena.symbols[arg_node.payload_index]
              named[name] = value
            end
          else
            if value = eval_value(arg_id, ast)
              positional << value
            else
              @diagnostics << Diagnostic.new(arg_node.span, "unsupported macro argument")
            end
          end
        end
        {positional, named}
      end

      private def expand_macro_def(ref : DeclRef, args : {Array(MacroValue), Hash(String, MacroValue)}, caller_idents : Hash(String, Bool), footprint : MacroFootprint?) : String
        def_node = ref.ast.node(ref.node_id)
        params_id = ref.ast.children(ref.node_id)[1]?
        body_id = ref.ast.children(ref.node_id)[3]?
        return "" unless body_id && params_id
        env = build_param_env(params_id, ref.ast, args)
        @env_stack << env
        body_span = ref.ast.node(body_id).span
        body_idents = collect_ident_spans(body_id, ref.ast)
        collisions = body_idents.map(&.last).select { |name| caller_idents.has_key?(name) }
        replacements = [] of Tuple(Span, String)
        collisions.uniq.each do |name|
          new_sym = @hygiene.gensym(name, ref.ast.arena.symbols)
          body_idents.each do |span, ident_name|
            if ident_name == name
              replacements << {span, ref.ast.arena.symbols[new_sym]}
            end
          end
          @diagnostics << Diagnostic.new(body_span, "macro identifier '#{name}' renamed to avoid collision")
        end
        replacements.sort_by! { |t| t[0].start }
        macros = [] of NodeId
        collect_macros(body_id, ref.ast, macros)
        text = expand_text(ref.ast, macros, @index, body_span, caller_idents, footprint, replacements)
        @env_stack.pop
        footprint.try &.merge_requires(ref.ast.source)
        text
      end

      private def rename_generated_idents(ast : AstFile, node_id : NodeId, seen : Hash(String, Bool))
        node = ast.node(node_id)
        if node.kind == NodeKind::Ident
          name = ast.arena.symbols[node.payload_index]
          if name.starts_with?("__gensym_")
            base = name.lchop("__gensym_")
            new_sym = @hygiene.gensym(base, ast.arena.symbols)
            ast.arena.set_ident_symbol(node_id, new_sym)
          elsif seen[name]?
            new_sym = @hygiene.gensym(name, ast.arena.symbols)
            ast.arena.set_ident_symbol(node_id, new_sym)
          else
            seen[name] = true
          end
        end
        ast.children(node_id).each do |child|
          rename_generated_idents(ast, child, seen)
        end
      end

      private def cache_key(ref : DeclRef, args : {Array(MacroValue), Hash(String, MacroValue)}) : String
        def_node = ref.ast.node(ref.node_id)
        name_id = ref.ast.children(ref.node_id)[0]
        name = ref.ast.arena.symbols[ref.ast.node(name_id).payload_index]
        body_id = ref.ast.children(ref.node_id)[3]?
        body_fp = body_id ? fingerprint_text(ref.ast.source, ref.ast.node(body_id).span) : "nil"
        args_fp = fingerprint_args(args)
        "#{name}|#{body_fp}|#{args_fp}"
      end

      private def build_param_env(params_id : NodeId, ast : AstFile, args : {Array(MacroValue), Hash(String, MacroValue)}) : Hash(String, MacroValue)
        env = {} of String => MacroValue
        params = ast.children(params_id)
        positional_args, named_args = args
        positional_index = 0
        trailing_named = named_args.dup

        params.each do |param_id|
          param = ast.node(param_id)
          case param.kind
          when NodeKind::Param
            name_node = ast.children(param_id)[0]
            name = ast.arena.symbols[ast.node(name_node).payload_index]
            if positional_index < positional_args.size
              env[name] = positional_args[positional_index]
              positional_index += 1
            elsif val = trailing_named.delete(name)
              env[name] = val
            else
              default_node = ast.children(param_id)[2]?
              if default_node && (val = eval_value(default_node, ast))
                env[name] = val
              else
                env[name] = nil
              end
            end
          when NodeKind::Splat
            name = splat_name(param_id, ast)
            splat_values = positional_args[positional_index..-1]? || [] of MacroValue
            positional_index = positional_args.size
            env[name] = splat_values if name
          when NodeKind::DoubleSplat
            name = splat_name(param_id, ast)
            if name
              env[name] = trailing_named.dup
              trailing_named.clear
            end
          when NodeKind::BlockParam
            # ignore
          end
        end

        env
      end

      private def splat_name(param_id : NodeId, ast : AstFile) : String?
        node = ast.node(param_id)
        return ast.arena.symbols[node.payload_index] if node.payload_index >= 0
        type_node = ast.children(param_id)[0]?
        return nil unless type_node
        child = ast.node(type_node)
        return nil unless child.kind == NodeKind::Ident
        ast.arena.symbols[child.payload_index]
      end

      private def fingerprint_args(args : {Array(MacroValue), Hash(String, MacroValue)}) : String
        positional, named = args
        pos_fp = positional.map { |v| fingerprint_value(v) }.join("|")
        named_fp = named.keys.sort.map { |k| "#{k}=#{fingerprint_value(named[k])}" }.join("|")
        "#{pos_fp}||#{named_fp}"
      end

      private def fingerprint_value(value : MacroValue) : String
        case value
        when Nil
          "nil"
        when Bool
          value ? "true" : "false"
        when Int64
          value.to_s
        when String
          value
        when Array(MacroValue)
          "[" + value.map { |v| fingerprint_value(v) }.join(",") + "]"
        when Hash(String, MacroValue)
          "{" + value.keys.sort.map { |k| "#{k}:#{fingerprint_value(value[k])}" }.join(",") + "}"
        else
          value.to_s
        end
      end

      private def fingerprint_text(source : Source, span : Span) : String
        slice_text(source, span).hash.to_s
      end

      private def collect_idents(node_id : NodeId, ast : AstFile, acc : Hash(String, Bool) = {} of String => Bool) : Hash(String, Bool)
        node = ast.node(node_id)
        if node.kind == NodeKind::Ident
          name = ast.arena.symbols[node.payload_index]
          acc[name] = true
        end
        ast.children(node_id).each do |child|
          collect_idents(child, ast, acc)
        end
        acc
      end

      private def collect_ident_spans(node_id : NodeId, ast : AstFile, acc : Array(Tuple(Span, String)) = [] of Tuple(Span, String)) : Array(Tuple(Span, String))
        node = ast.node(node_id)
        if node.kind == NodeKind::Ident
          name = ast.arena.symbols[node.payload_index]
          acc << {node.span, name}
        end
        ast.children(node_id).each do |child|
          collect_ident_spans(child, ast, acc)
        end
        acc
      end

      private def write_with_replacements(
        bytes : Bytes,
        start_pos : Int32,
        end_pos : Int32,
        replacements : Array(Tuple(Span, String)),
        builder : String::Builder,
        rep_index : Int32
      ) : Int32
        i = start_pos
        idx = rep_index
        while idx < replacements.size
          span, text = replacements[idx]
          break if span.start >= end_pos
          if span.start > i
            builder.write bytes[i, span.start - i]
          end
          builder << text
          i = span.finish
          idx += 1
        end
        if i < end_pos
          builder.write bytes[i, end_pos - i]
        end
        i
      end

      private def expand_macro_var(node_id : NodeId, ast : AstFile) : String?
        env = @env_stack.last?
        return nil unless env
        node = ast.node(node_id)
        name = ast.arena.symbols[node.payload_index]
        if val = env[name]?
          val_to_string(val)
        end
      end

      private def expand_macro_control(node_id : NodeId, ast : AstFile) : String?
        tag = ast.macro_control_tag(node_id)
        children = ast.children(node_id)
        case tag
        when TokenKind::KeywordIf, TokenKind::KeywordUnless
          header = children[0]
          then_body = children[1]
          else_body = children.size > 2 ? children[2] : nil
          cond = eval_truthy(header, ast)
          cond = !cond if tag == TokenKind::KeywordUnless
          body_span = cond ? ast.node(then_body).span : (else_body ? ast.node(else_body).span : nil)
          body_span ? slice_text(ast.source, body_span) : ""
        when TokenKind::KeywordFor
          header = children[0]
          body = children[1]?
          return "" unless body
          count = eval_for_count(header, ast)
          text = slice_text(ast.source, ast.node(body).span)
          String.build do |io|
            count.times { io << text }
          end
        else
          @diagnostics << Diagnostic.new(ast.node(node_id).span, "unsupported macro control tag #{tag}")
          nil
        end
      end

      private def eval_truthy(node_id : NodeId, ast : AstFile) : Bool
        node = ast.node(node_id)
        case node.kind
        when NodeKind::Expressions
          children = ast.children(node_id)
          return false if children.empty?
          return eval_truthy(children.first, ast)
        when NodeKind::LiteralBool
          node.flags == 1
        when NodeKind::LiteralNil
          false
        when NodeKind::LiteralNumber
          text = ast.node_string(node_id)
          text != "0"
        else
          true
        end
      end

      private def eval_for_count(header_id : NodeId, ast : AstFile) : Int32
        node = ast.node(header_id)
        return 0 unless node.kind == NodeKind::MacroForHeader
        parts = ast.macro_for_header_parts(header_id)
        return 0 unless parts
        _, iter_id = parts
        iter_node = ast.node(iter_id)
        case iter_node.kind
        when NodeKind::Array, NodeKind::Tuple
          ast.children(iter_id).size
        else
          0
        end
      end

      private def eval_to_text(node_id : NodeId, ast : AstFile) : String?
        if value = eval_value(node_id, ast)
          return val_to_string(value)
        end
        slice_text(ast.source, ast.node(node_id).span)
      end

      private def eval_value(node_id : NodeId, ast : AstFile) : MacroValue?
        node = ast.node(node_id)
        case node.kind
        when NodeKind::LiteralString
          text = ast.node_string(node_id)
          return text[1, text.size - 2] if text.size >= 2 && text.starts_with?("\"") && text.ends_with?("\"")
          text
        when NodeKind::LiteralNumber
          str = ast.node_string(node_id).delete('_')
          str.to_i64? || str
        when NodeKind::LiteralChar, NodeKind::LiteralRegex
          ast.node_string(node_id)
        when NodeKind::LiteralBool
          node.flags == 1
        when NodeKind::LiteralNil
          nil
        when NodeKind::Ident
          name = ast.arena.symbols[node.payload_index]
          if env = @env_stack.last?
            return env[name] if env.has_key?(name)
          end
          name
        when NodeKind::Binary
          left_id, right_id = ast.children(node_id)
          left = eval_value(left_id, ast)
          right = eval_value(right_id, ast)
          return nil unless left && right
          op = ast.arena.operator_kind(node.payload_index)
          eval_binary(op, left, right)
        when NodeKind::Range
          left_id, right_id = ast.children(node_id)
          left = eval_value(left_id, ast)
          right = eval_value(right_id, ast)
          return nil unless left.is_a?(Int64) && right.is_a?(Int64)
          exclusive = node.flags == 1
          Range.new(left, exclusive ? right - 1 : right).to_a.map { |i| i.to_i64.as(MacroValue) }
        when NodeKind::Call
          callee_id = ast.children(node_id)[0]
          callee = ast.node(callee_id)
          if callee.kind == NodeKind::Ident
            name = ast.arena.symbols[callee.payload_index]
            if name == "gensym"
              args = ast.children(node_id)[1]?
              base = "tmp"
              if args && (first = ast.children(args).first?)
                if val = eval_value(first, ast)
                  base = val_to_string(val)
                end
              end
              sym_id = @hygiene.gensym(base, ast.arena.symbols)
              return ast.arena.symbols[sym_id]
            end
          end
          nil
        when NodeKind::Unary
          child_id = ast.children(node_id)[0]
          val = eval_value(child_id, ast)
          return nil unless val
          op = ast.arena.operator_kind(node.payload_index)
          eval_unary(op, val)
        when NodeKind::Expressions
          children = ast.children(node_id)
          children.empty? ? nil : eval_value(children.first, ast)
        when NodeKind::Array
          values = [] of MacroValue
          ast.children(node_id).each do |cid|
            if val = eval_value(cid, ast)
              values << val
            end
          end
          values
        when NodeKind::Hash
          h = {} of String => MacroValue
          ast.children(node_id).each do |cid|
            child = ast.node(cid)
            if child.kind == NodeKind::Binary && ast.arena.operator_kind(child.payload_index) == TokenKind::HashRocket
              key_id, val_id = ast.children(cid)
              key = eval_to_text(key_id, ast)
              val = eval_value(val_id, ast)
              h[key.not_nil!] = val.not_nil! if key && val
            end
          end
          h
        else
          nil
        end
      end

      private def eval_binary(op : TokenKind, left : MacroValue, right : MacroValue) : MacroValue?
        case op
        when TokenKind::Plus
          if left.is_a?(Int64) && right.is_a?(Int64)
            left + right
          else
            val_to_string(left) + val_to_string(right)
          end
        when TokenKind::Minus
          if left.is_a?(Int64) && right.is_a?(Int64)
            left - right
          end
        when TokenKind::Star
          if left.is_a?(Int64) && right.is_a?(Int64)
            left * right
          end
        when TokenKind::Slash
          if left.is_a?(Int64) && right.is_a?(Int64) && right != 0
            left // right
          end
        when TokenKind::Percent
          if left.is_a?(Int64) && right.is_a?(Int64) && right != 0
            left % right
          end
        when TokenKind::EqualEqual
          left == right
        when TokenKind::BangEqual
          left != right
        when TokenKind::AndAnd
          truthy?(left) && truthy?(right)
        when TokenKind::OrOr
          truthy?(left) || truthy?(right)
        else
          nil
        end
      end

      private def eval_unary(op : TokenKind, value : MacroValue) : MacroValue?
        case op
        when TokenKind::Plus
          value if value.is_a?(Int64)
        when TokenKind::Minus
          value.is_a?(Int64) ? -value : nil
        when TokenKind::Bang
          !truthy?(value)
        else
          nil
        end
      end

      private def truthy?(value : MacroValue) : Bool
        case value
        when Nil
          false
        when Bool
          value
        when Int64
          value != 0
        when String
          !value.empty?
        else
          true
        end
      end

      private def val_to_string(value : MacroValue) : String
        case value
        when Nil
          "nil"
        when Bool
          value ? "true" : "false"
        when Int64
          value.to_s
        when String
          value
        when Array(MacroValue)
          value.map { |v| val_to_string(v) }.join(",")
        when Hash(String, MacroValue)
          value.map { |k, v| "#{k}=#{val_to_string(v)}" }.join(",")
        else
          value.to_s
        end
      end

      private def slice_text(source : Source, span : Span) : String
        String.new(source.bytes[span.start, span.length])
      end
    end
  end
end
