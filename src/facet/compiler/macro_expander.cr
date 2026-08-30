require "set"
require "./hygiene"
require "./macro_footprint"

module Facet
  module Compiler
    alias MacroValue = Int64 | String | Bool | Nil | Array(MacroValue) | Hash(String, MacroValue)

    # Keep a successful `nil` or `false` macro value distinct from a failed
    # evaluation. A bare MacroValue? cannot make that distinction in Crystal.
    record MacroEvaluation, value : MacroValue

    class MacroExpander
      getter diagnostics : Array(Diagnostic)
      getter cache_hits : Int32
      getter last_footprint : MacroFootprint?

      def initialize(@index : ProgramIndex? = nil, @max_passes : Int32 = 8)
        @diagnostics = [] of Diagnostic
        @env_stack = [] of Hash(String, MacroValue)
        @root_env = {} of String => MacroValue
        @macro_var_stack = [] of Hash(String, String)
        @root_macro_vars = {} of String => String
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
        @root_macro_vars.clear
        @root_env.clear
        passes = 0
        seen_texts = Set(String).new
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

          expanded_text = expand_text(current_ast, macros, idx, nil, footprint)
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
        footprint : MacroFootprint? = nil,
      ) : String
        macros.sort_by! { |id| ast.node(id).span.start }
        start_pos = slice_span ? slice_span.start : 0
        end_pos = slice_span ? slice_span.finish : ast.source.size

        src = ast.source
        bytes = src.bytes
        builder = String::Builder.new
        last = start_pos

        macros.each do |id|
          span = ast.node(id).span
          next if span.start < start_pos || span.start >= end_pos
          builder.write(bytes[last, span.start - last]) if span.start > last
          expansion = expand_macro(id, ast, index, footprint) || ""
          builder << expansion
          last = span.finish
        end

        if last < end_pos
          builder.write(bytes[last, end_pos - last])
        end
        builder.to_s
      end

      private def collect_macros(node_id : NodeId, ast : AstFile, acc : Array(NodeId))
        node = ast.node(node_id)
        # A macro definition is a template, not an expansion site. Its body is
        # traversed explicitly by `expand_macro_def` only when the macro is used.
        return if node.kind == NodeKind::MacroDef
        if node.kind == NodeKind::MacroExpr || node.kind == NodeKind::MacroControl || node.kind == NodeKind::MacroVar
          acc << node_id
          return
        end
        ast.children(node_id).each do |child|
          collect_macros(child, ast, acc)
        end
      end

      private def expand_macro(node_id : NodeId, ast : AstFile, index : ProgramIndex?, footprint : MacroFootprint?) : String?
        node = ast.node(node_id)
        case node.kind
        when NodeKind::MacroExpr
          expand_macro_expr(node_id, ast, index, footprint)
        when NodeKind::MacroControl
          expand_macro_control(node_id, ast, index, footprint)
        when NodeKind::MacroVar
          expand_macro_var(node_id, ast)
        else
          nil
        end
      end

      private def expand_macro_expr(node_id : NodeId, ast : AstFile, index : ProgramIndex?, footprint : MacroFootprint?) : String?
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
            cacheable = macro_cacheable?(refs.first)
            if cacheable
              if cached = @cache[key]?
                @cache_hits += 1
                return cached
              end
            end
            result = expand_macro_def(refs.first, call_args, index, footprint)
            @cache[key] = result if result && cacheable
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
            if evaluation = eval_value(value_id, ast)
              name = ast.arena.symbols[arg_node.payload_index]
              named[name] = evaluation.value
            end
          else
            if evaluation = eval_value(arg_id, ast)
              positional << evaluation.value
            else
              @diagnostics << Diagnostic.new(arg_node.span, "unsupported macro argument")
            end
          end
        end
        {positional, named}
      end

      private def expand_macro_def(ref : DeclRef, args : {Array(MacroValue), Hash(String, MacroValue)}, index : ProgramIndex?, footprint : MacroFootprint?) : String
        params_id = ref.ast.children(ref.node_id)[1]?
        body_id = ref.ast.children(ref.node_id)[3]?
        return "" unless body_id && params_id
        env = build_param_env(params_id, ref.ast, args)
        @env_stack << env
        @macro_var_stack << {} of String => String
        text = begin
          body_span = ref.ast.node(body_id).span
          macros = [] of NodeId
          collect_macros(body_id, ref.ast, macros)
          expand_text(ref.ast, macros, index, body_span, footprint)
        ensure
          @macro_var_stack.pop
          @env_stack.pop
        end
        footprint.try &.merge_requires(ref.ast.source)
        text
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

      private def macro_cacheable?(ref : DeclRef) : Bool
        body_id = ref.ast.children(ref.node_id)[3]?
        body_id ? !contains_hygienic_macro_value?(body_id, ref.ast) : true
      end

      private def contains_hygienic_macro_value?(node_id : NodeId, ast : AstFile) : Bool
        node = ast.node(node_id)
        return true if node.kind == NodeKind::MacroVar
        if node.kind == NodeKind::Ident && ast.arena.symbols[node.payload_index] == "gensym"
          return true
        end
        ast.children(node_id).any? { |child| contains_hygienic_macro_value?(child, ast) }
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
            elsif trailing_named.has_key?(name)
              env[name] = trailing_named.delete(name)
            else
              default_node = ast.children(param_id)[2]?
              if default_node && (evaluation = eval_value(default_node, ast))
                env[name] = evaluation.value
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

      private def expand_macro_var(node_id : NodeId, ast : AstFile) : String?
        node = ast.node(node_id)
        name = ast.arena.symbols[node.payload_index]
        arguments = ast.children(node_id).map do |argument|
          if evaluation = eval_value(argument, ast)
            fingerprint_value(evaluation.value)
          else
            ast.node_string(argument)
          end
        end
        key = "#{name}{#{arguments.join(",")}}"
        vars = @macro_var_stack.last? || @root_macro_vars
        return vars[key] if vars.has_key?(key)
        symbol_id = @hygiene.gensym(name, ast.arena.symbols)
        vars[key] = ast.arena.symbols[symbol_id]
      end

      private def expand_macro_control(node_id : NodeId, ast : AstFile, index : ProgramIndex?, footprint : MacroFootprint?) : String?
        tag = ast.macro_control_tag(node_id)
        children = ast.children(node_id)
        case tag
        when TokenKind::KeywordIf, TokenKind::KeywordUnless
          header = children[0]
          then_body = children[1]
          else_body = children.size > 2 ? children[2] : nil
          cond = eval_truthy(header, ast)
          cond = !cond if tag == TokenKind::KeywordUnless
          body = cond ? then_body : else_body
          body ? expand_template_body(body, ast, index, footprint) : ""
        when TokenKind::KeywordFor
          header = children[0]
          body = children[1]?
          return "" unless body
          parts = ast.macro_for_header_parts(header)
          return "" unless parts
          targets = ast.macro_for_targets(header)
          return "" unless targets
          iterable = eval_value(parts[1], ast)
          return "" unless iterable

          String.build do |io|
            case value = iterable.value
            when Array(MacroValue)
              value.each_with_index do |item, item_index|
                io << expand_macro_iteration(targets, ast, item, item_index.to_i64) do
                  expand_template_body(body, ast, index, footprint)
                end
              end
            when Hash(String, MacroValue)
              value.each do |key, item|
                io << expand_macro_iteration(targets, ast, key, item) do
                  expand_template_body(body, ast, index, footprint)
                end
              end
            else
              @diagnostics << Diagnostic.new(ast.node(parts[1]).span, "macro for iterable must be an array, tuple, range, or hash")
            end
          end
        when TokenKind::KeywordBegin
          body = children[1]?
          body ? expand_template_body(body, ast, index, footprint) : ""
        when TokenKind::KeywordVerbatim
          body = children[1]?
          body ? slice_text(ast.source, ast.node(body).span) : ""
        when TokenKind::Unknown
          header = children[0]?
          return "" unless header
          unless eval_value(header, ast)
            @diagnostics << Diagnostic.new(ast.node(header).span, "unsupported macro control expression")
          end
          ""
        else
          @diagnostics << Diagnostic.new(ast.node(node_id).span, "unsupported macro control tag #{tag}")
          nil
        end
      end

      private def expand_template_body(body_id : NodeId, ast : AstFile, index : ProgramIndex?, footprint : MacroFootprint?) : String
        body_span = ast.node(body_id).span
        macros = [] of NodeId
        collect_macros(body_id, ast, macros)
        return slice_text(ast.source, body_span) if macros.empty?
        expand_text(ast, macros, index, body_span, footprint)
      end

      private def macro_iteration_env(targets : Slice(NodeId), ast : AstFile, first : MacroValue, second : MacroValue) : Hash(String, MacroValue)
        env = current_macro_env.dup
        if target = targets[0]?
          if name = macro_target_name(target, ast)
            env[name] = first
          end
        end
        if target = targets[1]?
          if name = macro_target_name(target, ast)
            env[name] = second
          end
        end
        env
      end

      private def expand_macro_iteration(targets : Slice(NodeId), ast : AstFile, first : MacroValue, second : MacroValue, & : -> String) : String
        parent = current_macro_env
        env = macro_iteration_env(targets, ast, first, second)
        target_names = targets.compact_map { |target| macro_target_name(target, ast) }
        text = expand_with_env(env) { yield }
        env.each do |name, value|
          parent[name] = value unless target_names.includes?(name)
        end
        text
      end

      private def macro_target_name(node_id : NodeId, ast : AstFile) : String?
        node = ast.node(node_id)
        case node.kind
        when NodeKind::Ident, NodeKind::MacroVar
          ast.arena.symbols[node.payload_index]
        else
          nil
        end
      end

      private def expand_with_env(env : Hash(String, MacroValue), & : -> String) : String
        @env_stack << env
        begin
          yield
        ensure
          @env_stack.pop
        end
      end

      private def current_macro_env : Hash(String, MacroValue)
        @env_stack.last? || @root_env
      end

      private def eval_truthy(node_id : NodeId, ast : AstFile) : Bool
        evaluation = eval_value(node_id, ast)
        evaluation ? truthy?(evaluation.value) : false
      end

      private def eval_to_text(node_id : NodeId, ast : AstFile) : String?
        if evaluation = eval_value(node_id, ast)
          return val_to_string(evaluation.value)
        end
        slice_text(ast.source, ast.node(node_id).span)
      end

      private def eval_value(node_id : NodeId, ast : AstFile) : MacroEvaluation?
        node = ast.node(node_id)
        case node.kind
        when NodeKind::LiteralString
          content : String = ast.decoded_literal_string(node_id)
          MacroEvaluation.new(content)
        when NodeKind::LiteralNumber
          str = ast.node_string(node_id).delete('_')
          MacroEvaluation.new(str.to_i64? || str)
        when NodeKind::LiteralChar, NodeKind::LiteralRegex
          MacroEvaluation.new(ast.node_string(node_id))
        when NodeKind::LiteralBool
          MacroEvaluation.new(node.flags == 1)
        when NodeKind::LiteralNil
          MacroEvaluation.new(nil)
        when NodeKind::Ident
          name = ast.arena.symbols[node.payload_index]
          env = current_macro_env
          return MacroEvaluation.new(env[name]) if env.has_key?(name)
          MacroEvaluation.new(name)
        when NodeKind::MacroVar
          value = expand_macro_var(node_id, ast)
          value ? MacroEvaluation.new(value) : nil
        when NodeKind::Binary
          left_id, right_id = ast.children(node_id)
          op = ast.arena.operator_kind(node.payload_index)
          if assignment_operator?(op)
            return eval_macro_compound_assignment(op, left_id, right_id, ast)
          end
          if op == TokenKind::Dot || op == TokenKind::SafeNav
            return eval_macro_member(left_id, right_id, ast, safe: op == TokenKind::SafeNav)
          end
          left = eval_value(left_id, ast)
          return nil unless left
          if op == TokenKind::AndAnd
            return left unless truthy?(left.value)
            return eval_value(right_id, ast)
          elsif op == TokenKind::OrOr
            return left if truthy?(left.value)
            return eval_value(right_id, ast)
          end
          right = eval_value(right_id, ast)
          return nil unless right
          eval_binary(op, left.value, right.value)
        when NodeKind::Assign
          left_id, right_id = ast.children(node_id)
          evaluation = eval_value(right_id, ast)
          return nil unless evaluation
          assign_macro_value(left_id, evaluation.value, ast)
          evaluation
        when NodeKind::Range
          left_id, right_id = ast.children(node_id)
          left = eval_value(left_id, ast)
          right = eval_value(right_id, ast)
          return nil unless left && right
          left_value = left.value
          right_value = right.value
          return nil unless left_value.is_a?(Int64) && right_value.is_a?(Int64)
          exclusive = node.flags == 1
          values = Range.new(left_value, exclusive ? right_value - 1 : right_value).to_a.map { |i| i.to_i64.as(MacroValue) }
          MacroEvaluation.new(values)
        when NodeKind::Call
          callee_id = ast.children(node_id)[0]
          callee = ast.node(callee_id)
          if callee.kind == NodeKind::Ident
            name = ast.arena.symbols[callee.payload_index]
            if name == "gensym"
              args = ast.children(node_id)[1]?
              base = "tmp"
              if args && (first = ast.children(args).first?)
                if evaluation = eval_value(first, ast)
                  base = val_to_string(evaluation.value)
                end
              end
              sym_id = @hygiene.gensym(base, ast.arena.symbols)
              return MacroEvaluation.new(ast.arena.symbols[sym_id])
            end
          end
          nil
        when NodeKind::Index
          children = ast.children(node_id)
          return nil if children.size < 2
          receiver = eval_value(children[0], ast)
          index = eval_value(children[1], ast)
          return nil unless receiver && index
          eval_macro_index(receiver.value, index.value)
        when NodeKind::Unary
          child_id = ast.children(node_id)[0]
          evaluation = eval_value(child_id, ast)
          return nil unless evaluation
          op = ast.arena.operator_kind(node.payload_index)
          eval_unary(op, evaluation.value)
        when NodeKind::Expressions
          children = ast.children(node_id)
          result = MacroEvaluation.new(nil)
          children.each do |child|
            evaluation = eval_value(child, ast)
            return nil unless evaluation
            result = evaluation
          end
          result
        when NodeKind::If, NodeKind::Unless, NodeKind::Ternary
          children = ast.children(node_id)
          return nil if children.size < 2
          condition = eval_truthy(children[0], ast)
          condition = !condition if node.kind == NodeKind::Unless
          branch = condition ? children[1]? : children[2]?
          branch ? eval_value(branch, ast) : MacroEvaluation.new(nil)
        when NodeKind::Nop
          MacroEvaluation.new(nil)
        when NodeKind::Array, NodeKind::Tuple
          values = [] of MacroValue
          ast.children(node_id).each do |cid|
            if evaluation = eval_value(cid, ast)
              values << evaluation.value
            end
          end
          MacroEvaluation.new(values)
        when NodeKind::NamedTuple
          values = {} of String => MacroValue
          ast.children(node_id).each do |cid|
            child = ast.node(cid)
            next unless child.kind == NodeKind::NamedArg
            value_id = ast.children(cid)[0]
            if evaluation = eval_value(value_id, ast)
              name = ast.arena.symbols[child.payload_index]
              values[name] = evaluation.value
            end
          end
          MacroEvaluation.new(values)
        when NodeKind::Hash
          h = {} of String => MacroValue
          ast.children(node_id).each do |cid|
            child = ast.node(cid)
            if child.kind == NodeKind::Binary && ast.arena.operator_kind(child.payload_index) == TokenKind::HashRocket
              key_id, val_id = ast.children(cid)
              key = eval_to_text(key_id, ast)
              evaluation = eval_value(val_id, ast)
              h[key] = evaluation.value if key && evaluation
            end
          end
          MacroEvaluation.new(h)
        else
          nil
        end
      end

      private def eval_macro_member(receiver_id : NodeId, member_id : NodeId, ast : AstFile, safe : Bool) : MacroEvaluation?
        receiver = eval_value(receiver_id, ast)
        return nil unless receiver
        return MacroEvaluation.new(nil) if safe && receiver.value.nil?

        member = ast.node(member_id)
        name = nil
        args = [] of MacroValue
        case member.kind
        when NodeKind::Ident
          name = ast.arena.symbols[member.payload_index]
        when NodeKind::Call
          call_children = ast.children(member_id)
          callee = call_children[0]?
          return nil unless callee
          callee_node = ast.node(callee)
          return nil unless callee_node.kind == NodeKind::Ident
          name = ast.arena.symbols[callee_node.payload_index]
          if args_id = call_children[1]?
            ast.children(args_id).each do |arg_id|
              evaluation = eval_value(arg_id, ast)
              return nil unless evaluation
              args << evaluation.value
            end
          end
        else
          return nil
        end
        apply_macro_method(receiver.value, name, args)
      end

      private def apply_macro_method(receiver : MacroValue, name : String, args : Array(MacroValue)) : MacroEvaluation?
        case name
        when "size"
          size = case receiver
                 when String                   then receiver.size
                 when Array(MacroValue)        then receiver.size
                 when Hash(String, MacroValue) then receiver.size
                 else                               return nil
                 end
          MacroEvaluation.new(size.to_i64)
        when "empty?"
          empty = case receiver
                  when String                   then receiver.empty?
                  when Array(MacroValue)        then receiver.empty?
                  when Hash(String, MacroValue) then receiver.empty?
                  else                               return nil
                  end
          MacroEvaluation.new(empty)
        when "first", "last"
          return nil unless args.empty?
          value = case receiver
                  when String
                    char = name == "first" ? receiver.chars.first? : receiver.chars.last?
                    char ? char.to_s.as(MacroValue) : nil
                  when Array(MacroValue)
                    name == "first" ? receiver.first? : receiver.last?
                  else
                    return nil
                  end
          MacroEvaluation.new(value)
        when "keys"
          return nil unless receiver.is_a?(Hash(String, MacroValue)) && args.empty?
          MacroEvaluation.new(receiver.keys.map(&.as(MacroValue)))
        when "values"
          return nil unless receiver.is_a?(Hash(String, MacroValue)) && args.empty?
          values = receiver.map { |_, value| value.as(MacroValue) }
          MacroEvaluation.new(values)
        when "includes?"
          return nil unless args.size == 1
          result = case receiver
                   when String
                     value = args[0]
                     value.is_a?(String) ? receiver.includes?(value) : false
                   when Array(MacroValue)
                     receiver.includes?(args[0])
                   when Hash(String, MacroValue)
                     key = args[0]
                     key.is_a?(String) ? receiver.has_key?(key) : false
                   else
                     return nil
                   end
          MacroEvaluation.new(result)
        when "starts_with?", "ends_with?"
          return nil unless receiver.is_a?(String) && args.size == 1 && args[0].is_a?(String)
          other = args[0].as(String)
          MacroEvaluation.new(name == "starts_with?" ? receiver.starts_with?(other) : receiver.ends_with?(other))
        when "upcase", "downcase", "capitalize", "strip", "chomp"
          return nil unless receiver.is_a?(String) && args.empty?
          value = case name
                  when "upcase"     then receiver.upcase
                  when "downcase"   then receiver.downcase
                  when "capitalize" then receiver.capitalize
                  when "strip"      then receiver.strip
                  else                   receiver.chomp
                  end
          MacroEvaluation.new(value)
        when "split"
          return nil unless receiver.is_a?(String) && args.size <= 1
          pieces = if args.empty?
                     receiver.split
                   elsif separator = args[0]
                     return nil unless separator.is_a?(String)
                     receiver.split(separator)
                   else
                     return nil
                   end
          MacroEvaluation.new(pieces.map(&.as(MacroValue)))
        when "lines"
          return nil unless receiver.is_a?(String) && args.empty?
          MacroEvaluation.new(receiver.lines.map(&.as(MacroValue)))
        when "join"
          return nil unless receiver.is_a?(Array(MacroValue)) && args.size <= 1
          separator = args.empty? ? "" : args[0]
          return nil unless separator.is_a?(String)
          MacroEvaluation.new(receiver.map { |value| val_to_string(value) }.join(separator))
        when "to_i"
          return nil unless receiver.is_a?(String) && args.size <= 1
          base = args.empty? ? 10_i64 : args[0]
          return nil unless base.is_a?(Int64)
          value = receiver.to_i64?(base.to_i)
          value ? MacroEvaluation.new(value) : nil
        when "id"
          return nil unless receiver.is_a?(String) && args.empty?
          MacroEvaluation.new(receiver)
        else
          nil
        end
      end

      private def eval_macro_index(receiver : MacroValue, index : MacroValue) : MacroEvaluation?
        case receiver
        when Array(MacroValue)
          return nil unless index.is_a?(Int64)
          normalized = index < 0 ? receiver.size.to_i64 + index : index
          return nil unless normalized.in?(0_i64...receiver.size.to_i64)
          MacroEvaluation.new(receiver[normalized])
        when String
          return nil unless index.is_a?(Int64)
          char = receiver.chars[index]?
          char ? MacroEvaluation.new(char.to_s) : nil
        when Hash(String, MacroValue)
          return nil unless index.is_a?(String) && receiver.has_key?(index)
          MacroEvaluation.new(receiver[index])
        else
          nil
        end
      end

      private def assign_macro_value(target_id : NodeId, value : MacroValue, ast : AstFile) : Nil
        target = ast.node(target_id)
        case target.kind
        when NodeKind::Ident
          current_macro_env[ast.arena.symbols[target.payload_index]] = value
        when NodeKind::Tuple
          values = value.is_a?(Array(MacroValue)) ? value : [value] of MacroValue
          ast.children(target_id).each_with_index do |child, index|
            assign_macro_value(child, values[index]? || nil, ast)
          end
        end
      end

      private def eval_macro_compound_assignment(op : TokenKind, left_id : NodeId, right_id : NodeId, ast : AstFile) : MacroEvaluation?
        left = ast.node(left_id)
        return nil unless left.kind == NodeKind::Ident
        name = ast.arena.symbols[left.payload_index]
        env = current_macro_env
        return nil unless env.has_key?(name)
        current = env[name]

        if op == TokenKind::OrOrEqual
          return MacroEvaluation.new(current) if truthy?(current)
        elsif op == TokenKind::AndAndEqual
          return MacroEvaluation.new(current) unless truthy?(current)
        end

        right = eval_value(right_id, ast)
        return nil unless right
        value = if op == TokenKind::OrOrEqual || op == TokenKind::AndAndEqual
                  MacroEvaluation.new(right.value)
                else
                  binary_op = compound_binary_operator(op)
                  return nil unless binary_op
                  eval_binary(binary_op, current, right.value)
                end
        return nil unless value
        env[name] = value.value
        value
      end

      private def assignment_operator?(op : TokenKind) : Bool
        !compound_binary_operator(op).nil? || op == TokenKind::OrOrEqual || op == TokenKind::AndAndEqual
      end

      private def compound_binary_operator(op : TokenKind) : TokenKind?
        case op
        when TokenKind::PlusEqual              then TokenKind::Plus
        when TokenKind::MinusEqual             then TokenKind::Minus
        when TokenKind::StarEqual              then TokenKind::Star
        when TokenKind::SlashEqual             then TokenKind::Slash
        when TokenKind::SlashSlashEqual        then TokenKind::SlashSlash
        when TokenKind::PercentEqual           then TokenKind::Percent
        when TokenKind::PipeEqual              then TokenKind::Pipe
        when TokenKind::AmpersandEqual         then TokenKind::Ampersand
        when TokenKind::CaretEqual             then TokenKind::Caret
        when TokenKind::StarStarEqual          then TokenKind::StarStar
        when TokenKind::ShiftLeftEqual         then TokenKind::ShiftLeft
        when TokenKind::ShiftRightEqual        then TokenKind::ShiftRight
        when TokenKind::AmpersandPlusEqual     then TokenKind::AmpersandPlus
        when TokenKind::AmpersandMinusEqual    then TokenKind::AmpersandMinus
        when TokenKind::AmpersandStarEqual     then TokenKind::AmpersandStar
        when TokenKind::AmpersandStarStarEqual then TokenKind::AmpersandStarStar
        else                                        nil
        end
      end

      private def eval_binary(op : TokenKind, left : MacroValue, right : MacroValue) : MacroEvaluation?
        case op
        when TokenKind::Plus
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left + right)
          else
            MacroEvaluation.new(val_to_string(left) + val_to_string(right))
          end
        when TokenKind::Minus
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left - right)
          end
        when TokenKind::Star
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left * right)
          end
        when TokenKind::Slash
          if left.is_a?(Int64) && right.is_a?(Int64) && right != 0
            MacroEvaluation.new(left // right)
          end
        when TokenKind::Percent
          if left.is_a?(Int64) && right.is_a?(Int64) && right != 0
            MacroEvaluation.new(left % right)
          end
        when TokenKind::Less
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left < right)
          elsif left.is_a?(String) && right.is_a?(String)
            MacroEvaluation.new(left < right)
          end
        when TokenKind::LessEqual
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left <= right)
          elsif left.is_a?(String) && right.is_a?(String)
            MacroEvaluation.new(left <= right)
          end
        when TokenKind::Greater
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left > right)
          elsif left.is_a?(String) && right.is_a?(String)
            MacroEvaluation.new(left > right)
          end
        when TokenKind::GreaterEqual
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left >= right)
          elsif left.is_a?(String) && right.is_a?(String)
            MacroEvaluation.new(left >= right)
          end
        when TokenKind::SlashSlash
          MacroEvaluation.new(left // right) if left.is_a?(Int64) && right.is_a?(Int64) && right != 0
        when TokenKind::StarStar
          MacroEvaluation.new(left ** right) if left.is_a?(Int64) && right.is_a?(Int64) && right >= 0
        when TokenKind::Pipe
          MacroEvaluation.new(left | right) if left.is_a?(Int64) && right.is_a?(Int64)
        when TokenKind::Ampersand
          MacroEvaluation.new(left & right) if left.is_a?(Int64) && right.is_a?(Int64)
        when TokenKind::Caret
          MacroEvaluation.new(left ^ right) if left.is_a?(Int64) && right.is_a?(Int64)
        when TokenKind::ShiftLeft
          MacroEvaluation.new(left << right) if left.is_a?(Int64) && right.is_a?(Int64)
        when TokenKind::ShiftRight
          MacroEvaluation.new(left >> right) if left.is_a?(Int64) && right.is_a?(Int64)
        when TokenKind::EqualEqual
          MacroEvaluation.new(left == right)
        when TokenKind::BangEqual
          MacroEvaluation.new(left != right)
        else
          nil
        end
      end

      private def eval_unary(op : TokenKind, value : MacroValue) : MacroEvaluation?
        case op
        when TokenKind::Plus
          MacroEvaluation.new(value) if value.is_a?(Int64)
        when TokenKind::Minus
          value.is_a?(Int64) ? MacroEvaluation.new(-value) : nil
        when TokenKind::Bang
          MacroEvaluation.new(!truthy?(value))
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
