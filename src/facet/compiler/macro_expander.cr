require "set"
require "./hygiene"
require "./macro_footprint"

module Facet
  module Compiler
    enum MacroSyntaxKind
      StringLiteral
      SymbolLiteral
      CharLiteral
      RegexLiteral
      GeneratedStringLiteral
      GeneratedCharLiteral
      GeneratedSymbolLiteral
      Identifier
      Code
    end

    record MacroSourceLocation, filename : String, line_number : Int32, column_number : Int32

    record MacroCapturedField, source : String, kind : String

    class MacroCapturedNode
      getter source : String
      getter kind : String
      getter fields : Hash(String, MacroCapturedNode)
      getter collections : Hash(String, Array(MacroCapturedNode))
      getter booleans : Hash(String, Bool)
      getter nil_fields : Array(String)

      def initialize(
        @source : String,
        @kind : String,
        @fields = {} of String => MacroCapturedNode,
        @collections = {} of String => Array(MacroCapturedNode),
        @booleans = {} of String => Bool,
        @nil_fields = [] of String,
      )
      end
    end

    record MacroNodeMetadata,
      location : MacroSourceLocation? = nil,
      end_location : MacroSourceLocation? = nil,
      doc : String? = nil,
      fields : Hash(String, MacroCapturedField) = {} of String => MacroCapturedField,
      structure : MacroCapturedNode? = nil

    record MacroSyntaxValue,
      source : String,
      value : String,
      kind : MacroSyntaxKind,
      crystal_kind : String? = nil,
      metadata : MacroNodeMetadata? = nil do
      def self.string(value : String) : self
        new(value.inspect, value, MacroSyntaxKind::StringLiteral)
      end

      def self.symbol(value : String) : self
        source = value.matches?(/\A[a-zA-Z_][a-zA-Z0-9_]*[?!]?\z/) ? ":#{value}" : ":#{value.inspect}"
        new(source, value, MacroSyntaxKind::SymbolLiteral)
      end

      def self.char(value : String, source : String) : self
        new(source, value, MacroSyntaxKind::CharLiteral)
      end

      def self.regex(value : String, source : String) : self
        new(source, value, MacroSyntaxKind::RegexLiteral)
      end

      def self.generated_string(value : String) : self
        new(value.inspect, value, MacroSyntaxKind::GeneratedStringLiteral)
      end

      def self.generated_char(value : String) : self
        new(value.chars.first.inspect, value, MacroSyntaxKind::GeneratedCharLiteral)
      end

      def self.generated_symbol(value : String) : self
        source = value.matches?(/\A[a-zA-Z_][a-zA-Z0-9_]*[?!]?\z/) ? ":#{value}" : ":#{value.inspect}"
        new(source, value, MacroSyntaxKind::GeneratedSymbolLiteral)
      end

      def self.identifier(value : String) : self
        new(value, value, MacroSyntaxKind::Identifier)
      end

      def self.code(value : String) : self
        new(value, value, MacroSyntaxKind::Code)
      end

      def self.captured(source : String, crystal_kind : String, metadata : MacroNodeMetadata) : self
        kind = case crystal_kind.lchop("Crystal::")
               when "StringLiteral" then MacroSyntaxKind::StringLiteral
               when "SymbolLiteral" then MacroSyntaxKind::SymbolLiteral
               when "CharLiteral"   then MacroSyntaxKind::CharLiteral
               when "RegexLiteral"  then MacroSyntaxKind::RegexLiteral
               else                      MacroSyntaxKind::Code
               end
        new(source, source, kind, crystal_kind, metadata)
      end
    end

    record MacroBlockValue, body : String, parameters : Array(String)
    record MacroRangeValue, first : Int64?, last : Int64?, exclusive : Bool

    enum MacroNumberKind
      I8
      I16
      I32
      I64
      I128
      U8
      U16
      U32
      U64
      U128
      F32
      F64
    end

    record MacroNumberValue,
      value : Int128 | UInt128 | Float64,
      kind : MacroNumberKind,
      source : String,
      explicit_kind : Bool

    enum MacroTypeKind
      Class
      Module
      Struct
      Enum
      Lib
      Builtin
    end

    record MacroTypeValue, name : String, kind : MacroTypeKind
    record MacroAnnotationValue,
      name : String,
      positional_sources : Array(String),
      named_sources : Hash(String, String)
    record MacroMetaVarValue,
      name : String,
      type_name : String?,
      default_value : String?,
      annotations : Array(MacroAnnotationValue)
    record MacroMethodValue,
      name : String,
      args : Array(MacroMetaVarValue),
      return_type : String?,
      body : String?,
      source : String,
      annotations : Array(MacroAnnotationValue)

    class MacroTupleValue
    end

    class MacroArrayValue
    end

    class MacroHashEntry
    end

    class MacroHashValue
    end

    alias MacroValue = Int64 | String | Bool | Nil | MacroSyntaxValue | MacroBlockValue | MacroRangeValue | MacroNumberValue | MacroTypeValue | MacroAnnotationValue | MacroMetaVarValue | MacroMethodValue | MacroTupleValue | MacroArrayValue | MacroHashValue | Array(MacroValue) | Hash(String, MacroValue)
    alias MacroArguments = Tuple(Array(MacroValue), Hash(String, MacroValue), MacroBlockValue?)

    class MacroTupleValue
      getter values : Array(MacroValue)

      def initialize(@values : Array(MacroValue))
      end
    end

    # AST collection methods such as `Def#args` return Crystal's untyped
    # ArrayLiteral rendering (`[]`) when empty. Evaluator-created arrays retain
    # the existing `[] of ::NoReturn` representation.
    class MacroArrayValue
      getter values : Array(MacroValue)

      def initialize(@values : Array(MacroValue))
      end
    end

    class MacroHashEntry
      getter key : MacroValue
      property value : MacroValue

      def initialize(@key : MacroValue, @value : MacroValue)
      end
    end

    class MacroHashValue
      getter entries : Array(MacroHashEntry)

      def initialize(@entries : Array(MacroHashEntry))
      end
    end

    # Keep a successful `nil` or `false` macro value distinct from a failed
    # evaluation. A bare MacroValue? cannot make that distinction in Crystal.
    record MacroEvaluation, value : MacroValue

    class MacroExpander
      YIELD_ENV_KEY = "__facet_macro_yield__"

      private record MacroEvalBlock,
        ast : AstFile,
        body_id : NodeId,
        parameters : Array(String)

      private record BuiltinProperty,
        name : String,
        type : String?,
        default : String?

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
        @syntax_trees = {} of UInt64 => SyntaxTree
        @last_footprint = nil
        @active_index = nil.as(ProgramIndex?)
        @active_footprint = nil.as(MacroFootprint?)
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
        previous_index = @active_index
        previous_footprint = @active_footprint
        @active_index = idx
        @active_footprint = footprint

        begin
          loop do
            text_sig = current_ast.source.text
            if seen_texts.includes?(text_sig)
              @diagnostics << Diagnostic.new(Span.new(0, current_ast.source.size), "macro expansion cycle detected")
              break
            end
            seen_texts << text_sig

            macros = [] of NodeId
            collect_macros(current_ast.root, current_ast, macros, idx, footprint: footprint)
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
        ensure
          @active_index = previous_index
          @active_footprint = previous_footprint
        end

        footprint.merge_macro_uses
        @last_footprint = footprint
        @file_cache[cache_key] = current_ast
        current_ast
      end

      # Expands a macro template with already materialized AST-backed values.
      # This is used by compiler passes and parity fixtures that must retain
      # metadata which cannot be reconstructed by parsing rendered source.
      def expand_template(
        body : String,
        arguments : Hash(String, MacroValue),
        filename : String = "macro-template.cr",
      ) : String
        @root_macro_vars.clear
        @root_env.clear
        macro_name = "__facet_template"
        params = arguments.keys.join(", ")
        parser = Parser.new(Source.new("macro #{macro_name}(#{params});#{body};end", filename, SourceKind::Virtual))
        definition = parser.parse_file
        parser.diagnostics.each { |diagnostic| @diagnostics << diagnostic }
        return "" unless parser.diagnostics.empty?

        index = Indexer.index_macros([definition])
        ref = index.macros_for(macro_name, "").try(&.first?)
        return "" unless ref
        positional = [] of MacroValue
        arguments.each_value { |value| positional << value }
        macro_args = {positional, {} of String => MacroValue, nil.as(MacroBlockValue?)}
        previous_index = @active_index
        @active_index = index
        begin
          expand_macro_def(ref, macro_args, index, nil, "").chomp(';')
        ensure
          @active_index = previous_index
        end
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

      private def collect_macros(
        node_id : NodeId,
        ast : AstFile,
        acc : Array(NodeId),
        index : ProgramIndex? = nil,
        ordinary_call_allowed : Bool = true,
        footprint : MacroFootprint? = nil,
      )
        node = ast.node(node_id)
        # A macro definition is a template, not an expansion site. Its body is
        # traversed explicitly by `expand_macro_def` only when the macro is used.
        return if node.kind == NodeKind::MacroDef
        if node.kind == NodeKind::MacroExpr || node.kind == NodeKind::MacroControl || node.kind == NodeKind::MacroVar
          acc << node_id
          return
        end
        if ordinary_call_allowed && {NodeKind::Call, NodeKind::CallWithBlock}.includes?(node.kind)
          if name = macro_call_name(node_id, ast)
            footprint.try(&.macro_use(name))
            if refs = index.try { |value| value.macros_for(name, lexical_scope(node_id, ast)) }
              unless refs.empty?
                acc << node_id
                return
              end
            end
            if builtin_macro_expansion(node_id, ast)
              acc << node_id
              return
            end
          end
        end
        if ordinary_call_allowed && node.kind == NodeKind::Ident && index && bare_macro_identifier?(node_id, ast)
          name = ast.arena.symbols[node.payload_index]
          footprint.try(&.macro_use(name))
          if refs = index.macros_for(name, lexical_scope(node_id, ast))
            unless refs.empty?
              acc << node_id
              return
            end
          end
        end
        ast.children(node_id).each_with_index do |child, child_index|
          child_allows_ordinary_call = !(member_access?(node, ast) && child_index == 1)
          collect_macros(child, ast, acc, index, child_allows_ordinary_call, footprint)
        end
      end

      private def member_access?(node : Node, ast : AstFile) : Bool
        return false unless node.kind == NodeKind::Binary
        operator = ast.arena.operator_kind(node.payload_index)
        {TokenKind::Dot, TokenKind::SafeNav, TokenKind::DoubleColon}.includes?(operator)
      end

      private def bare_macro_identifier?(node_id : NodeId, ast : AstFile) : Bool
        tree = syntax_tree(ast)
        node = tree.node(node_id)
        parent = node.parent
        return false unless parent
        return false if {
                          NodeKind::Def,
                          NodeKind::Fun,
                          NodeKind::Class,
                          NodeKind::Module,
                          NodeKind::Struct,
                          NodeKind::Enum,
                          NodeKind::Lib,
                          NodeKind::Alias,
                          NodeKind::TypeDef,
                          NodeKind::AnnotationDef,
                          NodeKind::Param,
                          NodeKind::Splat,
                          NodeKind::DoubleSplat,
                          NodeKind::BlockParam,
                          NodeKind::Path,
                          NodeKind::TypeApply,
                        }.includes?(parent.kind)
        return false if {NodeKind::Call, NodeKind::CallWithBlock}.includes?(parent.kind) && parent.callee.try(&.id) == node_id
        return false if assignment_target?(node, parent)

        name = node.symbol_name
        return false unless name
        return false if parameter_in_scope?(node, name)
        !assigned_before?(node, name)
      end

      private def assignment_target?(node : SyntaxNode, parent : SyntaxNode) : Bool
        ([parent] + node.ancestors).any? do |ancestor|
          next false unless {NodeKind::Assign, NodeKind::VarDecl}.includes?(ancestor.kind)
          target = ancestor.target
          target && (target.id == node.id || target.descendants.any? { |descendant| descendant.id == node.id })
        end
      end

      private def parameter_in_scope?(node : SyntaxNode, name : String) : Bool
        node.ancestors.any? do |ancestor|
          next false unless {NodeKind::Def, NodeKind::Fun, NodeKind::Block}.includes?(ancestor.kind)
          ancestor.parameters.any? { |parameter| parameter.name == name }
        end
      end

      private def assigned_before?(node : SyntaxNode, name : String) : Bool
        boundary = node.ancestors.find do |ancestor|
          {
            NodeKind::Def,
            NodeKind::Fun,
            NodeKind::Class,
            NodeKind::Module,
            NodeKind::Struct,
            NodeKind::Enum,
            NodeKind::Lib,
          }.includes?(ancestor.kind)
        end
        root = boundary.try(&.body) || node.tree.root
        assigned_before_in?(root, name, node.span.start)
      end

      private def assigned_before_in?(node : SyntaxNode, name : String, offset : Int32) : Bool
        return false if node.span.start >= offset
        if {
             NodeKind::Def,
             NodeKind::Fun,
             NodeKind::MacroDef,
             NodeKind::Class,
             NodeKind::Module,
             NodeKind::Struct,
             NodeKind::Enum,
             NodeKind::Lib,
           }.includes?(node.kind)
          return false
        end
        if target = node.target
          return true if target.symbol_name == name
          return true if target.descendants.any? { |child| child.symbol_name == name }
        end
        node.children.any? { |child| assigned_before_in?(child, name, offset) }
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
        when NodeKind::Call, NodeKind::CallWithBlock
          expand_indexed_macro_call(node_id, ast, index, footprint)
        when NodeKind::Ident
          expand_indexed_macro_call(node_id, ast, index, footprint)
        else
          nil
        end
      end

      private def expand_macro_expr(node_id : NodeId, ast : AstFile, index : ProgramIndex?, footprint : MacroFootprint?) : String?
        body_id = ast.children(node_id)[0]
        exprs = ast.children(body_id)
        return "" if exprs.empty?
        expr = exprs.first
        if expanded = expand_indexed_macro_call(expr, ast, index, footprint)
          return expanded
        end
        eval_to_text(expr, ast)
      end

      private def expand_indexed_macro_call(
        node_id : NodeId,
        ast : AstFile,
        index : ProgramIndex?,
        footprint : MacroFootprint?,
      ) : String?
        name = macro_call_name(node_id, ast)
        return nil unless name
        footprint.try &.macro_use(name)
        call_scope = lexical_scope(node_id, ast)
        refs = index.try { |value| value.macros_for(name, call_scope) }
        return builtin_macro_expansion(node_id, ast) unless refs && !refs.empty?

        call_args = macro_call_args(node_id, ast)
        ref = select_macro_ref(refs, node_id, ast)
        key = cache_key(ref, call_args, call_scope)
        cacheable = macro_cacheable?(ref)
        if cacheable
          if cached = @cache[key]?
            @cache_hits += 1
            return cached
          end
        end
        result = expand_macro_def(ref, call_args, index, footprint, call_scope)
        @cache[key] = result if cacheable
        result
      end

      private def select_macro_ref(refs : Array(DeclRef), call_id : NodeId, call_ast : AstFile) : DeclRef
        call_arity = syntax_tree(call_ast).node(call_id).arguments.size
        refs.find do |ref|
          tree = syntax_tree(ref.ast)
          parameters = tree.node(ref.node_id).parameters
          required = parameters.count do |parameter|
            parameter.kind == NodeKind::Param && parameter.value.nil?
          end
          variadic = parameters.any? { |parameter| {NodeKind::Splat, NodeKind::DoubleSplat}.includes?(parameter.kind) }
          maximum = variadic ? nil : parameters.count { |parameter| parameter.kind != NodeKind::DoubleSplat && parameter.kind != NodeKind::BlockParam }
          call_arity >= required && (maximum.nil? || call_arity <= maximum)
        end || refs.first
      end

      private def lexical_scope(node_id : NodeId, ast : AstFile) : String
        names = [] of String
        node = syntax_tree(ast).node(node_id)
        node.ancestors.reverse_each do |ancestor|
          next unless {NodeKind::Class, NodeKind::Module, NodeKind::Struct, NodeKind::Enum, NodeKind::Lib}.includes?(ancestor.kind)
          name = ancestor.name
          next unless name
          normalized = name.lchop("::")
          names = if name.starts_with?("::") || normalized.includes?("::")
                    [normalized]
                  else
                    names + [normalized]
                  end
        end
        names.join("::")
      end

      private def syntax_tree(ast : AstFile) : SyntaxTree
        key = ast.arena.object_id
        @syntax_trees[key] ||= SyntaxTree.new(ast)
      end

      private def builtin_macro_expansion(node_id : NodeId, ast : AstFile) : String?
        name = macro_call_name(node_id, ast)
        return nil unless name
        call = syntax_tree(ast).node(node_id)
        case name
        when "record"
          expand_builtin_record(call)
        when "getter", "getter?", "getter!", "setter", "property", "property?", "property!",
             "class_getter", "class_getter?", "class_getter!", "class_setter",
             "class_property", "class_property?", "class_property!"
          expand_builtin_accessors(name, call)
        else
          nil
        end
      end

      private def expand_builtin_accessors(name : String, call : SyntaxNode) : String?
        properties = call.arguments.compact_map { |argument| builtin_property(argument) }
        return nil if properties.empty?
        class_accessor = name.starts_with?("class_")
        base = class_accessor ? name.lchop("class_") : name

        String.build do |io|
          properties.each do |property|
            case base
            when "getter"
              write_builtin_getter(io, property, class_accessor, "")
            when "getter?"
              write_builtin_getter(io, property, class_accessor, "?")
            when "getter!"
              write_builtin_getter(io, property, class_accessor, "?", nilable: true)
              write_builtin_getter(io, property, class_accessor, "")
            when "setter"
              write_builtin_setter(io, property, class_accessor)
            when "property"
              write_builtin_getter(io, property, class_accessor, "")
              write_builtin_setter(io, property, class_accessor)
            when "property?"
              write_builtin_getter(io, property, class_accessor, "?")
              write_builtin_setter(io, property, class_accessor)
            when "property!"
              write_builtin_getter(io, property, class_accessor, "?", nilable: true)
              write_builtin_getter(io, property, class_accessor, "")
              write_builtin_setter(io, property, class_accessor)
            end
          end
        end
      end

      private def write_builtin_getter(
        io : IO,
        property : BuiltinProperty,
        class_accessor : Bool,
        suffix : String,
        nilable : Bool = false,
      ) : Nil
        receiver = class_accessor ? "self." : ""
        variable = class_accessor ? "@@#{property.name}" : "@#{property.name}"
        return_type = property.type
        return_type = "#{return_type}?" if nilable && return_type
        io << "def " << receiver << property.name << suffix
        io << " : " << return_type if return_type
        io << '\n' << "  " << variable << '\n' << "end\n"
      end

      private def write_builtin_setter(io : IO, property : BuiltinProperty, class_accessor : Bool) : Nil
        receiver = class_accessor ? "self." : ""
        variable = class_accessor ? "@@#{property.name}" : "@#{property.name}"
        io << "def " << receiver << property.name << "=(value"
        io << " : " << property.type if property.type
        io << ")\n  " << variable << " = value\nend\n"
      end

      private def expand_builtin_record(call : SyntaxNode) : String?
        arguments = call.arguments
        name_node = arguments.first?
        return nil unless name_node
        record_name = builtin_identifier(name_node)
        return nil unless record_name
        properties = arguments.skip(1).compact_map { |argument| builtin_property(argument) }

        String.build do |io|
          io << "struct " << record_name << '\n'
          properties.each do |property|
            io << "  getter " << property.name
            io << " : " << property.type if property.type
            io << '\n'
          end
          io << "  def initialize("
          properties.each_with_index do |property, index|
            io << ", " if index > 0
            io << '@' << property.name
            io << " : " << property.type if property.type
            io << " = " << property.default if property.default
          end
          io << ")\n  end\nend\n"
        end
      end

      private def builtin_property(node : SyntaxNode) : BuiltinProperty?
        target = node
        type = nil.as(String?)
        default = nil.as(String?)
        if node.kind == NodeKind::VarDecl
          target = node.target || node
          type = node.declared_type.try(&.text)
          default = node.value.try(&.text)
        elsif node.kind == NodeKind::Assign
          target = node.target || node
          if target.kind == NodeKind::VarDecl
            type = target.declared_type.try(&.text)
            target = target.target || target
          end
          default = node.value.try(&.text)
        end
        name = builtin_identifier(target)
        return nil unless name
        BuiltinProperty.new(name.lstrip('@'), type, default)
      end

      private def builtin_identifier(node : SyntaxNode) : String?
        value = case node.kind
                when NodeKind::LiteralSymbol
                  node.tree.ast.decoded_literal_string(node.id)
                when NodeKind::Ident, NodeKind::Const, NodeKind::InstanceVar, NodeKind::ClassVar,
                     NodeKind::Path, NodeKind::TypeApply
                  node.symbol_name || node.name || node.text
                when NodeKind::Call
                  node.receiver ? nil : node.call_name
                else
                  nil
                end
        return nil unless value
        normalized = value.lchop(":").lstrip('@')
        normalized.empty? ? nil : normalized
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
        when NodeKind::CallWithBlock
          call_id = ast.children(node_id)[0]?
          call_id ? macro_call_name(call_id, ast) : nil
        else
          nil
        end
      end

      private def macro_call_args(node_id : NodeId, ast : AstFile) : MacroArguments
        node = ast.node(node_id)
        positional = [] of MacroValue
        named = {} of String => MacroValue
        if node.kind == NodeKind::CallWithBlock
          call = syntax_tree(ast).node(node_id)
          block = MacroBlockValue.new(
            call.body.try(&.text) || "",
            call.parameters.compact_map(&.name)
          )
          call_id = ast.children(node_id)[0]?
          if call_id
            call_positional, call_named, _ = macro_call_args(call_id, ast)
            return {call_positional, call_named, block}
          end
          return {positional, named, block}
        end
        return {positional, named, nil} unless node.kind == NodeKind::Call
        args_id = ast.children(node_id)[1]?
        return {positional, named, nil} unless args_id
        ast.children(args_id).each do |arg_id|
          arg_node = ast.node(arg_id)
          if arg_node.kind == NodeKind::NamedArg
            value_id = ast.children(arg_id)[0]
            name = ast.arena.symbols[arg_node.payload_index]
            named[name] = macro_argument_value(value_id, ast)
          else
            positional << macro_argument_value(arg_id, ast)
          end
        end
        {positional, named, nil}
      end

      # Crystal macro parameters are AST nodes. The lightweight evaluator uses
      # scalar values when it understands an expression, but an unsupported
      # expression must still survive as source-backed syntax for `{{arg}}`
      # substitution instead of being silently discarded.
      private def macro_argument_value(node_id : NodeId, ast : AstFile) : MacroValue
        if value = macro_structured_argument_value(node_id, ast)
          return value
        end
        evaluation = eval_value(node_id, ast)
        evaluation ? evaluation.value : MacroSyntaxValue.code(ast.node_string(node_id))
      end

      private def macro_structured_argument_value(node_id : NodeId, ast : AstFile) : MacroSyntaxValue?
        node = syntax_tree(ast).node(node_id)
        if value = macro_structured_type_syntax_argument_value(node, ast)
          return value
        end
        unless macro_structured_call_node?(node, ast)
          return macro_structured_case_argument_value(node) if node.kind == NodeKind::Case
          return macro_structured_exception_argument_value(node, ast) if macro_exception_handler_node?(node)
          if node.kind == NodeKind::Rescue && !node.semantic_flag?(SemanticFlag::RescueClause)
            return macro_structured_inline_rescue_argument_value(node)
          end
          return macro_structured_inline_ensure_argument_value(node) if node.kind == NodeKind::Ensure && node.children.size == 2
          if {NodeKind::Def, NodeKind::MacroDef, NodeKind::Fun}.includes?(node.kind)
            return macro_structured_declaration_argument_value(node)
          end
          if {NodeKind::Class, NodeKind::Module, NodeKind::Struct, NodeKind::Enum,
              NodeKind::Lib, NodeKind::AnnotationDef}.includes?(node.kind)
            return macro_structured_type_declaration_argument_value(node)
          end
          return macro_structured_asm_argument_value(node) if node.kind == NodeKind::Asm
          return macro_structured_asm_operand_argument_value(node) if node.kind == NodeKind::AsmOperand
          return nil
        end

        fields = {} of String => MacroCapturedNode
        fields["receiver"] = macro_captured_syntax_node(node.receiver)
        fields["block_arg"] = if block_arg = node.arguments.find { |argument| macro_call_block_argument?(argument) }
                                name = block_arg.name || block_arg.text.lstrip.lchop('&')
                                MacroCapturedNode.new(name, "Crystal::Arg")
                              else
                                macro_captured_syntax_node(nil)
                              end
        fields["block"] = macro_captured_call_block(node, ast)

        positional = node.arguments.reject do |argument|
          argument.kind == NodeKind::NamedArg || macro_call_block_argument?(argument)
        end
        collections = {
          "args"       => positional.map { |argument| macro_captured_syntax_node(argument) },
          "named_args" => node.named_arguments.map { |argument| macro_captured_named_argument(argument) },
        }
        source = node.text
        structure = MacroCapturedNode.new(
          source,
          "Crystal::Call",
          fields,
          collections,
          {"global?" => node.receiver.nil? && source.lstrip.starts_with?("::")}
        )
        name_fields = {} of String => MacroCapturedField
        if name = node.call_name || macro_global_call_name(node)
          name_fields["name"] = MacroCapturedField.new(name, "identifier")
        end
        metadata = MacroNodeMetadata.new(fields: name_fields, structure: structure)
        MacroSyntaxValue.captured(source, "Crystal::Call", metadata)
      end

      private def macro_structured_case_argument_value(node : SyntaxNode) : MacroSyntaxValue
        is_select = node.semantic_flag?(SemanticFlag::Select)
        exhaustive = node.semantic_flag?(SemanticFlag::Exhaustive)
        fields = {} of String => MacroCapturedNode
        fields["cond"] = macro_captured_syntax_node(node.child(0)) unless is_select
        fields["else"] = macro_captured_syntax_node(node.child(2))

        whens = node.child(1).try(&.children) || [] of SyntaxNode
        collections = {
          "whens" => whens.map { |item| macro_captured_when(item, exhaustive && !is_select) },
        }
        booleans = is_select ? ({} of String => Bool) : {"exhaustive?" => exhaustive}
        kind = is_select ? "Crystal::Select" : "Crystal::Case"
        structure = MacroCapturedNode.new(node.text, kind, fields, collections, booleans)
        MacroSyntaxValue.captured(node.text, kind, MacroNodeMetadata.new(structure: structure))
      end

      private def macro_captured_when(node : SyntaxNode, exhaustive : Bool) : MacroCapturedNode
        fields = {
          "body" => macro_captured_syntax_node(node.child(1)),
        }
        conditions = node.child(0).try(&.children) || [] of SyntaxNode
        collections = {
          "conds" => conditions.map { |condition| macro_captured_syntax_node(condition) },
        }
        MacroCapturedNode.new(
          node.text,
          "Crystal::When",
          fields,
          collections,
          {"exhaustive?" => exhaustive}
        )
      end

      private def macro_structured_exception_argument_value(node : SyntaxNode, ast : AstFile) : MacroSyntaxValue
        ensure_node = node.child(3)
        ensure_body = if ensure_node && ensure_node.kind == NodeKind::Ensure
                        ensure_node.child(0)
                      end
        fields = {
          "body"   => macro_captured_syntax_node(node.child(0)),
          "else"   => macro_captured_syntax_node(node.child(2)),
          "ensure" => macro_captured_syntax_node(ensure_body),
        }
        rescues = node.child(1).try do |clauses|
          clauses.kind == NodeKind::Expressions ? clauses.children : [] of SyntaxNode
        end || [] of SyntaxNode
        collections = {
          "rescues" => rescues.map { |clause| macro_captured_rescue(clause, ast) },
        }
        kind = "Crystal::ExceptionHandler"
        structure = MacroCapturedNode.new(node.text, kind, fields, collections)
        MacroSyntaxValue.captured(node.text, kind, MacroNodeMetadata.new(structure: structure))
      end

      private def macro_exception_handler_node?(node : SyntaxNode) : Bool
        return false unless node.kind == NodeKind::Begin
        node.children[1..3].any? { |child| child.kind != NodeKind::Nop }
      end

      private def macro_structured_inline_rescue_argument_value(node : SyntaxNode) : MacroSyntaxValue
        rescue_fields = {
          "body" => macro_captured_syntax_node(node.child(1)),
          "name" => macro_captured_syntax_node(nil),
        }
        rescue_node = MacroCapturedNode.new(
          node.text,
          "Crystal::Rescue",
          rescue_fields,
          nil_fields: ["types"]
        )
        macro_inline_exception_handler_value(node, [rescue_node], nil)
      end

      private def macro_structured_inline_ensure_argument_value(node : SyntaxNode) : MacroSyntaxValue
        macro_inline_exception_handler_value(node, [] of MacroCapturedNode, node.child(1))
      end

      private def macro_inline_exception_handler_value(
        node : SyntaxNode,
        rescues : Array(MacroCapturedNode),
        ensure_body : SyntaxNode?,
      ) : MacroSyntaxValue
        fields = {
          "body"   => macro_captured_syntax_node(node.child(0)),
          "else"   => macro_captured_syntax_node(nil),
          "ensure" => macro_captured_syntax_node(ensure_body),
        }
        collections = {"rescues" => rescues}
        kind = "Crystal::ExceptionHandler"
        structure = MacroCapturedNode.new(node.text, kind, fields, collections)
        MacroSyntaxValue.captured(node.text, kind, MacroNodeMetadata.new(structure: structure))
      end

      private def macro_captured_rescue(node : SyntaxNode, ast : AstFile) : MacroCapturedNode
        header = node.child(0)
        name = nil.as(SyntaxNode?)
        types = nil.as(Array(SyntaxNode)?)
        if header
          case header.kind
          when NodeKind::VarDecl
            name = header.target
            if declared_type = header.declared_type
              types = macro_union_type_nodes(declared_type, ast)
            end
          when NodeKind::Ident
            name = header
          when NodeKind::Nop
          else
            types = macro_union_type_nodes(header, ast)
          end
        end

        fields = {
          "body" => macro_captured_syntax_node(node.child(1)),
          "name" => name ? MacroCapturedNode.new(name.text, "identifier") : macro_captured_syntax_node(nil),
        }
        collections = {} of String => Array(MacroCapturedNode)
        nil_fields = [] of String
        if resolved_types = types
          collections["types"] = resolved_types.map { |type| macro_captured_syntax_node(type) }
        else
          nil_fields << "types"
        end
        MacroCapturedNode.new(node.text, "Crystal::Rescue", fields, collections, nil_fields: nil_fields)
      end

      private def macro_union_type_nodes(node : SyntaxNode, ast : AstFile) : Array(SyntaxNode)
        if node.kind == NodeKind::Binary &&
           ast.arena.operator_kind(node.raw.payload_index) == TokenKind::Pipe
          left = node.child(0)
          right = node.child(1)
          return [] of SyntaxNode unless left && right
          return macro_union_type_nodes(left, ast) + macro_union_type_nodes(right, ast)
        end
        [node]
      end

      private def macro_structured_type_syntax_argument_value(node : SyntaxNode, ast : AstFile) : MacroSyntaxValue?
        structure = macro_captured_type_syntax_node(node, ast)
        return nil unless structure
        fields = structure.fields.transform_values do |field|
          MacroCapturedField.new(field.source, field.kind)
        end
        metadata = MacroNodeMetadata.new(fields: fields, structure: structure)
        MacroSyntaxValue.captured(node.text, structure.kind, metadata)
      end

      private def macro_captured_type_syntax_node(node : SyntaxNode, ast : AstFile) : MacroCapturedNode?
        case node.kind
        when NodeKind::VarDecl
          macro_captured_type_declaration(node, ast)
        when NodeKind::ProcType
          macro_captured_proc_notation(node, ast)
        when NodeKind::TypeApply
          macro_captured_generic(node, ast)
        when NodeKind::Binary
          if macro_metaclass_node?(node, ast)
            macro_captured_metaclass(node, ast)
          elsif macro_union_node?(node, ast)
            macro_captured_union(node, ast)
          end
        when NodeKind::Path
          macro_metaclass_path_node?(node) ? macro_captured_metaclass(node, ast) : macro_captured_path(node)
        when NodeKind::Const
          macro_captured_path(node)
        when NodeKind::Ident
          macro_path_identifier?(node) ? macro_captured_path(node) : nil
        end
      end

      private def macro_captured_type_declaration(node : SyntaxNode, ast : AstFile) : MacroCapturedNode
        target = node.target
        variable = if target && target.kind == NodeKind::Ident
                     MacroCapturedNode.new(target.name || target.text, "Crystal::MacroId")
                   else
                     macro_captured_syntax_node(target)
                   end
        declared_type = node.declared_type
        fields = {
          "var"   => variable,
          "type"  => declared_type.try { |type| macro_captured_type_syntax_node(type, ast) } || macro_captured_syntax_node(declared_type),
          "value" => macro_captured_syntax_node(node.value),
        }
        MacroCapturedNode.new(node.text.strip, "Crystal::TypeDeclaration", fields)
      end

      private def macro_captured_proc_notation(node : SyntaxNode, ast : AstFile) : MacroCapturedNode
        inputs = node.child(0).try(&.children) || [] of SyntaxNode
        output = node.child(1)
        fields = {
          "output" => if output && output.kind != NodeKind::Nop
            macro_captured_type_syntax_node(output, ast) || macro_captured_syntax_node(output)
          else
            MacroCapturedNode.new("nil", "Crystal::NilLiteral")
          end,
        }
        collections = {
          "inputs" => inputs.map { |input| macro_captured_type_syntax_node(input, ast) || macro_captured_syntax_node(input) },
        }
        MacroCapturedNode.new(node.text.strip, "Crystal::ProcNotation", fields, collections)
      end

      private def macro_captured_metaclass(node : SyntaxNode, ast : AstFile) : MacroCapturedNode
        instance = node.child(0)
        fields = {
          "instance" => instance.try { |type| macro_captured_type_syntax_node(type, ast) } || macro_captured_syntax_node(instance),
        }
        MacroCapturedNode.new(node.text.strip, "Crystal::Metaclass", fields)
      end

      private def macro_captured_generic(node : SyntaxNode, ast : AstFile) : MacroCapturedNode
        name_node = node.child(0)
        arguments = node.child(1).try(&.children) || [] of SyntaxNode
        type_vars = arguments.reject { |argument| argument.kind == NodeKind::NamedArg }
        named_args = arguments.select { |argument| argument.kind == NodeKind::NamedArg }
        named_source = if named_args.empty?
                         "nil"
                       else
                         entries = named_args.map do |argument|
                           "#{argument.name}: #{argument.value.try(&.text)}"
                         end
                         "{#{entries.join(", ")}}"
                       end
        fields = {
          "name"       => name_node.try { |name| macro_captured_type_syntax_node(name, ast) } || macro_captured_syntax_node(name_node),
          "named_args" => MacroCapturedNode.new(named_source, named_args.empty? ? "Crystal::NilLiteral" : "Crystal::NamedTupleLiteral"),
        }
        collections = {
          "type_vars" => type_vars.map { |type_var| macro_captured_type_syntax_node(type_var, ast) || macro_captured_syntax_node(type_var) },
          "types"     => [MacroCapturedNode.new(node.text.strip, "Crystal::Generic")],
        }
        MacroCapturedNode.new(node.text.strip, "Crystal::Generic", fields, collections)
      end

      private def macro_captured_union(node : SyntaxNode, ast : AstFile) : MacroCapturedNode
        types = macro_union_type_nodes(node, ast)
        collections = {
          "types" => types.map { |type| macro_captured_type_syntax_node(type, ast) || macro_captured_syntax_node(type) },
        }
        MacroCapturedNode.new(node.text.strip, "Crystal::Union", collections: collections)
      end

      private def macro_captured_path(node : SyntaxNode) : MacroCapturedNode
        source = node.text.strip
        global = source.starts_with?("::")
        names = source.lchop("::").split("::").reject(&.empty?)
        collections = {
          "names" => names.map { |name| MacroCapturedNode.new(name, "Crystal::MacroId") },
          "types" => [MacroCapturedNode.new(source, "Crystal::Path")],
        }
        booleans = {
          "global?" => global,
          "global"  => global,
        }
        MacroCapturedNode.new(source, "Crystal::Path", collections: collections, booleans: booleans)
      end

      private def macro_path_identifier?(node : SyntaxNode) : Bool
        source = node.text.lchop("::")
        first = source.byte_at?(0)
        !!first && first >= 'A'.ord && first <= 'Z'.ord
      end

      private def macro_metaclass_node?(node : SyntaxNode, ast : AstFile) : Bool
        return false unless node.kind == NodeKind::Binary
        operator_index = node.raw.payload_index
        return false unless operator_index.in?(0...ast.arena.operators.size)
        return false unless ast.arena.operator_kind(operator_index) == TokenKind::Dot
        instance = node.child(0)
        member = node.child(1)
        return false unless instance && member && member.symbol_name == "class"
        macro_type_syntax_candidate?(instance, ast)
      end

      private def macro_metaclass_path_node?(node : SyntaxNode) : Bool
        node.kind == NodeKind::Path && node.children.size == 2 && node.child(1).try(&.symbol_name) == "class"
      end

      private def macro_union_node?(node : SyntaxNode, ast : AstFile) : Bool
        return false unless node.kind == NodeKind::Binary
        operator_index = node.raw.payload_index
        return false unless operator_index.in?(0...ast.arena.operators.size)
        ast.arena.operator_kind(operator_index) == TokenKind::Pipe &&
          node.children.all? { |child| macro_type_syntax_candidate?(child, ast) }
      end

      private def macro_type_syntax_candidate?(node : SyntaxNode, ast : AstFile) : Bool
        return true if {NodeKind::TypeApply, NodeKind::ProcType, NodeKind::Path, NodeKind::Const}.includes?(node.kind)
        return macro_path_identifier?(node) if node.kind == NodeKind::Ident
        return macro_metaclass_node?(node, ast) || macro_union_node?(node, ast) if node.kind == NodeKind::Binary
        false
      end

      private def macro_structured_asm_argument_value(node : SyntaxNode) : MacroSyntaxValue
        fields = {
          "text" => macro_captured_syntax_node(node.child(0)),
        }
        collections = {
          "outputs"  => macro_captured_asm_operands(node.child(1)),
          "inputs"   => macro_captured_asm_operands(node.child(2)),
          "clobbers" => (node.child(3).try(&.children) || [] of SyntaxNode).map { |clobber| macro_captured_syntax_node(clobber) },
        }
        flags = node.raw.flags
        booleans = {
          "volatile?"   => (flags & 0x0001_u16) != 0,
          "alignstack?" => (flags & 0x0002_u16) != 0,
          "intel?"      => (flags & 0x0004_u16) != 0,
          "can_throw?"  => (flags & 0x0008_u16) != 0,
        }
        structure = MacroCapturedNode.new(node.text, "Crystal::Asm", fields, collections, booleans)
        MacroSyntaxValue.captured(node.text, "Crystal::Asm", MacroNodeMetadata.new(structure: structure))
      end

      private def macro_captured_asm_operands(arguments : SyntaxNode?) : Array(MacroCapturedNode)
        (arguments.try(&.children) || [] of SyntaxNode).map { |operand| macro_captured_asm_operand(operand) }
      end

      private def macro_captured_asm_operand(node : SyntaxNode) : MacroCapturedNode
        fields = {
          "constraint" => macro_captured_syntax_node(node.child(0)),
          "exp"        => macro_captured_syntax_node(node.child(1)),
        }
        MacroCapturedNode.new(node.text.strip, "Crystal::AsmOperand", fields)
      end

      private def macro_structured_asm_operand_argument_value(node : SyntaxNode) : MacroSyntaxValue
        structure = macro_captured_asm_operand(node)
        MacroSyntaxValue.captured(node.text, "Crystal::AsmOperand", MacroNodeMetadata.new(structure: structure))
      end

      private def macro_structured_type_declaration_argument_value(node : SyntaxNode) : MacroSyntaxValue
        kind = macro_type_declaration_crystal_kind(node)
        fields = {
          "kind" => MacroCapturedNode.new(macro_type_declaration_kind_name(node), "Crystal::MacroId"),
          "body" => macro_captured_syntax_node(node.body),
        }
        collections = {} of String => Array(MacroCapturedNode)
        booleans = {} of String => Bool
        nil_fields = [] of String

        case kind
        when "Crystal::ClassDef"
          parameters = macro_captured_type_parameters(node)
          fields["superclass"] = macro_captured_syntax_node(node.superclass)
          collections["type_vars"] = parameters[:values]
          if splat_index = parameters[:splat_index]
            fields["splat_index"] = MacroCapturedNode.new(splat_index.to_s, "Crystal::NumberLiteral")
          else
            nil_fields << "splat_index"
          end
          booleans["abstract?"] = node.semantic_flag?(SemanticFlag::Abstract)
          booleans["struct?"] = node.kind == NodeKind::Struct
        when "Crystal::ModuleDef"
          parameters = macro_captured_type_parameters(node)
          collections["type_vars"] = parameters[:values]
          if splat_index = parameters[:splat_index]
            fields["splat_index"] = MacroCapturedNode.new(splat_index.to_s, "Crystal::NumberLiteral")
          else
            nil_fields << "splat_index"
          end
        when "Crystal::EnumDef"
          fields["base_type"] = macro_captured_syntax_node(node.superclass)
        when "Crystal::CStructOrUnionDef"
          booleans["union?"] = node.semantic_flag?(SemanticFlag::Union)
        end

        structure = MacroCapturedNode.new(node.text, kind, fields, collections, booleans, nil_fields)
        metadata_fields = {
          "name"                      => MacroCapturedField.new(macro_type_declaration_name(node, generic_args: true), "identifier"),
          "name_without_generic_args" => MacroCapturedField.new(macro_type_declaration_name(node, generic_args: false), "identifier"),
        }
        metadata = MacroNodeMetadata.new(fields: metadata_fields, structure: structure)
        MacroSyntaxValue.captured(node.text, kind, metadata)
      end

      private def macro_type_declaration_crystal_kind(node : SyntaxNode) : String
        case node.kind
        when NodeKind::Class  then "Crystal::ClassDef"
        when NodeKind::Module then "Crystal::ModuleDef"
        when NodeKind::Struct
          if node.semantic_flag?(SemanticFlag::Union) || node.ancestors.any? { |ancestor| ancestor.kind == NodeKind::Lib }
            "Crystal::CStructOrUnionDef"
          else
            "Crystal::ClassDef"
          end
        when NodeKind::Enum          then "Crystal::EnumDef"
        when NodeKind::Lib           then "Crystal::LibDef"
        when NodeKind::AnnotationDef then "Crystal::AnnotationDef"
        else                              "Crystal::ASTNode"
        end
      end

      private def macro_type_declaration_kind_name(node : SyntaxNode) : String
        return "union" if node.kind == NodeKind::Struct && node.semantic_flag?(SemanticFlag::Union)
        case node.kind
        when NodeKind::Class         then "class"
        when NodeKind::Module        then "module"
        when NodeKind::Struct        then "struct"
        when NodeKind::Enum          then "enum"
        when NodeKind::Lib           then "lib"
        when NodeKind::AnnotationDef then "annotation"
        else                              ""
        end
      end

      private def macro_captured_type_parameters(node : SyntaxNode)
        values = [] of MacroCapturedNode
        splat_index = nil.as(Int32?)
        name_node = node.name_node
        if name_node && name_node.kind == NodeKind::TypeApply
          if arguments = name_node.child(1)
            arguments.children.each_with_index do |parameter, index|
              target = parameter.kind == NodeKind::Splat ? parameter.child(0) : parameter
              name = target.try(&.symbol_name) || target.try(&.text) || parameter.text.lchop('*')
              splat_index = index if parameter.kind == NodeKind::Splat
              values << MacroCapturedNode.new(name, "Crystal::MacroId")
            end
          end
        end
        {values: values, splat_index: splat_index}
      end

      private def macro_type_declaration_name(node : SyntaxNode, generic_args : Bool) : String
        name_node = node.name_node
        return "" unless name_node
        base = if name_node.kind == NodeKind::TypeApply
                 name_node.child(0).try(&.text) || name_node.text
               else
                 name_node.text
               end
        return base unless generic_args && name_node.kind == NodeKind::TypeApply
        parameters = macro_captured_type_parameters(node)
        rendered = parameters[:values].map_with_index do |parameter, index|
          index == parameters[:splat_index] ? "*#{parameter.source}" : parameter.source
        end
        "#{base}(#{rendered.join(", ")})"
      end

      private def macro_structured_declaration_argument_value(node : SyntaxNode) : MacroSyntaxValue
        case node.kind
        when NodeKind::Def
          macro_structured_def_argument_value(node)
        when NodeKind::MacroDef
          macro_structured_macro_def_argument_value(node)
        when NodeKind::Fun
          macro_structured_fun_argument_value(node)
        else
          raise "unsupported structured declaration: #{node.kind}"
        end
      end

      private def macro_structured_def_argument_value(node : SyntaxNode) : MacroSyntaxValue
        parameters = macro_captured_declaration_parameters(node)
        fields = {
          "body"         => macro_captured_syntax_node(node.child(3)),
          "double_splat" => parameters[:double_splat] || macro_captured_syntax_node(nil),
          "block_arg"    => parameters[:block_arg] || macro_captured_syntax_node(nil),
          "return_type"  => macro_captured_syntax_node(node.return_type),
          "receiver"     => macro_captured_syntax_node(macro_declaration_receiver(node)),
          "visibility"   => macro_captured_visibility(node),
        }
        nil_fields = [] of String
        if splat_index = parameters[:splat_index]
          fields["splat_index"] = MacroCapturedNode.new(splat_index.to_s, "Crystal::NumberLiteral")
        else
          nil_fields << "splat_index"
        end
        free_vars = node.child(4).try(&.children) || [] of SyntaxNode
        collections = {
          "args"      => parameters[:args],
          "free_vars" => free_vars.map { |variable| MacroCapturedNode.new(variable.text, "Crystal::MacroId") },
        }
        booleans = {
          "accepts_block?" => parameters[:accepts_block],
          "abstract?"      => node.semantic_flag?(SemanticFlag::Abstract),
        }
        macro_captured_declaration_value(node, "Crystal::Def", fields, collections, booleans, nil_fields)
      end

      private def macro_structured_macro_def_argument_value(node : SyntaxNode) : MacroSyntaxValue
        parameters = macro_captured_declaration_parameters(node)
        fields = {
          "body"         => macro_captured_macro_body(node.child(3)),
          "double_splat" => parameters[:double_splat] || macro_captured_syntax_node(nil),
          "block_arg"    => parameters[:block_arg] || macro_captured_syntax_node(nil),
          "visibility"   => macro_captured_visibility(node),
        }
        nil_fields = [] of String
        if splat_index = parameters[:splat_index]
          fields["splat_index"] = MacroCapturedNode.new(splat_index.to_s, "Crystal::NumberLiteral")
        else
          nil_fields << "splat_index"
        end
        collections = {"args" => parameters[:args]}
        macro_captured_declaration_value(
          node,
          "Crystal::Macro",
          fields,
          collections,
          {} of String => Bool,
          nil_fields
        )
      end

      private def macro_captured_macro_body(body : SyntaxNode?) : MacroCapturedNode
        return macro_captured_syntax_node(nil) unless body
        source = body.text.strip
        return macro_captured_syntax_node(nil) if source.empty?
        MacroCapturedNode.new(source, macro_crystal_syntax_kind(body.kind))
      end

      private def macro_structured_fun_argument_value(node : SyntaxNode) : MacroSyntaxValue
        parameters = macro_captured_declaration_parameters(node)
        name = macro_declaration_name(node)
        external_name = node.child(3)
        real_name = if external_name && external_name.kind != NodeKind::Nop && external_name.symbol_name != name
                      MacroCapturedNode.new(external_name.text, "Crystal::StringLiteral")
                    else
                      macro_captured_syntax_node(nil)
                    end
        body = node.child(4)
        fields = {
          "real_name"   => real_name,
          "return_type" => macro_captured_syntax_node(node.return_type),
          "body"        => macro_captured_syntax_node(body),
        }
        collections = {"args" => parameters[:args]}
        booleans = {
          "variadic?" => parameters[:variadic],
          "has_body?" => !!body && body.kind != NodeKind::Nop,
        }
        macro_captured_declaration_value(node, "Crystal::FunDef", fields, collections, booleans)
      end

      private def macro_captured_declaration_value(
        node : SyntaxNode,
        kind : String,
        fields : Hash(String, MacroCapturedNode),
        collections : Hash(String, Array(MacroCapturedNode)),
        booleans : Hash(String, Bool),
        nil_fields = [] of String,
      ) : MacroSyntaxValue
        structure = MacroCapturedNode.new(node.text, kind, fields, collections, booleans, nil_fields)
        metadata_fields = {
          "name" => MacroCapturedField.new(macro_declaration_name(node), "identifier"),
        }
        metadata = MacroNodeMetadata.new(fields: metadata_fields, structure: structure)
        MacroSyntaxValue.captured(node.text, kind, metadata)
      end

      private def macro_captured_declaration_parameters(node : SyntaxNode)
        args = [] of MacroCapturedNode
        double_splat = nil.as(MacroCapturedNode?)
        block_arg = nil.as(MacroCapturedNode?)
        splat_index = nil.as(Int32?)
        accepts_block = false
        variadic = false

        node.parameters.each do |parameter|
          case parameter.kind
          when NodeKind::Splat
            splat_index = args.size
            args << macro_captured_arg(parameter) if parameter.name
          when NodeKind::DoubleSplat
            double_splat = macro_captured_arg(parameter)
          when NodeKind::BlockParam
            accepts_block = true
            block_arg = macro_captured_arg(parameter) if parameter.name
          when NodeKind::Param
            if parameter.children.empty? && parameter.raw.payload_index < 0
              variadic = true
            else
              args << macro_captured_arg(parameter)
            end
          end
        end

        {
          args:          args,
          double_splat:  double_splat,
          block_arg:     block_arg,
          splat_index:   splat_index,
          accepts_block: accepts_block,
          variadic:      variadic,
        }
      end

      private def macro_captured_arg(node : SyntaxNode) : MacroCapturedNode
        internal_name = node.name || macro_payload_symbol(node) || ""
        external_name = node.external_name || internal_name
        restriction = node.declared_type
        default_value = node.value
        source = String.build do |io|
          io << external_name << ' ' if external_name != internal_name
          io << internal_name
          io << " : " << restriction.text if restriction
          io << " = " << default_value.text if default_value
        end
        fields = {
          "name"          => MacroCapturedNode.new(external_name, "identifier"),
          "internal_name" => MacroCapturedNode.new(internal_name, "identifier"),
          "default_value" => macro_captured_syntax_node(default_value),
          "restriction"   => macro_captured_syntax_node(restriction),
        }
        MacroCapturedNode.new(source, "Crystal::Arg", fields)
      end

      private def macro_payload_symbol(node : SyntaxNode) : String?
        index = node.raw.payload_index
        index.in?(0...node.tree.ast.arena.symbols.entries.size) ? node.tree.ast.arena.symbols[index] : nil
      end

      private def macro_declaration_name(node : SyntaxNode) : String
        name_node = node.name_node
        return "" unless name_node
        if name_node.kind == NodeKind::Path
          return name_node.children.last?.try(&.symbol_name) || name_node.symbol_name || ""
        end
        name_node.symbol_name || name_node.text
      end

      private def macro_declaration_receiver(node : SyntaxNode) : SyntaxNode?
        name_node = node.name_node
        return nil unless name_node && name_node.kind == NodeKind::Path
        name_node.child(0)
      end

      private def macro_captured_visibility(node : SyntaxNode) : MacroCapturedNode
        value = if node.semantic_flag?(SemanticFlag::Private)
                  "private"
                elsif node.semantic_flag?(SemanticFlag::Protected)
                  "protected"
                else
                  "public"
                end
        MacroCapturedNode.new(":#{value}", "Crystal::SymbolLiteral")
      end

      private def macro_structured_call_node?(node : SyntaxNode, ast : AstFile) : Bool
        return true if {NodeKind::Call, NodeKind::CallWithBlock}.includes?(node.kind)
        return true unless macro_global_call_name(node).nil?
        return false unless node.kind == NodeKind::Binary
        operator_index = node.raw.payload_index
        return false unless operator_index.in?(0...ast.arena.operators.size)
        {TokenKind::Dot, TokenKind::SafeNav, TokenKind::DoubleColon}.includes?(ast.arena.operator_kind(operator_index))
      end

      private def macro_global_call_name(node : SyntaxNode) : String?
        source = node.text.strip
        return nil unless source.starts_with?("::")
        name = source.lchop("::")
        name.matches?(/\A[a-z_][a-zA-Z0-9_]*[?!]?\z/) ? name : nil
      end

      private def macro_call_block_argument?(node : SyntaxNode) : Bool
        node.kind == NodeKind::BlockParam || node.text.lstrip.starts_with?('&')
      end

      private def macro_captured_call_block(node : SyntaxNode, ast : AstFile) : MacroCapturedNode
        block_call = if node.kind == NodeKind::CallWithBlock
                       node
                     elsif node.kind == NodeKind::Binary
                       node.child(1).try { |right| right.kind == NodeKind::CallWithBlock ? right : nil }
                     end
        return macro_captured_syntax_node(nil) unless block_call
        call = block_call.child(0)
        return macro_captured_syntax_node(nil) unless call
        start = call.span.finish
        finish = block_call.span.finish
        source = start < finish ? ast.source.text.byte_slice(start, finish - start).strip : ""
        MacroCapturedNode.new(source, "Crystal::Block")
      end

      private def macro_captured_named_argument(node : SyntaxNode) : MacroCapturedNode
        fields = {} of String => MacroCapturedNode
        fields["name"] = MacroCapturedNode.new(node.name || "", "identifier")
        fields["value"] = macro_captured_syntax_node(node.value)
        MacroCapturedNode.new(node.text, "Crystal::NamedArgument", fields)
      end

      private def macro_captured_syntax_node(node : SyntaxNode?) : MacroCapturedNode
        return MacroCapturedNode.new("", "Crystal::Nop") unless node
        if node.kind == NodeKind::Expressions
          expressions = node.children
          return MacroCapturedNode.new("", "Crystal::Nop") if expressions.empty?
          return macro_captured_syntax_node(expressions.first) if expressions.size == 1
        end
        if type_syntax = macro_captured_type_syntax_node(node, node.tree.ast)
          return type_syntax
        end
        MacroCapturedNode.new(node.text.strip, macro_crystal_syntax_kind(node.kind))
      end

      private def macro_crystal_syntax_kind(kind : NodeKind) : String
        case kind
        when NodeKind::LiteralNumber then "Crystal::NumberLiteral"
        when NodeKind::LiteralString then "Crystal::StringLiteral"
        when NodeKind::LiteralSymbol then "Crystal::SymbolLiteral"
        when NodeKind::LiteralChar   then "Crystal::CharLiteral"
        when NodeKind::LiteralRegex  then "Crystal::RegexLiteral"
        when NodeKind::LiteralBool   then "Crystal::BoolLiteral"
        when NodeKind::LiteralNil    then "Crystal::NilLiteral"
        when NodeKind::NamedArg      then "Crystal::NamedArgument"
        when NodeKind::Call          then "Crystal::Call"
        when NodeKind::CallWithBlock then "Crystal::Call"
        when NodeKind::Def           then "Crystal::Def"
        when NodeKind::MacroDef      then "Crystal::Macro"
        when NodeKind::Fun           then "Crystal::FunDef"
        when NodeKind::Class         then "Crystal::ClassDef"
        when NodeKind::Module        then "Crystal::ModuleDef"
        when NodeKind::Struct        then "Crystal::ClassDef"
        when NodeKind::Enum          then "Crystal::EnumDef"
        when NodeKind::Lib           then "Crystal::LibDef"
        when NodeKind::AnnotationDef then "Crystal::AnnotationDef"
        when NodeKind::Asm           then "Crystal::Asm"
        when NodeKind::AsmOperand    then "Crystal::AsmOperand"
        when NodeKind::VarDecl       then "Crystal::TypeDeclaration"
        when NodeKind::ProcType      then "Crystal::ProcNotation"
        when NodeKind::TypeApply     then "Crystal::Generic"
        when NodeKind::Const         then "Crystal::Path"
        when NodeKind::Path          then "Crystal::Path"
        when NodeKind::Ident         then "Crystal::Var"
        when NodeKind::InstanceVar   then "Crystal::InstanceVar"
        when NodeKind::ClassVar      then "Crystal::ClassVar"
        when NodeKind::Global        then "Crystal::Global"
        when NodeKind::Expressions   then "Crystal::Expressions"
        when NodeKind::Nop           then "Crystal::Nop"
        else                              "Crystal::ASTNode"
        end
      end

      private def expand_macro_def(
        ref : DeclRef,
        args : MacroArguments,
        index : ProgramIndex?,
        footprint : MacroFootprint?,
        call_scope : String,
      ) : String
        params_id = ref.ast.children(ref.node_id)[1]?
        body_id = ref.ast.children(ref.node_id)[3]?
        return "" unless body_id && params_id
        env = build_param_env(params_id, ref.ast, args)
        unless call_scope.empty?
          if type = macro_type_value(call_scope, index, absolute: true)
            env["@type"] = type
          end
        end
        @env_stack << env
        @macro_var_stack << {} of String => String
        text = begin
          body_span = ref.ast.node(body_id).span
          macros = [] of NodeId
          collect_macros(body_id, ref.ast, macros, index, footprint: footprint)
          expand_text(ref.ast, macros, index, body_span, footprint)
        ensure
          @macro_var_stack.pop
          @env_stack.pop
        end
        footprint.try &.merge_requires(ref.ast.source)
        text
      end

      private def cache_key(ref : DeclRef, args : MacroArguments, call_scope : String) : String
        def_node = ref.ast.node(ref.node_id)
        name_id = ref.ast.children(ref.node_id)[0]
        name = ref.ast.arena.symbols[ref.ast.node(name_id).payload_index]
        body_id = ref.ast.children(ref.node_id)[3]?
        body_fp = body_id ? fingerprint_text(ref.ast.source, ref.ast.node(body_id).span) : "nil"
        args_fp = fingerprint_args(args)
        "#{ref.scope}|#{call_scope}|#{name}|#{body_fp}|#{args_fp}"
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

      private def build_param_env(params_id : NodeId, ast : AstFile, args : MacroArguments) : Hash(String, MacroValue)
        env = {} of String => MacroValue
        params = ast.children(params_id)
        positional_args, named_args, block = args
        positional_index = 0
        trailing_named = named_args.dup
        env[YIELD_ENV_KEY] = block if block

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
              if default_node && ast.node(default_node).kind != NodeKind::Nop
                env[name] = macro_argument_value(default_node, ast)
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
            name = splat_name(param_id, ast)
            env[name] = block if name && block
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

      private def fingerprint_args(args : MacroArguments) : String
        positional, named, block = args
        pos_fp = positional.map { |v| fingerprint_value(v) }.join("|")
        named_fp = named.keys.sort.map { |k| "#{k}=#{fingerprint_value(named[k])}" }.join("|")
        block_fp = block ? fingerprint_value(block) : "nil"
        "#{pos_fp}||#{named_fp}||#{block_fp}"
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
        when MacroSyntaxValue
          "#{value.kind}:#{value.source}"
        when MacroBlockValue
          "block(#{value.parameters.join(",")}){#{value.body}}"
        when MacroRangeValue
          "range(#{value.first}:#{value.last}:#{value.exclusive})"
        when MacroNumberValue
          "number(#{value.kind}:#{value.source})"
        when MacroTypeValue
          "type(#{value.kind}:#{value.name})"
        when MacroAnnotationValue
          positional = value.positional_sources.join(",")
          named = value.named_sources.keys.sort.map { |key| "#{key}=#{value.named_sources[key]}" }.join(",")
          "annotation(#{value.name}:#{positional}:#{named})"
        when MacroMetaVarValue
          annotations = value.annotations.map { |entry| fingerprint_value(entry) }.join(",")
          "meta-var(#{value.name}:#{value.type_name}=#{value.default_value}:#{annotations})"
        when MacroMethodValue
          annotations = value.annotations.map { |entry| fingerprint_value(entry) }.join(",")
          "method(#{value.name}:#{value.source}:#{annotations})"
        when MacroTupleValue
          "tuple(" + value.values.map { |entry| fingerprint_value(entry) }.join(",") + ")"
        when MacroArrayValue
          "ast-array(" + value.values.map { |entry| fingerprint_value(entry) }.join(",") + ")"
        when MacroHashValue
          "hash(" + value.entries.map { |entry| "#{fingerprint_value(entry.key)}:#{fingerprint_value(entry.value)}" }.join(",") + ")"
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
                io << expand_macro_iteration(targets, ast, [item, item_index.to_i64] of MacroValue) do
                  expand_template_body(body, ast, index, footprint)
                end
              end
            when MacroArrayValue
              value.values.each_with_index do |item, item_index|
                io << expand_macro_iteration(targets, ast, [item, item_index.to_i64] of MacroValue) do
                  expand_template_body(body, ast, index, footprint)
                end
              end
            when MacroTupleValue
              value.values.each_with_index do |item, item_index|
                io << expand_macro_iteration(targets, ast, [item, item_index.to_i64] of MacroValue) do
                  expand_template_body(body, ast, index, footprint)
                end
              end
            when Hash(String, MacroValue)
              value.each_with_index do |(key, item), item_index|
                values = [MacroSyntaxValue.string(key), item, item_index.to_i64] of MacroValue
                io << expand_macro_iteration(targets, ast, values) do
                  expand_template_body(body, ast, index, footprint)
                end
              end
            when MacroHashValue
              value.entries.each_with_index do |entry, item_index|
                values = [entry.key, entry.value, item_index.to_i64] of MacroValue
                io << expand_macro_iteration(targets, ast, values) do
                  expand_template_body(body, ast, index, footprint)
                end
              end
            when MacroRangeValue
              first = value.first
              last = value.last
              if first && last
                finish = value.exclusive ? last - 1 : last
                Range.new(first, finish).each_with_index do |item, item_index|
                  io << expand_macro_iteration(targets, ast, [item, item_index.to_i64] of MacroValue) do
                    expand_template_body(body, ast, index, footprint)
                  end
                end
              else
                @diagnostics << Diagnostic.new(ast.node(parts[1]).span, "macro for range must have both bounds")
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
        collect_macros(body_id, ast, macros, index, footprint: footprint)
        return slice_text(ast.source, body_span) if macros.empty?
        expand_text(ast, macros, index, body_span, footprint)
      end

      private def macro_iteration_env(targets : Slice(NodeId), ast : AstFile, values : Array(MacroValue)) : Hash(String, MacroValue)
        env = current_macro_env.dup
        targets.each_with_index do |target, index|
          if name = macro_target_name(target, ast)
            env[name] = values[index]? || nil
          end
        end
        env
      end

      private def expand_macro_iteration(targets : Slice(NodeId), ast : AstFile, values : Array(MacroValue), & : -> String) : String
        parent = current_macro_env
        env = macro_iteration_env(targets, ast, values)
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
        if {
             NodeKind::Array,
             NodeKind::Tuple,
             NodeKind::NamedTuple,
             NodeKind::Hash,
             NodeKind::Range,
           }.includes?(ast.node(node_id).kind)
          return slice_text(ast.source, ast.node(node_id).span)
        end
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
          MacroEvaluation.new(MacroSyntaxValue.string(content))
        when NodeKind::LiteralSymbol
          MacroEvaluation.new(MacroSyntaxValue.symbol(ast.decoded_literal_string(node_id)))
        when NodeKind::LiteralNumber
          source = ast.node_string(node_id)
          MacroEvaluation.new(macro_number_literal(source) || MacroSyntaxValue.code(source))
        when NodeKind::LiteralChar
          source = ast.node_string(node_id)
          MacroEvaluation.new(MacroSyntaxValue.char(ast.decoded_literal_string(node_id), source))
        when NodeKind::LiteralRegex
          source = ast.node_string(node_id)
          MacroEvaluation.new(MacroSyntaxValue.regex(ast.decoded_literal_string(node_id), source))
        when NodeKind::StringInterpolation
          value = String.build do |io|
            ast.children(node_id).each do |child_id|
              evaluation = eval_value(child_id, ast)
              return nil unless evaluation
              io << (macro_scalar_text(evaluation.value) || val_to_string(evaluation.value))
            end
          end
          MacroEvaluation.new(MacroSyntaxValue.string(value))
        when NodeKind::LiteralBool
          MacroEvaluation.new(node.flags == 1)
        when NodeKind::LiteralNil
          MacroEvaluation.new(nil)
        when NodeKind::Ident
          name = ast.arena.symbols[node.payload_index]
          env = current_macro_env
          return MacroEvaluation.new(env[name]) if env.has_key?(name)
          MacroEvaluation.new(MacroSyntaxValue.identifier(name))
        when NodeKind::Const, NodeKind::Path, NodeKind::TypeApply
          source = ast.node_string(node_id)
          MacroEvaluation.new(MacroSyntaxValue.code(source))
        when NodeKind::InstanceVar
          name = ast.arena.symbols[node.payload_index]
          env = current_macro_env
          return MacroEvaluation.new(env[name]) if env.has_key?(name)
          if name == "@type"
            mark_type_introspection
            scope = lexical_scope(node_id, ast)
            if type = macro_type_value(scope, @active_index, absolute: true)
              return MacroEvaluation.new(type)
            end
          end
          MacroEvaluation.new(MacroSyntaxValue.identifier(name))
        when NodeKind::MacroVar
          value = expand_macro_var(node_id, ast)
          value ? MacroEvaluation.new(MacroSyntaxValue.identifier(value)) : nil
        when NodeKind::Yield
          if block = current_macro_env[YIELD_ENV_KEY]?
            MacroEvaluation.new(block)
          else
            @diagnostics << Diagnostic.new(node.span, "can't use macro yield without a block")
            MacroEvaluation.new(nil)
          end
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
          first = left.value.nil? ? nil : macro_integer_index(left.value)
          last = right.value.nil? ? nil : macro_integer_index(right.value)
          return nil if !left.value.nil? && first.nil?
          return nil if !right.value.nil? && last.nil?
          MacroEvaluation.new(MacroRangeValue.new(first, last, node.flags == 1))
        when NodeKind::Call
          callee_id = ast.children(node_id)[0]
          callee = ast.node(callee_id)
          if callee.kind == NodeKind::Binary
            operator = ast.arena.operator_kind(callee.payload_index)
            if {TokenKind::Dot, TokenKind::SafeNav}.includes?(operator)
              receiver_id, member_id = ast.children(callee_id)
              member = ast.node(member_id)
              if member.kind == NodeKind::Ident
                receiver = eval_value(receiver_id, ast)
                return nil unless receiver
                return MacroEvaluation.new(nil) if operator == TokenKind::SafeNav && receiver.value.nil?
                name = ast.arena.symbols[member.payload_index]
                args = [] of MacroValue
                syntax_tree(ast).node(node_id).arguments.each do |argument|
                  evaluation = eval_value(argument.id, ast)
                  if evaluation
                    args << evaluation.value
                  elsif name == "is_a?"
                    args << MacroSyntaxValue.code(argument.text)
                  else
                    return nil
                  end
                end
                return apply_macro_method(receiver.value, name, args)
              end
            end
          end
          if callee.kind == NodeKind::Ident
            name = ast.arena.symbols[callee.payload_index]
            if name == "gensym"
              args = ast.children(node_id)[1]?
              base = "tmp"
              if args && (first = ast.children(args).first?)
                if evaluation = eval_value(first, ast)
                  base = macro_scalar_text(evaluation.value) || val_to_string(evaluation.value)
                end
              end
              sym_id = @hygiene.gensym(base, ast.arena.symbols)
              return MacroEvaluation.new(MacroSyntaxValue.identifier(ast.arena.symbols[sym_id]))
            end
            if {"flag?", "compare_versions"}.includes?(name)
              values = [] of MacroValue
              syntax_tree(ast).node(node_id).arguments.each do |argument|
                evaluation = eval_value(argument.id, ast)
                return nil unless evaluation
                values << evaluation.value
              end
              if name == "flag?"
                return nil unless values.size == 1
                return MacroEvaluation.new(false)
              end
              return nil unless values.size == 2
              left = macro_scalar_text(values[0])
              right = macro_scalar_text(values[1])
              return nil unless left && right
              return MacroEvaluation.new(compare_macro_versions(left, right).to_i64)
            end
            if {"sizeof", "alignof"}.includes?(name)
              arguments = syntax_tree(ast).node(node_id).arguments
              return nil unless arguments.size == 1
              # Facet has no codegen target data layout. Preserve the compiler's
              # macro value shape without claiming that this is the target size.
              return MacroEvaluation.new(MacroNumberValue.new(0_i128, MacroNumberKind::I32, "0", false))
            end
          end
          nil
        when NodeKind::CallWithBlock
          children = ast.children(node_id)
          call_id = children.first?
          return nil unless call_id
          call_node = ast.node(call_id)
          return nil unless call_node.kind == NodeKind::Call
          callee_id = ast.children(call_id).first?
          return nil unless callee_id
          callee = ast.node(callee_id)
          return nil unless callee.kind == NodeKind::Binary
          operator = ast.arena.operator_kind(callee.payload_index)
          return nil unless {TokenKind::Dot, TokenKind::SafeNav}.includes?(operator)
          receiver_id, member_id = ast.children(callee_id)
          member = ast.node(member_id)
          return nil unless member.kind == NodeKind::Ident
          receiver = eval_value(receiver_id, ast)
          return nil unless receiver
          return MacroEvaluation.new(nil) if operator == TokenKind::SafeNav && receiver.value.nil?
          args = [] of MacroValue
          syntax_tree(ast).node(call_id).arguments.each do |argument|
            evaluation = eval_value(argument.id, ast)
            return nil unless evaluation
            args << evaluation.value
          end
          wrapper = syntax_tree(ast).node(node_id)
          body = wrapper.body
          return nil unless body
          block = MacroEvalBlock.new(ast, body.id, wrapper.parameters.compact_map(&.name))
          name = ast.arena.symbols[member.payload_index]
          apply_macro_method(receiver.value, name, args, block)
        when NodeKind::NamedArg
          value_id = ast.children(node_id).last?
          value_id ? eval_value(value_id, ast) : nil
        when NodeKind::Splat
          value_id = ast.children(node_id).first?
          return nil unless value_id
          evaluation = eval_value(value_id, ast)
          return nil unless evaluation
          values = macro_sequence_values(evaluation.value)
          return nil unless values
          MacroEvaluation.new(MacroSyntaxValue.code(values.map { |value| val_to_string(value) }.join(", ")))
        when NodeKind::DoubleSplat
          value_id = ast.children(node_id).first?
          return nil unless value_id
          evaluation = eval_value(value_id, ast)
          return nil unless evaluation
          value = evaluation.value
          if value.is_a?(MacroHashValue)
            MacroEvaluation.new(MacroSyntaxValue.code(macro_double_splat(value)))
          elsif value.is_a?(Hash(String, MacroValue))
            body = value.map do |key, entry|
              rendered_key = key.matches?(/\A[a-zA-Z_][a-zA-Z0-9_]*[?!]?\z/) ? key : key.inspect
              "#{rendered_key}: #{val_to_string(entry)}"
            end.join(", ")
            MacroEvaluation.new(MacroSyntaxValue.code(body))
          end
        when NodeKind::Index
          children = ast.children(node_id)
          return nil if children.size < 2
          receiver = eval_value(children[0], ast)
          index = eval_value(children[1], ast)
          return nil unless receiver && index
          if children.size == 3
            count = eval_value(children[2], ast)
            return nil unless count
            values = macro_sequence_values(receiver.value)
            first = macro_integer_index(index.value)
            length = macro_integer_index(count.value)
            return nil unless values && first && length && length >= 0
            size = values.size.to_i64
            return MacroEvaluation.new(nil) if first < -size || first > size
            first += size if first < 0
            sliced = values[first.to_i, Math.min(length, size - first).to_i].map(&.as(MacroValue))
            return MacroEvaluation.new(macro_collection_result(receiver.value, sliced))
          end
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
          children = ast.children(node_id)
          children = children[0...-1] if node.kind == NodeKind::Array && node.flags == 1 && !children.empty?
          children.each do |cid|
            if evaluation = eval_value(cid, ast)
              values << evaluation.value
            end
          end
          if node.kind == NodeKind::Tuple
            MacroEvaluation.new(MacroTupleValue.new(values))
          else
            MacroEvaluation.new(values)
          end
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
          entries = [] of MacroHashEntry
          ast.children(node_id).each do |cid|
            child = ast.node(cid)
            if child.kind == NodeKind::Binary && ast.arena.operator_kind(child.payload_index) == TokenKind::HashRocket
              key_id, val_id = ast.children(cid)
              key = eval_value(key_id, ast)
              value = eval_value(val_id, ast)
              entries << MacroHashEntry.new(key.value, value.value) if key && value
            end
          end
          MacroEvaluation.new(MacroHashValue.new(entries))
        else
          nil
        end
      end

      private def macro_number_literal(source : String) : MacroValue?
        raw = source.strip.lchop("(").rchop(")")
        compact = raw.delete('_')
        suffix = compact.match(/(i8|i16|i32|i64|i128|u8|u16|u32|u64|u128|f32|f64)\z/).try(&.[1])
        number = suffix ? compact.rchop(suffix) : compact

        if suffix.try(&.starts_with?('f')) || (!suffix && (number.includes?('.') || number.includes?('e') || number.includes?('E')))
          value = number.to_f64?
          return nil unless value
          kind = suffix == "f32" ? MacroNumberKind::F32 : MacroNumberKind::F64
          return MacroNumberValue.new(value, kind, raw, !suffix.nil?)
        end

        if suffix.try(&.starts_with?('u'))
          value = number.to_u128?(prefix: true)
          return nil unless value
          return MacroNumberValue.new(value, macro_number_kind(suffix.not_nil!), raw, true)
        end

        if suffix
          value = number.to_i128?(prefix: true)
          return nil unless value
          return MacroNumberValue.new(value, macro_number_kind(suffix), raw, true)
        end

        if value = number.to_i64?(prefix: true)
          if raw.starts_with?('+')
            kind = value.in?(Int32::MIN.to_i64..Int32::MAX.to_i64) ? MacroNumberKind::I32 : MacroNumberKind::I64
            MacroNumberValue.new(value.to_i128, kind, raw, false)
          else
            value
          end
        elsif value = number.to_i128?(prefix: true)
          MacroNumberValue.new(value, MacroNumberKind::I128, raw, false)
        end
      end

      private def macro_number_kind(suffix : String) : MacroNumberKind
        case suffix
        when "i8"   then MacroNumberKind::I8
        when "i16"  then MacroNumberKind::I16
        when "i32"  then MacroNumberKind::I32
        when "i64"  then MacroNumberKind::I64
        when "i128" then MacroNumberKind::I128
        when "u8"   then MacroNumberKind::U8
        when "u16"  then MacroNumberKind::U16
        when "u32"  then MacroNumberKind::U32
        when "u64"  then MacroNumberKind::U64
        when "u128" then MacroNumberKind::U128
        when "f32"  then MacroNumberKind::F32
        else             MacroNumberKind::F64
        end
      end

      private def eval_macro_member(receiver_id : NodeId, member_id : NodeId, ast : AstFile, safe : Bool) : MacroEvaluation?
        receiver = eval_value(receiver_id, ast)
        return nil unless receiver
        return MacroEvaluation.new(nil) if safe && receiver.value.nil?

        member = ast.node(member_id)
        name = nil
        args = [] of MacroValue
        block = nil.as(MacroEvalBlock?)
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
              if evaluation
                args << evaluation.value
              elsif name == "is_a?"
                args << MacroSyntaxValue.code(ast.node_string(arg_id))
              else
                return nil
              end
            end
          end
        when NodeKind::CallWithBlock
          call = syntax_tree(ast).node(member_id)
          name = call.call_name
          call.arguments.each do |argument|
            evaluation = eval_value(argument.id, ast)
            return nil unless evaluation
            args << evaluation.value
          end
          body = call.body
          return nil unless body
          block = MacroEvalBlock.new(ast, body.id, call.parameters.compact_map(&.name))
        else
          return nil
        end
        return nil unless name
        apply_macro_method(receiver.value, name, args, block)
      end

      private def apply_macro_method(
        receiver : MacroValue,
        name : String,
        args : Array(MacroValue),
        block : MacroEvalBlock? = nil,
      ) : MacroEvaluation?
        if evaluation = apply_type_aware_macro_method(receiver, name, args)
          return evaluation
        end

        if receiver.is_a?(MacroSyntaxValue) && args.empty? && name != "name"
          if evaluation = apply_captured_macro_member(receiver, name)
            return evaluation
          end
        end

        case name
        when "name"
          return nil unless receiver.is_a?(MacroSyntaxValue) && args.size <= 1
          generic_args = args.empty? ? true : args.first
          return nil unless generic_args.is_a?(Bool)
          fields = receiver.metadata.try(&.fields)
          field = if generic_args
                    fields.try(&.["name"]?)
                  else
                    fields.try(&.["name_without_generic_args"]?) || fields.try(&.["name"]?)
                  end
          field ? MacroEvaluation.new(macro_captured_field_value(field)) : nil
        when "filename", "line_number", "column_number", "end_line_number", "end_column_number"
          return nil unless args.empty? && receiver.is_a?(MacroSyntaxValue)
          metadata = receiver.metadata
          location = name.starts_with?("end_") ? metadata.try(&.end_location) : metadata.try(&.location)
          return MacroEvaluation.new(nil) unless location
          case name
          when "filename"
            MacroEvaluation.new(MacroSyntaxValue.string(location.filename))
          when "line_number", "end_line_number"
            MacroEvaluation.new(location.line_number.to_i64)
          else
            MacroEvaluation.new(location.column_number.to_i64)
          end
        when "doc"
          return nil unless args.empty? && receiver.is_a?(MacroSyntaxValue)
          MacroEvaluation.new(MacroSyntaxValue.generated_string(receiver.metadata.try(&.doc) || ""))
        when "doc_comment"
          return nil unless args.empty? && receiver.is_a?(MacroSyntaxValue)
          doc = receiver.metadata.try(&.doc)
          MacroEvaluation.new(MacroSyntaxValue.code(doc ? doc.gsub("\n", "\n# ") : ""))
        when "class_name"
          return nil unless args.empty?
          MacroEvaluation.new(MacroSyntaxValue.string(macro_class_name(receiver)))
        when "kind"
          return nil unless args.empty?
          number = macro_number(receiver)
          return nil unless number
          MacroEvaluation.new(MacroSyntaxValue.symbol(macro_number_kind_name(number.kind)))
        when "to_number"
          return nil unless args.empty?
          number = macro_number(receiver)
          return nil unless number
          MacroEvaluation.new(MacroSyntaxValue.code(macro_number_text(number.value)))
        when "zero?"
          return nil unless args.empty?
          number = macro_number(receiver)
          return nil unless number
          MacroEvaluation.new(number.value == 0)
        when "ord"
          return nil unless args.empty? && receiver.is_a?(MacroSyntaxValue) && receiver.kind == MacroSyntaxKind::CharLiteral
          char = receiver.value.chars.first?
          char ? MacroEvaluation.new(char.ord.to_i64) : nil
        when "source"
          return nil unless args.empty? && receiver.is_a?(MacroSyntaxValue) && receiver.kind == MacroSyntaxKind::RegexLiteral
          MacroEvaluation.new(MacroSyntaxValue.string(receiver.value))
        when "options"
          return nil unless args.empty? && receiver.is_a?(MacroSyntaxValue) && receiver.kind == MacroSyntaxKind::RegexLiteral
          slash = receiver.source.rindex('/')
          options = slash ? receiver.source.byte_slice(slash + 1..).chars : [] of Char
          values = {'i', 'm', 'x'}.select { |option| options.includes?(option) }
            .map { |option| MacroSyntaxValue.generated_symbol(option.to_s).as(MacroValue) }
          if values.empty?
            MacroEvaluation.new(MacroSyntaxValue.code("[] of ::Symbol"))
          else
            MacroEvaluation.new(values)
          end
        when "count"
          return nil unless args.size == 1
          text = macro_scalar_text(receiver)
          search = macro_scalar_text(args[0])
          return nil unless text && search
          MacroEvaluation.new(text.chars.count { |char| search.includes?(char) }.to_i64)
        when "tr"
          return nil unless args.size == 2
          text = macro_scalar_text(receiver)
          from = macro_scalar_text(args[0])
          to = macro_scalar_text(args[1])
          return nil unless text && from && to
          MacroEvaluation.new(MacroSyntaxValue.string(text.tr(from, to)))
        when "gsub"
          text = macro_scalar_text(receiver)
          return nil unless text
          if block
            return nil unless args.size == 1
            pattern = macro_regex(args[0])
            return nil unless pattern
            failed = false
            value = text.gsub(pattern) do |full_text, match|
              captures = {} of String => MacroValue
              match.size.times do |index|
                capture = match[index]?
                captures[index.to_s] = capture ? MacroSyntaxValue.generated_string(capture) : nil
              end
              full = MacroSyntaxValue.generated_string(full_text)
              evaluation = eval_macro_eval_block(block, [full.as(MacroValue), captures.as(MacroValue)] of MacroValue)
              unless evaluation
                failed = true
                next full_text
              end
              macro_scalar_text(evaluation.value) || val_to_string(evaluation.value)
            end
            return nil if failed
            MacroEvaluation.new(MacroSyntaxValue.string(value))
          else
            return nil unless args.size == 2
            pattern = macro_regex(args[0])
            replacement = macro_scalar_text(args[1])
            return nil unless pattern && replacement
            MacroEvaluation.new(MacroSyntaxValue.string(text.gsub(pattern, replacement)))
          end
        when "match"
          return nil unless args.size == 1
          text = macro_scalar_text(receiver)
          pattern = macro_regex(args[0])
          return nil unless text && pattern
          match = pattern.match(text)
          MacroEvaluation.new(match ? MacroSyntaxValue.code(macro_match_source(match, args[0])) : nil)
        when "scan"
          return nil unless args.size == 1
          text = macro_scalar_text(receiver)
          pattern = macro_regex(args[0])
          return nil unless text && pattern
          matches = text.scan(pattern).map { |match| macro_match_source(match, args[0]) }
          type = "::Hash(::Int32 | ::String, ::String | ::Nil)"
          source = "[#{matches.join(", ")}] of #{type}"
          MacroEvaluation.new(MacroSyntaxValue.code(source))
        when "camelcase"
          return nil unless args.size <= 1
          text = macro_scalar_text(receiver)
          return nil unless text
          value = text.split('_').map(&.capitalize).join
          if lower = args.first?
            return nil unless lower.is_a?(Bool)
            value = value[0]?.try(&.downcase).to_s + value.byte_slice(1..)
          end
          MacroEvaluation.new(MacroSyntaxValue.string(value))
        when "underscore"
          return nil unless args.empty?
          text = macro_scalar_text(receiver)
          return nil unless text
          value = text.gsub(/([a-z\d])([A-Z])/, "\\1_\\2").downcase
          MacroEvaluation.new(MacroSyntaxValue.string(value))
        when "titleize"
          return nil unless args.empty?
          text = macro_scalar_text(receiver)
          return nil unless text
          MacroEvaluation.new(MacroSyntaxValue.string(text.split.map(&.capitalize).join(' ')))
        when "identify"
          return nil unless args.empty?
          text = macro_scalar_text(receiver)
          return nil unless text
          MacroEvaluation.new(MacroSyntaxValue.string(text.gsub("::", "__")))
        when "to_utf16"
          return nil unless args.empty?
          text = macro_scalar_text(receiver)
          return nil unless text
          units = text.to_utf16.to_a
          literal = (units + [0_u16]).map { |unit| "#{unit}_u16" }.join(", ")
          source = "(::Slice(::UInt16).literal(#{literal}))[0, #{units.size}]"
          MacroEvaluation.new(MacroSyntaxValue.code(source))
        when "map", "map_with_index"
          return nil unless block && args.empty?
          values = macro_iteration_values(receiver, block.parameters.size, name == "map_with_index")
          return nil unless values
          mapped = [] of MacroValue
          values.each do |iteration|
            evaluation = eval_macro_eval_block(block, iteration)
            return nil unless evaluation
            mapped << evaluation.value
          end
          MacroEvaluation.new(macro_collection_result(receiver, mapped))
        when "reduce"
          return nil unless block && args.size <= 1
          values = macro_sequence_values(receiver)
          return nil unless values
          if args.empty?
            accumulator = values.first?
            return MacroEvaluation.new(nil) unless accumulator
            remaining = values.skip(1)
          else
            accumulator = args[0]
            remaining = values
          end
          remaining.each do |value|
            evaluation = eval_macro_eval_block(block, [accumulator, value] of MacroValue)
            return nil unless evaluation
            accumulator = evaluation.value
          end
          MacroEvaluation.new(accumulator)
        when "find"
          return nil unless block && args.empty?
          values = macro_sequence_values(receiver)
          return nil unless values
          values.each do |value|
            evaluation = eval_macro_eval_block(block, [value] of MacroValue)
            return nil unless evaluation
            return MacroEvaluation.new(value) if truthy?(evaluation.value)
          end
          MacroEvaluation.new(nil)
        when "sort_by"
          return nil unless block && args.empty?
          sequence = macro_sequence_values(receiver)
          return nil unless sequence
          values = sequence.dup
          keys = [] of String
          values.each do |value|
            evaluation = eval_macro_eval_block(block, [value] of MacroValue)
            return nil unless evaluation
            keys << (macro_scalar_text(evaluation.value) || val_to_string(evaluation.value))
          end
          index = 1
          while index < values.size
            cursor = index
            while cursor > 0 && keys[cursor] < keys[cursor - 1]
              keys.swap(cursor, cursor - 1)
              values.swap(cursor, cursor - 1)
              cursor -= 1
            end
            index += 1
          end
          MacroEvaluation.new(macro_collection_result(receiver, values))
        when "unshift"
          return nil unless !args.empty? && (values = macro_sequence_values(receiver))
          args.reverse_each { |value| values.unshift(value) }
          MacroEvaluation.new(macro_collection_result(receiver, values))
        when "push"
          return nil unless !args.empty? && (values = macro_sequence_values(receiver))
          values.concat(args)
          MacroEvaluation.new(macro_collection_result(receiver, values))
        when "splat"
          return nil unless args.size <= 1
          values = macro_sequence_values(receiver)
          return nil unless values
          suffix = args.empty? ? "" : macro_scalar_text(args[0])
          return nil unless suffix
          MacroEvaluation.new(MacroSyntaxValue.code(values.map { |value| val_to_string(value) }.join(", ") + suffix))
        when "select", "reject"
          if receiver.is_a?(MacroHashValue)
            selected = if block
                         return nil unless args.empty?
                         receiver.entries.select do |entry|
                           evaluation = eval_macro_eval_block(block, [entry.key, entry.value] of MacroValue)
                           return nil unless evaluation
                           keep = truthy?(evaluation.value)
                           name == "select" ? keep : !keep
                         end
                       else
                         return nil if args.empty?
                         receiver.entries.select do |entry|
                           included = args.includes?(entry.key)
                           name == "select" ? included : !included
                         end
                       end
            MacroEvaluation.new(MacroHashValue.new(selected))
          elsif receiver.is_a?(Hash(String, MacroValue))
            selected = if block
                         return nil unless args.empty?
                         receiver.select do |key, value|
                           key_value = MacroSyntaxValue.identifier(key).as(MacroValue)
                           evaluation = eval_macro_eval_block(block, [key_value, value] of MacroValue)
                           return nil unless evaluation
                           keep = truthy?(evaluation.value)
                           name == "select" ? keep : !keep
                         end
                       else
                         return nil if args.empty?
                         requested = args.compact_map { |value| macro_scalar_text(value) }
                         receiver.select do |key, _|
                           included = requested.includes?(key)
                           name == "select" ? included : !included
                         end
                       end
            MacroEvaluation.new(selected)
          else
            return nil unless block && args.empty?
            values = macro_sequence_values(receiver)
            return nil unless values
            selected = values.select do |value|
              evaluation = eval_macro_eval_block(block, [value] of MacroValue)
              return nil unless evaluation
              keep = truthy?(evaluation.value)
              name == "select" ? keep : !keep
            end
            MacroEvaluation.new(macro_collection_result(receiver, selected))
          end
        when "any?", "all?"
          return nil unless block && args.empty?
          values = macro_iteration_values(receiver, block.parameters.size, false)
          return nil unless values
          result = name == "all?"
          values.each do |iteration|
            evaluation = eval_macro_eval_block(block, iteration)
            return nil unless evaluation
            truthy = truthy?(evaluation.value)
            if name == "any?" && truthy
              result = true
              break
            elsif name == "all?" && !truthy
              result = false
              break
            end
          end
          MacroEvaluation.new(result)
        when "each", "each_with_index"
          return nil unless block && args.empty?
          values = macro_iteration_values(receiver, block.parameters.size, name == "each_with_index")
          return nil unless values
          values.each do |iteration|
            return nil unless eval_macro_eval_block(block, iteration)
          end
          MacroEvaluation.new(receiver)
        when "size"
          size = if text = macro_scalar_text(receiver)
                   text.size
                 else
                   case receiver
                   when Array(MacroValue)        then receiver.size
                   when MacroArrayValue          then receiver.values.size
                   when MacroTupleValue          then receiver.values.size
                   when MacroHashValue           then receiver.entries.size
                   when Hash(String, MacroValue) then receiver.size
                   else                               return nil
                   end
                 end
          MacroEvaluation.new(size.to_i64)
        when "empty?"
          empty = if text = macro_scalar_text(receiver)
                    text.empty?
                  else
                    case receiver
                    when MacroBlockValue          then receiver.body.empty?
                    when Array(MacroValue)        then receiver.empty?
                    when MacroArrayValue          then receiver.values.empty?
                    when MacroTupleValue          then receiver.values.empty?
                    when MacroHashValue           then receiver.entries.empty?
                    when Hash(String, MacroValue) then receiver.empty?
                    else                               return nil
                    end
                  end
          MacroEvaluation.new(empty)
        when "first", "last"
          return nil unless args.empty?
          value = if text = macro_scalar_text(receiver)
                    char = name == "first" ? text.chars.first? : text.chars.last?
                    char ? MacroSyntaxValue.string(char.to_s).as(MacroValue) : nil
                  elsif values = macro_sequence_values(receiver)
                    name == "first" ? values.first? : values.last?
                  else
                    return nil
                  end
          MacroEvaluation.new(value)
        when "sort"
          return nil unless args.empty?
          values = macro_sequence_values(receiver)
          return nil unless values
          MacroEvaluation.new(macro_collection_result(receiver, sort_macro_values(values)))
        when "reverse"
          return nil unless args.empty?
          values = macro_sequence_values(receiver)
          return nil unless values
          MacroEvaluation.new(macro_collection_result(receiver, values.reverse))
        when "uniq"
          return nil unless args.empty?
          values = macro_sequence_values(receiver)
          return nil unless values
          unique = [] of MacroValue
          values.each do |value|
            unique << value unless unique.any? { |existing| existing == value }
          end
          MacroEvaluation.new(macro_collection_result(receiver, unique))
        when "compact"
          return nil unless args.empty?
          values = macro_sequence_values(receiver)
          return nil unless values
          MacroEvaluation.new(macro_collection_result(receiver, values.reject(&.nil?)))
        when "keys"
          return nil unless args.empty?
          if receiver.is_a?(MacroHashValue)
            MacroEvaluation.new(receiver.entries.map { |entry| entry.key.as(MacroValue) })
          elsif receiver.is_a?(Hash(String, MacroValue))
            MacroEvaluation.new(receiver.keys.map { |key| MacroSyntaxValue.identifier(key).as(MacroValue) })
          end
        when "values"
          return nil unless args.empty?
          if receiver.is_a?(MacroHashValue)
            MacroEvaluation.new(receiver.entries.map { |entry| entry.value.as(MacroValue) })
          elsif receiver.is_a?(Hash(String, MacroValue))
            values = receiver.map { |_, value| value.as(MacroValue) }
            MacroEvaluation.new(values)
          end
        when "to_a"
          return nil unless args.empty?
          values = if receiver.is_a?(MacroHashValue)
                     receiver.entries.map do |entry|
                       MacroTupleValue.new([entry.key, entry.value] of MacroValue).as(MacroValue)
                     end
                   elsif receiver.is_a?(Hash(String, MacroValue))
                     receiver.map do |key, value|
                       key_value = MacroSyntaxValue.identifier(key).as(MacroValue)
                       MacroTupleValue.new([key_value, value] of MacroValue).as(MacroValue)
                     end
                   else
                     return nil
                   end
          MacroEvaluation.new(values)
        when "double_splat"
          return nil unless args.size <= 1
          suffix = args.empty? ? "" : macro_scalar_text(args[0])
          return nil unless suffix
          body = if receiver.is_a?(MacroHashValue)
                   macro_double_splat(receiver)
                 elsif receiver.is_a?(Hash(String, MacroValue))
                   receiver.map do |key, value|
                     rendered_key = key.matches?(/\A[a-zA-Z_][a-zA-Z0-9_]*[?!]?\z/) ? key : key.inspect
                     "#{rendered_key}: #{val_to_string(value)}"
                   end.join(", ")
                 else
                   return nil
                 end
          MacroEvaluation.new(MacroSyntaxValue.code(body + suffix))
        when "of", "type", "of_key", "of_value"
          supported = macro_sequence_values(receiver) || receiver.is_a?(MacroHashValue) || receiver.is_a?(Hash(String, MacroValue))
          return nil unless args.empty? && supported
          MacroEvaluation.new(MacroSyntaxValue.code(""))
        when "has_key?"
          return nil unless args.size == 1
          if receiver.is_a?(MacroHashValue)
            MacroEvaluation.new(receiver.entries.any? { |entry| entry.key == args[0] })
          elsif receiver.is_a?(Hash(String, MacroValue))
            key = macro_scalar_text(args[0])
            key ? MacroEvaluation.new(receiver.has_key?(key)) : nil
          end
        when "body"
          return nil unless receiver.is_a?(MacroBlockValue) && args.empty?
          MacroEvaluation.new(receiver.body)
        when "args"
          return nil unless receiver.is_a?(MacroBlockValue) && args.empty?
          MacroEvaluation.new(receiver.parameters.map(&.as(MacroValue)))
        when "nil?"
          return nil unless args.empty?
          MacroEvaluation.new(receiver.nil?)
        when "is_a?"
          return nil unless args.size == 1
          type_name = macro_scalar_text(args[0])
          return nil unless type_name
          MacroEvaluation.new(macro_value_is_a?(receiver, type_name))
        when "responds_to?"
          return nil unless args.size == 1
          method_name = macro_scalar_text(args[0])
          return nil unless method_name
          MacroEvaluation.new(macro_value_responds_to?(receiver, method_name))
        when "includes?"
          return nil unless args.size == 1
          result = if text = macro_scalar_text(receiver)
                     other = macro_scalar_text(args[0])
                     other ? text.includes?(other) : false
                   else
                     case receiver
                     when Array(MacroValue)
                       receiver.includes?(args[0])
                     when MacroArrayValue
                       receiver.values.includes?(args[0])
                     when MacroTupleValue
                       receiver.values.includes?(args[0])
                     when MacroHashValue
                       receiver.entries.any? { |entry| entry.key == args[0] }
                     when Hash(String, MacroValue)
                       key = macro_scalar_text(args[0])
                       key ? receiver.has_key?(key) : false
                     else
                       return nil
                     end
                   end
          MacroEvaluation.new(result)
        when "starts_with?", "ends_with?"
          return nil unless args.size == 1
          text = macro_scalar_text(receiver)
          other = macro_scalar_text(args[0])
          return nil unless text && other
          MacroEvaluation.new(name == "starts_with?" ? text.starts_with?(other) : text.ends_with?(other))
        when "upcase", "downcase", "capitalize", "strip", "chomp"
          return nil unless args.empty?
          text = macro_scalar_text(receiver)
          return nil unless text
          value = case name
                  when "upcase"     then text.upcase
                  when "downcase"   then text.downcase
                  when "capitalize" then text.capitalize
                  when "strip"      then text.strip
                  else                   text.chomp
                  end
          MacroEvaluation.new(MacroSyntaxValue.string(value))
        when "split"
          return nil unless args.size <= 1
          text = macro_scalar_text(receiver)
          return nil unless text
          pieces = if args.empty?
                     text.split
                   elsif separator = macro_regex(args[0])
                     text.split(separator)
                   elsif separator = macro_scalar_text(args[0])
                     text.split(separator)
                   else
                     return nil
                   end
          MacroEvaluation.new(pieces.map { |piece| MacroSyntaxValue.generated_string(piece).as(MacroValue) })
        when "lines"
          return nil unless args.empty?
          text = macro_scalar_text(receiver)
          return nil unless text
          MacroEvaluation.new(text.lines.map { |line| MacroSyntaxValue.generated_string(line).as(MacroValue) })
        when "join"
          return nil unless args.size <= 1
          values = macro_sequence_values(receiver)
          return nil unless values
          separator = args.empty? ? "" : macro_scalar_text(args[0])
          return nil unless separator
          joined = values.map { |value| macro_scalar_text(value) || val_to_string(value) }.join(separator)
          MacroEvaluation.new(MacroSyntaxValue.string(joined))
        when "to_i"
          return nil unless args.size <= 1
          text = macro_scalar_text(receiver)
          return nil unless text
          base = args.empty? ? 10_i64 : args[0]
          return nil unless base.is_a?(Int64)
          value = text.to_i64?(base.to_i)
          value ? MacroEvaluation.new(value) : nil
        when "id"
          return nil unless args.empty?
          text = macro_scalar_text(receiver)
          text ? MacroEvaluation.new(MacroSyntaxValue.identifier(text)) : nil
        when "stringify"
          return nil unless args.empty?
          MacroEvaluation.new(MacroSyntaxValue.string(val_to_string(receiver)))
        when "symbolize"
          return nil unless args.empty?
          text = macro_scalar_text(receiver)
          text ? MacroEvaluation.new(MacroSyntaxValue.symbol(text)) : nil
        else
          nil
        end
      end

      private def apply_type_aware_macro_method(
        receiver : MacroValue,
        name : String,
        args : Array(MacroValue),
      ) : MacroEvaluation?
        case receiver
        when MacroSyntaxValue
          if args.empty? && {"resolve", "resolve?"}.includes?(name)
            if structure = receiver.metadata.try(&.structure)
              if resolved = macro_resolve_captured_type_syntax(structure)
                mark_type_introspection
                return MacroEvaluation.new(resolved)
              end
              if macro_captured_resolvable_type_syntax?(structure)
                mark_type_introspection
                return MacroEvaluation.new(nil) if name == "resolve?"
                return nil
              end
            end
          end
          captured_name = receiver.metadata.try(&.fields.has_key?("name")) || false
          if name == "name" && !captured_name && receiver.kind == MacroSyntaxKind::Code && args.size <= 1 && receiver.value.matches?(/\A::?[A-Z]|\A[A-Z]/)
            generic_args = args.empty? ? true : args[0]
            return nil unless generic_args.is_a?(Bool)
            value = generic_args ? receiver.value : receiver.value.split('(', 2).first
            return MacroEvaluation.new(MacroSyntaxValue.code(value))
          end
          return nil unless {"resolve", "resolve?"}.includes?(name) && args.empty?
          mark_type_introspection
          resolved = macro_type_value(receiver.value, @active_index, current_type_scope)
          return MacroEvaluation.new(resolved) if resolved
          return MacroEvaluation.new(nil) if name == "resolve?"
        when MacroTypeValue
          if evaluation = apply_annotation_lookup(macro_type_annotations(receiver), name, args)
            return evaluation
          end
          return nil unless args.empty?
          mark_type_introspection
          case name
          when "name"
            return MacroEvaluation.new(MacroSyntaxValue.identifier(receiver.name))
          when "methods"
            methods = macro_methods(receiver).map(&.as(MacroValue))
            return MacroEvaluation.new(methods)
          when "instance_vars"
            variables = macro_instance_vars(receiver).map(&.as(MacroValue))
            return MacroEvaluation.new(variables)
          when "constants"
            constants = macro_constants(receiver).map(&.as(MacroValue))
            return MacroEvaluation.new(constants)
          when "superclass"
            return MacroEvaluation.new(macro_superclass(receiver))
          when "ancestors"
            ancestors = macro_ancestors(receiver).map(&.as(MacroValue))
            return MacroEvaluation.new(ancestors)
          when "class?"
            return MacroEvaluation.new(receiver.kind == MacroTypeKind::Class)
          when "module?"
            return MacroEvaluation.new(receiver.kind == MacroTypeKind::Module)
          when "struct?"
            return MacroEvaluation.new(receiver.kind == MacroTypeKind::Struct)
          when "enum?"
            return MacroEvaluation.new(receiver.kind == MacroTypeKind::Enum)
          when "lib?"
            return MacroEvaluation.new(receiver.kind == MacroTypeKind::Lib)
          end
        when MacroMethodValue
          if evaluation = apply_annotation_lookup(receiver.annotations, name, args)
            return evaluation
          end
          return nil unless args.empty?
          case name
          when "name"
            return MacroEvaluation.new(MacroSyntaxValue.identifier(receiver.name))
          when "args"
            return MacroEvaluation.new(receiver.args.map(&.as(MacroValue)))
          when "return_type"
            value = receiver.return_type.try { |source| MacroSyntaxValue.code(source).as(MacroValue) }
            return MacroEvaluation.new(value)
          when "body"
            value = receiver.body.try { |source| MacroSyntaxValue.code(source).as(MacroValue) }
            return MacroEvaluation.new(value)
          when "source"
            return MacroEvaluation.new(MacroSyntaxValue.code(receiver.source))
          end
        when MacroMetaVarValue
          if evaluation = apply_annotation_lookup(receiver.annotations, name, args)
            return evaluation
          end
          return nil unless args.empty?
          case name
          when "name"
            return MacroEvaluation.new(MacroSyntaxValue.identifier(receiver.name))
          when "type"
            value = receiver.type_name.try { |source| MacroSyntaxValue.code(source).as(MacroValue) }
            return MacroEvaluation.new(value)
          when "default_value"
            value = receiver.default_value.try { |source| MacroSyntaxValue.code(source).as(MacroValue) }
            return MacroEvaluation.new(value)
          when "has_default_value?"
            return MacroEvaluation.new(!receiver.default_value.nil?)
          end
        when MacroAnnotationValue
          return nil unless args.empty?
          case name
          when "name"
            return MacroEvaluation.new(MacroSyntaxValue.identifier(receiver.name))
          when "args"
            values = receiver.positional_sources.map { |source| eval_annotation_source(source).as(MacroValue) }
            return MacroEvaluation.new(values)
          when "named_args"
            values = receiver.named_sources.transform_values { |source| eval_annotation_source(source).as(MacroValue) }
            return MacroEvaluation.new(values)
          end
        end
        nil
      end

      private def apply_annotation_lookup(
        annotations : Array(MacroAnnotationValue),
        name : String,
        args : Array(MacroValue),
      ) : MacroEvaluation?
        case name
        when "annotation"
          return nil unless args.size == 1
          annotation_name = macro_annotation_lookup_name(args[0])
          return nil unless annotation_name
          value = annotations.reverse_each.find { |entry| entry.name == annotation_name }
          MacroEvaluation.new(value)
        when "annotations"
          return MacroEvaluation.new(annotations.map(&.as(MacroValue))) if args.empty?
          return nil unless args.size == 1
          annotation_name = macro_annotation_lookup_name(args[0])
          return nil unless annotation_name
          values = annotations.select { |entry| entry.name == annotation_name }.map(&.as(MacroValue))
          MacroEvaluation.new(values)
        else
          nil
        end
      end

      private def macro_annotation_lookup_name(value : MacroValue) : String?
        name = case value
               when MacroTypeValue
                 value.name
               else
                 macro_scalar_text(value)
               end
        name.try(&.lchop("::"))
      end

      private def macro_type_annotations(type : MacroTypeValue) : Array(MacroAnnotationValue)
        index = @active_index
        return [] of MacroAnnotationValue unless index
        refs = index.types[type.name]? || [] of DeclRef
        refs.flat_map do |ref|
          macro_annotations(syntax_tree(ref.ast).node(ref.node_id))
        end
      end

      private def eval_annotation_source(source : String) : MacroValue
        ast = Parser.new(Source.new(source, "macro-annotation-value.cr", SourceKind::Virtual)).parse_file
        root_children = ast.children(ast.root)
        expressions = root_children.first?
        expression = expressions.try { |node_id| ast.children(node_id).first? }
        return MacroSyntaxValue.code(source) unless expression
        evaluation = eval_value(expression, ast)
        evaluation ? evaluation.value : MacroSyntaxValue.code(source)
      end

      private def macro_type_value(
        name : String,
        index : ProgramIndex?,
        scope : String? = nil,
        absolute : Bool = false,
      ) : MacroTypeValue?
        normalized = normalize_macro_type_name(name)
        return nil if normalized.empty?
        lookup = absolute ? "::#{normalized}" : normalized
        if ref = index.try(&.type_for(lookup, scope))
          node = syntax_tree(ref.ast).node(ref.node_id)
          return MacroTypeValue.new(ref.scope, macro_type_kind(node.kind))
        end
        return MacroTypeValue.new(normalized, MacroTypeKind::Builtin) if builtin_type_name?(normalized)
        nil
      end

      private def macro_resolve_captured_type_syntax(node : MacroCapturedNode) : MacroTypeValue?
        case node.kind
        when "Crystal::Path"
          macro_type_value(node.source, @active_index, current_type_scope)
        when "Crystal::Generic"
          name = node.fields["name"]?
          return nil unless name
          base = macro_resolve_captured_type_syntax(name)
          return nil unless base
          type_vars = node.collections["type_vars"]? || [] of MacroCapturedNode
          rendered = [] of String
          type_vars.each do |type_var|
            resolved = macro_resolve_captured_type_syntax(type_var)
            return nil unless resolved
            rendered << resolved.name
          end
          MacroTypeValue.new("#{base.name}(#{rendered.join(", ")})", base.kind)
        when "Crystal::Union"
          types = node.collections["types"]? || [] of MacroCapturedNode
          rendered = [] of String
          types.each do |type|
            resolved = macro_resolve_captured_type_syntax(type)
            return nil unless resolved
            rendered << resolved.name
          end
          MacroTypeValue.new("(#{rendered.join(" | ")})", MacroTypeKind::Builtin)
        when "Crystal::ProcNotation"
          inputs = node.collections["inputs"]? || [] of MacroCapturedNode
          rendered = [] of String
          inputs.each do |input|
            resolved = macro_resolve_captured_type_syntax(input)
            return nil unless resolved
            rendered << resolved.name
          end
          if output = node.fields["output"]?
            unless output.kind == "Crystal::NilLiteral"
              resolved_output = macro_resolve_captured_type_syntax(output)
              return nil unless resolved_output
              rendered << resolved_output.name
            end
          end
          MacroTypeValue.new("Proc(#{rendered.join(", ")})", MacroTypeKind::Builtin)
        when "Crystal::Metaclass"
          instance = node.fields["instance"]?
          return nil unless instance
          resolved = macro_resolve_captured_type_syntax(instance)
          return nil unless resolved
          MacroTypeValue.new("#{macro_metaclass_instance_name(resolved.name)}.class", MacroTypeKind::Builtin)
        else
          if {"Crystal::Var", "Crystal::MacroId"}.includes?(node.kind)
            macro_type_value(node.source, @active_index, current_type_scope)
          end
        end
      end

      private def macro_captured_resolvable_type_syntax?(node : MacroCapturedNode) : Bool
        {"Crystal::Path", "Crystal::Generic", "Crystal::Union", "Crystal::ProcNotation", "Crystal::Metaclass"}.includes?(node.kind)
      end

      private def macro_metaclass_instance_name(name : String) : String
        case name
        when "Array"       then "Array(T)"
        when "Hash"        then "Hash(K, V)"
        when "Pointer"     then "Pointer(T)"
        when "Slice"       then "Slice(T)"
        when "StaticArray" then "StaticArray(T, N)"
        when "Range"       then "Range(B, E)"
        else                    name
        end
      end

      private def macro_type_kind(kind : NodeKind) : MacroTypeKind
        case kind
        when NodeKind::Module then MacroTypeKind::Module
        when NodeKind::Struct then MacroTypeKind::Struct
        when NodeKind::Enum   then MacroTypeKind::Enum
        when NodeKind::Lib    then MacroTypeKind::Lib
        else                       MacroTypeKind::Class
        end
      end

      private def macro_methods(type : MacroTypeValue) : Array(MacroMethodValue)
        index = @active_index
        return [] of MacroMethodValue unless index
        index.methods_for(type.name).compact_map do |ref|
          node = syntax_tree(ref.ast).node(ref.node_id)
          name = node.name
          next unless name
          args = node.parameters.compact_map { |parameter| macro_meta_var(parameter) }
          MacroMethodValue.new(
            name,
            args,
            node.return_type.try(&.text),
            node.body.try(&.text),
            node.text,
            macro_annotations(node)
          )
        end
      end

      private def macro_instance_vars(type : MacroTypeValue) : Array(MacroMetaVarValue)
        index = @active_index
        return [] of MacroMetaVarValue unless index
        seen = Set(String).new
        index.instance_vars_for(type.name).compact_map do |ref|
          node = syntax_tree(ref.ast).node(ref.node_id)
          name = node.symbol_name.try(&.lstrip('@'))
          next unless name
          next if seen.includes?(name)
          seen << name
          owner = node.ancestors.find do |ancestor|
            {NodeKind::Param, NodeKind::VarDecl}.includes?(ancestor.kind)
          end
          MacroMetaVarValue.new(
            name,
            owner.try(&.declared_type).try(&.text),
            owner.try(&.value).try(&.text),
            owner ? macro_annotations(owner) : [] of MacroAnnotationValue
          )
        end
      end

      private def macro_constants(type : MacroTypeValue) : Array(MacroSyntaxValue)
        index = @active_index
        return [] of MacroSyntaxValue unless index
        seen = Set(String).new
        index.constants_for(type.name).compact_map do |ref|
          node = syntax_tree(ref.ast).node(ref.node_id)
          name = node.symbol_name
          next unless name
          next if seen.includes?(name)
          seen << name
          MacroSyntaxValue.identifier(name)
        end
      end

      private def macro_superclass(type : MacroTypeValue) : MacroTypeValue?
        index = @active_index
        return nil unless index
        name = index.superclass_for(type.name)
        return nil unless name
        macro_type_value(name, index, type.name)
      end

      private def macro_ancestors(type : MacroTypeValue) : Array(MacroTypeValue)
        ancestors = [] of MacroTypeValue
        seen = Set(String).new
        current = type
        while parent = macro_superclass(current)
          break if seen.includes?(parent.name)
          seen << parent.name
          ancestors << parent
          current = parent
        end
        ancestors
      end

      private def macro_meta_var(node : SyntaxNode) : MacroMetaVarValue?
        target = unannotated_node(node)
        return nil unless {NodeKind::Param, NodeKind::Splat, NodeKind::DoubleSplat, NodeKind::BlockParam}.includes?(target.kind)
        name = target.name.try(&.lstrip('@'))
        return nil unless name
        MacroMetaVarValue.new(
          name,
          target.declared_type.try(&.text),
          target.value.try(&.text),
          macro_annotations(node)
        )
      end

      private def unannotated_node(node : SyntaxNode) : SyntaxNode
        current = node
        while current.kind == NodeKind::Annotation
          target = current.child(1)
          break unless target
          current = target
        end
        current
      end

      private def macro_annotations(node : SyntaxNode) : Array(MacroAnnotationValue)
        wrappers = [] of SyntaxNode
        current = node
        if current.kind == NodeKind::Annotation
          while current.kind == NodeKind::Annotation
            wrappers << current
            target = current.child(1)
            break unless target
            current = target
          end
        else
          child = current
          while parent = child.parent
            break unless parent.kind == NodeKind::Annotation && parent.child(1).try(&.id) == child.id
            wrappers << parent
            child = parent
          end
          wrappers.reverse!
        end
        wrappers.compact_map { |wrapper| macro_annotation(wrapper) }
      end

      private def macro_annotation(wrapper : SyntaxNode) : MacroAnnotationValue?
        expression = wrapper.child(0)
        return nil unless expression
        name = macro_annotation_name(expression)
        return nil unless name
        call = if expression.kind == NodeKind::Call
                 expression
               else
                 expression.descendants.find { |child| child.kind == NodeKind::Call }
               end
        positional = [] of String
        named = {} of String => String
        if call
          call.arguments.each do |argument|
            if argument.kind == NodeKind::NamedArg
              argument_name = argument.name
              argument_value = argument.value
              named[argument_name] = argument_value.text if argument_name && argument_value
            else
              positional << argument.text
            end
          end
        end
        MacroAnnotationValue.new(name, positional, named)
      end

      private def macro_annotation_name(expression : SyntaxNode) : String?
        case expression.kind
        when NodeKind::Path
          parts = expression.children.compact_map do |child|
            child.kind == NodeKind::Call ? child.call_name : child.symbol_name
          end
          parts.empty? ? nil : parts.join("::")
        when NodeKind::Call
          expression.call_name
        else
          expression.symbol_name
        end
      end

      private def current_type_scope : String?
        value = current_macro_env["@type"]?
        value.is_a?(MacroTypeValue) ? value.name : nil
      end

      private def mark_type_introspection : Nil
        @active_footprint.try(&.type_introspection)
      end

      private def normalize_macro_type_name(name : String) : String
        normalized = name.strip.lchop("::")
        normalized = normalized.split('(', 2).first
        normalized.rchop('?')
      end

      private def builtin_type_name?(name : String) : Bool
        {
          "Nil", "Bool", "Char", "String", "Symbol", "Regex", "Number",
          "Int8", "Int16", "Int32", "Int64", "Int128",
          "UInt8", "UInt16", "UInt32", "UInt64", "UInt128",
          "Float32", "Float64", "Array", "Hash", "Tuple", "NamedTuple",
          "Proc", "Pointer", "Slice", "StaticArray", "Range", "IO",
        }.includes?(name)
      end

      private def sort_macro_values(values : Array(MacroValue)) : Array(MacroValue)
        sorted = values.dup
        index = 1
        while index < sorted.size
          cursor = index
          while cursor > 0
            left = macro_scalar_text(sorted[cursor - 1]) || val_to_string(sorted[cursor - 1])
            right = macro_scalar_text(sorted[cursor]) || val_to_string(sorted[cursor])
            break unless right < left
            sorted.swap(cursor - 1, cursor)
            cursor -= 1
          end
          index += 1
        end
        sorted
      end

      private def macro_iteration_values(
        receiver : MacroValue,
        parameter_count : Int32,
        with_index : Bool,
      ) : Array(Array(MacroValue))?
        values = [] of Array(MacroValue)
        case receiver
        when Array(MacroValue)
          receiver.each_with_index do |value, index|
            iteration = [value] of MacroValue
            iteration << index.to_i64 if with_index
            values << iteration
          end
        when MacroArrayValue
          receiver.values.each_with_index do |value, index|
            iteration = [value] of MacroValue
            iteration << index.to_i64 if with_index
            values << iteration
          end
        when MacroHashValue
          receiver.entries.each_with_index do |entry, index|
            iteration = if parameter_count <= 1
                          [entry.key] of MacroValue
                        else
                          [entry.key, entry.value] of MacroValue
                        end
            iteration << index.to_i64 if with_index
            values << iteration
          end
        when MacroTupleValue
          receiver.values.each_with_index do |value, index|
            iteration = [value] of MacroValue
            iteration << index.to_i64 if with_index
            values << iteration
          end
        when Hash(String, MacroValue)
          receiver.each_with_index do |(key, value), index|
            key_value = MacroSyntaxValue.identifier(key).as(MacroValue)
            iteration = if parameter_count <= 1
                          [key_value] of MacroValue
                        else
                          [key_value, value] of MacroValue
                        end
            iteration << index.to_i64 if with_index
            values << iteration
          end
        when String
          receiver.each_char_with_index do |char, index|
            iteration = [char.to_s.as(MacroValue)] of MacroValue
            iteration << index.to_i64 if with_index
            values << iteration
          end
        when MacroSyntaxValue
          receiver.value.each_char_with_index do |char, index|
            iteration = [MacroSyntaxValue.string(char.to_s).as(MacroValue)] of MacroValue
            iteration << index.to_i64 if with_index
            values << iteration
          end
        when MacroRangeValue
          first = receiver.first
          last = receiver.last
          return nil unless first && last
          finish = receiver.exclusive ? last - 1 : last
          Range.new(first, finish).each_with_index do |value, index|
            iteration = [value.as(MacroValue)] of MacroValue
            iteration << index.to_i64 if with_index
            values << iteration
          end
        else
          return nil
        end
        values
      end

      private def macro_number(value : MacroValue) : MacroNumberValue?
        case value
        when Int64
          kind = if value.in?(Int32::MIN.to_i64..Int32::MAX.to_i64)
                   MacroNumberKind::I32
                 else
                   MacroNumberKind::I64
                 end
          MacroNumberValue.new(value.to_i128, kind, value.to_s, false)
        when MacroNumberValue
          value
        end
      end

      private def macro_sequence_values(value : MacroValue) : Array(MacroValue)?
        case value
        when Array(MacroValue)
          value
        when MacroArrayValue
          value.values
        when MacroTupleValue
          value.values
        end
      end

      private def macro_collection_result(receiver : MacroValue, values : Array(MacroValue)) : MacroValue
        case receiver
        when MacroTupleValue then MacroTupleValue.new(values)
        when MacroArrayValue then MacroArrayValue.new(values)
        else                      values
        end
      end

      private def macro_integer_index(value : MacroValue) : Int64?
        case value
        when Int64
          value
        when MacroNumberValue
          case number = value.value
          when Int128
            number.to_i64 if number.in?(Int64::MIN.to_i128..Int64::MAX.to_i128)
          when UInt128
            number.to_i64 if number <= Int64::MAX.to_u128
          end
        end
      end

      private def macro_regex(value : MacroValue) : Regex?
        return nil unless value.is_a?(MacroSyntaxValue) && value.kind == MacroSyntaxKind::RegexLiteral
        Regex.new(value.value)
      rescue Regex::Error
        nil
      end

      private def macro_double_splat(value : MacroHashValue, suffix : String = "") : String
        body = value.entries.map do |entry|
          "#{val_to_string(entry.key)} => #{val_to_string(entry.value)}"
        end.join(", ")
        body + suffix
      end

      private def compare_macro_versions(left : String, right : String) : Int32
        left_parts = left.split('.').map { |part| part.to_i? || 0 }
        right_parts = right.split('.').map { |part| part.to_i? || 0 }
        size = Math.max(left_parts.size, right_parts.size)
        size.times do |index|
          comparison = (left_parts[index]? || 0) <=> (right_parts[index]? || 0)
          return comparison unless comparison == 0
        end
        0
      end

      private def macro_match_source(match : Regex::MatchData, regex_value : MacroValue) : String
        pattern = regex_value.as(MacroSyntaxValue).value
        names = macro_regex_capture_names(pattern)
        entries = [] of String
        match.size.times do |index|
          key = names[index]? ? names[index].inspect : index.to_s
          value = match[index]?.try(&.inspect) || "nil"
          entries << "#{key} => #{value}"
        end
        "{#{entries.join(", ")}} of ::Int32 | ::String => ::String | ::Nil"
      end

      private def macro_regex_capture_names(pattern : String) : Hash(Int32, String)
        names = {} of Int32 => String
        capture_index = 0
        index = 0
        in_class = false
        while index < pattern.bytesize
          char = pattern.byte_at(index).unsafe_chr
          if char == '\\'
            index += 2
            next
          elsif char == '['
            in_class = true
          elsif char == ']'
            in_class = false
          elsif char == '(' && !in_class
            if pattern.byte_at?(index + 1).try(&.unsafe_chr) == '?'
              marker = pattern.byte_at?(index + 2).try(&.unsafe_chr)
              if marker == '<'
                discriminator = pattern.byte_at?(index + 3).try(&.unsafe_chr)
                unless {'=', '!'}.includes?(discriminator)
                  finish = pattern.index('>', index + 3)
                  if finish
                    capture_index += 1
                    names[capture_index] = pattern.byte_slice(index + 3, finish - index - 3)
                  end
                end
              end
            else
              capture_index += 1
            end
          end
          index += 1
        end
        names
      end

      private def macro_number_kind_name(kind : MacroNumberKind) : String
        kind.to_s.downcase
      end

      private def macro_number_text(value : Int128 | UInt128 | Float64) : String
        case value
        when Float64
          value.to_s
        else
          value.to_s
        end
      end

      private def macro_class_name(value : MacroValue) : String
        case value
        when Nil                     then "NilLiteral"
        when Bool                    then "BoolLiteral"
        when Int64, MacroNumberValue then "NumberLiteral"
        when MacroSyntaxValue
          if crystal_kind = value.crystal_kind
            return crystal_kind.lchop("Crystal::")
          end
          case value.kind
          when MacroSyntaxKind::StringLiteral          then "StringLiteral"
          when MacroSyntaxKind::SymbolLiteral          then "SymbolLiteral"
          when MacroSyntaxKind::CharLiteral            then "CharLiteral"
          when MacroSyntaxKind::RegexLiteral           then "RegexLiteral"
          when MacroSyntaxKind::GeneratedStringLiteral then "StringLiteral"
          when MacroSyntaxKind::GeneratedCharLiteral   then "CharLiteral"
          when MacroSyntaxKind::GeneratedSymbolLiteral then "SymbolLiteral"
          when MacroSyntaxKind::Identifier             then "MacroId"
          else                                              "ASTNode"
          end
        when MacroBlockValue          then "Block"
        when MacroRangeValue          then "RangeLiteral"
        when MacroTypeValue           then "TypeNode"
        when MacroAnnotationValue     then "Annotation"
        when MacroMetaVarValue        then "MetaVar"
        when MacroMethodValue         then "Def"
        when MacroTupleValue          then "TupleLiteral"
        when MacroArrayValue          then "ArrayLiteral"
        when MacroHashValue           then "HashLiteral"
        when Array(MacroValue)        then "ArrayLiteral"
        when Hash(String, MacroValue) then "NamedTupleLiteral"
        else                               "ASTNode"
        end
      end

      private def macro_captured_field_value(field : MacroCapturedField) : MacroValue
        case field.kind
        when "identifier"
          MacroSyntaxValue.identifier(field.source)
        else
          if field.kind.starts_with?("Crystal::")
            MacroSyntaxValue.captured(field.source, field.kind, MacroNodeMetadata.new)
          else
            MacroSyntaxValue.code(field.source)
          end
        end
      end

      private def apply_captured_macro_member(receiver : MacroSyntaxValue, name : String) : MacroEvaluation?
        structure = receiver.metadata.try(&.structure)
        return nil unless structure
        if field = structure.fields[name]?
          return MacroEvaluation.new(macro_captured_node_value(field))
        end
        if collection = structure.collections[name]?
          values = collection.map { |node| macro_captured_node_value(node).as(MacroValue) }
          typed_collection = {"free_vars", "outputs", "inputs", "clobbers"}.includes?(name) ||
                             (name == "type_vars" && {"Crystal::ClassDef", "Crystal::ModuleDef"}.includes?(structure.kind))
          value = typed_collection ? values.as(MacroValue) : MacroArrayValue.new(values).as(MacroValue)
          return MacroEvaluation.new(value)
        end
        if structure.booleans.has_key?(name)
          return MacroEvaluation.new(structure.booleans[name])
        end
        return MacroEvaluation.new(nil) if structure.nil_fields.includes?(name)
        nil
      end

      private def macro_captured_node_value(node : MacroCapturedNode) : MacroValue
        fields = node.fields.transform_values do |field|
          MacroCapturedField.new(field.source, field.kind)
        end
        structure = MacroCapturedNode.new(node.source, node.kind, node.fields, node.collections, node.booleans, node.nil_fields)
        metadata = MacroNodeMetadata.new(fields: fields, structure: structure)
        MacroSyntaxValue.captured(node.source, node.kind, metadata)
      end

      private def macro_value_is_a?(value : MacroValue, type_name : String) : Bool
        normalized = type_name.lchop("::")
        if normalized.includes?('|')
          return normalized.split('|').any? { |member| macro_value_is_a?(value, member.strip) }
        end
        return true if normalized == "ASTNode" && !value.is_a?(String)
        case value
        when MacroSyntaxValue
          if crystal_kind = value.crystal_kind
            return crystal_kind.lchop("Crystal::") == normalized
          end
          case normalized
          when "StringLiteral"
            {MacroSyntaxKind::StringLiteral, MacroSyntaxKind::GeneratedStringLiteral}.includes?(value.kind)
          when "SymbolLiteral"
            {MacroSyntaxKind::SymbolLiteral, MacroSyntaxKind::GeneratedSymbolLiteral}.includes?(value.kind)
          when "CharLiteral"
            {MacroSyntaxKind::CharLiteral, MacroSyntaxKind::GeneratedCharLiteral}.includes?(value.kind)
          when "RegexLiteral"
            value.kind == MacroSyntaxKind::RegexLiteral
          when "MacroId", "Var", "Path"
            value.kind == MacroSyntaxKind::Identifier
          else
            false
          end
        when Int64, MacroNumberValue
          normalized == "NumberLiteral"
        when Bool
          normalized == "BoolLiteral"
        when Nil
          normalized == "NilLiteral"
        when MacroBlockValue
          normalized == "Block"
        when MacroRangeValue
          normalized == "RangeLiteral"
        when MacroTypeValue
          normalized == "TypeNode"
        when MacroAnnotationValue
          normalized == "Annotation"
        when MacroMethodValue
          normalized == "Def"
        when MacroMetaVarValue
          normalized == "MetaVar"
        when MacroTupleValue
          normalized == "TupleLiteral"
        when MacroArrayValue
          normalized == "ArrayLiteral"
        when MacroHashValue
          normalized == "HashLiteral"
        when Array(MacroValue)
          normalized == "ArrayLiteral"
        when Hash(String, MacroValue)
          normalized == "NamedTupleLiteral"
        else
          false
        end
      end

      private def macro_value_responds_to?(value : MacroValue, method_name : String) : Bool
        common = {
          "class_name", "stringify", "nil?", "filename", "line_number", "column_number",
          "end_line_number", "end_column_number", "doc", "doc_comment",
        }
        return true if common.includes?(method_name)
        if value.is_a?(MacroSyntaxValue) && method_name == "name"
          return value.metadata.try(&.fields.has_key?("name")) || false
        end
        if value.is_a?(MacroSyntaxValue)
          if structure = value.metadata.try(&.structure)
            return true if structure.fields.has_key?(method_name) ||
                           structure.collections.has_key?(method_name) ||
                           structure.booleans.has_key?(method_name) ||
                           structure.nil_fields.includes?(method_name)
          end
        end
        case value
        when Int64, MacroNumberValue
          {"kind", "to_number", "zero?"}.includes?(method_name)
        when MacroSyntaxValue
          {"id", "symbolize", "size", "empty?", "starts_with?", "ends_with?", "ord", "count", "tr",
           "source", "options", "match", "scan"}.includes?(method_name)
        when Array(MacroValue), MacroArrayValue
          {"size", "empty?", "first", "last", "map", "select", "reject", "any?", "all?", "join",
           "reduce", "find", "splat", "sort_by", "unshift", "push"}.includes?(method_name)
        when MacroTupleValue
          {"size", "empty?", "first", "last", "map", "select", "reject", "any?", "all?", "join",
           "reduce", "find", "splat", "sort_by"}.includes?(method_name)
        when MacroHashValue
          {"size", "empty?", "keys", "values", "includes?", "map", "to_a", "has_key?",
           "of_key", "of_value", "double_splat", "select", "reject"}.includes?(method_name)
        when Hash(String, MacroValue)
          {"size", "empty?", "keys", "values", "includes?", "map", "to_a", "has_key?",
           "double_splat", "select", "reject", "of_key", "of_value"}.includes?(method_name)
        when MacroBlockValue
          {"body", "args", "empty?"}.includes?(method_name)
        when MacroTypeValue
          {"name", "methods", "instance_vars", "constants", "superclass", "ancestors",
           "class?", "module?", "struct?", "enum?", "lib?", "annotation", "annotations"}.includes?(method_name)
        when MacroAnnotationValue
          {"name", "args", "named_args", "[]"}.includes?(method_name)
        when MacroMethodValue
          {"name", "args", "return_type", "body", "source", "annotation", "annotations"}.includes?(method_name)
        when MacroMetaVarValue
          {"name", "type", "default_value", "has_default_value?", "annotation", "annotations"}.includes?(method_name)
        else
          false
        end
      end

      private def eval_macro_eval_block(block : MacroEvalBlock, values : Array(MacroValue)) : MacroEvaluation?
        parent = current_macro_env
        env = parent.dup
        block.parameters.each_with_index do |parameter, index|
          env[parameter] = values[index]? || nil
        end
        @env_stack << env
        evaluation = begin
          eval_value(block.body_id, block.ast)
        ensure
          @env_stack.pop
        end
        env.each do |name, value|
          parent[name] = value unless block.parameters.includes?(name)
        end
        evaluation
      end

      private def eval_macro_index(receiver : MacroValue, index : MacroValue) : MacroEvaluation?
        case receiver
        when MacroTupleValue
          evaluation = eval_macro_index(receiver.values, index)
          return nil unless evaluation
          value = evaluation.value
          value = MacroTupleValue.new(value) if value.is_a?(Array(MacroValue))
          MacroEvaluation.new(value)
        when MacroArrayValue
          evaluation = eval_macro_index(receiver.values, index)
          return nil unless evaluation
          value = evaluation.value
          value = MacroArrayValue.new(value) if value.is_a?(Array(MacroValue))
          MacroEvaluation.new(value)
        when Array(MacroValue)
          if range = index.as?(MacroRangeValue)
            size = receiver.size.to_i64
            first = range.first || 0_i64
            return MacroEvaluation.new(nil) if first < -size || first > size
            first += size if first < 0
            last = range.last || (range.exclusive ? size : size - 1)
            last += size if last < 0
            finish = range.exclusive ? last : last + 1
            finish = finish.clamp(first, size)
            return MacroEvaluation.new(receiver[first.to_i...finish.to_i].map(&.as(MacroValue)))
          end
          position = macro_integer_index(index)
          return nil unless position
          normalized = position < 0 ? receiver.size.to_i64 + position : position
          value = normalized.in?(0_i64...receiver.size.to_i64) ? receiver[normalized] : nil
          MacroEvaluation.new(value)
        when String
          eval_macro_text_index(receiver, index, syntax: false)
        when MacroSyntaxValue
          eval_macro_text_index(receiver.value, index, syntax: true)
        when MacroHashValue
          entry = receiver.entries.find { |candidate| candidate.key == index }
          MacroEvaluation.new(entry.try(&.value))
        when Hash(String, MacroValue)
          key = macro_scalar_text(index) || macro_integer_index(index).try(&.to_s)
          return nil unless key
          MacroEvaluation.new(receiver[key]?)
        when MacroAnnotationValue
          position = macro_integer_index(index)
          source = if position
                     normalized = position < 0 ? receiver.positional_sources.size.to_i64 + position : position
                     receiver.positional_sources[normalized]?
                   elsif key = macro_scalar_text(index)
                     receiver.named_sources[key]?
                   end
          MacroEvaluation.new(source ? eval_annotation_source(source) : nil)
        else
          nil
        end
      end

      private def eval_macro_text_index(text : String, index : MacroValue, syntax : Bool) : MacroEvaluation?
        if range = index.as?(MacroRangeValue)
          chars = text.chars
          first = range.first || 0_i64
          last = range.last || (range.exclusive ? chars.size.to_i64 : chars.size.to_i64 - 1)
          first += chars.size if first < 0
          last += chars.size if last < 0
          finish = range.exclusive ? last : last + 1
          first = first.clamp(0_i64, chars.size.to_i64)
          finish = finish.clamp(first, chars.size.to_i64)
          value = chars[first.to_i...finish.to_i].join
          return MacroEvaluation.new(syntax ? MacroSyntaxValue.string(value) : value)
        end

        position = macro_integer_index(index)
        return nil unless position
        char = text.chars[position]?
        return nil unless char
        MacroEvaluation.new(syntax ? MacroSyntaxValue.string(char.to_s) : char.to_s)
      end

      private def assign_macro_value(target_id : NodeId, value : MacroValue, ast : AstFile) : Nil
        target = ast.node(target_id)
        case target.kind
        when NodeKind::Ident
          current_macro_env[ast.arena.symbols[target.payload_index]] = value
        when NodeKind::Index
          children = ast.children(target_id)
          return if children.size < 2
          receiver = eval_value(children[0], ast)
          index = eval_value(children[1], ast)
          return unless receiver && index
          case collection = receiver.value
          when Array(MacroValue)
            position = macro_integer_index(index.value)
            return unless position
            position += collection.size if position < 0
            collection[position] = value if position.in?(0_i64...collection.size.to_i64)
          when MacroArrayValue
            position = macro_integer_index(index.value)
            return unless position
            position += collection.values.size if position < 0
            collection.values[position] = value if position.in?(0_i64...collection.values.size.to_i64)
          when MacroHashValue
            if entry = collection.entries.find { |candidate| candidate.key == index.value }
              entry.value = value
            else
              collection.entries << MacroHashEntry.new(index.value, value)
            end
          when Hash(String, MacroValue)
            key = macro_scalar_text(index.value)
            collection[key] = value if key
          end
        when NodeKind::Tuple
          values = macro_sequence_values(value) || [value] of MacroValue
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
        if op == TokenKind::Less && left.is_a?(MacroTypeValue)
          right_type = case right
                       when MacroTypeValue
                         right
                       when MacroSyntaxValue
                         macro_type_value(right.value, @active_index, left.name)
                       end
          if right_type
            mark_type_introspection
            return MacroEvaluation.new(macro_type_subtype?(left, right_type))
          end
        end

        if op == TokenKind::Spaceship || left.is_a?(MacroNumberValue) || right.is_a?(MacroNumberValue)
          if left_number = macro_number(left)
            if right_number = macro_number(right)
              if evaluation = eval_number_binary(op, left_number, right_number)
                return evaluation
              end
            end
          end
        end

        if {TokenKind::Match, TokenKind::NotMatch}.includes?(op)
          text = macro_scalar_text(left)
          pattern = macro_regex(right)
          if text && pattern
            matched = pattern.matches?(text)
            return MacroEvaluation.new(op == TokenKind::Match ? matched : !matched)
          end
        end

        case op
        when TokenKind::Plus
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left + right)
          elsif left.is_a?(Array(MacroValue)) && (right_values = macro_sequence_values(right))
            MacroEvaluation.new(left + right_values)
          elsif left.is_a?(MacroArrayValue) && (right_values = macro_sequence_values(right))
            MacroEvaluation.new(MacroArrayValue.new(left.values + right_values))
          elsif left.is_a?(MacroTupleValue) && (right_values = macro_sequence_values(right))
            MacroEvaluation.new(MacroTupleValue.new(left.values + right_values))
          elsif (left_text = macro_scalar_text(left)) && (right_text = macro_scalar_text(right))
            MacroEvaluation.new(MacroSyntaxValue.string(left_text + right_text))
          else
            MacroEvaluation.new(MacroSyntaxValue.code(val_to_string(left) + val_to_string(right)))
          end
        when TokenKind::Minus
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left - right)
          elsif left.is_a?(Array(MacroValue)) && (right_values = macro_sequence_values(right))
            MacroEvaluation.new(left.reject { |value| right_values.includes?(value) })
          elsif left.is_a?(MacroArrayValue) && (right_values = macro_sequence_values(right))
            values = left.values.reject { |value| right_values.includes?(value) }
            MacroEvaluation.new(MacroArrayValue.new(values))
          elsif left.is_a?(MacroTupleValue) && (right_values = macro_sequence_values(right))
            values = left.values.reject { |value| right_values.includes?(value) }
            MacroEvaluation.new(MacroTupleValue.new(values))
          end
        when TokenKind::Star
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left * right)
          elsif left.is_a?(MacroSyntaxValue) && left.kind == MacroSyntaxKind::StringLiteral && right.is_a?(Int64) && right >= 0
            MacroEvaluation.new(MacroSyntaxValue.string(left.value * right.to_i))
          elsif left.is_a?(Array(MacroValue)) && right.is_a?(Int64) && right >= 0
            values = if left.empty?
                       [] of MacroValue
                     else
                       Array(MacroValue).new(left.size * right.to_i) { |index| left[index % left.size] }
                     end
            MacroEvaluation.new(values)
          elsif left.is_a?(MacroArrayValue) && right.is_a?(Int64) && right >= 0
            values = if left.values.empty?
                       [] of MacroValue
                     else
                       Array(MacroValue).new(left.values.size * right.to_i) { |index| left.values[index % left.values.size] }
                     end
            MacroEvaluation.new(MacroArrayValue.new(values))
          elsif left.is_a?(MacroTupleValue) && right.is_a?(Int64) && right >= 0
            values = if left.values.empty?
                       [] of MacroValue
                     else
                       Array(MacroValue).new(left.values.size * right.to_i) { |index| left.values[index % left.values.size] }
                     end
            MacroEvaluation.new(MacroTupleValue.new(values))
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
          elsif (left_text = macro_scalar_text(left)) && (right_text = macro_scalar_text(right))
            MacroEvaluation.new(left_text < right_text)
          end
        when TokenKind::LessEqual
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left <= right)
          elsif (left_text = macro_scalar_text(left)) && (right_text = macro_scalar_text(right))
            MacroEvaluation.new(left_text <= right_text)
          end
        when TokenKind::Greater
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left > right)
          elsif (left_text = macro_scalar_text(left)) && (right_text = macro_scalar_text(right))
            MacroEvaluation.new(left_text > right_text)
          end
        when TokenKind::GreaterEqual
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left >= right)
          elsif (left_text = macro_scalar_text(left)) && (right_text = macro_scalar_text(right))
            MacroEvaluation.new(left_text >= right_text)
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
          if left.is_a?(Int64) && right.is_a?(Int64)
            MacroEvaluation.new(left << right)
          elsif left.is_a?(Array(MacroValue))
            left << right
            MacroEvaluation.new(left)
          elsif left.is_a?(MacroArrayValue)
            left.values << right
            MacroEvaluation.new(left)
          elsif left.is_a?(MacroTupleValue)
            left.values << right
            MacroEvaluation.new(left)
          end
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

      private def eval_number_binary(
        op : TokenKind,
        left : MacroNumberValue,
        right : MacroNumberValue,
      ) : MacroEvaluation?
        if left.value.is_a?(Float64) || right.value.is_a?(Float64)
          return eval_float_binary(op, left, right)
        end
        if left.value.is_a?(UInt128)
          eval_unsigned_binary(op, left, right)
        else
          eval_signed_binary(op, left, right)
        end
      rescue OverflowError | DivisionByZeroError
        nil
      end

      private def eval_float_binary(
        op : TokenKind,
        left : MacroNumberValue,
        right : MacroNumberValue,
      ) : MacroEvaluation?
        left_value = left.value.to_f64
        right_value = right.value.to_f64
        case op
        when TokenKind::EqualEqual
          MacroEvaluation.new(left_value == right_value)
        when TokenKind::BangEqual
          MacroEvaluation.new(left_value != right_value)
        when TokenKind::Less
          MacroEvaluation.new(left_value < right_value)
        when TokenKind::LessEqual
          MacroEvaluation.new(left_value <= right_value)
        when TokenKind::Greater
          MacroEvaluation.new(left_value > right_value)
        when TokenKind::GreaterEqual
          MacroEvaluation.new(left_value >= right_value)
        when TokenKind::Spaceship
          raw_comparison = left_value <=> right_value
          comparison = raw_comparison.try(&.to_i64)
          MacroEvaluation.new(comparison)
        else
          value = case op
                  when TokenKind::Plus    then left_value + right_value
                  when TokenKind::Minus   then left_value - right_value
                  when TokenKind::Star    then left_value * right_value
                  when TokenKind::Slash   then left_value / right_value
                  when TokenKind::Percent then left_value % right_value
                  else                         return nil
                  end
          MacroEvaluation.new(macro_number_result(value, left))
        end
      end

      private def eval_unsigned_binary(
        op : TokenKind,
        left : MacroNumberValue,
        right : MacroNumberValue,
      ) : MacroEvaluation?
        left_value = left.value.as(UInt128)
        right_value = case value = right.value
                      when UInt128 then value
                      when Int128
                        return nil if value < 0
                        value.to_u128
                      else
                        return nil
                      end
        case op
        when TokenKind::EqualEqual
          MacroEvaluation.new(left_value == right_value)
        when TokenKind::BangEqual
          MacroEvaluation.new(left_value != right_value)
        when TokenKind::Less
          MacroEvaluation.new(left_value < right_value)
        when TokenKind::LessEqual
          MacroEvaluation.new(left_value <= right_value)
        when TokenKind::Greater
          MacroEvaluation.new(left_value > right_value)
        when TokenKind::GreaterEqual
          MacroEvaluation.new(left_value >= right_value)
        when TokenKind::Spaceship
          MacroEvaluation.new((left_value <=> right_value).to_i64)
        else
          value = case op
                  when TokenKind::Plus  then left_value + right_value
                  when TokenKind::Minus then left_value - right_value
                  when TokenKind::Star  then left_value * right_value
                  when TokenKind::Slash, TokenKind::SlashSlash
                    return nil if right_value == 0
                    left_value // right_value
                  when TokenKind::Percent
                    return nil if right_value == 0
                    left_value % right_value
                  when TokenKind::StarStar   then left_value ** right_value.to_i
                  when TokenKind::Pipe       then left_value | right_value
                  when TokenKind::Ampersand  then left_value & right_value
                  when TokenKind::Caret      then left_value ^ right_value
                  when TokenKind::ShiftLeft  then left_value << right_value.to_i
                  when TokenKind::ShiftRight then left_value >> right_value.to_i
                  else                            return nil
                  end
          MacroEvaluation.new(macro_number_result(value, left))
        end
      end

      private def eval_signed_binary(
        op : TokenKind,
        left : MacroNumberValue,
        right : MacroNumberValue,
      ) : MacroEvaluation?
        left_value = left.value.as(Int128)
        right_value = case value = right.value
                      when Int128 then value
                      when UInt128
                        return nil if value > Int128::MAX.to_u128
                        value.to_i128
                      else
                        return nil
                      end
        case op
        when TokenKind::EqualEqual
          MacroEvaluation.new(left_value == right_value)
        when TokenKind::BangEqual
          MacroEvaluation.new(left_value != right_value)
        when TokenKind::Less
          MacroEvaluation.new(left_value < right_value)
        when TokenKind::LessEqual
          MacroEvaluation.new(left_value <= right_value)
        when TokenKind::Greater
          MacroEvaluation.new(left_value > right_value)
        when TokenKind::GreaterEqual
          MacroEvaluation.new(left_value >= right_value)
        when TokenKind::Spaceship
          MacroEvaluation.new((left_value <=> right_value).to_i64)
        else
          value = case op
                  when TokenKind::Plus  then left_value + right_value
                  when TokenKind::Minus then left_value - right_value
                  when TokenKind::Star  then left_value * right_value
                  when TokenKind::Slash, TokenKind::SlashSlash
                    return nil if right_value == 0
                    left_value // right_value
                  when TokenKind::Percent
                    return nil if right_value == 0
                    left_value % right_value
                  when TokenKind::StarStar   then left_value ** right_value.to_i
                  when TokenKind::Pipe       then left_value | right_value
                  when TokenKind::Ampersand  then left_value & right_value
                  when TokenKind::Caret      then left_value ^ right_value
                  when TokenKind::ShiftLeft  then left_value << right_value.to_i
                  when TokenKind::ShiftRight then left_value >> right_value.to_i
                  else                            return nil
                  end
          MacroEvaluation.new(macro_number_result(value, left))
        end
      end

      private def macro_number_result(
        value : Int128 | UInt128 | Float64,
        prototype : MacroNumberValue,
      ) : MacroNumberValue
        source = macro_number_result_source(value, prototype.kind, prototype.explicit_kind)
        MacroNumberValue.new(value, prototype.kind, source, prototype.explicit_kind)
      end

      private def macro_number_result_source(
        value : Int128 | UInt128 | Float64,
        kind : MacroNumberKind,
        explicit_kind : Bool,
      ) : String
        source = macro_number_text(value)
        explicit_kind ? "#{source}_#{macro_number_kind_name(kind)}" : source
      end

      private def macro_type_subtype?(type : MacroTypeValue, target : MacroTypeValue) : Bool
        return false if type.name == target.name
        return true if target.name == "Reference" && type.kind == MacroTypeKind::Class
        return true if target.name == "Value" && {MacroTypeKind::Struct, MacroTypeKind::Enum}.includes?(type.kind)
        macro_ancestors(type).any? { |ancestor| ancestor.name == target.name }
      end

      private def eval_unary(op : TokenKind, value : MacroValue) : MacroEvaluation?
        case op
        when TokenKind::Plus
          if number = macro_number(value)
            result = MacroNumberValue.new(number.value, number.kind, "+#{number.source.lchop("+")}", number.explicit_kind)
            MacroEvaluation.new(result)
          end
        when TokenKind::Minus
          if number = macro_number(value)
            negated : Int128 | Float64 = case numeric = number.value
            when Float64 then -numeric
            when UInt128
              unsigned = numeric
              return nil if unsigned > Int128::MAX.to_u128 + 1
              unsigned == Int128::MAX.to_u128 + 1 ? Int128::MIN : -unsigned.to_i128
            when Int128 then -numeric
            else             return nil
            end
            source = macro_number_result_source(negated, number.kind, number.explicit_kind)
            MacroEvaluation.new(MacroNumberValue.new(negated, number.kind, source, number.explicit_kind))
          end
        when TokenKind::Tilde
          if number = macro_number(value)
            inverted : Int128 | UInt128 = case numeric = number.value
            when Int128  then ~numeric
            when UInt128 then ~numeric
            else              return nil
            end
            source = macro_number_result_source(inverted, number.kind, number.explicit_kind)
            MacroEvaluation.new(MacroNumberValue.new(inverted, number.kind, source, number.explicit_kind))
          end
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
        when MacroSyntaxValue
          value.source
        when MacroBlockValue
          value.body
        when MacroRangeValue
          operator = value.exclusive ? "..." : ".."
          "#{value.first}#{operator}#{value.last}"
        when MacroNumberValue
          value.source
        when MacroTypeValue
          value.name
        when MacroAnnotationValue
          value.name
        when MacroMetaVarValue
          value.name
        when MacroMethodValue
          value.source
        when MacroTupleValue
          "{" + value.values.map { |entry| val_to_string(entry) }.join(", ") + "}"
        when MacroArrayValue
          "[" + value.values.map { |entry| val_to_string(entry) }.join(", ") + "]"
        when MacroHashValue
          "{" + value.entries.map { |entry| "#{val_to_string(entry.key)} => #{val_to_string(entry.value)}" }.join(", ") + "}"
        when Array(MacroValue)
          elements = value.map { |entry| val_to_string(entry) }.join(", ")
          type_suffix = if value.empty?
                          " of ::NoReturn"
                        elsif value.all? { |entry| entry.is_a?(MacroSyntaxValue) && entry.kind == MacroSyntaxKind::GeneratedStringLiteral }
                          " of ::String"
                        elsif value.all? { |entry| entry.is_a?(MacroSyntaxValue) && entry.kind == MacroSyntaxKind::GeneratedCharLiteral }
                          " of ::Char"
                        elsif value.all? { |entry| entry.is_a?(MacroSyntaxValue) && entry.kind == MacroSyntaxKind::GeneratedSymbolLiteral }
                          " of ::Symbol"
                        else
                          ""
                        end
          "[#{elements}]#{type_suffix}"
        when Hash(String, MacroValue)
          entries = value.map do |key, entry|
            rendered_key = key.matches?(/\A[a-zA-Z_][a-zA-Z0-9_]*[?!]?\z/) ? key : key.inspect
            "#{rendered_key}: #{val_to_string(entry)}"
          end.join(", ")
          "{#{entries}}"
        else
          value.to_s
        end
      end

      private def macro_scalar_text(value : MacroValue) : String?
        case value
        when String
          value
        when MacroSyntaxValue
          value.value
        when MacroTypeValue
          value.name
        when MacroAnnotationValue
          value.name
        when MacroMetaVarValue
          value.name
        else
          nil
        end
      end

      private def slice_text(source : Source, span : Span) : String
        String.new(source.bytes[span.start, span.length])
      end
    end
  end
end
