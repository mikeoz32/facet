module Facet
  module Compiler
    private class BodyAnalyzer
      BUILTIN_TYPE_NAMES = Set{
        "Object", "Reference", "Value", "Number", "Int", "Float", "Signed", "Unsigned",
        "Int8", "Int16", "Int32", "Int64", "Int128",
        "UInt8", "UInt16", "UInt32", "UInt64", "UInt128",
        "Float32", "Float64", "Bool", "Char", "String", "Symbol", "Nil", "Regex",
        "Array", "Hash", "Tuple", "NamedTuple", "Proc", "Range", "Pointer", "Slice", "StaticArray",
      }

      getter body_executions : Int32 = 0

      def initialize(
        @types : TypeStore,
        @semantic_options : SemanticOptions,
        @trees : Hash(FileId, SyntaxTree),
        @revisions : Hash(FileId, UInt64),
        @definitions : Hash(DefId, SemanticDefinition),
        @methods_by_owner : Hash(String, Array(DefId)),
        @type_definitions : Hash(String, DefId),
        @type_parameters : Hash(String, Array(String)),
        @macro_names : Set(String),
        @superclasses : Hash(String, String),
        @includes : Hash(String, Array(String)),
        @node_types : Hash(NodeRef, TypeId),
        @bindings : Hash(NodeRef, DefId),
        @diagnostics : Array(SemanticDiagnostic),
        @reasons : Set(SemanticCompletenessReason),
        @emit_diagnostics : Bool,
      )
        @inference_stack = Set(DefId).new
        @block_inference_stack = Set({FileId, NodeId}).new
        @specialized_returns = {} of {DefId, Array(TypeId), TypeId, TypeId} => TypeId
      end

      def analyze_file(file_id : FileId) : Nil
        tree = @trees[file_id]
        analyze_top_level(tree.root, file_id, "", {"self" => @types.named("Object")})
        @definitions.each_value do |definition|
          next unless definition.kind == SemanticDefinitionKind::Method
          node_ref = definition.node
          next unless node_ref && node_ref.file_id == file_id
          analyze_method(definition, record: true)
        end
      end

      def infer_method_returns : Hash(DefId, TypeId)
        values = {} of DefId => TypeId
        @definitions.each do |id, definition|
          next unless definition.kind == SemanticDefinitionKind::Method
          next unless definition.return_type == @types.unknown
          values[id] = analyze_method(definition, record: false)
        end
        values
      end

      private def analyze_method(
        definition : SemanticDefinition,
        record : Bool,
        argument_types : Array(TypeId)? = nil,
        self_type : TypeId? = nil,
        free_bindings : Hash(String, TypeId) = {} of String => TypeId,
      ) : TypeId
        node_ref = definition.node
        return @types.unknown unless node_ref
        tree = @trees[node_ref.file_id]?
        return @types.unknown unless tree
        node = tree.node(node_ref.node_id)
        body = node.body
        return @types.named("Nil") unless body
        env = {} of String => TypeId
        if owner = definition.owner
          instance = self_type || @types.named(owner)
          env["self"] = definition.class_method ? @types.metaclass(instance) : instance
        end
        definition.free_variables.each do |name|
          bound = free_bindings[name]? || @types.type_parameter(name)
          env[name] = @types.metaclass(bound)
        end
        node.parameters.reject { |parameter| parameter.kind == NodeKind::BlockParam }.each_with_index do |parameter, index|
          name = parameter.name
          next unless name
          declared_type = definition.parameter_types[index]? || @types.unknown
          declared_type = substitute_free_type(declared_type, free_bindings)
          argument_type = argument_types.try { |types| types[index]? }
          type_id = argument_type && argument_type != @types.unknown ? argument_type : declared_type
          if type_id == @types.unknown
            type_id = parameter.value.try { |value| infer(value, node_ref.file_id, definition.owner || "", env, false) } || @types.unknown
          end
          env[name.lchop('@')] = type_id
          env[name] = type_id if name.starts_with?('@')
        end
        @body_executions += 1
        infer(body, node_ref.file_id, definition.owner || "", env, record)
      end

      private def analyze_top_level(node : SyntaxNode, file_id : FileId, scope : String, env : Hash(String, TypeId)) : Nil
        case node.kind
        when NodeKind::Def, NodeKind::MacroDef
          return
        when NodeKind::Class, NodeKind::Module, NodeKind::Struct, NodeKind::Enum, NodeKind::Lib
          type_scope = qualify(scope, node.name || "")
          type_env = env.dup
          type_env["self"] = @types.named(type_scope)
          node.body.try do |body|
            body.children.each do |child|
              next if child.kind == NodeKind::Def
              next if {"include", "extend"}.includes?(child.call_name)
              analyze_top_level(child, file_id, type_scope, type_env)
            end
          end
          return
        when NodeKind::Expressions, NodeKind::File
          node.children.each { |child| analyze_top_level(child, file_id, scope, env) }
        else
          infer(node, file_id, scope, env, true)
        end
      end

      private def infer(
        node : SyntaxNode,
        file_id : FileId,
        scope : String,
        env : Hash(String, TypeId),
        record : Bool,
      ) : TypeId
        type_id = case node.kind
                  when NodeKind::LiteralString then @types.named("String")
                  when NodeKind::LiteralChar   then @types.named("Char")
                  when NodeKind::LiteralRegex  then @types.named("Regex")
                  when NodeKind::LiteralSymbol then @types.named("Symbol")
                  when NodeKind::LiteralBool   then @types.named("Bool")
                  when NodeKind::LiteralNil    then @types.named("Nil")
                  when NodeKind::LiteralNumber then number_type(node.text)
                  when NodeKind::TypeApply
                    infer_type_apply(node, file_id, scope, env, record)
                  when NodeKind::Ident, NodeKind::Const, NodeKind::InstanceVar, NodeKind::ClassVar, NodeKind::Global, NodeKind::Path
                    infer_name(node, file_id, scope, env, record)
                  when NodeKind::Assign
                    infer_assign(node, file_id, scope, env, record)
                  when NodeKind::VarDecl
                    infer_var_decl(node, file_id, scope, env, record)
                  when NodeKind::Expressions, NodeKind::File, NodeKind::Begin, NodeKind::Ensure
                    infer_sequence(node.children, file_id, scope, env, record)
                  when NodeKind::Array
                    values = semantic_children(node).map { |child| infer(child, file_id, scope, env, record) }
                    @types.named("Array", [values.empty? ? @types.unknown : @types.union(values)])
                  when NodeKind::Hash
                    infer_hash(node, file_id, scope, env, record)
                  when NodeKind::Tuple
                    @types.tuple(semantic_children(node).map { |child| infer(child, file_id, scope, env, record) })
                  when NodeKind::NamedTuple
                    entries = semantic_children(node)
                    @types.named_tuple(
                      entries.map { |child| infer(child.value || child, file_id, scope, env, record) },
                      entries.map { |child| child.name || "" }
                    )
                  when NodeKind::Range
                    elements = semantic_children(node).map { |child| infer(child, file_id, scope, env, record) }
                    @types.named("Range", [elements.empty? ? @types.unknown : @types.union(elements)])
                  when NodeKind::If, NodeKind::Unless, NodeKind::Ternary
                    infer_conditional(node, file_id, scope, env, record)
                  when NodeKind::Call, NodeKind::CallWithBlock
                    infer_call(node, file_id, scope, env, record)
                  when NodeKind::Binary
                    infer_binary(node, file_id, scope, env, record)
                  when NodeKind::Unary
                    infer_unary(node, file_id, scope, env, record)
                  when NodeKind::Return, NodeKind::Next, NodeKind::Break
                    child = semantic_children(node).first?
                    child ? infer(child, file_id, scope, env, record) : @types.named("Nil")
                  when NodeKind::NamedArg
                    node.value.try { |value| infer(value, file_id, scope, env, record) } || @types.unknown
                  when NodeKind::Index
                    infer_index(node, file_id, scope, env, record)
                  else
                    children = semantic_children(node)
                    children.empty? ? @types.unknown : infer_sequence(children, file_id, scope, env, record)
                  end
        remember(node, file_id, type_id) if record
        type_id
      end

      private def infer_name(node : SyntaxNode, file_id : FileId, scope : String, env : Hash(String, TypeId), record : Bool) : TypeId
        name = node.symbol_name || node.text
        return env[name] if env.has_key?(name)
        if name == "self"
          return env["self"]? || @types.unknown
        end
        if constant_name?(name)
          resolved = resolve_type_name(name, scope)
          if definition_id = @type_definitions[resolved]?
            bind(node, file_id, definition_id) if record
            return @types.metaclass(@definitions[definition_id].type_id)
          end
          if BUILTIN_TYPE_NAMES.includes?(resolved)
            return @types.metaclass(@types.named(resolved))
          end
        end
        if self_type = env["self"]?
          return @types.unknown if @macro_names.includes?(name)
          if callable_without_receiver?(self_type, name)
            return resolve_call(node, self_type, file_id, scope, env, record, name)
          end
        end
        @types.unknown
      end

      private def callable_without_receiver?(receiver_type : TypeId, name : String) : Bool
        members = receiver_members(receiver_type)
        return false unless members
        members.any? do |instance_type, class_method|
          type_name = @types[instance_type].name
          next false unless type_name
          !lookup_methods(type_name, name, class_method).empty? || method_missing?(instance_type, class_method)
        end
      end

      private def infer_type_apply(node : SyntaxNode, file_id : FileId, scope : String, env : Hash(String, TypeId), record : Bool) : TypeId
        name = node.symbol_name || node.child(0).try(&.symbol_name)
        return @types.unknown unless name
        resolved = resolve_type_name(name, scope)
        return @types.unknown unless @type_definitions.has_key?(resolved)
        arguments = node.child(1).try(&.children).try do |nodes|
          nodes.map { |argument| infer_type_expression(argument, file_id, scope, env, record) }
        end || [] of TypeId
        @types.metaclass(@types.named(resolved, arguments))
      end

      private def infer_type_expression(
        node : SyntaxNode,
        file_id : FileId,
        scope : String,
        env : Hash(String, TypeId),
        record : Bool,
      ) : TypeId
        case node.kind
        when NodeKind::TypeApply
          inferred = infer_type_apply(node, file_id, scope, env, record)
          type = @types[inferred]
          type.kind == SemanticTypeKind::Metaclass ? type.arguments.first : inferred
        when NodeKind::Ident, NodeKind::Const, NodeKind::Path
          name = node.symbol_name || node.text
          if name == "self"
            inferred = env["self"]? || @types.unknown
            type = @types[inferred]
            return type.kind == SemanticTypeKind::Metaclass ? type.arguments.first : inferred
          end
          if inferred = env[name]?
            type = @types[inferred]
            return type.kind == SemanticTypeKind::Metaclass ? type.arguments.first : inferred
          end
          @types.named(resolve_type_name(name, scope))
        when NodeKind::LiteralNumber, NodeKind::LiteralSymbol, NodeKind::LiteralString
          @types.named(node.text)
        when NodeKind::Binary
          if node.text.includes?('|')
            return @types.union(semantic_children(node).map { |child| infer_type_expression(child, file_id, scope, env, record) })
          end
          inferred_type_expression(node, file_id, scope, env, record)
        else
          inferred_type_expression(node, file_id, scope, env, record)
        end
      end

      private def inferred_type_expression(
        node : SyntaxNode,
        file_id : FileId,
        scope : String,
        env : Hash(String, TypeId),
        record : Bool,
      ) : TypeId
        inferred = infer(node, file_id, scope, env, record)
        type = @types[inferred]
        type.kind == SemanticTypeKind::Metaclass ? type.arguments.first : inferred
      end

      private def infer_assign(node : SyntaxNode, file_id : FileId, scope : String, env : Hash(String, TypeId), record : Bool) : TypeId
        value = node.value
        type_id = value ? infer(value, file_id, scope, env, record) : @types.unknown
        if target = node.target
          assign_target(target, type_id, env)
          remember(target, file_id, type_id) if record
        end
        type_id
      end

      private def infer_var_decl(node : SyntaxNode, file_id : FileId, scope : String, env : Hash(String, TypeId), record : Bool) : TypeId
        type_id = node.declared_type.try { |type| resolve_type_text(type.text, scope) }
        type_id ||= node.value.try { |value| infer(value, file_id, scope, env, record) }
        type_id ||= @types.unknown
        if target = node.target
          assign_target(target, type_id, env)
          remember(target, file_id, type_id) if record
        end
        type_id
      end

      private def infer_sequence(nodes : Array(SyntaxNode), file_id : FileId, scope : String, env : Hash(String, TypeId), record : Bool) : TypeId
        result = @types.named("Nil")
        nodes.each { |child| result = infer(child, file_id, scope, env, record) }
        result
      end

      private def infer_hash(node : SyntaxNode, file_id : FileId, scope : String, env : Hash(String, TypeId), record : Bool) : TypeId
        keys = [] of TypeId
        values = [] of TypeId
        semantic_children(node).each do |entry|
          parts = semantic_children(entry)
          if parts.size >= 2
            keys << infer(parts[0], file_id, scope, env, record)
            values << infer(parts[1], file_id, scope, env, record)
          end
        end
        @types.named("Hash", [keys.empty? ? @types.unknown : @types.union(keys), values.empty? ? @types.unknown : @types.union(values)])
      end

      private def infer_conditional(node : SyntaxNode, file_id : FileId, scope : String, env : Hash(String, TypeId), record : Bool) : TypeId
        children = semantic_children(node)
        condition = node.condition || children.first?
        infer(condition, file_id, scope, env, record) if condition
        branch_env = env.dup
        then_node = node.body || children[1]?
        else_node = children[2]?
        then_type = then_node ? infer(then_node, file_id, scope, branch_env, record) : @types.named("Nil")
        else_type = else_node ? infer(else_node, file_id, scope, env.dup, record) : @types.named("Nil")
        @types.union([then_type, else_type])
      end

      private def infer_call(node : SyntaxNode, file_id : FileId, scope : String, env : Hash(String, TypeId), record : Bool) : TypeId
        if receiver = node.receiver
          receiver_type = infer(receiver, file_id, scope, env, record)
          return resolve_call(node, receiver_type, file_id, scope, env, record)
        end
        name = node.call_name
        return @types.unknown unless name
        node.arguments.each { |argument| infer(argument.value || argument, file_id, scope, env, record) }
        return @types.unknown if @macro_names.includes?(name)
        if name == "typeof"
          return @types.metaclass(node.arguments.first?.try { |argument| infer(argument, file_id, scope, env, record) } || @types.unknown)
        end
        self_type = env["self"]?
        return @types.unknown unless self_type
        resolve_call(node, self_type, file_id, scope, env, record)
      end

      private def infer_binary(node : SyntaxNode, file_id : FileId, scope : String, env : Hash(String, TypeId), record : Bool) : TypeId
        if receiver = node.receiver
          receiver_type = infer(receiver, file_id, scope, env, record)
          return resolve_call(node, receiver_type, file_id, scope, env, record)
        end
        children = semantic_children(node)
        left = children.first?.try { |child| infer(child, file_id, scope, env, record) } || @types.unknown
        right = children[1]?.try { |child| infer(child, file_id, scope, env, record) } || @types.unknown
        text = node.text
        return @types.union([left, right]) if text.includes?("&&") || text.includes?("||")
        return @types.named("Bool") if text.includes?("==") || text.includes?("!=") || text.includes?('<') || text.includes?('>')
        left
      end

      private def infer_unary(node : SyntaxNode, file_id : FileId, scope : String, env : Hash(String, TypeId), record : Bool) : TypeId
        child = semantic_children(node).first?
        value = child ? infer(child, file_id, scope, env, record) : @types.unknown
        node.text.lstrip.starts_with?('!') ? @types.named("Bool") : value
      end

      private def infer_index(node : SyntaxNode, file_id : FileId, scope : String, env : Hash(String, TypeId), record : Bool) : TypeId
        receiver = semantic_children(node).first?
        return @types.unknown unless receiver
        receiver_type = infer(receiver, file_id, scope, env, record)
        type = @types[receiver_type]
        return type.arguments.first if type.kind == SemanticTypeKind::Nominal && {"Array", "Slice", "StaticArray"}.includes?(type.name) && type.arguments.first?
        return type.arguments[1] if type.kind == SemanticTypeKind::Nominal && type.name == "Hash" && type.arguments.size > 1
        @types.unknown
      end

      private def resolve_call(
        node : SyntaxNode,
        receiver_type : TypeId,
        file_id : FileId,
        scope : String,
        env : Hash(String, TypeId),
        record : Bool,
        explicit_name : String? = nil,
      ) : TypeId
        name = explicit_name || node.call_name
        return @types.unknown unless name
        arguments = node.arguments.map { |argument| infer(argument.value || argument, file_id, scope, env, record) }
        block_return = nil.as(TypeId?)

        if name == "as" || name == "as?"
          target = node.arguments.first?
          return target ? resolve_type_text(target.text, scope) : @types.unknown
        elsif name == "nil?" || name == "is_a?" || name == "responds_to?"
          return @types.named("Bool")
        elsif name == "not_nil!"
          return without_nil(receiver_type)
        end

        receiver_members = receiver_members(receiver_type)
        return unknown_call unless receiver_members
        if {"new", "allocate"}.includes?(name) && receiver_members.all? { |member| member[1] }
          instances = receiver_members.map do |member|
            name == "new" ? infer_constructed_type(member[0], arguments) : member[0]
          end
          return @types.union(instances)
        end

        resolved = [] of SemanticDefinition
        resolved_returns = [] of TypeId
        missing = [] of String
        receiver_members.each do |instance_type, class_method|
          type = @types[instance_type]
          type_name = type.name
          unless type_name && @type_definitions.has_key?(type_name)
            return unknown_call
          end
          candidates = lookup_methods(type_name, name, class_method)
          if candidates.empty?
            missing << type_name
          else
            applicable = candidates.select { |candidate| arity_matches?(candidate, arguments.size) }
            selected = select_overloads(applicable.empty? ? candidates : applicable, arguments)
            if block_return.nil? && selected.any? { |candidate| candidate.block_type != @types.unknown && !candidate.free_variables.empty? }
              block_return = infer_call_block_return(node, file_id, scope, env, record)
            end
            resolved.concat(selected)
            selected.each do |candidate|
              return_type = inferred_return_type(candidate, arguments, instance_type, block_return)
              resolved_returns << substitute_type(return_type, instance_type, type_name)
            end
          end
        end

        if missing.size == receiver_members.size
          unless receiver_members.any? { |member| method_missing?(member[0], member[1]) }
            emit_undefined_method(node, file_id, receiver_type, name) if record && @emit_diagnostics
          end
          return @types.error
        end
        return @types.unknown if resolved.empty? || missing.any?

        if record
          callee = node.callee
          bind(callee, file_id, resolved.first.id) if callee
        end
        @types.union(resolved_returns)
      end

      private def infer_call_block_return(
        node : SyntaxNode,
        file_id : FileId,
        scope : String,
        env : Hash(String, TypeId),
        record : Bool,
      ) : TypeId
        body = node.body
        body ||= node.child(1).try(&.body) if node.kind == NodeKind::Binary
        return @types.unknown unless body
        key = {file_id, node.id}
        return @types.unknown if @block_inference_stack.includes?(key)
        @block_inference_stack << key
        infer(body, file_id, scope, env.dup, record)
      ensure
        @block_inference_stack.delete(key) if key
      end

      private def substitute_type(type_id : TypeId, instance_type : TypeId, owner : String) : TypeId
        type = @types[type_id]
        instance = @types[instance_type]
        if type.kind == SemanticTypeKind::TypeParameter
          parameters = @type_parameters[owner]? || [] of String
          if index = parameters.index(type.name || "")
            return instance.arguments[index]? || @types.unknown
          end
          return @types.unknown
        end
        return type_id if type.arguments.empty?
        arguments = type.arguments.map { |argument| substitute_type(argument, instance_type, owner) }
        case type.kind
        when SemanticTypeKind::Nominal    then @types.named(type.name || "Unknown", arguments)
        when SemanticTypeKind::Metaclass  then @types.metaclass(arguments.first)
        when SemanticTypeKind::Union      then @types.union(arguments)
        when SemanticTypeKind::Tuple      then @types.tuple(arguments)
        when SemanticTypeKind::NamedTuple then @types.named_tuple(arguments, type.name.try(&.split('\0')) || [] of String)
        when SemanticTypeKind::Proc       then @types.proc_type(arguments)
        else                                   type_id
        end
      end

      private def infer_constructed_type(instance_type : TypeId, arguments : Array(TypeId)) : TypeId
        instance = @types[instance_type]
        owner = instance.name
        return instance_type unless owner
        parameters = @type_parameters[owner]? || [] of String
        return instance_type if parameters.empty? || !instance.arguments.empty?
        bindings = {} of String => TypeId
        initializers = lookup_methods(owner, "initialize", false).select do |candidate|
          arity_matches?(candidate, arguments.size)
        end
        initializers.each do |initializer|
          initializer.parameter_types.each_with_index do |parameter_type, index|
            argument_type = arguments[index]?
            collect_type_parameter_bindings(parameter_type, argument_type, bindings) if argument_type
          end
        end
        inferred = parameters.map { |parameter| bindings[parameter]? || @types.unknown }
        @types.named(owner, inferred)
      end

      private def infer_free_bindings(
        definition : SemanticDefinition,
        arguments : Array(TypeId),
        block_return : TypeId?,
      ) : Hash(String, TypeId)
        names = definition.free_variables.to_set
        return {} of String => TypeId if names.empty?

        effective_arguments = arguments.dup
        if node_ref = definition.node
          if tree = @trees[node_ref.file_id]?
            node = tree.node(node_ref.node_id)
            env = {} of String => TypeId
            if owner = definition.owner
              instance = @types.named(owner)
              env["self"] = definition.class_method ? @types.metaclass(instance) : instance
            end
            names.each { |name| env[name] = @types.metaclass(@types.type_parameter(name)) }
            node.parameters.reject { |parameter| parameter.kind == NodeKind::BlockParam }.each_with_index do |parameter, index|
              while effective_arguments.size <= index
                effective_arguments << @types.unknown
              end
              if effective_arguments[index] == @types.unknown
                if value = parameter.value
                  effective_arguments[index] = infer(value, node_ref.file_id, definition.owner || "", env, false)
                end
              end
              if name = parameter.name
                env[name.lchop('@')] = effective_arguments[index]
                env[name] = effective_arguments[index] if name.starts_with?('@')
              end
            end
          end
        end

        bindings = {} of String => TypeId
        definition.parameter_types.each_with_index do |parameter_type, index|
          if argument_type = effective_arguments[index]?
            collect_free_type_bindings(parameter_type, argument_type, names, bindings)
          end
        end
        if block_return && block_return != @types.unknown && definition.block_type != @types.unknown
          collect_free_type_bindings(
            definition.block_type,
            @types.proc_type([block_return]),
            names,
            bindings
          )
        end
        bindings
      end

      private def collect_free_type_bindings(
        parameter_id : TypeId,
        argument_id : TypeId,
        names : Set(String),
        bindings : Hash(String, TypeId),
      ) : Nil
        parameter = @types[parameter_id]
        if parameter.kind == SemanticTypeKind::TypeParameter && names.includes?(parameter.name || "")
          name = parameter.name.not_nil!
          bindings[name] = bindings[name]?.try { |current| @types.union([current, argument_id]) } || argument_id
          return
        end

        actual_id = argument_id
        argument = @types[actual_id]
        if parameter.kind == SemanticTypeKind::Nominal && argument.kind == SemanticTypeKind::Nominal && parameter.name != argument.name
          if ancestor = matching_ancestor_type(actual_id, parameter.name || "")
            actual_id = ancestor
            argument = @types[actual_id]
          end
        end
        if parameter.kind == SemanticTypeKind::Union
          argument_members = argument.kind == SemanticTypeKind::Union ? argument.arguments : [actual_id]
          free_members = parameter.arguments.select { |member| contains_free_type?(member, names) }
          concrete_members = parameter.arguments.reject { |member| contains_free_type?(member, names) }
          argument_members.each do |member|
            next if concrete_members.any? { |concrete| type_compatible?(member, concrete) }
            free_members.each { |free| collect_free_type_bindings(free, member, names, bindings) }
          end
          return
        end

        return unless parameter.kind == argument.kind && parameter.name == argument.name
        parameter.arguments.zip(argument.arguments) do |parameter_argument, actual_argument|
          collect_free_type_bindings(parameter_argument, actual_argument, names, bindings)
        end
      end

      private def contains_free_type?(type_id : TypeId, names : Set(String)) : Bool
        type = @types[type_id]
        return true if type.kind == SemanticTypeKind::TypeParameter && names.includes?(type.name || "")
        type.arguments.any? { |argument| contains_free_type?(argument, names) }
      end

      private def substitute_free_type(type_id : TypeId, bindings : Hash(String, TypeId)) : TypeId
        type = @types[type_id]
        if type.kind == SemanticTypeKind::TypeParameter
          return bindings[type.name || ""]? || type_id
        end
        return type_id if type.arguments.empty?
        arguments = type.arguments.map { |argument| substitute_free_type(argument, bindings) }
        case type.kind
        when SemanticTypeKind::Nominal    then @types.named(type.name || "Unknown", arguments)
        when SemanticTypeKind::Metaclass  then @types.metaclass(arguments.first)
        when SemanticTypeKind::Union      then @types.union(arguments)
        when SemanticTypeKind::Tuple      then @types.tuple(arguments)
        when SemanticTypeKind::NamedTuple then @types.named_tuple(arguments, type.name.try(&.split('\0')) || [] of String)
        when SemanticTypeKind::Proc       then @types.proc_type(arguments)
        else                                   type_id
        end
      end

      private def collect_type_parameter_bindings(
        parameter_type_id : TypeId,
        argument_type_id : TypeId,
        bindings : Hash(String, TypeId),
      ) : Nil
        parameter_type = @types[parameter_type_id]
        if parameter_type.kind == SemanticTypeKind::TypeParameter
          if name = parameter_type.name
            bindings[name] = bindings[name]?.try { |existing| @types.union([existing, argument_type_id]) } || argument_type_id
          end
          return
        end
        argument_type = @types[argument_type_id]
        return unless parameter_type.kind == argument_type.kind && parameter_type.name == argument_type.name
        parameter_type.arguments.zip(argument_type.arguments) do |parameter, argument|
          collect_type_parameter_bindings(parameter, argument, bindings)
        end
      end

      private def inferred_return_type(
        definition : SemanticDefinition,
        arguments : Array(TypeId),
        self_type : TypeId,
        block_return : TypeId? = nil,
      ) : TypeId
        free_bindings = infer_free_bindings(definition, arguments, block_return)
        unless definition.return_type == @types.unknown
          return substitute_free_type(definition.return_type, free_bindings)
        end
        return @types.unknown if definition.generated || @inference_stack.includes?(definition.id)
        parameter_specific = !definition.parameter_types.empty?
        receiver_specific = definition.owner.try { |owner| self_type != @types.named(owner) } || false
        cache_key = {definition.id, arguments, self_type, block_return || @types.unknown}
        if parameter_specific || receiver_specific
          if cached = @specialized_returns[cache_key]?
            return cached
          end
        end
        @inference_stack << definition.id
        inferred = analyze_method(
          definition,
          record: false,
          argument_types: arguments,
          self_type: self_type,
          free_bindings: free_bindings
        )
        @inference_stack.delete(definition.id)
        return @types.unknown if inferred == @types.error
        if parameter_specific || receiver_specific
          @specialized_returns[cache_key] = inferred
        elsif inferred != @types.unknown
          @definitions[definition.id] = SemanticDefinition.new(
            definition.id,
            definition.kind,
            definition.name,
            definition.qualified_name,
            definition.span,
            definition.type_id,
            definition.node,
            definition.owner,
            definition.class_method,
            definition.min_arity,
            definition.max_arity,
            definition.parameter_types,
            inferred,
            definition.generated,
            definition.free_variables,
            definition.block_type
          )
        end
        inferred
      ensure
        @inference_stack.delete(definition.id)
      end

      private def lookup_methods(type_name : String, name : String, class_method : Bool) : Array(SemanticDefinition)
        values = [] of SemanticDefinition
        visited = Set(String).new
        queue = [type_name]
        until queue.empty?
          current = queue.shift
          next if visited.includes?(current)
          visited << current
          (@methods_by_owner[current]? || [] of DefId).each do |id|
            method = @definitions[id]
            values << method if method.name == name && method.class_method == class_method
          end
          if superclass = @superclasses[current]?
            queue << resolve_type_name(superclass, current)
          end
          (@includes[current]? || [] of String).each do |included|
            queue << resolve_type_name(included, current)
          end
        end
        values
      end

      private def method_missing?(instance_type : TypeId, class_method : Bool) : Bool
        type_name = @types[instance_type].name
        return true unless type_name
        !lookup_methods(type_name, "method_missing", class_method).empty?
      end

      private def receiver_members(type_id : TypeId) : Array({TypeId, Bool})?
        type = @types[type_id]
        case type.kind
        when SemanticTypeKind::Nominal
          [{type_id, false}]
        when SemanticTypeKind::Metaclass
          instance = type.arguments.first?
          instance ? [{instance, true}] : nil
        when SemanticTypeKind::Union
          members = [] of {TypeId, Bool}
          type.arguments.each do |member|
            expanded = receiver_members(member)
            return nil unless expanded
            members.concat(expanded)
          end
          members
        else
          nil
        end
      end

      private def unknown_call : TypeId
        @reasons << SemanticCompletenessReason::UnknownType
        @types.unknown
      end

      private def emit_undefined_method(node : SyntaxNode, file_id : FileId, receiver_type : TypeId, name : String) : Nil
        span = node.callee.try(&.span) || node.span
        diagnostic = SemanticDiagnostic.new(
          "facet.undefined_method",
          file_id,
          span,
          "undefined method '#{name}' for #{@types.display(receiver_type)}",
          confidence: diagnostic_confidence(node, file_id, receiver_type)
        )
        key = {diagnostic.code, diagnostic.file_id, diagnostic.span.start, diagnostic.span.finish}
        unless @diagnostics.any? { |existing| {existing.code, existing.file_id, existing.span.start, existing.span.finish} == key }
          @diagnostics << diagnostic
        end
      end

      private def diagnostic_confidence(
        node : SyntaxNode,
        file_id : FileId,
        receiver_type : TypeId,
      ) : SemanticDiagnosticConfidence
        return SemanticDiagnosticConfidence::Provisional unless node.receiver
        return SemanticDiagnosticConfidence::Provisional if @reasons.includes?(SemanticCompletenessReason::ParseRecovery)
        return SemanticDiagnosticConfidence::Provisional if @reasons.includes?(SemanticCompletenessReason::MissingRequire)
        return SemanticDiagnosticConfidence::Provisional if @reasons.includes?(SemanticCompletenessReason::MacroExpansion)
        members = receiver_members(receiver_type)
        return SemanticDiagnosticConfidence::Provisional unless members
        local = members.all? do |instance_type, _|
          type_name = @types[instance_type].name
          definition_id = type_name.try { |name| @type_definitions[name]? }
          definition_id.try { |id| @definitions[id].node.try(&.file_id) } == file_id
        end
        local ? SemanticDiagnosticConfidence::Conclusive : SemanticDiagnosticConfidence::Provisional
      end

      private def arity_matches?(definition : SemanticDefinition, arity : Int32) : Bool
        return false if arity < definition.min_arity
        max = definition.max_arity
        max.nil? || arity <= max
      end

      private def select_overloads(
        candidates : Array(SemanticDefinition),
        arguments : Array(TypeId),
      ) : Array(SemanticDefinition)
        compatible = candidates.select do |candidate|
          candidate.parameter_types.each_with_index.all? do |parameter_type, index|
            argument_type = arguments[index]?
            argument_type.nil? || type_compatible?(argument_type, parameter_type)
          end
        end
        compatible = candidates if compatible.empty?
        if @semantic_options.preview_overload_order? && compatible.all? { |candidate| candidate.parameter_types.all? { |type_id| type_id == @types.unknown } }
          best = compatible.max_of? { |candidate| preview_positional_score(candidate) } || 0
          return compatible.select { |candidate| preview_positional_score(candidate) == best }
        end
        scored = compatible.map { |candidate| {candidate, overload_score(candidate, arguments)} }
        best = scored.max_of? { |entry| entry[1] } || 0
        scored.select { |_, score| score == best }.map { |entry| entry[0] }
      end

      # Crystal's preview order treats required positional parameters as more
      # specific than optional parameters, and optional parameters as more
      # specific than a splat. With the same required prefix, a smaller finite
      # maximum wins; between splats, the later splat position wins.
      private def preview_positional_score(definition : SemanticDefinition) : Int32
        score = definition.min_arity * 10_000
        if max = definition.max_arity
          score + 5_000 - max
        else
          score + definition.parameter_types.size
        end
      end

      private def overload_score(definition : SemanticDefinition, arguments : Array(TypeId)) : Int32
        score = if @semantic_options.preview_overload_order? && definition.min_arity == arguments.size && definition.max_arity == arguments.size
                  4
                else
                  0
                end
        definition.parameter_types.each_with_index do |parameter_type, index|
          argument_type = arguments[index]?
          next unless argument_type
          score += 8 if parameter_type != @types.unknown && parameter_type == argument_type
        end
        score
      end

      private def type_compatible?(actual_id : TypeId, expected_id : TypeId) : Bool
        return true if actual_id == @types.unknown || expected_id == @types.unknown
        return true if actual_id == expected_id
        expected = @types[expected_id]
        return true if expected.kind == SemanticTypeKind::TypeParameter
        return expected.arguments.any? { |member| type_compatible?(actual_id, member) } if expected.kind == SemanticTypeKind::Union
        actual = @types[actual_id]
        return false unless actual.kind == SemanticTypeKind::Nominal && expected.kind == SemanticTypeKind::Nominal
        !matching_ancestor_type(actual_id, expected.name || "").nil?
      end

      private def matching_ancestor_type(actual_id : TypeId, expected_name : String) : TypeId?
        queue = [{actual_id, 0}]
        visited = Set(TypeId).new
        until queue.empty?
          current_id, depth = queue.shift
          next if visited.includes?(current_id)
          visited << current_id
          current = @types[current_id]
          owner = current.name
          next unless current.kind == SemanticTypeKind::Nominal && owner
          return current_id if owner == expected_name
          next if depth >= 64 || visited.size >= 512

          ancestors = [] of String
          ancestors << @superclasses[owner].not_nil! if @superclasses[owner]?
          ancestors.concat(@includes[owner]? || [] of String)
          parameters = (@type_parameters[owner]? || [] of String).to_set
          ancestors.each do |source|
            resolved = TypeTextResolver.new(@types, @type_definitions, parameters).resolve(source, owner)
            queue << {substitute_type(resolved, current_id, owner), depth + 1}
          end
        end
        nil
      end

      private def without_nil(type_id : TypeId) : TypeId
        type = @types[type_id]
        return type_id unless type.kind == SemanticTypeKind::Union
        members = type.arguments.reject { |member| @types[member].kind == SemanticTypeKind::Nominal && @types[member].name == "Nil" }
        @types.union(members)
      end

      private def resolve_type_text(text : String, scope : String) : TypeId
        TypeTextResolver.new(@types, @type_definitions).resolve(text, scope)
      end

      private def resolve_type_name(name : String, scope : String) : String
        text = name.strip.lchop("::")
        return text if name.starts_with?("::") || text.includes?("::")
        parts = scope.split("::")
        parts.pop if @type_definitions.has_key?(scope)
        until parts.empty?
          candidate = "#{parts.join("::")}::#{text}"
          return candidate if @type_definitions.has_key?(candidate)
          parts.pop
        end
        text
      end

      private def assign_target(target : SyntaxNode, type_id : TypeId, env : Hash(String, TypeId)) : Nil
        if {NodeKind::Tuple, NodeKind::Destructure}.includes?(target.kind)
          element_types = @types[type_id].arguments
          semantic_children(target).each_with_index do |child, index|
            assign_target(child, element_types[index]? || @types.unknown, env)
          end
        elsif name = target.symbol_name
          env[name] = type_id
          env[name.lchop('@')] = type_id if name.starts_with?('@')
        end
      end

      private def number_type(text : String) : TypeId
        normalized = text.downcase
        name = if normalized.ends_with?("i8")
                 "Int8"
               elsif normalized.ends_with?("i16")
                 "Int16"
               elsif normalized.ends_with?("i64")
                 "Int64"
               elsif normalized.ends_with?("i128")
                 "Int128"
               elsif normalized.ends_with?("u8")
                 "UInt8"
               elsif normalized.ends_with?("u16")
                 "UInt16"
               elsif normalized.ends_with?("u32")
                 "UInt32"
               elsif normalized.ends_with?("u64")
                 "UInt64"
               elsif normalized.ends_with?("u128")
                 "UInt128"
               elsif normalized.ends_with?("f32")
                 "Float32"
               elsif normalized.ends_with?("f64") || normalized.includes?('.') || normalized.includes?('e')
                 "Float64"
               else
                 "Int32"
               end
        @types.named(name)
      end

      private def semantic_children(node : SyntaxNode) : Array(SyntaxNode)
        node.children.reject { |child| child.kind == NodeKind::Nop }
      end

      private def constant_name?(name : String) : Bool
        value = name.lchop("::").split("::").first?
        value.try { |part| part[0]?.try(&.uppercase?) } || false
      end

      private def remember(node : SyntaxNode, file_id : FileId, type_id : TypeId) : Nil
        @node_types[NodeRef.new(file_id, node.id, current_revision(file_id))] = type_id
      end

      private def bind(node : SyntaxNode, file_id : FileId, definition_id : DefId) : Nil
        @bindings[NodeRef.new(file_id, node.id, current_revision(file_id))] = definition_id
      end

      private def current_revision(file_id : FileId) : UInt64
        @revisions[file_id]
      end

      private def qualify(scope : String, name : String) : String
        normalized = name.lchop("::")
        return normalized if scope.empty? || name.starts_with?("::") || normalized.includes?("::")
        "#{scope}::#{normalized}"
      end
    end
  end
end
