module Facet
  module Compiler
    alias TypeId = Int32
    alias DefId = Int32
    alias BindingId = Int32
    alias MethodId = DefId

    # A syntax reference is valid only for the source revision it was created
    # from. Consumers must resolve it through the SemanticSnapshot that
    # produced it instead of retaining arena-local NodeIds across edits.
    record NodeRef,
      file_id : FileId,
      node_id : NodeId,
      revision : UInt64

    enum SemanticMode
      Tolerant
      Strict
    end

    struct SemanticOptions
      def initialize(flags : Enumerable(String) = [] of String)
        @flags = flags.to_set
      end

      def flags : Set(String)
        @flags.dup
      end

      def preview_overload_order? : Bool
        @flags.includes?("preview_overload_order")
      end

      def fingerprint : UInt64
        @flags.to_a.sort.reduce(0_u64) { |value, flag| value ^ flag.hash.to_u64 }
      end
    end

    enum SemanticTypeKind
      Unknown
      Error
      Nominal
      Metaclass
      TypeParameter
      Union
      Tuple
      NamedTuple
      Proc
    end

    struct SemanticType
      getter kind : SemanticTypeKind
      getter name : String?
      getter arguments : Array(TypeId)

      def initialize(
        @kind : SemanticTypeKind,
        @name : String? = nil,
        @arguments : Array(TypeId) = [] of TypeId,
      )
      end
    end

    # Canonical semantic types make equality and dependency keys inexpensive.
    class TypeStore
      getter types : Array(SemanticType)
      getter unknown : TypeId
      getter error : TypeId

      def initialize
        @types = [] of SemanticType
        @index = {} of String => TypeId
        @unknown = intern(SemanticType.new(SemanticTypeKind::Unknown))
        @error = intern(SemanticType.new(SemanticTypeKind::Error))
      end

      def [](id : TypeId) : SemanticType
        @types[id]
      end

      def named(name : String, arguments : Array(TypeId) = [] of TypeId) : TypeId
        intern(SemanticType.new(SemanticTypeKind::Nominal, normalize_name(name), arguments))
      end

      def metaclass(instance_type : TypeId) : TypeId
        intern(SemanticType.new(SemanticTypeKind::Metaclass, arguments: [instance_type]))
      end

      def type_parameter(name : String) : TypeId
        intern(SemanticType.new(SemanticTypeKind::TypeParameter, name))
      end

      def tuple(elements : Array(TypeId)) : TypeId
        intern(SemanticType.new(SemanticTypeKind::Tuple, arguments: elements))
      end

      def named_tuple(elements : Array(TypeId)) : TypeId
        intern(SemanticType.new(SemanticTypeKind::NamedTuple, arguments: elements))
      end

      def proc_type(elements : Array(TypeId)) : TypeId
        intern(SemanticType.new(SemanticTypeKind::Proc, arguments: elements))
      end

      def union(types : Enumerable(TypeId)) : TypeId
        members = [] of TypeId
        types.each do |type_id|
          type = self[type_id]
          if type.kind == SemanticTypeKind::Union
            members.concat(type.arguments)
          elsif type.kind != SemanticTypeKind::Error
            members << type_id
          end
        end
        members = members.uniq.sort_by do |type_id|
          type = self[type_id]
          nil_type = type.kind == SemanticTypeKind::Nominal && type.name == "Nil"
          {nil_type ? 1 : 0, display(type_id)}
        end
        return @unknown if members.empty?
        return members.first if members.size == 1
        intern(SemanticType.new(SemanticTypeKind::Union, arguments: members))
      end

      def display(id : TypeId) : String
        display(id, false)
      end

      private def display(id : TypeId, nested : Bool) : String
        type = self[id]
        case type.kind
        when SemanticTypeKind::Unknown then "Unknown"
        when SemanticTypeKind::Error   then "Error"
        when SemanticTypeKind::Nominal
          name = type.name || "Unknown"
          type.arguments.empty? ? name : "#{name}(#{type.arguments.map { |arg| display(arg, true) }.join(", ")})"
        when SemanticTypeKind::Metaclass
          instance = type.arguments.first
          rendered = display(instance, true)
          self[instance].kind == SemanticTypeKind::Union ? "(#{rendered}).class" : "#{rendered}.class"
        when SemanticTypeKind::TypeParameter then type.name || "T"
        when SemanticTypeKind::Union
          body = type.arguments.map { |arg| display(arg, true) }.join(" | ")
          nested ? body : "(#{body})"
        when SemanticTypeKind::Tuple      then "Tuple(#{type.arguments.map { |arg| display(arg, false) }.join(", ")})"
        when SemanticTypeKind::NamedTuple then "NamedTuple(#{type.arguments.map { |arg| display(arg, false) }.join(", ")})"
        when SemanticTypeKind::Proc       then "Proc(#{type.arguments.map { |arg| display(arg, false) }.join(", ")})"
        else                                   "Unknown"
        end
      end

      private def intern(type : SemanticType) : TypeId
        key = "#{type.kind.value}:#{type.name}:#{type.arguments.join(',')}"
        if id = @index[key]?
          return id
        end
        id = @types.size.to_i32
        @types << type
        @index[key] = id
        id
      end

      private def normalize_name(name : String) : String
        name.strip.lchop("::")
      end
    end

    enum SemanticDefinitionKind
      Class
      Module
      Struct
      Enum
      Lib
      Alias
      Method
      Constant
      Local
      Parameter
    end

    struct SemanticDefinition
      getter id : DefId
      getter kind : SemanticDefinitionKind
      getter name : String
      getter qualified_name : String
      getter node : NodeRef?
      getter span : Span
      getter owner : String?
      getter type_id : TypeId
      getter class_method : Bool
      getter min_arity : Int32
      getter max_arity : Int32?
      getter parameter_types : Array(TypeId)
      getter return_type : TypeId
      getter generated : Bool
      getter free_variables : Array(String)

      def initialize(
        @id : DefId,
        @kind : SemanticDefinitionKind,
        @name : String,
        @qualified_name : String,
        @span : Span,
        @type_id : TypeId,
        @node : NodeRef? = nil,
        @owner : String? = nil,
        @class_method : Bool = false,
        @min_arity : Int32 = 0,
        @max_arity : Int32? = 0,
        @parameter_types : Array(TypeId) = [] of TypeId,
        @return_type : TypeId = 0,
        @generated : Bool = false,
        @free_variables : Array(String) = [] of String,
      )
      end
    end

    enum SemanticDiagnosticSeverity
      Error
      Warning
    end

    enum SemanticDiagnosticConfidence
      Provisional
      Conclusive
    end

    struct SemanticRelatedLocation
      getter file_id : FileId
      getter span : Span
      getter message : String

      def initialize(@file_id : FileId, @span : Span, @message : String)
      end
    end

    struct SemanticDiagnostic
      getter code : String
      getter file_id : FileId
      getter span : Span
      getter message : String
      getter severity : SemanticDiagnosticSeverity
      getter related : Array(SemanticRelatedLocation)
      getter confidence : SemanticDiagnosticConfidence

      def initialize(
        @code : String,
        @file_id : FileId,
        @span : Span,
        @message : String,
        @severity : SemanticDiagnosticSeverity = SemanticDiagnosticSeverity::Error,
        @related : Array(SemanticRelatedLocation) = [] of SemanticRelatedLocation,
        @confidence : SemanticDiagnosticConfidence = SemanticDiagnosticConfidence::Conclusive,
      )
      end
    end

    enum SemanticCompletenessReason
      ParseRecovery
      MissingRequire
      MacroExpansion
      UnknownType
      StaleNode
    end
  end
end
