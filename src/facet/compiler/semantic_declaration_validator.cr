module Facet
  module Compiler
    private class DeclarationValidator
      def self.ordered_types(indexes : Hash(FileId, SemanticFileIndex), graph : RequireGraph) : Array({FileId, IndexedTypeDecl})
        result = [] of {FileId, IndexedTypeDecl}
        visited = Set(FileId).new
        targets = graph.edges.map(&.to).to_set
        roots = graph.reachable.reject { |file| graph.entries.includes?(file) || targets.includes?(file) }.to_a + graph.entries
        (roots + graph.reachable.to_a).each { |file| append_types(file, indexes, graph, visited, result) }
        result
      end

      private def self.append_types(file : FileId, indexes : Hash(FileId, SemanticFileIndex), graph : RequireGraph,
                                    visited : Set(FileId), result : Array({FileId, IndexedTypeDecl})) : Nil
        return if visited.includes?(file)
        visited << file
        index = indexes[file]?
        return unless index
        edges = graph.edges.select { |edge| edge.from == file }.sort_by(&.span.start)
        cursor = 0
        index.types.each_with_index.to_a.sort_by { |entry| {entry[0].span.start, entry[1]} }.each do |declaration, _|
          while cursor < edges.size && edges[cursor].span.start < declaration.span.start
            append_types(edges[cursor].to, indexes, graph, visited, result)
            cursor += 1
          end
          result << {file, declaration}
        end
        edges.skip(cursor).each { |edge| append_types(edge.to, indexes, graph, visited, result) }
      end

      def initialize(@types : TypeStore, @type_definitions : Hash(String, DefId),
                     @definitions : Hash(DefId, SemanticDefinition), @trees : Hash(FileId, SyntaxTree),
                     @reasons : Set(SemanticCompletenessReason), @diagnostics : Array(SemanticDiagnostic),
                     @aliases : Hash(String, {String, String}))
        @resolver = TypeTextResolver.new(@types, @type_definitions, aliases: @aliases)
      end

      def validate(order : Array({FileId, IndexedTypeDecl})) : Nil
        first = {} of String => {FileId, IndexedTypeDecl}
        parents = {} of String => String
        order.each do |file, declaration|
          next if declaration.generated
          if declaration.dynamic
            report("facet.dynamic_type_declaration", file, declaration.span, "can't declare #{declaration.kind.to_s.downcase} dynamically")
            next
          end
          name = declaration.qualified_name
          next if declaration.implicit && first.has_key?(name)
          if prior = first[name]?
            original = prior[1]
            if declaration.kind != original.kind
              report("facet.type_kind_mismatch", file, declaration.span, "#{name} was declared as #{original.kind}, not #{declaration.kind}", prior)
            elsif !declaration.parameter_shapes.empty? && declaration.parameter_shapes != original.parameter_shapes
              report("facet.generic_parameter_mismatch", file, declaration.span, "generic parameters of #{name} must remain #{original.parameter_shapes.join(", ")}", prior)
            elsif superclass = declaration.superclass
              previous = original.superclass || (original.kind == SemanticDefinitionKind::Struct ? "Value" : "Reference")
              if @resolver.resolve(superclass, name) != @resolver.resolve(previous, name)
                report("facet.superclass_mismatch", file, declaration.span, "superclass mismatch for #{name}", prior)
              end
            end
          else
            first[name] = {file, declaration}
          end
          if superclass = declaration.superclass
            next unless {SemanticDefinitionKind::Class, SemanticDefinitionKind::Struct}.includes?(declaration.kind)
            target = @trees[file].node(declaration.node_id).superclass
            span = target.try(&.span) || declaration.span
            resolved = @resolver.resolve(superclass, name)
            type = @types[resolved]
            next if resolved == @types.unknown || resolved == @types.error
            parent_name = type.name
            parent = parent_name.try { |parent_key| @type_definitions[parent_key]?.try { |id| @definitions[id]? } }
            parent_kind = parent.try(&.kind) || parent_name.try { |key| builtin_kind(key) }
            concrete_struct = parent && parent.kind == SemanticDefinitionKind::Struct && !parent.abstract
            if type.kind != SemanticTypeKind::Nominal || (parent_kind && parent_kind != declaration.kind) || concrete_struct
              report("facet.invalid_superclass", file, span, "#{superclass} is not a valid superclass for #{name}")
            elsif parent_name && !parent && !TypeStore::BUILTIN_NAMES.includes?(parent_name)
              unless @reasons.includes?(SemanticCompletenessReason::MissingRequire)
                report("facet.undefined_constant", file, span, "undefined superclass #{superclass}")
              end
            elsif parent_name
              parents[name] = parent_name if first[name] == {file, declaration}
            end
          end
        end
        validate_cycles(first, parents)
      end

      private def builtin_kind(name : String) : SemanticDefinitionKind?
        return nil unless TypeStore::BUILTIN_NAMES.includes?(name)
        {"Object", "Reference", "String", "Regex", "Array", "Hash"}.includes?(name) ? SemanticDefinitionKind::Class : SemanticDefinitionKind::Struct
      end

      private def validate_cycles(first : Hash(String, {FileId, IndexedTypeDecl}), parents : Hash(String, String)) : Nil
        visited = Set(String).new
        parents.each_key do |root|
          path = [] of String
          current = root
          until visited.includes?(current)
            if start = path.index(current)
              path.skip(start).each do |name|
                file, declaration = first[name]
                report("facet.inheritance_cycle", file, declaration.span, "cyclic inheritance for #{name}")
              end
              break
            end
            path << current
            parent = parents[current]?
            break unless parent
            current = parent
          end
          visited.concat(path)
        end
      end

      private def report(code : String, file : FileId, span : Span, message : String,
                         prior : {FileId, IndexedTypeDecl}? = nil) : Nil
        related = [] of SemanticRelatedLocation
        if prior
          related << SemanticRelatedLocation.new(prior[0], prior[1].span, "previous declaration")
        end
        @diagnostics << SemanticDiagnostic.new(code, file, span, message, related: related)
      end
    end
  end
end
