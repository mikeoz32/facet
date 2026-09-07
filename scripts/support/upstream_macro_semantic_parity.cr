require "../../src/facet"
require "./facet_ast_normalizer"
require "json"

record UpstreamSemanticMacroHeader,
  kind : String,
  crystal_version : String,
  crystal_revision : String,
  suites : Array(String),
  semantic_example_count : Int32,
  event_count : Int32,
  call_count : Int32,
  inline_count : Int32,
  success_count : Int32,
  error_count : Int32,
  raw_event_count : Int32? = nil,
  filtered_event_count : Int32? = nil,
  duplicate_event_count : Int32? = nil,
  pending_count : Int32? = nil do
  include JSON::Serializable
end

record UpstreamSemanticMacroCase,
  kind : String,
  invocation : String,
  definition : String,
  macro_name : String?,
  scope : String,
  flags : Array(String),
  free_vars : Hash(String, JSON::Any),
  instance_vars : Array(String),
  resolved_paths : Hash(String, JSON::Any),
  scope_class_methods : Array(JSON::Any),
  path_errors : Hash(String, String),
  expected : String?,
  expected_error_type : String?,
  expected_error_message : String?,
  scope_abstract : Bool = false,
  scope_constants : Array(JSON::Any) = [] of JSON::Any,
  scope_annotations : Array(JSON::Any) = [] of JSON::Any,
  scope_instance_vars : Array(JSON::Any) = [] of JSON::Any,
  command_outputs : Hash(String, String) = {} of String => String do
  include JSON::Serializable
end

record UpstreamSemanticMacroResult,
  actual : String?,
  diagnostics : Array(String),
  skipped_file : Bool = false do
  def matches?(fixture_case : UpstreamSemanticMacroCase) : Bool
    if fixture_case.expected_error_type
      return skipped_file if fixture_case.expected_error_type == "Crystal::SkipMacroException"
      if expected_error = fixture_case.expected_error_message
        diagnostics == [expected_error]
      else
        false
      end
    elsif expected = fixture_case.expected
      diagnostics.empty? && UpstreamSemanticMacroParity.equivalent_output?(actual, expected)
    else
      false
    end
  end
end

module UpstreamSemanticMacroParity
  extend self

  def load(path : String) : {UpstreamSemanticMacroHeader, Array(UpstreamSemanticMacroCase)}
    lines = File.read_lines(path)
    header = UpstreamSemanticMacroHeader.from_json(lines.shift)
    cases = lines.map { |line| UpstreamSemanticMacroCase.from_json(line) }
    {header, cases}
  end

  def expand(fixture_case : UpstreamSemanticMacroCase, index : Int32) : UpstreamSemanticMacroResult
    if fixture_case.kind == "call"
      expand_call(fixture_case, index)
    else
      expand_inline(fixture_case, index)
    end
  rescue ex
    UpstreamSemanticMacroResult.new(nil, ["#{ex.class}: #{ex.message}"])
  end

  def equivalent_output?(actual : String?, expected : String) : Bool
    return false unless actual
    return true if actual.strip == expected.strip

    actual_parser = Facet::Compiler::Parser.new(
      Facet::Compiler::Source.new(actual, "facet-semantic-macro-actual.cr", Facet::Compiler::SourceKind::Virtual)
    )
    actual_ast = actual_parser.parse_file
    expected_parser = Facet::Compiler::Parser.new(
      Facet::Compiler::Source.new(expected, "facet-semantic-macro-expected.cr", Facet::Compiler::SourceKind::Virtual)
    )
    expected_ast = expected_parser.parse_file
    actual_context = actual_parser.diagnostics.select { |diagnostic| caller_context_diagnostic?(diagnostic.message) }.map(&.message)
    expected_context = expected_parser.diagnostics.select { |diagnostic| caller_context_diagnostic?(diagnostic.message) }.map(&.message)
    return false unless actual_context == expected_context
    return false unless actual_parser.diagnostics.all? { |diagnostic| caller_context_diagnostic?(diagnostic.message) }
    return false unless expected_parser.diagnostics.all? { |diagnostic| caller_context_diagnostic?(diagnostic.message) }
    FacetAstNormalizer.normalize_macro_output(actual_ast) == FacetAstNormalizer.normalize_macro_output(expected_ast) &&
      FacetAstNormalizer.macro_literal_payloads(actual_ast) == FacetAstNormalizer.macro_literal_payloads(expected_ast)
  end

  private def caller_context_diagnostic?(message : String) : Bool
    message.matches?(/\A'.+' before definition of '.+'\z/) ||
      message == "dynamic constant assignment. Constants can only be declared at the top level or inside other types."
  end

  private def expand_call(fixture_case : UpstreamSemanticMacroCase, index : Int32) : UpstreamSemanticMacroResult
    definition_parser = Facet::Compiler::Parser.new(
      Facet::Compiler::Source.new(
        fixture_case.definition,
        "semantic-macro-definition-#{index}.cr",
        Facet::Compiler::SourceKind::Virtual
      )
    )
    definition = definition_parser.parse_file
    call_parser = Facet::Compiler::Parser.new(
      Facet::Compiler::Source.new(
        selected_call_source(fixture_case),
        "semantic-macro-call-#{index}.cr",
        Facet::Compiler::SourceKind::Virtual
      )
    )
    call = call_parser.parse_file
    diagnostics = (definition_parser.diagnostics + call_parser.diagnostics).map(&.message)
    return UpstreamSemanticMacroResult.new(nil, diagnostics) unless diagnostics.empty?

    program_index = Facet::Compiler::Indexer.index_macros([definition])
    context = semantic_context(fixture_case)
    expander = Facet::Compiler::MacroExpander.new(program_index, context: context)
    expanded = expander.expand_once(call, program_index)
    diagnostics.concat(expander.diagnostics.map(&.message))
    UpstreamSemanticMacroResult.new(expanded.source.text, diagnostics, expander.skipped_file)
  end

  private def expand_inline(fixture_case : UpstreamSemanticMacroCase, index : Int32) : UpstreamSemanticMacroResult
    expander = Facet::Compiler::MacroExpander.new(context: semantic_context(fixture_case))
    scope = semantic_instance_scope(fixture_case.scope)
    type = Facet::Compiler::MacroTypeValue.new(
      scope,
      Facet::Compiler::MacroTypeKind::Class,
      abstract: fixture_case.scope_abstract
    )
    actual = expander.expand_template(
      fixture_case.invocation,
      {
        "@type"   => type.as(Facet::Compiler::MacroValue),
        "@caller" => nil.as(Facet::Compiler::MacroValue),
      },
      "semantic-macro-inline-#{index}.cr"
    )
    UpstreamSemanticMacroResult.new(actual, expander.diagnostics.map(&.message), expander.skipped_file)
  end

  private def selected_call_source(fixture_case : UpstreamSemanticMacroCase) : String
    macro_name = fixture_case.macro_name
    return fixture_case.invocation unless macro_name
    marker = ".#{macro_name}"
    marker_index = fixture_case.invocation.rindex(marker)
    return fixture_case.invocation unless marker_index
    receiver = fixture_case.invocation.byte_slice(0, marker_index).strip
    return fixture_case.invocation unless receiver.matches?(/\A::?[A-Z]/) || receiver.matches?(/\A[A-Z]/)
    fixture_case.invocation.byte_slice(marker_index + 1..)
  end

  private def semantic_context(fixture_case : UpstreamSemanticMacroCase) : Facet::Compiler::MacroExpansionContext
    paths = {} of String => Facet::Compiler::MacroSemanticPathSnapshot
    type_methods = {} of String => Array(Facet::Compiler::MacroSemanticMethodSnapshot)
    type_constants = {} of String => Array(String)
    type_constant_values = {} of String => Hash(String, String)
    type_annotations = {} of String => Array(Facet::Compiler::MacroSemanticAnnotationSnapshot)
    type_superclasses = {} of String => String
    type_superclass_annotations = {} of String => Array(Facet::Compiler::MacroSemanticAnnotationSnapshot)
    type_subclasses = {} of String => Array(String)
    fixture_case.resolved_paths.each do |name, raw_path|
      keys = fixture_case.free_vars[name]?.try do |raw_variable|
        raw_variable["keys"].as_a.map do |raw_key|
          Facet::Compiler::MacroSemanticKeySnapshot.new(
            raw_key["name"].as_s,
            raw_key["line"]?.try(&.as_i?).try(&.to_i),
            raw_key["column"]?.try(&.as_i?).try(&.to_i)
          )
        end
      end || [] of Facet::Compiler::MacroSemanticKeySnapshot
      entries = raw_path["entries"].as_a.map do |raw_entry|
        Facet::Compiler::MacroSemanticEntrySnapshot.new(
          raw_entry["key"].as_s,
          raw_entry["value"].as_s
        )
      end
      annotations = semantic_annotations(raw_path["annotations"]?)
      superclass = raw_path["superclass"]?.try(&.as_h?)
      superclass_name = superclass.try(&.["source"]?).try(&.as_s?)
      superclass_annotations = semantic_annotations(superclass.try(&.["annotations"]?))
      subclass_names = raw_path["subclasses"]?.try(&.as_a?).try(&.map(&.as_s)) || [] of String
      paths[name] = Facet::Compiler::MacroSemanticPathSnapshot.new(
        source: raw_path["source"].as_s,
        kind: raw_path["kind"].as_s,
        type_kind: raw_path["type_kind"]?.try(&.as_s?),
        module_type: raw_path["module"]?.try(&.as_bool?) || false,
        class_type: raw_path["class"]?.try(&.as_bool?) || false,
        struct_type: raw_path["struct"]?.try(&.as_bool?) || false,
        keys: keys,
        entries: entries,
        annotations: annotations,
        superclass_name: superclass_name,
        superclass_annotations: superclass_annotations,
        subclass_names: subclass_names
      )
      if raw_methods = raw_path["class_methods"]?.try(&.as_a?)
        unless raw_methods.empty?
          type_methods["#{raw_path["source"].as_s}.class"] = semantic_methods(raw_methods)
        end
      end
      if fixture_case.invocation.includes?(".methods") || fixture_case.definition.includes?(".methods") ||
         fixture_case.invocation.includes?(".has_method?") || fixture_case.definition.includes?(".has_method?")
        if raw_methods = raw_path["methods"]?.try(&.as_a?)
          type_methods[raw_path["source"].as_s] = semantic_methods(raw_methods)
        end
      end
      if fixture_case.invocation.includes?(".constants") || fixture_case.definition.includes?(".constants") ||
         fixture_case.invocation.includes?(".has_constant?") || fixture_case.definition.includes?(".has_constant?")
        if raw_constants = raw_path["constants"]?.try(&.as_a?)
          type_constants[raw_path["source"].as_s] = raw_constants.map(&.as_s)
        end
      end
    end
    type_name = semantic_instance_scope(fixture_case.scope)
    unless fixture_case.scope_annotations.empty?
      type_annotations[type_name] = semantic_annotations(JSON::Any.new(fixture_case.scope_annotations))
    end
    unless fixture_case.scope_instance_vars.empty?
      type_instance_var_snapshots = {
        type_name => fixture_case.scope_instance_vars.map do |raw_variable|
          Facet::Compiler::MacroSemanticInstanceVarSnapshot.new(
            raw_variable["name"].as_s,
            semantic_annotations(raw_variable["annotations"]?)
          )
        end,
      }
    else
      type_instance_var_snapshots = {} of String => Array(Facet::Compiler::MacroSemanticInstanceVarSnapshot)
    end
    unless fixture_case.scope_constants.empty?
      constants = {} of String => String
      fixture_case.scope_constants.each do |raw_constant|
        constants[raw_constant["name"].as_s] = raw_constant["source"].as_s
      end
      type_constants[type_name] = constants.keys
      type_constant_values[type_name] = constants
    end
    unless fixture_case.scope_class_methods.empty?
      type_methods["#{type_name}.class"] = semantic_methods(fixture_case.scope_class_methods)
    end
    instance_vars = fixture_case.instance_vars.empty? ? ({} of String => Array(String)) : {type_name => fixture_case.instance_vars}
    Facet::Compiler::MacroExpansionContext.new(
      flags: fixture_case.flags,
      command_outputs: fixture_case.command_outputs,
      resolve_type_arguments: true,
      lexical_scope: fixture_case.kind == "call" && type_name != "main" ? type_name : nil,
      semantic_paths: paths,
      semantic_path_errors: fixture_case.path_errors,
      type_instance_vars: instance_vars,
      type_instance_var_snapshots: type_instance_var_snapshots,
      type_methods: type_methods,
      type_constants: type_constants,
      type_constant_values: type_constant_values,
      type_abstractness: type_name == "main" ? ({} of String => Bool) : {type_name => fixture_case.scope_abstract},
      type_annotations: type_annotations,
      type_superclasses: type_superclasses,
      type_superclass_annotations: type_superclass_annotations,
      type_subclasses: type_subclasses
    )
  end

  private def semantic_instance_scope(scope : String) : String
    scope.rchop(".class").rchop('+')
  end

  private def semantic_methods(raw_methods : Array(JSON::Any)) : Array(Facet::Compiler::MacroSemanticMethodSnapshot)
    raw_methods.map do |raw_method|
      Facet::Compiler::MacroSemanticMethodSnapshot.new(
        raw_method["name"].as_s,
        raw_method["source"].as_s,
        semantic_annotations(raw_method["annotations"]?)
      )
    end
  end

  private def semantic_annotations(raw_annotations : JSON::Any?) : Array(Facet::Compiler::MacroSemanticAnnotationSnapshot)
    return [] of Facet::Compiler::MacroSemanticAnnotationSnapshot unless raw_annotations
    raw_annotations.as_a.map do |raw_annotation|
      named_sources = {} of String => String
      raw_annotation["named_args"]?.try(&.as_h).try do |raw_named|
        raw_named.each { |name, source| named_sources[name] = source.as_s }
      end
      Facet::Compiler::MacroSemanticAnnotationSnapshot.new(
        raw_annotation["name"].as_s,
        raw_annotation["args"]?.try(&.as_a).try(&.map(&.as_s)) || [] of String,
        named_sources
      )
    end
  end
end
