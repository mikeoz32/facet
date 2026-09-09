require "json"

record RawSemanticContract,
  kind : String,
  source : String,
  suite : String,
  file : String,
  line : Int32,
  inject_primitives : Bool,
  flags : String?,
  expected_type : String?,
  actual_type : String?,
  expected_error : String?,
  actual_error : String?,
  error_line : Int32?,
  error_column : Int32?,
  error_size : Int32? do
  include JSON::Serializable
end

record SemanticContractHeader,
  kind : String,
  crystal_version : String,
  crystal_revision : String,
  suites : Array(String),
  example_count : Int32,
  pending_count : Int32,
  contract_count : Int32,
  type_count : Int32,
  error_count : Int32,
  no_errors_count : Int32 do
  include JSON::Serializable
end

record SemanticContractCase,
  id : String,
  kind : String,
  source : String,
  suite : String,
  line : Int32,
  inject_primitives : Bool,
  flags : String?,
  expected_type : String?,
  expected_error : String?,
  expected_code : String?,
  error_line : Int32?,
  error_column : Int32?,
  error_size : Int32? do
  include JSON::Serializable
end

capture_path = ARGV[0]? || abort "usage: crystal run scripts/normalize_upstream_semantic_capture.cr -- CAPTURE.jsonl OUTPUT.jsonl"
output_path = ARGV[1]? || abort "usage: crystal run scripts/normalize_upstream_semantic_capture.cr -- CAPTURE.jsonl OUTPUT.jsonl"

raw = File.read_lines(capture_path).reject(&.blank?).map { |line| RawSemanticContract.from_json(line) }
occurrences = Hash(String, Int32).new(0)
cases = raw.map do |contract|
  key = "#{contract.suite}:#{contract.line}:#{contract.kind}"
  occurrence = occurrences[key]
  occurrences[key] = occurrence + 1
  expected_code = if contract.kind == "error"
                    message = contract.actual_error
                    if message.try(&.includes?("undefined method"))
                      "facet.undefined_method"
                    elsif message.try(&.includes?("undefined constant"))
                      "facet.undefined_constant"
                    elsif message.try(&.includes?("undefined local variable or method"))
                      "facet.undefined_local"
                    elsif message.try(&.includes?("can't infer type of constant"))
                      "facet.constant_cycle"
                    elsif message.try(&.includes?("is not a type, it's a constant"))
                      "facet.constant_as_type"
                    end
                  end
  SemanticContractCase.new(
    "#{key}:#{occurrence}",
    contract.kind,
    contract.source,
    "spec/compiler/semantic/#{contract.suite}",
    contract.line,
    contract.inject_primitives,
    contract.flags,
    contract.expected_type,
    contract.expected_error,
    expected_code,
    contract.error_line,
    contract.error_column,
    contract.error_size
  )
end

header = SemanticContractHeader.new(
  "facet-upstream-semantic-contracts",
  "1.21.0",
  "57cf7da5094db6c5d3c058c6d054a757b5ced19e",
  cases.map(&.suite).uniq.sort,
  449,
  2,
  cases.size,
  cases.count { |entry| entry.kind == "type" },
  cases.count { |entry| entry.kind == "error" },
  cases.count { |entry| entry.kind == "no_errors" }
)

File.open(output_path, "w") do |io|
  io.puts header.to_json
  cases.each { |entry| io.puts entry.to_json }
end

puts "contracts=#{header.contract_count} types=#{header.type_count} errors=#{header.error_count} no_errors=#{header.no_errors_count} suites=#{header.suites.size} output=#{output_path}"
