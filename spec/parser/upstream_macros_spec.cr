require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (macros)" do
  it "parses macro expressions and controls" do
    parse_ok("{{ 1 + 2 }}")
    parse_ok("{% if true %} 1 {% elsif false %} 2 {% else %} 3 {% end %}")
    parse_ok("{% for x in items %} 1 {% end %}")
    parse_ok("{% for x, y in items %} 1 {% end %}")
    parse_ok("{% if true %} {% end %}")
  end

  it "parses macro vars and nested controls" do
    ast = parse_ok("{{ %foo }}")
    exprs = ast.children(ast.root)[0]
    macro_id = ast.children(exprs)[0]
    body = ast.children(macro_id)[0]
    ast.children(body).size.should eq(1)
  end

  it "parses macro begin/end and verbatim" do
    parse_ok(<<-CRYSTAL)
      macro finished
        {% begin %}
          {{2 * 2}}
           {%
             1 + 1
             2 + 2
           %}
        {% end %}
      end
    CRYSTAL

    parse_ok(<<-CRYSTAL)
      macro finished
        {% verbatim do %}
          {%

            a = 1 %}
        {% end %}
      end
    CRYSTAL

    parse_ok(<<-CRYSTAL)
      macro finished
        {% verbatim do %}
          {%


            a = 1
            b = 2 %}
        {% end %}
      end
    CRYSTAL

    parse_ok(<<-CRYSTAL)
      {% if true %}
        %a = {{ 1 + 1 }}
      {% else %}
        %b = {{ 2 + 2 }}
      {% end %}
    CRYSTAL
  end

  it "parses operator macro names" do
    %w(` << < <= == === != =~ !~ >> > >= + - * / // ~ % & | ^ ** []? []= <=> &+ &- &* &**).each do |name|
      parse_ok("macro #{name}; end")
    end
  end

  it "parses macro suffix if/unless after macro vars" do
    parse_ok("macro foo;%var if true;end")
    parse_ok("macro foo;var if true;end")
    parse_ok("macro foo;if %var;true;end;end")
    parse_ok("macro foo;if var;true;end;end")
    parse_ok("macro foo;%var unless true;end")
    parse_ok("macro foo;var unless true;end")
    parse_ok("macro foo;unless %var;true;end;end")
    parse_ok("macro foo;unless var;true;end;end")
  end

  it "parses additional macro forms" do
    parse_ok("macro foo;bar{% begin %}body{% end %}baz;end")
    parse_ok("macro x\n%{}\nend")
    parse_ok("macro foo;%var;end")
    parse_ok("macro foo;%var{1, x} = hello;end")
  end
end
