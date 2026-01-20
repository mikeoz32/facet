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
    parse_ok("macro foo; end")
    parse_ok("macro [];end")
    parse_ok(%(macro foo; 1 + 2; end))
    parse_ok(%(macro foo(x); 1 + 2; end))
    parse_ok(%(macro foo(x)\n 1 + 2; end))
    parse_ok("macro foo; 1 + 2 {{foo}} 3 + 4; end")
    parse_ok("macro foo; 1 + 2 {{ foo }} 3 + 4; end")
    parse_ok("macro foo;bar{% for x in y %}body{% end %}baz;end")
    parse_ok("macro foo;bar{% for x, y in z %}body{% end %}baz;end")
    parse_ok("macro foo;bar{% if x %}body{% end %}baz;end")
    parse_ok("macro foo;bar{% if x %}body{% else %}body2{%end%}baz;end")
    parse_ok("macro foo;bar{% if x %}body{% elsif y %}body2{%end%}baz;end")
    parse_ok("macro foo;bar{% if x %}body{% elsif y %}body2{% else %}body3{%end%}baz;end")
    parse_ok("macro foo;bar{% unless x %}body{% end %}baz;end")
    parse_ok("macro foo;bar{% for x in y %}\\  \n   body{% end %}baz;end")
    parse_ok("macro foo;bar{% for x in y %}\\  \n   body{% end %}\\   baz;end")
    parse_ok("macro foo; 1 + 2 {{foo}}\\ 3 + 4; end")
    parse_ok("macro foo(\na = 0\n)\nend")
    parse_ok("macro foo;{% verbatim do %}1{% foo %}2{% end %};end")
    parse_ok("macro foo\n{%\nif 1\n2\nelse\n3\nend\n%}end")
    parse_ok("macro foo\neenum\nend")
    parse_ok("macro foo\n'\\''\nend")
    parse_ok("macro foo\n'\\\\'\nend")
    parse_ok(%(macro foo\n"\\'"\nend))
    parse_ok(%(macro foo\n"\\\\"\nend))
    parse_ok("macro foo;bar(end: 1);end")
    parse_ok("macro foo(@[Foo] var);end")
    parse_ok("macro foo(@[Foo] outer inner);end")
    parse_ok("macro foo(@[Foo]  var);end")
    parse_ok("macro foo(a, @[Foo] var);end")
    parse_ok("macro foo(a, @[Foo] &block);end")
    parse_ok("macro foo(@[Foo] *args);end")
    parse_ok("macro foo(@[Foo] **args);end")
    parse_ok(<<-CRYSTAL)
      macro foo(
        @[Foo]
        id,
        @[Bar] name
      );end
    CRYSTAL

    ast = parse_ok("macro foo(@[Foo] var);end")
    params = macro_params(ast)
    params.size.should eq(1)
    node_kind(ast, params[0]).should eq(Facet::Compiler::NodeKind::Annotation)
  end
end
