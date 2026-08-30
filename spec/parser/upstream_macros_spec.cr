require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (macros)" do
  it_parses "def to_u{{n}} : UInt{{n}}\nend"
  it_parses "fun {{name}}(a : {{from}}) : {{to}}\n  a\nend"
  it_parses "struct {{name}}\nend"
  it_parses "def self.{{name.id}}({{args.splat}}) : {{return_type}}\nend"
  it_parses "::String.build do |%io|\nend"
  it_parses "macro choose\n  value\n  \\{% else %}\nend"
  it_parses "def to_i{{n}}! : Int{{n}}\nend"
  it_parses "def to_i{{n}}! : Int{{n}}\n  to_u{{n}}!.to_i{{n}}!\nend"
  it_parses "@\\{{ivar.id}} = other.@\\{{ivar.id}}.clone"
  it_parses "property {{name}} : {{type}} | Nil"
  it_parses "\\{{ run(\"tool\", {{name}}, {{io.stringify}}) }}"
  it_parses "{% if outer %}\n{% if doc %}# {{doc}}{% end %}\n{% else %}\nfallback\n{% end %}"
  it_parses "{% verbatim do %}{% begin %}{% if true %}body{% end %}{% end %}{% end %}"
  it "parses macro expressions and controls" do
    parse_ok("{{ 1 + 2 }}")
    parse_ok("{% if true %} 1 {% elsif false %} 2 {% else %} 3 {% end %}")
    parse_ok("{% for x in items %} 1 {% end %}")
    parse_ok("{% for x, y in items %} 1 {% end %}")
    parse_ok("{% if true %} {% end %}")
    parse_ok("{{ foo.nil? }}")
    parse_ok("{{ foo &.nil? }}")
    parse_ok("{{ foo.nil?(foo) }}")
    parse_ok("{{ nil?(foo) }}")
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

  it_parses "verbatim = 1; verbatim.to_s"

  [{'(', ')'}, {'[', ']'}, {'<', '>'}, {'{', '}'}, {'|', '|'}].each do |open, close|
    it_parses "{% begin %}%#{open} %s #{close}{% end %}"
    it_parses "{% begin %}%q#{open} %s #{close}{% end %}"
    it_parses "{% begin %}%Q#{open} %s #{close}{% end %}"
    it_parses "{% begin %}%i#{open} %s #{close}{% end %}"
    it_parses "{% begin %}%w#{open} %s #{close}{% end %}"
    it_parses "{% begin %}%x#{open} %s #{close}{% end %}"
    it_parses "{% begin %}%r#{open}\\A#{close}{% end %}"
  end

  it "parses operator macro names" do
    %w(` << < <= == === != =~ !~ >> > >= + - * / // ~ % & | ^ ** []? []= <=> &+ &- &* &**).each do |name|
      parse_ok("macro #{name}; end")
    end
  end

  # Explicit operator forms without space before end (to match upstream strings)
  it_parses "macro `;end"
  it_parses "macro <<;end"
  it_parses "macro <;end"
  it_parses "macro <=;end"
  it_parses "macro ==;end"
  it_parses "macro ===;end"
  it_parses "macro !=;end"
  it_parses "macro =~;end"
  it_parses "macro !~;end"
  it_parses "macro >>;end"
  it_parses "macro >;end"
  it_parses "macro >=;end"
  it_parses "macro +;end"
  it_parses "macro -;end"
  it_parses "macro *;end"
  it_parses "macro /;end"
  it_parses "macro //;end"
  it_parses "macro ~;end"
  it_parses "macro %;end"
  it_parses "macro &;end"
  it_parses "macro |;end"
  it_parses "macro ^;end"
  it_parses "macro **;end"
  it_parses "macro []?;end"
  it_parses "macro []=;end"
  it_parses "macro <=>;end"
  it_parses "macro &+;end"
  it_parses "macro &-;end"
  it_parses "macro &*;end"
  it_parses "macro &**;end"
  it_parses "macro foo;end"

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

  %w(bar? bar!).each do |name|
    it_parses "macro foo(#{name} foo); end"
  end

  %w(i q r w x Q).each do |ch|
    it_parses "macro foo;%#{ch}[#{ch}];end"
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
    parse_ok("macro foo; bar class: 1; end")
    parse_ok("macro foo(@[Foo] var);end")
    parse_ok("macro foo(@[Foo] outer inner);end")
    parse_ok("macro foo(@[Foo]  var);end")
    parse_ok("macro foo(a, @[Foo] var);end")
    parse_ok("macro foo(a, @[Foo] &block);end")
    parse_ok("macro foo(@[Foo] *args);end")
    parse_ok("macro foo(@[Foo] **args);end")
    parse_ok("macro foo(**args)\n1\nend")
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

  # Macro expressions in regular code (parse-only)
  it_parses "puts {{1}}"
  it_parses "puts {{\n1\n}}"
  it_parses "puts {{*1}}"
  it_parses "puts {{**1}}"
  it_parses "{{a = 1 if 2}}"
  it_parses "{% a = 1 %}"
  it_parses "{%\na = 1\n%}"
  it_parses "{% a = 1 if 2 %}"
  it_parses "{% if 1; 2; end %}"
  it_parses "{%\nif 1; 2; end\n%}"
  it_parses "{% if 1\n  x\nend %}"
  it_parses "{% x if 1 %}"
  it_parses "{% unless 1; 2; end %}"
  it_parses "{% unless 1; 2; else 3; end %}"
  it_parses "{% unless 1\n  x\nend %}"
  it_parses "{% x unless 1 %}"
  it_parses "{%\n1\n2\n3\n%}"
  it_parses "{% if 1; 2; end; %}"
  it_parses "{% if 1; 2; end; 3 %}"
  it_parses "{%\nif 1; 2; end; 3\n%}"
  it_parses "{% 2 if 1; 3 %}"
  it_parses "{%\n2 if 1; 3\n%}"
  it_parses "{% if 1; 2; elsif 3; 4; else; 5; end; 6 %}"
  it_parses "{% unless 1; 2; end; %}"
  it_parses "{% unless 1; 2; end; 3 %}"
  it_parses "{%\nunless 1; 2; end; 3\n%}"
  it_parses "{% 2 unless 1; 3 %}"
  it_parses "{%\n2 unless 1; 3\n%}"
  it_parses "{% if true %}\n{% end %}\n{% if true %}\n{% end %}"
  it_parses "{{ 1 // 2 }}"
  it_parses "{{ //.options }}"
  it_parses "macro foo=;end"
  it_parses "{{ foo }}"
  it_parses "{% for x in y %}body{% end %}"
  it_parses "{% if x %}body{% end %}"
  it_parses "{% for _, x, _ in y %}body{% end %}"
  it_parses "{% begin %}{% if true %}if true{% end %}\n{% if true %}end{% end %}{% end %}"
  it_parses "macro foo(x, *y);end"
end
