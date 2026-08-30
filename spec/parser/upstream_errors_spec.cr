require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (selected errors)" do
  it "rejects wrong param type/default order" do
    source = Facet::Compiler::Source.new("def foo(var = 1 : Int32); end", "err")
    parser = Facet::Compiler::Parser.new(source)
    parser.parse_file
    parser.diagnostics.should_not be_empty
  end

  it "rejects macro unmatched end" do
    source = Facet::Compiler::Source.new("{% if true %} 1 ", "err")
    parser = Facet::Compiler::Parser.new(source)
    parser.parse_file
    parser.diagnostics.should_not be_empty
  end

  it "rejects nesting type/def inside def head" do
    %w(class module struct enum fun alias abstract include extend lib macro).each do |kw|
      source = Facet::Compiler::Source.new("def foo\n#{kw}\nend", "err")
      parser = Facet::Compiler::Parser.new(source)
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end

  it "rejects && and || as methods" do
    [
      "foo.&&", "foo.&&()", "foo &.&&", "foo &.&&()",
      "foo.||", "foo.||()", "foo &.||", "foo &.||()",
    ].each do |code|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new(code, "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end

  it "rejects redefining pseudo-methods" do
    %w(! is_a? as as? responds_to? nil?).each do |name|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new("def #{name}; end", "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty

      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new("def self.#{name}; end", "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end

  it "rejects responds_to? without target" do
    parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new("foo.responds_to?", "err"))
    parser.parse_file
    parser.diagnostics.should_not be_empty
  end

  it "rejects include/extend without arguments" do
    %w(include extend).each do |kw|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new("#{kw}\n", "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end

  it "rejects obviously unterminated literals and calls" do
    [
      " [1, 2, 3 end",
      " {1 => end",
      " {1, 2, 3 end",
      " (1, 2, 3 end",
      "foo(1, 2, 3 end",
      "foo(foo(&.block)",
    ].each do |code|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new(code, "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end

  it "rejects invalid assignment targets and operator forms" do
    [
      "a, b.<=",
      "*a == 1",
      "*a === 1",
      "a {}, b = 1",
      "a.b {}, c = 1",
      "a.b(), c.d = 1",
      "a.b, c.d() = 1",
      "a() = 1",
      "a {} = 1",
      "a.b() = 1",
      "a.[]() = 1",
      "a() += 1",
      "a {} += 1",
      "a.b() += 1",
      "a.[]() += 1",
      "a.[] 0 = 1",
      "a.[] 0 += 1",
      "a b: 0 = 1",
    ].each do |code|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new(code, "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end

  it "rejects invalid def names and setters" do
    [
      "def foo!=; end",
      "def foo?=(x); end",
      "def foo=(a,b); end",
      "def foo=(a = 1, b = 2); end",
      "def foo=(*args); end",
      "def foo=(**kwargs); end",
      "def foo=(&block); end",
    ].each do |code|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new(code, "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end

  # Upstream syntax error cases (ported)
  assert_syntax_error "b? = 1", %(unexpected token: "=")
  assert_syntax_error "b! = 1", %(unexpected token: "=")
  assert_syntax_error "a, B = 1, 2", "can't assign to constant in multiple assignment"
  assert_syntax_error "1 == 2, a = 4"
  assert_syntax_error "x : String, a = 4"
  assert_syntax_error "b, 1 == 2, a = 4"
  assert_syntax_error "a = 1, 2, 3", "Multiple assignment count mismatch"
  assert_syntax_error "a = 1, b = 2", "Multiple assignment count mismatch"

  assert_syntax_error "*a"
  assert_syntax_error "*a if true"
  assert_syntax_error "*a if true = 2"
  assert_syntax_error "*a, 1 = 2"
  assert_syntax_error "*1, a = 2"
  assert_syntax_error "*a, *b = 1", "splat assignment already specified"
  assert_syntax_error "*a, b, c, d = 1, 2", "Multiple assignment count mismatch"
  assert_syntax_error "a, b, *c, d = 1, 2", "Multiple assignment count mismatch"
  assert_syntax_error "*a, b, c, d, e = 1, 2", "Multiple assignment count mismatch"
  assert_syntax_error "a, b, c, d, *e = 1, 2, 3", "Multiple assignment count mismatch"

  assert_syntax_error "a = *1", %(unexpected token: "*")
  assert_syntax_error "a = *1, 2", %(unexpected token: "*")
  assert_syntax_error "a = 1, *2", %(unexpected token: "*")
  assert_syntax_error "a, b = *1", %(unexpected token: "*")
  assert_syntax_error "a, b = *1, 2", %(unexpected token: "*")
  assert_syntax_error "a, b = 1, *2", %(unexpected token: "*")
  assert_syntax_error "a, *b = *1", %(unexpected token: "*")
  assert_syntax_error "a, *b = *1, 2", %(unexpected token: "*")
  assert_syntax_error "a, *b = 1, *2", %(unexpected token: "*")

  assert_syntax_error "a, b.<="
  assert_syntax_error "*a == 1"
  assert_syntax_error "*a === 1"
  assert_syntax_error "a {}, b = 1"
  assert_syntax_error "a.b {}, c = 1"
  assert_syntax_error "a.b(), c.d = 1"
  assert_syntax_error "a.b, c.d() = 1"
  assert_syntax_error "a() = 1"
  assert_syntax_error "a {} = 1"
  assert_syntax_error "a.b() = 1"
  assert_syntax_error "a.[]() = 1"
  assert_syntax_error "a() += 1"
  assert_syntax_error "a {} += 1"
  assert_syntax_error "a.b() += 1"
  assert_syntax_error "a.[]() += 1"
  assert_syntax_error "a.[] 0 = 1"
  assert_syntax_error "a.[] 0 += 1"
  assert_syntax_error "a b: 0 = 1"

  assert_syntax_error "def foo!=; end", %(unexpected token: "!=")
  assert_syntax_error "def foo?=(x); end", %(unexpected token: "?")
  assert_syntax_error "def foo=(a,b); end", "setter method 'foo=' cannot have more than one parameter"
  assert_syntax_error "def foo=(a = 1, b = 2); end", "setter method 'foo=' cannot have more than one parameter"
  assert_syntax_error "def foo=(*args); end", "setter method 'foo=' cannot have more than one parameter"
  assert_syntax_error "def foo=(**kwargs); end", "setter method 'foo=' cannot have more than one parameter"
  assert_syntax_error "def foo=(&block); end", "setter method 'foo=' cannot have a block"

  assert_syntax_error "x { |*a, *b| }", "splat block parameter already specified"

  assert_syntax_error "def foo(x, *); 1; end", "named parameters must follow bare *"
  assert_syntax_error "def foo(var = 1 : Int32); end", "the syntax for a parameter with a default value V and type T is `param : T = V`"
  assert_syntax_error "def foo(var = x : Int); end", "the syntax for a parameter with a default value V and type T is `param : T = V`"
  assert_syntax_error "def foo(**args, **args2); end", "only block parameter is allowed after double splat"
  assert_syntax_error "def foo(**args, x); end", "only block parameter is allowed after double splat"
  assert_syntax_error "def foo(**args, *x); end", "only block parameter is allowed after double splat"
  assert_syntax_error "def foo(_ y); y; end"
  assert_syntax_error "def foo(\"\" y); y; end", "external parameter name cannot be empty"
  assert_syntax_error "def foo(x x); 1; end", "when specified, external name must be different than internal name"
  assert_syntax_error "def foo(x @x); 1; end", "when specified, external name must be different than internal name"
  assert_syntax_error "def foo(x @@x); 1; end", "when specified, external name must be different than internal name"
  assert_syntax_error "def foo(*a foo); end"
  assert_syntax_error "def foo(**a foo); end"
  assert_syntax_error "def foo(&a foo); end"
  assert_syntax_error "macro foo(\"\" y); end", "external parameter name cannot be empty"
  assert_syntax_error "macro foo(x, *); 1; end", "named parameters must follow bare *"
  assert_syntax_error "macro foo(**x, **y)", "only block parameter is allowed after double splat"
  assert_syntax_error "macro foo(**x, y)", "only block parameter is allowed after double splat"

  assert_syntax_error "def foo var; end", "parentheses are mandatory for def parameters"
  assert_syntax_error "def foo var\n end", "parentheses are mandatory for def parameters"
  assert_syntax_error "def foo &block ; end", "parentheses are mandatory for def parameters"
  assert_syntax_error "def foo &block : Int -> Double ; end", "parentheses are mandatory for def parameters"
  assert_syntax_error "def foo @var, &block; end", "parentheses are mandatory for def parameters"
  assert_syntax_error "def foo @@var, &block; end", "parentheses are mandatory for def parameters"
  assert_syntax_error "def foo *y; 1; end", "parentheses are mandatory for def parameters"

  assert_syntax_error "def foo(x : U) forall; end"
  assert_syntax_error "def foo(x : U) forall U,; end"
  assert_syntax_error "def foo(x : U) forall U, U; end", "duplicated free variable name: U"

  assert_syntax_error "foo(\"\": 1)", "named argument cannot have an empty name"
  assert_syntax_error "class Foo(); end", "must specify at least one type var"
  assert_syntax_error "class Foo(*T, *U); end", "splat type parameter already specified"

  assert_syntax_error "Foo(T, x: U)"
  assert_syntax_error "Foo(x: T y: U)"
  assert_syntax_error "Foo(\"\": T)", "named argument cannot have an empty name"
  assert_syntax_error "Foo({x: X, x: Y})", "duplicated key: x"
  assert_syntax_error "foo(&block) {}"
  assert_syntax_error "foo { |a b| }", "expecting ',' or '|', not b"
  assert_syntax_error "foo { |(a b)| }", "expecting ',' or ')', not b"

  assert_syntax_error "1 2", %(unexpected token: "2")
  assert_syntax_error "foo(1 2)"
  assert_syntax_error %(foo("bar" "baz"))
  assert_syntax_error "false foo"
  assert_syntax_error "nil foo"
  assert_syntax_error "'a' foo"
  assert_syntax_error %("hello" foo)
  assert_syntax_error %(:bar foo)
  assert_syntax_error "1 foo"
  assert_syntax_error "1 then"
  assert_syntax_error "return 1 foo"
  assert_syntax_error "return false foo"

  assert_syntax_error "macro foo(x : Int32); end"
  assert_syntax_error "/foo)/", "invalid regex"
  assert_syntax_error "def =\nend"
  assert_syntax_error "{1, ->{ |x| x } }", "unexpected token: \"|\""
  assert_syntax_error "{1, ->do\n|x| x\\end }", "unexpected token: \"|\""
  assert_syntax_error "{1, ->{ |_| x } }", "unexpected token: \"|\""
  assert_syntax_error "macro foo; {% foo = 1 }; end"
  assert_syntax_error "macro def foo : String; 1; end"
  assert_syntax_error "macro Foo;end", "macro can't have a receiver"
  assert_syntax_error "macro foo.bar;end", "macro can't have a receiver"
  assert_syntax_error "macro Foo.bar;end", "macro can't have a receiver"
  assert_syntax_error "macro foo&&;end"
  assert_syntax_error "macro foo"
  assert_syntax_error "macro !;end", "'!' is a pseudo-method and can't be redefined"
  assert_syntax_error "macro is_a?; end", "'is_a?' is a pseudo-method and can't be redefined"
  assert_syntax_error "macro as; end", "'as' is a pseudo-method and can't be redefined"
  assert_syntax_error "macro as?; end", "'as?' is a pseudo-method and can't be redefined"
  assert_syntax_error "macro responds_to?; end", "'responds_to?' is a pseudo-method and can't be redefined"
  assert_syntax_error "macro nil?; end", "'nil?' is a pseudo-method and can't be redefined"
  assert_syntax_error "{{ {{ 1 }} }}", "can't nest macro expressions"
  assert_syntax_error "{{ {% begin %} }}", "can't nest macro expressions"
  assert_syntax_error "macro foo;{%end};end"
  assert_syntax_error "macro foo(x y z); end"
  assert_syntax_error "macro foo x y; end", "parentheses are mandatory for macro parameters"
  assert_syntax_error "macro foo *y;end", "parentheses are mandatory for macro parameters"
  assert_syntax_error %(macro foo x; 1 + 2; end), "parentheses are mandatory for macro parameters"
  assert_syntax_error %(macro foo x\n 1 + 2; end), "parentheses are mandatory for macro parameters"
  assert_syntax_error "macro foo(*x, *y); end", %(unexpected token: "*")

  assert_syntax_error "a = 1; b = 2; a, b += 1, 2"
  assert_syntax_error "lib LibC\n$Errno : Int32\nend", "external variables must start with lowercase, use for example `$errno = Errno : Int32`"
  assert_syntax_error "a += 1", "'+=' before definition of 'a'"
  ["-=", "*=", "/=", "//=", "%=", "|=", "&=", "^=", "**=", "<<=", ">>=",
   "&&=", "||=", "&+=", "&-=", "&*=", "&**="].each do |operator|
    it "rejects #{operator} before local definition" do
      parser = parse_error("local #{operator} 1")
      parser.diagnostics.first.message.should eq("'#{operator}' before definition of 'local'")
    end
  end
  it_diagnoses "& value", %(unexpected token: "&")
  it_diagnoses "&* value", %(unexpected token: "&*")
  it_diagnoses ".foo", %(unexpected token: ".")
  it_diagnoses "left : right", %(unexpected token: "right")
  it_diagnoses "left : Foo::right", "expecting token 'CONST', not 'right'"
  assert_syntax_error "self = 1", "can't change the value of self"
  assert_syntax_error "self += 1", "can't change the value of self"
  assert_syntax_error "FOO, BAR = 1, 2", "Multiple assignment is not allowed for constants"
  assert_syntax_error "self, x = 1, 2", "can't change the value of self"
  assert_syntax_error "x, self = 1, 2", "can't change the value of self"
  assert_syntax_error "def foo; A = 1; end", "dynamic constant assignment. Constants can only be declared at the top level or inside other types."
  assert_syntax_error "A = B = 1", "dynamic constant assignment"
  assert_syntax_error "A = (B = 1)", "dynamic constant assignment"
  assert_syntax_error "A = foo(B = 1)", "dynamic constant assignment"
  assert_syntax_error "A = foo { B = 1 }", "dynamic constant assignment"
  assert_syntax_error "A = begin; B = 1; end", "dynamic constant assignment"
  assert_syntax_error "A = begin; 1; rescue; B = 1; end", "dynamic constant assignment"
  assert_syntax_error "A = begin; 1; rescue; 1; else; B = 1; end", "dynamic constant assignment"
  assert_syntax_error "A = begin; 1; ensure; B = 1; end", "dynamic constant assignment"
  assert_syntax_error "1 while 3", "trailing `while` is not supported"
  assert_syntax_error "1 until 3", "trailing `until` is not supported"
  assert_syntax_error "x++", "postfix increment is not supported, use `exp += 1`"
  assert_syntax_error "x--", "postfix decrement is not supported, use `exp -= 1`"
  assert_syntax_error "if 1 == 1 a; end", "unexpected token"
  assert_syntax_error "unless 1 == 1 a; end", "unexpected token"
  assert_syntax_error "while 1 == 1 a; end", "unexpected token"
  assert_syntax_error "case 1 == 1 a; when 2; end", "unexpected token"
  assert_syntax_error "case 1 == 1; when 2 a; end", "unexpected token"
  assert_syntax_error %(class Foo; require "bar"; end), "can't require inside type declarations"
  assert_syntax_error %(module Foo; require "bar"; end), "can't require inside type declarations"
  assert_syntax_error %(def foo; require "bar"; end), "can't require inside def"
  assert_syntax_error "def foo(x: Int32); end", "space required before colon in type restriction"
  assert_syntax_error "def foo(x :Int32); end", "space required after colon in type restriction"
  assert_syntax_error "def f end", %(unexpected token: "end")
  assert_syntax_error "fun foo\nclass", "can't define class inside fun"
  assert_syntax_error "fun foo\nFoo = 1", "dynamic constant assignment"
  assert_syntax_error %([\n"foo"\n"bar"\n])
  assert_syntax_error %({\n1 => 2\n3 => 4\n})
  assert_syntax_error %({\n1 => 2, 3 => 4\n5 => 6})
  assert_syntax_error %({\n"foo"\n"bar"\n})
  assert_syntax_error "[1\n,2]"
  assert_syntax_error "{1\n,2}"
  assert_syntax_error "{1, 2\n,3}"
  assert_syntax_error "{1 => 2\n,3 => 4}"
  assert_syntax_error "foo(1\n,2)"
  assert_syntax_error "foo(a: 1\n,b: 2)"
  assert_syntax_error "def foo(x\n,y); 1; end"
  assert_syntax_error "macro foo(x\n,y); 1; end"
  assert_syntax_error "class Foo(X\n,Y); 1; end"
  assert_syntax_error "Foo(X\n,Y)"
  assert_syntax_error "Foo(x: X\n,y: Y)"
  assert_syntax_error %({"a" : 1}), "space not allowed between named argument name and ':'"
  assert_syntax_error %({"a": 1, "b" : 2}), "space not allowed between named argument name and ':'"
  assert_syntax_error "case x; when nil; 2; when nil; end", "duplicate when nil in case"
  assert_syntax_error "case x; when true; 2; when true; end", "duplicate when true in case"
  assert_syntax_error "case x; when 1; 2; when 1; end", "duplicate when 1 in case"
  assert_syntax_error "case x; when 'a'; 2; when 'a'; end", "duplicate when 'a' in case"
  assert_syntax_error %(case x; when "a"; 2; when "a"; end), %(duplicate when "a" in case)
  assert_syntax_error %(case x; when :a; 2; when :a; end), "duplicate when :a in case"
  assert_syntax_error %(case x; when {1, 2}; 2; when {1, 2}; end), "duplicate when {1, 2} in case"
  assert_syntax_error %(case x; when [1, 2]; 2; when [1, 2]; end), "duplicate when [1, 2] in case"
  assert_syntax_error %(case x; when 1..2; 2; when 1..2; end), "duplicate when 1..2 in case"
  assert_syntax_error %(case x; when /x/; 2; when /x/; end), "duplicate when /x/ in case"
  assert_syntax_error %(case x; when X; 2; when X; end), "duplicate when X in case"
  assert_syntax_error "case x; when _; end", "'when _' is not supported, use 'else' block instead"
  assert_syntax_error "case x; when 1; when _; end", "'when _' is not supported, use 'else' block instead"
  assert_syntax_error "case x; when 1, _; end", "'when _' is not supported, use 'else' block instead"
  assert_syntax_error "%w(", "Unterminated string array literal"
  assert_syntax_error "%w{one}}", "expecting token 'EOF', not '}'"
  assert_syntax_error "%w{{one}", "Unterminated string array literal"
  assert_syntax_error "%i(", "Unterminated symbol array literal"
  assert_syntax_error "%i{one}}", "expecting token 'EOF', not '}'"
  assert_syntax_error "%i{{one}", "Unterminated symbol array literal"
  assert_syntax_error "%x(", "Unterminated command literal"
  assert_syntax_error "%r(", "Unterminated regular expression"
  assert_syntax_error "%q(", "Unterminated string literal"
  assert_syntax_error "%Q(", "Unterminated string literal"
  assert_syntax_error "<<-HEREDOC", "Unexpected EOF on heredoc identifier"
  assert_syntax_error "<<-HEREDOC\n", "Unterminated heredoc"
  assert_syntax_error "<<-'HEREDOC'", "Unexpected EOF on heredoc identifier"
  assert_syntax_error "<<-'HEREDOC'\n", "Unterminated heredoc"
  assert_syntax_error "def foo(x : *Int32); end", "invalid type splat"
  assert_syntax_error "def foo(x : (*Int32)); end", "invalid type splat"
  assert_syntax_error "def foo(x : Int32, Int32); end"
  assert_syntax_error "def foo(x : (Int32, Int32)); end"
  assert_syntax_error "def foo(x : (Int32, Int32) | Int32); end"
  assert_syntax_error "def foo(x : Int32 | (Int32, Int32)); end"
  assert_syntax_error "def foo(x : {Int32, (Int32, Int32)}); end"
  assert_syntax_error "def foo(x : 1); end"
  assert_syntax_error "def foo(x : {sizeof(Int32), 2}); end"
  assert_syntax_error "def foo(x : Array({sizeof(Int32), 2})); end"
  assert_syntax_error "foo x: 1, x: 1", "duplicated named argument: x"
  assert_syntax_error "def foo(x, x); end", "duplicated def parameter name: x"
  assert_syntax_error "def foo(x y, x z); end", "duplicated def parameter external name: x"
  assert_syntax_error "class Foo(T, T); end", "duplicated type parameter name: T"
  assert_syntax_error "->(x : Int32, x : Int32) {}", "duplicated proc literal parameter name: x"
  assert_syntax_error "foo { |x, x| }", "duplicated block parameter name: x"
  assert_syntax_error "foo { |x, (x)| }", "duplicated block parameter name: x"
  assert_syntax_error "foo { |(x, x)| }", "duplicated block parameter name: x"
  assert_syntax_error "def foo(*x, **x); end", "duplicated def parameter name: x"
  assert_syntax_error "def foo(*x, &x); end", "duplicated def parameter name: x"
  assert_syntax_error "def foo(**x, &x); end", "duplicated def parameter name: x"
  assert_syntax_error "def foo(x, **x); end", "duplicated def parameter name: x"
  assert_syntax_error "fun foo(x : Int32, x : Int64); end", "duplicated fun parameter name: x"
  assert_syntax_error "lib Foo; fun foo(x : Int32, x : Int64); end", "duplicated fun parameter name: x"
  assert_syntax_error "Set {1, 2, 3} of Int32"
  assert_syntax_error "Hash {foo: 1} of Int32 => Int32"
  assert_syntax_error "enum Foo < UInt16; end"
  assert_syntax_error "@:Foo"
  assert_syntax_error "Foo{one: :two, three: :four}", "can't use named tuple syntax for Hash-like literal"
  assert_syntax_error "{one: :two, three: :four} of Symbol => Symbol"
  assert_syntax_error %(Hash{"foo": 1}), "can't use named tuple syntax for Hash-like literal"
  assert_syntax_error %(Hash{"foo": 1, "bar": 2}), "can't use named tuple syntax for Hash-like literal"
  assert_syntax_error "{foo: 1\nbar: 2}"
  assert_syntax_error "{foo: 1, bar: 2\nbaz: 3}"
  assert_syntax_error "'''", "invalid empty char literal"
  assert_syntax_error "def foo(*args = 1); end", "splat parameter can't have default value"
  assert_syntax_error "def foo(**args = 1); end", "double splat parameter can't have default value"
  assert_syntax_error "require 1", "expected string literal for require"
  assert_syntax_error %(def foo("bar \#{1} qux" y); y; end), "interpolation not allowed in external name"
  assert_syntax_error "def Foo(Int32).bar;end"
  assert_syntax_error "[\n]", "for empty arrays use '[] of ElementType'"
  assert_syntax_error "[1 1]"
  assert_syntax_error "{\n}", "for empty hashes use '{} of KeyType => ValueType'"
  assert_syntax_error "{1 => 2 3 => 4}"
  assert_syntax_error "{1 => 2, 3 => 4 5 => 6}"
  assert_syntax_error "{a: 1 b: 2}"
  assert_syntax_error "{a: 1, b: 2 c: 3}"
  assert_syntax_error "{1 2}"
  assert_syntax_error "{1, 2 3}"
  assert_syntax_error "(1, 2 3)"
  assert_syntax_error "Foo(T U)"
  assert_syntax_error "Foo(T, U V)"
  assert_syntax_error "class Foo(T U)"
  assert_syntax_error "class Foo(T, U V)"
  assert_syntax_error "->(x y) { }"
  assert_syntax_error "->(x, y z) { }"
  assert_syntax_error "x[1:-2]"
  assert_syntax_error "1 ? : 2 : 3"
  assert_syntax_error %(def foo("bar");end), "expected parameter internal name"

  %w(break return next).each do |keyword|
    assert_syntax_error "a = #{keyword}", "void value expression"
    assert_syntax_error "a = 1; a += #{keyword}", "void value expression"
    assert_syntax_error "yield #{keyword}", "void value expression"
    assert_syntax_error "foo(#{keyword})", "void value expression"
    assert_syntax_error "foo[#{keyword}]", "void value expression"
    assert_syntax_error "foo[1] = #{keyword}", "void value expression"
    assert_syntax_error "if #{keyword}; end", "void value expression"
    assert_syntax_error "unless #{keyword}; end", "void value expression"
    assert_syntax_error "while #{keyword}; end", "void value expression"
    assert_syntax_error "until #{keyword}; end", "void value expression"
    assert_syntax_error "1 if #{keyword}", "void value expression"
    assert_syntax_error "1 unless #{keyword}", "void value expression"
    assert_syntax_error "#{keyword}.foo", "void value expression"
    assert_syntax_error "#{keyword}.as(Int32)", "void value expression"
    assert_syntax_error "#{keyword}[]", "void value expression"
    assert_syntax_error "#{keyword}[0]", "void value expression"
    assert_syntax_error "#{keyword}[0]= 1", "void value expression"
    assert_syntax_error "#{keyword} .. 1", "void value expression"
    assert_syntax_error "#{keyword} ... 1", "void value expression"
    assert_syntax_error "1 .. #{keyword}", "void value expression"
    assert_syntax_error "1 ... #{keyword}", "void value expression"
    assert_syntax_error "#{keyword} ? 1 : 2", "void value expression"
    assert_syntax_error "+#{keyword}", "void value expression"
    assert_syntax_error "#{keyword} << 1", "void value expression"
    assert_syntax_error "#{keyword} < 1", "void value expression"
    assert_syntax_error "#{keyword} <= 1", "void value expression"
    assert_syntax_error "#{keyword} == 1", "void value expression"
    assert_syntax_error "#{keyword} >> 1", "void value expression"
    assert_syntax_error "#{keyword} > 1", "void value expression"
    assert_syntax_error "#{keyword} >= 1", "void value expression"
    assert_syntax_error "#{keyword} + 1", "void value expression"
    assert_syntax_error "#{keyword} - 1", "void value expression"
    assert_syntax_error "#{keyword} * 1", "void value expression"
    assert_syntax_error "#{keyword} / 1", "void value expression"
    assert_syntax_error "#{keyword} // 1", "void value expression"
    assert_syntax_error "#{keyword} % 1", "void value expression"
    assert_syntax_error "#{keyword} | 1", "void value expression"
    assert_syntax_error "#{keyword} & 1", "void value expression"
    assert_syntax_error "#{keyword} ^ 1", "void value expression"
    assert_syntax_error "#{keyword} ** 1", "void value expression"
    assert_syntax_error "#{keyword} === 1", "void value expression"
    assert_syntax_error "#{keyword} &+ 1", "void value expression"
    assert_syntax_error "#{keyword} &- 1", "void value expression"
    assert_syntax_error "#{keyword} &* 1", "void value expression"
    assert_syntax_error "#{keyword} &** 1", "void value expression"
    assert_syntax_error "case #{keyword}; when 1; end; end", "void value expression"
    assert_syntax_error "case 1; when #{keyword}; end; end", "void value expression"
  end

  assert_syntax_error "break when true"
end
