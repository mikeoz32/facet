require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (literals)" do
  # Numbers
  it_parses "1"
  it_parses "+1"
  it_parses "-1"
  it_parses "1_i64"
  it_parses "+1_i64"
  it_parses "-1_i64"
  it_parses "1_u128"
  it_parses "1_i128"
  it_parses "1.0"
  it_parses "+1.0"
  it_parses "-1.0"
  it_parses "1.0_f32"
  it_parses "+1.0_f32"
  it_parses "-1.0_f32"
  it_parses "2.3_f32"

  # Chars/strings
  it_parses %('a')
  it_parses %("foo")
  it_parses %("")
  it_parses %("hello \\\n     world")
  it_parses %("hello \\\r\n     world")
  it_parses %(%Q{hello \\n world})
  it_parses %(%q{hello \\n world})
  it_parses %(%q{hello \#{foo} world})

  # Symbols
  it_parses ":foo"
  it_parses ":foo!"
  it_parses ":foo?"
  it_parses ":\"foo\""
  it_parses ":かたな"
  it_parses ":+"
  it_parses ":-"
  it_parses ":*"
  it_parses ":/"
  it_parses ":=="
  it_parses ":<"
  it_parses ":<="
  it_parses ":>"
  it_parses ":>="
  it_parses ":!"
  it_parses ":!="
  it_parses ":=~"
  it_parses ":!~"
  it_parses ":&"
  it_parses ":|"
  it_parses ":^"
  it_parses ":~"
  it_parses ":**"
  it_parses ":&**"
  it_parses ":>>"
  it_parses ":<<"
  it_parses ":%"
  it_parses ":[]"
  it_parses ":[]?"
  it_parses ":[]="
  it_parses ":<=>"
  it_parses ":==="
  it_parses %(:"\\\\foo")
  it_parses %(:"\\"foo")
  it_parses %(:"\\"foo\\"")
  it_parses %(:"\\a\\b\\n\\r\\t\\v\\f\\e")
  it_parses %(:"\\u{61}")
  it_parses %(:"")

  it_parses %("hello")
  it_parses %('a')
  it_parses %(:foo)
  it_parses "/foo/"
  it_parses "%w(one two)"
  it_parses "%i(foo bar)"
  it_parses %(foo("#{1 + 1}"))

  it_parses "{\"foo\" => 1, :bar => 2}"
  it_parses "{foo: 1, bar: 2}"
  it_parses %({foo:"a", bar:"b"})
  it_parses %({foo:'a', bar:'b'})
  it_parses %({foo:a, bar:b})

  it_parses %(foo(bar:"a", baz:"b"))
  it_parses %(foo(bar:a, baz:b))
  it_parses %(foo "foo bar": 1, "baz": 2)
  it_parses %(foo(Foo: 1, Bar: 2))

  it_parses "{*1}"
  it_parses "{*1, 2}"
  it_parses "{1, *2}"
  it_parses "{*1, *2}"
  it_parses "{1, *2, 3, *4, 5}"
  it_parses "[*1]"
  it_parses "[*1, 2]"
  it_parses "[1, *2]"
  it_parses "[*1, *2]"
  it_parses "[1, *2, 3, *4, 5]"
  it_parses "Set {*1, 2, *3}"
  it_parses "[*[*[1]], *[2]]"
  it_parses "[] of Int"
  it_parses "[1, 2] of Int"
  it_parses "puts %w(one two)"
  it_parses "puts %w{one two}"
  it_parses "puts %i(one two)"

  it_diagnoses "{*1 => 2}", "expected '}' to close literal"
  it_diagnoses "{*a: 1}", "expected '}' to close literal"
  it_diagnoses "{1 => 2, *3}", "mixed tuple/hash/named tuple entries"
  it_diagnoses "{a: 1, *2}", "mixed tuple/hash/named tuple entries"
  it_parses "case {*1}\nwhen {2}; 3; end"
  it_parses "case {1}\nwhen {*2}; 3; end"

  # Range literals and slicing
  it_parses "1 .. 2"
  it_parses "1 ... 2"
  it_parses "(1 .. )"
  it_parses "(1 ... )"
  it_parses "foo(1.., 2)"
  it_parses "1..;"
  it_parses "1..\n2.."
  it_parses "{1.. => 2};"
  it_parses "..2"
  it_parses "...2"
  it_parses "foo..2"
  it_parses "foo ..2"
  it_parses "foo(..2)"
  it_parses "x[..2]"
  it_parses "x[1, ..2]"
  it_parses "{..2}"
  it_parses "[..2]"
end
