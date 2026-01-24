require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (control flow)" do
  it_parses <<-CRYSTAL
    if a
      b
    elsif c
      d
    else
      e
    end
  CRYSTAL

  it_parses "unless cond\n  body\nend"

  it_parses "while ready?\n  tick\nend"
  it_parses "until done?\n  step\nend"
  it_parses "while true; end;"
  it_parses "while true; 1; end;"
  it_parses "until true; end;"
  it_parses "until true; 1; end;"

  it_parses <<-CRYSTAL
    case x
    when 1, 2 then foo
    when 3
      bar
    else
      baz
    end
  CRYSTAL

  it_parses "case 1; when 1; 2; else; 3; end"
  it_parses "case 1; when 0, 1; 2; else; 3; end"
  it_parses "case 1\nwhen 1\n2\nelse\n3\nend"
  it_parses "case 1\nwhen 1\n2\nend"
  it_parses "case / /\nwhen / /\n/ /\nelse\n/ /\nend"
  it_parses "case 1; when 1 then 2; else; 3; end"
  it_parses "case 1; when x then 2; else; 3; end"
  it_parses "case 1\nwhen 1\n2\nend\nif a\nend"
  it_parses "case\n1\nwhen 1\n2\nend\nif a\nend"
  it_parses "case 1\nwhen .foo\n2\nend"
  it_parses "case 1\nwhen .responds_to?(:foo)\n2\nend"
  it_parses "case 1\nwhen .is_a?(T)\n2\nend"
  it_parses "case 1\nwhen .as(T)\n2\nend"
  it_parses "case 1\nwhen .as?(T)\n2\nend"
  it_parses "case 1\nwhen .!()\n2\nend"
  it_parses "case when 1\n2\nend"
  it_parses "case \nwhen 1\n2\nend"
  it_parses "case {1, 2}\nwhen {3, 4}\n5\nend"
  it_parses "case {1, 2}\nwhen {3, 4}, {5, 6}\n7\nend"
  it_parses "case {1, 2}\nwhen {.foo, .bar}\n5\nend"
  it_parses "case {1, 2}\nwhen foo\n5\nend"
  it_parses "case a\nwhen b\n1 / 2\nelse\n1 / 2\nend"
  it_parses "case a\nwhen b\n/ /\n\nelse\n/ /\nend"
  it_parses "case 1; end"
  it_parses "case foo; end"
  it_parses "case\nend"
  it_parses "case;end"
  it_parses "case 1\nelse\n2\nend"
  it_parses "a = 1\ncase 1\nwhen a then 1\nend"
  it_parses "case\nwhen true\n1\nend"
  it_parses "case;when true;1;end"
  it_parses "case 1\nin Int32; 2; end"
  it_parses "case 1\nin Int32.class; 2; end"
  it_parses "case 1\nin Foo(Int32); 2; end"
  it_parses "case 1\nin false; 2; end"
  it_parses "case 1\nin true; 2; end"
  it_parses "case 1\nin nil; 2; end"
  it_parses "case 1\nin .bar?; 2; end"
  it_parses "case {1}\nin {Int32}; 2; end"
  it_parses "case {1}\nin {Int32.class}; 2; end"
  it_parses "case {1}\nin {Foo(Int32)}; 2; end"
  it_parses "case {1}\nin {false}; 2; end"
  it_parses "case {1}\nin {true}; 2; end"
  it_parses "case {1}\nin {nil}; 2; end"
  it_parses "case {1}\nin {.bar?}; 2; end"
  it_parses "case {1}\nin {_}; 2; end"
  it_parses "case 1; when 2 then /foo/; end"

  it_parses <<-CRYSTAL
    begin
      risky
    rescue
      handle
    ensure
      cleanup
    end
  CRYSTAL

  it_parses "begin; rescue; end"
  it_parses "begin; 1; rescue; 2; end"
  it_parses "begin; 1; ensure; 2; end"
  it_parses "begin\n1\nensure\n2\nend"
  it_parses "begin; 1; rescue Foo; 2; end"
  it_parses "begin; 1; rescue ::Foo; 2; end"
  it_parses "begin; 1; rescue Foo | Bar; 2; end"
  it_parses "begin; 1; rescue ::Foo | ::Bar; 2; end"
  it_parses "begin; 1; rescue ex : Foo | Bar; 2; end"
  it_parses "begin; 1; rescue ex : ::Foo | ::Bar; 2; end"
  it_parses "begin; 1; rescue ex; 2; end"
  it_parses "begin; 1; rescue; 2; else; 3; end"
  it_parses "begin; 1; rescue ex; 2; end; ex"
  it_parses "def foo(); 1; rescue; 2; end"
  it_parses "1.tap do; 1; rescue; 2; end"
  it_parses "-> do; 1; rescue; 2; end"
  it_parses "1.tap do |x|; 1; rescue; x; end"
  it_parses "x = 1 rescue 2"
  it_parses "x = 1 ensure 2"
  it_parses "a = 1; a rescue a"
  it_parses "a = 1; yield a rescue a"
  it_parses "foo ensure 2"
  it_parses "foo rescue 2"
  it_parses "a = 1; a ensure a"
  it_parses "a = 1; yield a ensure a"

  it_parses "1 rescue 2"
  it_parses "1 ensure 2"
  it_parses "1 if 3"
  it_parses "1 unless 3"
  it_parses "foo if 3"
  it_parses "foo unless 3"

  it_parses <<-CRYSTAL
    select
    when foo
      2
    else
      3
    end
  CRYSTAL

  it_parses "select\nwhen foo\n2\nend"
  it_parses "select\nwhen foo\n2\nwhen bar\n4\nend"
  it_parses "select\nwhen foo\n2\nelse\n3\nend"

  it_parses "for x, y in items\n  x\nend"
end
