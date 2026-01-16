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

  it_parses <<-CRYSTAL
    case x
    when 1, 2 then foo
    when 3
      bar
    else
      baz
    end
  CRYSTAL

  it_parses <<-CRYSTAL
    begin
      risky
    rescue
      handle
    ensure
      cleanup
    end
  CRYSTAL

  it_parses "1 rescue 2"
  it_parses "1 ensure 2"

  it_parses <<-CRYSTAL
    select
    when foo
      2
    else
      3
    end
  CRYSTAL

  it_parses "for x, y in items\n  x\nend"
end
