require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (libs)" do
  it_parses <<-CRYSTAL
    lib LibC
      type Void = Void*
      struct Stat
        st_size : SizeT
      end
      fun puts(str : UInt8*) : Int32
    end
  CRYSTAL

  it_parses <<-CRYSTAL
    @[Foo]
    lib LibX
      fun foo(x : Int32) : Void
    end
  CRYSTAL

  it_parses "fun foo(x : Int32) : Void\nend"
end
