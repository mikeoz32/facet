require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (FFI and externals)" do
  it_parses "lib LibC\nfun printf(format : UInt8*, ...) : Int32\nend"
  it_parses "lib LibC\nalias SizeT = UInt64\n type VoidPtr = Void*\nend"

  it_parses "lib LibC\nfun getchar\nend"
  it_parses "lib LibC\nfun getchar(...)\nend"
  it_parses "lib LibC\nfun getchar : Int\nend"
  it_parses "lib LibC\nfun getchar(Int, Float)\nend"
  it_parses "lib LibC\nfun getchar(a : Int, b : Float)\nend"
  it_parses "lib LibC\nfun getchar(a : Int)\nend"
  it_parses "lib LibC\nfun getchar(a : Int, b : Float) : Int\nend"
  it_parses "lib LibC; fun getchar(a : Int, b : Float) : Int; end"
  it_parses "lib LibC; fun foo(a : Int*); end"
  it_parses "lib LibC; fun foo(a : Int**); end"
  it_parses "lib LibC; fun foo : Int*; end"
  it_parses "lib LibC; fun foo : Int**; end"
  it_parses "lib LibC; type A = B; end"
  it_parses "lib LibC; type A = B*; end"
  it_parses "lib LibC; type A = B**; end"
  it_parses "lib LibC; type A = B.class; end"
  it_parses "lib LibC; struct Foo; end end"
  it_parses "lib LibC; struct Foo; x : Int; y : Float; end end"
  it_parses "lib LibC; struct Foo; x : Int*; end end"
  it_parses "lib LibC; struct Foo; x : Int**; end end"
  it_parses "lib LibC; union Foo; end end"
  it_parses "lib LibC; enum Foo; A\nB; C\nD = 1; end end"
  it_parses "lib LibC; enum Foo; A = 1; B; end end"
  it_parses "lib LibC; Foo = 1; end"
  it_parses "lib LibC\nfun getch = GetChar\nend"
  it_parses %(lib LibC\nfun getch = "get.char"\nend)
  it_parses %(lib LibC\nfun getch = "get.char" : Int32\nend)
  it_parses %(lib LibC\nfun getch = "get.char"(x : Int32)\nend)
  it_parses "lib LibC\n$errno : Int32\n$errno2 : Int32\nend"
  it_parses "lib LibC\n$errno : Int32\nend"
  it_parses "lib LibC\nalias Foo = Bar\nend"
  it_parses "lib LibC; struct Foo; include Bar; end; end"

  it_parses "fun foo(x : Int32) : Void\nend"
end
