module Facet
  module Compiler
    enum TokenKind
      Eof
      Identifier
      InstanceVar
      ClassVar
      GlobalVar
      Annotation
      Symbol
      Number
      String
      Regex
      Char
      # Keywords
      KeywordAbstract
      KeywordAlias
      KeywordAlignof
      KeywordAnnotation
      KeywordAs
      KeywordAsQuestion
      KeywordAsm
      KeywordBegin
      KeywordBreak
      KeywordCase
      KeywordClass
      KeywordDef
      KeywordDo
      KeywordElse
      KeywordElsif
      KeywordEnd
      KeywordEnsure
      KeywordEnum
      KeywordExtend
      KeywordFalse
      KeywordFor
      KeywordFun
      KeywordIf
      KeywordIn
      KeywordInclude
      KeywordInstanceAlignof
      KeywordInstanceSizeof
      KeywordIsAQuestion
      KeywordLib
      KeywordMacro
      KeywordModule
      KeywordNext
      KeywordNil
      KeywordNilQuestion
      KeywordOf
      KeywordOffsetof
      KeywordOut
      KeywordProperty
      KeywordGetter
      KeywordSetter
      KeywordPointerof
      KeywordPrivate
      KeywordProtected
      KeywordRequire
      KeywordRescue
      KeywordRespondsToQuestion
      KeywordReturn
      KeywordSelect
      KeywordSelf
      KeywordSizeof
      KeywordStruct
      KeywordSuper
      KeywordThen
      KeywordTrue
      KeywordType
      KeywordTypeof
      KeywordUninitialized
      KeywordUnion
      KeywordUnless
      KeywordUntil
      KeywordVerbatim
      KeywordWhen
      KeywordWhile
      KeywordWith
      KeywordYield
      # Operators
      Plus
      Minus
      Star
      Slash
      SlashSlash
      Percent
      Caret
      Ampersand
      Pipe
      Bang
      Tilde
      Assign
      EqualEqual
      BangEqual
      Less
      LessEqual
      Greater
      GreaterEqual
      AndAnd
      OrOr
      Arrow
      HashRocket
      StarStar
      ShiftLeft
      ShiftRight
      TripleEqual
      Spaceship
      Match
      NotMatch
      SafeNav
      PlusEqual
      MinusEqual
      StarEqual
      SlashEqual
      SlashSlashEqual
      PercentEqual
      # Punctuation
      LParen
      RParen
      LBrace
      RBrace
      LBracket
      RBracket
      Comma
      Dot
      DotDot
      DotDotDot
      Colon
      DoubleColon
      Semicolon
      Question
      At
      Unknown
    end

    struct Token
      getter kind : TokenKind
      getter span : Span

      def initialize(@kind : TokenKind, @span : Span)
      end

      def eof? : Bool
        @kind == TokenKind::Eof
      end
    end
  end
end
