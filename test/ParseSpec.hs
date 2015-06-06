module ParseSpec where

import Control.Exception (evaluate)
import Control.Arrow (second)
import Data.Either
import Test.Hspec

import Expr
import Parse
import Eval
import JSNum

s :: SrcLoc
s = SrcLoc "" 0 0 Nothing

testParse :: String -> Program
testParse input =
  let Program strictness stmts = simpleParse input
      srcLoc = SrcLoc "" 0 0
      overrideAll = map overrideSrcLoc
      overrideSrcLoc stmt = case stmt of
        Block _ sts            -> Block s $ overrideAll sts
        LabelledStatement _ label st -> LabelledStatement s label $ overrideSrcLoc st
        VarDecl _ a            -> VarDecl s (map fixVars a)
        ExprStmt _ a           -> ExprStmt s (fixExpr a)
        IfStatement _ a b c    -> IfStatement s a (overrideSrcLoc b) (fmap overrideSrcLoc c)
        WhileStatement _ a b   -> WhileStatement s a $ overrideSrcLoc b
        DoWhileStatement _ a b -> DoWhileStatement s a $ overrideSrcLoc b
        For _ a b              -> For s a $ overrideSrcLoc b
        ContinueStatement _ l  -> ContinueStatement s l
        BreakStatement _ l     -> BreakStatement s l
        Return _ a             -> Return s a
        WithStatement _ a b    -> WithStatement s a $ overrideSrcLoc b
        ThrowStatement _ a     -> ThrowStatement s a
        TryStatement _ a b c   -> TryStatement s a b c
        EmptyStatement _       -> EmptyStatement s
        DebuggerStatement _    -> DebuggerStatement s
        FunDecl _ a b c d      -> FunDecl s a b c $ overrideAll d
      fixExpr e = case e of
        FunExpr a b c body -> FunExpr a b c (overrideAll body)
        ObjectLiteral a   -> ObjectLiteral $ map (second f) a
          where f (Value v) = Value v
                f (Getter xs) = Getter (overrideAll xs)
                f (Setter x xs) = Setter x (overrideAll xs)
        _ -> e
      fixVars (x, Just e) = (x, Just (fixExpr e))
      fixVars other = other
  in Program strictness $ overrideAll stmts

unparseable :: String -> IO ()
unparseable str = parseJS str `shouldSatisfy` isLeft

unparseableInStrictMode :: String -> IO ()
unparseableInStrictMode str = do
  parseJS'' str "" Strict False `shouldSatisfy` isLeft
  parseJS'' str "" NotStrict False `shouldSatisfy` isRight


spec :: Spec
spec = do
  describe "ParseExpr" $ do
    it "parses a number" $ do
      parseExpr "1" `shouldBe` Num (JSNum 1)
      parseExpr "1.3" `shouldBe` Num (JSNum 1.3)
      parseExpr "1.3e3" `shouldBe` Num (JSNum 1300)
      parseExpr "1.4E3" `shouldBe` Num (JSNum 1400)
      parseExpr "1.5e+3" `shouldBe` Num (JSNum 1500)
      parseExpr "1.6e-3" `shouldBe` Num (JSNum 0.0016)
      parseExpr "3e3" `shouldBe` Num (JSNum 3000)
      parseExpr ".5" `shouldBe` Num (JSNum 0.5)
      parseExpr "5." `shouldBe` Num (JSNum 5)
      parseExpr "5.e1" `shouldBe` Num (JSNum 50)

    it "doesn't mistake other things for numbers" $ do
      parseExpr "e1" `shouldBe` ReadVar "e1"

    it "parses a negative number" $ do
      parseExpr "-1" `shouldBe` UnOp "-" (Num (JSNum 1))

    it "parses literals" $ do
      parseExpr "true" `shouldBe` Boolean True
      parseExpr "false" `shouldBe` Boolean False
      parseExpr "null" `shouldBe` LiteralNull

    it "parses a variable" $ do
      parseExpr "a" `shouldBe` (ReadVar "a")
      parseExpr "_a123" `shouldBe` (ReadVar "_a123")
      parseExpr "$" `shouldBe` (ReadVar "$")
      parseExpr "$12" `shouldBe` (ReadVar "$12")
      parseExpr "$12$" `shouldBe` (ReadVar "$12$")
      parseExpr "$err" `shouldBe` (ReadVar "$err")
      parseExpr "XYZ" `shouldBe` (ReadVar "XYZ")

    it "parses strings" $ do
      parseExpr "'single quoted'" `shouldBe` Str "single quoted"
      parseExpr "\"double quoted\"" `shouldBe` Str "double quoted"

    it "allows escapes in single quoted strings" $ do
      parseExpr "'a\\t'" `shouldBe` Str "a\t"

    it "parses escape characters in strings" $ do
      parseExpr "\"a\\u0009b\"" `shouldBe` Str "a\tb"
      parseExpr "'it\\'s ok now'" `shouldBe` Str "it's ok now"
      parseExpr "'back\\\\quote'" `shouldBe` Str "back\\quote"

    it "allows line breaks in strings when preceded by backslash" $ do
      parseExpr "\"abc\\\ndef\"" `shouldBe` Str "abc\ndef"
      parseExpr "'abc\\\ndef'" `shouldBe` Str "abc\ndef"

    describe "Object literals" $ do
      it "parses object literals" $ do
        parseExpr "{}" `shouldBe` ObjectLiteral []
        parseExpr "{a: 1}" `shouldBe` ObjectLiteral [("a", Value $ Num 1)]
        parseExpr "{a: 1, b: 2}" `shouldBe`
          ObjectLiteral [("a", Value $ Num 1),
                         ("b", Value $ Num 2)]
        parseExpr "{a: 1, 2: 3}" `shouldBe`
          ObjectLiteral [("a", Value $ Num 1),
                         ("2", Value $ Num 3)]

      it "parses an object literal with a getter" $ do
        let obj = ObjectLiteral [ ("x", Getter [ Return s $ Just (Num 1) ]) ]
        testParse "var a = { get x() { return 1; } }" `shouldBe`
          Program NotStrict [ VarDecl s  [("a", Just $ obj)] ]

      it "parses an object literal with a setter" $ do
        let obj = ObjectLiteral [ ("x", Setter "v" [ ]) ]
        testParse "var a = { set x(v) { } }" `shouldBe`
          Program NotStrict [ VarDecl s  [("a", Just $ obj)] ]

      it "doesn't count a getter and setter as duplicate keys" $ do
        parseExpr "{get foo(){}, set foo(arg){}}" `shouldBe`
          ObjectLiteral [ ("foo", Getter [] ),
                          ("foo", Setter "arg" []) ]


    describe "Array literals" $ do
      it "parses an empty array literal" $ do
        parseExpr "[]" `shouldBe` ArrayLiteral []

      it "parses an array literal with one elision" $ do
        parseExpr "[,]" `shouldBe` ArrayLiteral [Nothing]

      it "parses an array literal with one element" $ do
        parseExpr "[1]" `shouldBe` ArrayLiteral [Just $ Num 1]

      it "parses an array literal with 2 elements" $ do
        parseExpr "[1,2]" `shouldBe` ArrayLiteral [Just $ Num 1, Just $ Num 2]

      it "parses an array literal with elision in the middle" $ do
        parseExpr "[1,,2]" `shouldBe` ArrayLiteral [Just $ Num 1, Nothing, Just $ Num 2 ]

      it "parses an array literal with elision at the end" $ do
        parseExpr "[1,2,]" `shouldBe`
          ArrayLiteral [Just $ Num 1, Just $ Num 2 ]

      it "parses an array literal with elision at the start" $ do
        parseExpr "[,1,2]" `shouldBe`
          ArrayLiteral [Nothing, Just $ Num 1, Just $ Num 2 ]

      it "parses an array literal with only elision" $ do
        parseExpr "[,,]" `shouldBe` ArrayLiteral [Nothing,Nothing]

    it "parses regular expression literals" $ do
      parseExpr "/[a]/i" `shouldBe` RegularExpression "[a]" "i"

    it "parses a unary operator" $ do
      parseExpr "++u" `shouldBe` UnOp "++" (ReadVar "u")

    it "parses a unop assignment" $ do
      parseExpr "e = -1" `shouldBe` Assign (ReadVar "e") "=" (UnOp "-" (Num (JSNum 1)))

    it "parses a chained assignment" $ do
      parseExpr "x = x = 1" `shouldBe` Assign (ReadVar "x") "=" (Assign (ReadVar "x") "=" (Num 1))

    it "parses a plus-equals" $ do
      parseExpr "a += b" `shouldBe` Assign (ReadVar "a") "+=" (ReadVar "b")

    it "parses lvars" $ do
      parseExpr "a.b = 1" `shouldBe` Assign (MemberDot (ReadVar "a") "b") "=" (Num 1)
      parseExpr "a[i] = 1" `shouldBe` Assign (MemberGet (ReadVar "a") (ReadVar "i")) "=" (Num 1)

    it "interprets this tricky case right" $ do
      parseExpr "a++ + ++b" `shouldBe` BinOp "+" (PostOp "++" (ReadVar "a")) (UnOp "++" (ReadVar "b"))
      parseExpr "a+++ ++b" `shouldBe` BinOp "+" (PostOp "++" (ReadVar "a")) (UnOp "++" (ReadVar "b"))
      evaluate (parseExpr "a+++++b") `shouldThrow` anyException
      evaluate (parseExpr "a++ +++b") `shouldThrow` anyException

    it "parses a unop assignment without spaces" $ do
      parseExpr "e=-1" `shouldBe` Assign (ReadVar "e") "=" (UnOp "-" (Num (JSNum 1)))

    it "parses a binop" $ do
      parseExpr "1+2" `shouldBe` BinOp "+" (Num (JSNum 1)) (Num (JSNum 2))
      parseExpr "(1)&&(2)" `shouldBe` BinOp "&&" (Num (JSNum 1)) (Num (JSNum 2))

    it "parses a chained binop" $ do
      parseExpr "a+b+c" `shouldBe` BinOp "+" (BinOp "+" (ReadVar "a") (ReadVar "b")) (ReadVar "c")

    it "parses a ." $ do
      parseExpr "a.b" `shouldBe` MemberDot (ReadVar "a") "b"
      parseExpr "-a.b" `shouldBe` UnOp "-" (MemberDot (ReadVar "a") "b")

    it "parses a []" $ do
      parseExpr "a[\"b\"]" `shouldBe` MemberGet (ReadVar "a") (Str "b")

    it "parses a function call" $ do
      parseExpr "f()" `shouldBe` FunCall (ReadVar "f") []

    it "parses a function call with an argument" $ do
      parseExpr "f(x)" `shouldBe` FunCall (ReadVar "f") [ReadVar "x"]

    it "parses a function call with two arguments" $ do
      parseExpr "f(x,y)" `shouldBe` FunCall (ReadVar "f") [ReadVar "x", ReadVar "y"]

    it "parses a chained function call" $ do
      parseExpr "f(1)(2)" `shouldBe` FunCall (FunCall (ReadVar "f") [Num 1]) [Num 2]

    it "parses a chained member access with a function call" $ do
      parseExpr "a.b.c()" `shouldBe` FunCall (MemberDot (MemberDot (ReadVar "a") "b") "c") []

    it "parses an 'in' expression" $ do
      parseExpr "x in xs" `shouldBe` BinOp "in" (ReadVar "x") (ReadVar "xs")

    it "parses a double-bang" $ do
      parseExpr "!!x" `shouldBe` UnOp "!" (UnOp "!" (ReadVar "x"))

    describe "The comma operator" $ do
      it "separates expressions" $ do
        parseExpr "1,2" `shouldBe` BinOp "," (Num 1) (Num 2)

      it "separates assignments" $ do
        parseExpr "a = 1,2" `shouldBe` BinOp "," (Assign (ReadVar "a") "=" (Num 1)) (Num 2)

  describe "Parsing programs" $ do
    it "parses a strict-mode program" $ do
      testParse "'use strict';\nvar a;" `shouldBe`
        Program Strict [VarDecl s [("a", Nothing)]]

    it "parses a while statement" $ do
      testParse "while (a) { }" `shouldBe` Program NotStrict [WhileStatement s (ReadVar "a") (Block s [])]

    it "treats extra words as reserved when in strict mode" $ do
      testParse "var public;" `shouldBe` Program NotStrict [VarDecl s [("public", Nothing)]]
      unparseable "'use strict'; var public = 1;"

    it "does not eat leading strings that aren't 'use strict'" $ do
      testParse "'tigers are cool'" `shouldBe`
        Program NotStrict [ExprStmt s $ Str "tigers are cool"]

    it "parses a do-while statement" $ do
      testParse "do {} while (a);" `shouldBe` Program NotStrict [DoWhileStatement s (ReadVar "a") (Block s [])]


    it "parses a var declaration" $ do
      testParse "var a;" `shouldBe` Program NotStrict [VarDecl s [("a", Nothing)]]
      testParse "var a, b;" `shouldBe` Program NotStrict [VarDecl s [("a", Nothing),
                                                           ("b", Nothing)]]
      testParse "var a = 1, b;" `shouldBe` Program NotStrict [VarDecl s [("a", Just $ Num 1),
                                                               ("b", Nothing)]]
      testParse "var a = 1, b = a;" `shouldBe` Program NotStrict [VarDecl s [("a", Just $ Num 1),
                                                                   ("b", (Just $ ReadVar "a"))]]

    it "ignores comments at the start of the file" $ do
      testParse "" `shouldBe` Program NotStrict []
      testParse "// this is a comment\n" `shouldBe` Program NotStrict []
      testParse "// a\n//b\n2\n" `shouldBe` Program NotStrict [ExprStmt s (Num 2)]

    it "resolves the if-then-else ambiguity" $ do
      testParse "if (a) if (b) 1; else 2" `shouldBe`
        Program NotStrict [IfStatement s (ReadVar "a")
                               (IfStatement s (ReadVar "b")
                                              (ExprStmt s $ Num 1)
                                              (Just $ ExprStmt s $ Num 2))
                               Nothing]

    it "parses a continue statement" $ do
      testParse "while (false) continue" `shouldBe`
        Program NotStrict [WhileStatement s (Boolean False) $
                   ContinueStatement s Nothing]
      testParse "retry: while (false) continue retry" `shouldBe`
        Program NotStrict [LabelledStatement s "retry" $
                  WhileStatement s (Boolean False) $
                    ContinueStatement s $ Just "retry"]
    it "can break a line before a continue semicolon" $ do
      testParse "while (false) { continue\n; }" `shouldBe`
        Program NotStrict [WhileStatement s (Boolean False) $
          Block s [ ContinueStatement s Nothing, EmptyStatement s] ]


    it "disallows a bare continue statement" $ do
      unparseable "continue"

    it "parses empty statements" $ do
      testParse ";\n;\n" `shouldBe`
        Program NotStrict [ EmptyStatement s, EmptyStatement s ]

    it "parses a for..in statement" $ do
      testParse "for (x in xs) {} " `shouldBe`
        Program NotStrict [For s (ForIn (ReadVar "x") (ReadVar "xs")) $ Block s []]

    it "parses a for..var..in statement" $ do
      testParse "for (var x in xs) {}" `shouldBe`
        Program NotStrict [For s (ForInVar ("x", Nothing) (ReadVar "xs")) $ Block s []]

    it "parses a labelled statement" $ do
      testParse "xyz: f()" `shouldBe`
        Program NotStrict [ LabelledStatement s "xyz" $
                   ExprStmt s $ FunCall (ReadVar "f")[] ]

    it "parses a new object" $ do
      testParse "new X()" `shouldBe`
        Program NotStrict [ ExprStmt s $ NewExpr (ReadVar "X") [] ]

    it "parses a new object without empty parens" $ do
      testParse "new X" `shouldBe` testParse "new X()"

    it "doesn't get confused by variables starting with 'new'" $ do
      testParse "newx" `shouldBe` Program NotStrict [ ExprStmt s $ ReadVar "newx" ]

  describe "Automatic semicolon insertion" $ do
    it "requires a semicolon on the same line" $ do
      unparseable "{ 1 2 } 3"

    it "does not require a semicolon with a line break" $ do
      testParse "{ 1\n2 } 3" `shouldBe` testParse "{ 1; 2 } 3"

    it "ignores white space before and after semicolons" $ do
      testParse "1 ; 2" `shouldBe` testParse "1;2"
      testParse "1; 2" `shouldBe` testParse "1;2"
      testParse "1 ;2" `shouldBe` testParse "1;2"

    it "parses a semicolon-terminated statement in a function" $ do
      testParse "function b() { return 3 }" `shouldBe` testParse "function b() { return 3; }"

    it "is OK with a semicolon in an if" $ do
      testParse "if (1) { x = 2; }" `shouldBe` Program NotStrict [IfStatement s (Num 1) (ExprStmt s (Assign (ReadVar "x") "=" (Num 2))) Nothing]

    it "treats semicolons as optional" $ do
      testParse "a()\nb()\n" `shouldBe` testParse "a(); b();"

    it "treats a return outside a function as a syntax error" $ do
      unparseable "return"

    it "parses a return statement with a value" $ do
      testParse "function f() { return 4 }" `shouldBe`
        Program NotStrict [ FunDecl s "f" [] NotStrict $ [ Return s $ Just $ Num 4 ] ]

    it "does not let a return statement break onto a newline" $ do
      testParse "function f() { return\n5\n}" `shouldBe`
        Program NotStrict [ FunDecl s "f" [] NotStrict $ [ Return s Nothing, ExprStmt s (Num 5) ] ]

    it "splits a statement on ++ if on a new line" $ do
      testParse "a=b\n++c" `shouldBe`
        Program NotStrict [ ExprStmt s (Assign (ReadVar "a") "=" (ReadVar "b")),
                  ExprStmt s (UnOp "++" (ReadVar "c")) ]

  describe "Unicode whitespace" $ do
    describe "Newline characters" $ do
      let expectedParse =
            Program NotStrict [ ExprStmt s (Num 1),
                                ExprStmt s (Num 2) ]

      it "treats \\n as a line break" $
        testParse "1\n2" `shouldBe` expectedParse

      it "treats \\r as a line break" $
        testParse "1\r2" `shouldBe` expectedParse

      it "treats unicode line separator as a line break" $
        testParse "1\x2028\&2" `shouldBe` expectedParse

      it "treats unicode paragraph separator as a line break" $
        testParse "1\x2029\&2" `shouldBe` expectedParse

      it "treats \\r\\n as a single line break" $
        testParse "1\r\n2" `shouldBe` expectedParse

    it "understands other kinds of whitespace" $ do
      testParse "1\x0009\x000B\x000C\x0020\x00A0\x000A\x000D\x2028\x2029+\x0009\x000B\x000C\x0020\x00A0\x000A\x000D\x2028\x2029\&1" `shouldBe` testParse "1 + 1"
      testParse "Number\t.\tPI" `shouldBe` testParse "Number.PI"
      testParse "Number[\t'PI'\t]" `shouldBe` testParse "Number['PI']"

  describe "Strict mode" $ do
    it "disallows modifications of eval and arguments" $ do
      unparseableInStrictMode "++eval"
      unparseableInStrictMode "--eval"
      unparseableInStrictMode "++arguments"
      unparseableInStrictMode "--arguments"
      unparseableInStrictMode "eval++"
      unparseableInStrictMode "eval--"
      unparseableInStrictMode "arguments++"
      unparseableInStrictMode "arguments--"

    it "disallows duplicate keys in object literals" $ do
      unparseableInStrictMode "var a = { x: 1, x: 1 }"
      unparseableInStrictMode "var a = { x: 1, x: 2 }"

    it "disallows eval and arguments as setter args" $ do
      unparseableInStrictMode "var obj = { set x(eval) {}};"
      unparseableInStrictMode "var obj = { set x(arguments) {}};"

    it "disallows eval and arguments as LHS of assignments" $ do
      unparseableInStrictMode "eval = 42"
      unparseableInStrictMode "arguments = 42"
      unparseableInStrictMode "x = eval = 42"
      unparseableInStrictMode "x = arguments = 42"

  describe "with" $ do
    it "parses a with statement" $ do
      testParse "with(obj) { }" `shouldBe`
        Program NotStrict [
          WithStatement s (ReadVar "obj") (Block s []) ]

  describe "An ExpressionStatement" $ do
    it "cannot begin with a curly bracket" $
      testParse "{}" `shouldBe`
        Program NotStrict [ Block s [ ] ]

    it "cannot begin with the word 'function'" $
      testParse "function f() { }" `shouldBe`
        Program NotStrict [ FunDecl s "f" [] NotStrict [] ]

  describe "an identifier" $ do
    it "can have unicode escapes in it" $ do
      testParse "function \\u005f\\u005f\\u0066\\u0075\\u006e\\u0063(){ }"
        `shouldBe` testParse "function __func() { }"
