module ParseSpec where

import Control.Exception (evaluate)
import Control.Arrow (second)
import Data.Either
import Test.Hspec

import Expr
import Parse
import Eval

s :: SrcLoc
s = SrcLoc "" 0 0 Nothing []

testParseStrict :: String -> Program
testParseStrict input = eraseSrcLoc prog
  where prog = case parseJS'' input "" Strict False of
                Right p -> p
                Left err -> error (show err)


testParse :: String -> Program
testParse input = eraseSrcLoc (simpleParse input)

eraseSrcLoc :: Program -> Program
eraseSrcLoc (Program strictness stmts) =
  let srcLoc = SrcLoc "" 0 0
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
        SwitchStatement _ a b  -> SwitchStatement s a $ fixCase b
        ThrowStatement _ a     -> ThrowStatement s a
        TryStatement _ a b c   -> TryStatement s a b c
        Catch _ a b            -> Catch s a (overrideSrcLoc b)
        Finally _ a            -> Finally s (overrideSrcLoc a)
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
      fixCase (a,b,c) = (fmap fixCaseClause a,
                         fmap fixDefaultClause b,
                         fmap fixCaseClause c)
      fixCaseClause (CaseClause a b) = CaseClause a (overrideAll b)
      fixDefaultClause (DefaultClause b) = DefaultClause (overrideAll b)
  in Program strictness $ overrideAll stmts

unparseable :: String -> IO ()
unparseable str = parseJS str `shouldSatisfy` isLeft

unparseableInStrictMode :: String -> IO ()
unparseableInStrictMode str = do
  case parseJS'' str "" Strict False of
    Left _ -> return ()
    Right prog -> expectationFailure $ "Parseable in strict mode: " ++ show str ++ "\nParsed as: " ++ show prog
  case parseJS'' str "" NotStrict False of
    Left err -> expectationFailure $ "Unparseable in non-strict mode: " ++ show str ++ "\nError: " ++ show err
    Right _ -> return ()

spec :: Spec
spec = do
  describe "ParseExpr" $ do
    it "parses a number" $ do
      parseExpr "1" `shouldBe` INum 1
      parseExpr "1.3" `shouldBe` Num 1.3
      parseExpr "1.3e3" `shouldBe` Num 1300
      parseExpr "1.4E3" `shouldBe` Num 1400
      parseExpr "1.5e+3" `shouldBe` Num 1500
      parseExpr "1.6e-3" `shouldBe` Num 0.0016
      parseExpr "3e3" `shouldBe` Num 3000
      parseExpr ".5" `shouldBe` Num 0.5
      parseExpr "5." `shouldBe` Num 5
      parseExpr "5.e1" `shouldBe` Num 50

    it "distinguishes between num and float" $ do
      parseExpr "1" `shouldBe` INum 1
      parseExpr "1.0" `shouldBe` Num 1

    it "doesn't mistake other things for numbers" $ do
      parseExpr "e1" `shouldBe` ReadVar "e1"

    it "parses a negative number" $ do
      parseExpr "-1" `shouldBe` UnOp "-" (INum 1)

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
      parseExpr "'\\117\\143\\164\\141\\154'" `shouldBe` Str "Octal"

    it "parses \\0" $ do
      parseExpr "'a\\0b'" `shouldBe` Str [ 'a', '\0', 'b' ]

    it "parses \\0 only if the following char is not a number" $ do
      unparseable "'a\\00b'"
      unparseable "'a\\01b'"
      unparseable "'a\\02b'"
      unparseable "'a\\03b'"
      unparseable "'a\\04b'"
      unparseable "'a\\05b'"
      unparseable "'a\\06b'"
      unparseable "'a\\07b'"
      unparseable "'a\\08b'"
      unparseable "'a\\09b'"

    it "allows line breaks in strings when preceded by backslash" $ do
      parseExpr "\"abc\\\ndef\"" `shouldBe` Str "abcdef"
      parseExpr "'abc\\\ndef'" `shouldBe` Str "abcdef"

    it "disallows actual line breaks in strings" $ do
      unparseable "'abc\ndef'"
      unparseable "'abc\rdef'"
      unparseable "'abc\x2028\&def'"
      unparseable "'abc\x2029\&def'"
      unparseable "\"abc\ndef\""
      unparseable "\"abc\rdef\""
      unparseable "\"abc\x2028\&def\""
      unparseable "\"abc\x2029\&def\""

    describe "Object literals" $ do
      it "parses object literals" $ do
        parseExpr "{}" `shouldBe` ObjectLiteral []
        parseExpr "{a: 1}" `shouldBe` ObjectLiteral [("a", Value $ INum 1)]
        parseExpr "{a: 1, b: 2}" `shouldBe`
          ObjectLiteral [("a", Value $ INum 1),
                         ("b", Value $ INum 2)]
        parseExpr "{a: 1, 2: 3}" `shouldBe`
          ObjectLiteral [("a", Value $ INum 1),
                         ("2", Value $ INum 3)]

      it "parses an object literal with a trailing comma" $ do
        parseExpr "{a: 1, }" `shouldBe` parseExpr "{a: 1}"

      it "parses an object literal with a getter" $ do
        let obj = ObjectLiteral [ ("x", Getter [ Return s $ Just (INum 1) ]) ]
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

      it "allows reserved words as names" $ do
        parseExpr "{ var: 3 }" `shouldBe`
          ObjectLiteral [ ("var", Value $ INum 3) ]

    describe "Array literals" $ do
      it "parses an empty array literal" $ do
        parseExpr "[]" `shouldBe` ArrayLiteral []

      it "parses an array literal with one elision" $ do
        parseExpr "[,]" `shouldBe` ArrayLiteral [Nothing]

      it "parses an array literal with one element" $ do
        parseExpr "[1]" `shouldBe` ArrayLiteral [Just $ INum 1]

      it "parses an array literal with 2 elements" $ do
        parseExpr "[1,2]" `shouldBe` ArrayLiteral [Just $ INum 1, Just $ INum 2]

      it "parses an array literal with elision in the middle" $ do
        parseExpr "[1,,2]" `shouldBe` ArrayLiteral [Just $ INum 1, Nothing, Just $ INum 2 ]

      it "parses an array literal with elision at the end" $ do
        parseExpr "[1,2,]" `shouldBe`
          ArrayLiteral [Just $ INum 1, Just $ INum 2 ]

      it "parses an array literal with elision at the start" $ do
        parseExpr "[,1,2]" `shouldBe`
          ArrayLiteral [Nothing, Just $ INum 1, Just $ INum 2 ]

      it "parses an array literal with only elision" $ do
        parseExpr "[,,]" `shouldBe` ArrayLiteral [Nothing,Nothing]

    it "parses a unary operator" $ do
      parseExpr "++u" `shouldBe` UnOp "++" (ReadVar "u")

    it "parses a unop assignment" $ do
      parseExpr "e = -1" `shouldBe` Assign (ReadVar "e") "=" (UnOp "-" (INum 1))

    it "parses a chained assignment" $ do
      parseExpr "x = x = 1" `shouldBe` Assign (ReadVar "x") "=" (Assign (ReadVar "x") "=" (INum 1))

    it "parses a plus-equals" $ do
      parseExpr "a += b" `shouldBe` Assign (ReadVar "a") "+=" (ReadVar "b")

    it "parses lvars" $ do
      parseExpr "a.b = 1" `shouldBe` Assign (MemberDot (ReadVar "a") "b") "=" (INum 1)
      parseExpr "a[i] = 1" `shouldBe` Assign (MemberGet (ReadVar "a") (ReadVar "i")) "=" (INum 1)

    it "interprets this tricky case right" $ do
      parseExpr "a++ + ++b" `shouldBe` BinOp "+" (PostOp "++" (ReadVar "a")) (UnOp "++" (ReadVar "b"))
      parseExpr "a+++ ++b" `shouldBe` BinOp "+" (PostOp "++" (ReadVar "a")) (UnOp "++" (ReadVar "b"))
      evaluate (parseExpr "a+++++b") `shouldThrow` anyException
      evaluate (parseExpr "a++ +++b") `shouldThrow` anyException

    it "parses a unop assignment without spaces" $ do
      parseExpr "e=-1" `shouldBe` Assign (ReadVar "e") "=" (UnOp "-" (INum 1))

    it "parses a binop" $ do
      parseExpr "1+2" `shouldBe` BinOp "+" (INum 1) (INum 2)
      parseExpr "(1)&&(2)" `shouldBe` BinOp "&&" (INum 1) (INum 2)

    it "parses a chained binop" $ do
      parseExpr "a+b+c" `shouldBe` BinOp "+" (BinOp "+" (ReadVar "a") (ReadVar "b")) (ReadVar "c")

    it "parses a ." $ do
      parseExpr "a.b" `shouldBe` MemberDot (ReadVar "a") "b"
      parseExpr "-a.b" `shouldBe` UnOp "-" (MemberDot (ReadVar "a") "b")

    it "allows keywords after a dot" $ do
      parseExpr "a.in" `shouldBe` MemberDot (ReadVar "a") "in"


    it "parses a []" $ do
      parseExpr "a[\"b\"]" `shouldBe` MemberGet (ReadVar "a") (Str "b")

    it "parses a function call" $ do
      parseExpr "f()" `shouldBe` FunCall (ReadVar "f") []

    it "parses a function call with an argument" $ do
      parseExpr "f(x)" `shouldBe` FunCall (ReadVar "f") [ReadVar "x"]

    it "parses a function call with two arguments" $ do
      parseExpr "f(x,y)" `shouldBe` FunCall (ReadVar "f") [ReadVar "x", ReadVar "y"]

    it "parses a chained function call" $ do
      parseExpr "f(1)(2)" `shouldBe` FunCall (FunCall (ReadVar "f") [INum 1]) [INum 2]

    it "parses a chained member access with a function call" $ do
      parseExpr "a.b.c()" `shouldBe` FunCall (MemberDot (MemberDot (ReadVar "a") "b") "c") []

    it "parses an 'in' expression" $ do
      parseExpr "x in xs" `shouldBe` BinOp "in" (ReadVar "x") (ReadVar "xs")

    it "parses a double-bang" $ do
      parseExpr "!!x" `shouldBe` UnOp "!" (UnOp "!" (ReadVar "x"))

    describe "The comma operator" $ do
      it "separates expressions" $ do
        parseExpr "1,2" `shouldBe` BinOp "," (INum 1) (INum 2)

      it "separates assignments" $ do
        parseExpr "a = 1,2" `shouldBe` BinOp "," (Assign (ReadVar "a") "=" (INum 1)) (INum 2)

  describe "Parsing programs" $ do
    it "parses a strict-mode program" $ do
      testParse "'use strict';\nvar a;" `shouldBe`
        Program Strict [ExprStmt s (Str "use strict"), VarDecl s [("a", Nothing)]]

    it "recognizes use strict when preceded by another string" $ do
      testParse "'xyz'\n'use strict';\nvar a;" `shouldBe`
        Program Strict [ExprStmt s (Str "xyz"), ExprStmt s (Str "use strict"), VarDecl s [("a", Nothing)]]

    it "only counts complete ExprStmts in the directive prologue" $ do
      testParse "'use strict' === x" `shouldBe`
        Program NotStrict [ExprStmt s (BinOp "===" (Str "use strict") (ReadVar "x"))]


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
      testParse "var a = 1, b;" `shouldBe` Program NotStrict [VarDecl s [("a", Just $ INum 1),
                                                               ("b", Nothing)]]
      testParse "var a = 1, b = a;" `shouldBe` Program NotStrict [VarDecl s [("a", Just $ INum 1),
                                                                   ("b", (Just $ ReadVar "a"))]]

    it "ignores comments at the start of the file" $ do
      testParse "" `shouldBe` Program NotStrict []
      testParse "// this is a comment\n" `shouldBe` Program NotStrict []
      testParse "// a\n//b\n2\n" `shouldBe` Program NotStrict [ExprStmt s (INum 2)]
      testParse "//s" `shouldBe` Program NotStrict []

    it "resolves the if-then-else ambiguity" $ do
      testParse "if (a) if (b) 1; else 2" `shouldBe`
        Program NotStrict [IfStatement s (ReadVar "a")
                               (IfStatement s (ReadVar "b")
                                              (ExprStmt s $ INum 1)
                                              (Just $ ExprStmt s $ INum 2))
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

    it "keeps track of current labels" $ do
      simpleParse "xyz: while (1) { abc: while (1) { } }" `shouldBe`
        Program NotStrict [
          LabelledStatement (SrcLoc "" 1 1 Nothing []) "xyz" $
            WhileStatement (SrcLoc "" 1 12 Nothing ["xyz"]) (INum 1) $
              LabelledStatement (SrcLoc "" 1 18 Nothing []) "abc" $
                WhileStatement (SrcLoc "" 1 29 Nothing ["abc"]) (INum 1) $
                  Block (SrcLoc "" 1 33 Nothing []) [] ]

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
      testParse "if (1) { x = 2; }" `shouldBe` Program NotStrict [IfStatement s (INum 1) (ExprStmt s (Assign (ReadVar "x") "=" (INum 2))) Nothing]

    it "treats semicolons as optional" $ do
      testParse "a()\nb()\n" `shouldBe` testParse "a(); b();"

    it "treats a return outside a function as a syntax error" $ do
      unparseable "return"

    it "parses a return statement with a value" $ do
      testParse "function f() { return 4 }" `shouldBe`
        Program NotStrict [ FunDecl s "f" [] NotStrict $ [ Return s $ Just $ INum 4 ] ]

    it "does not let a return statement break onto a newline" $ do
      testParse "function f() { return\n5\n}" `shouldBe`
        Program NotStrict [ FunDecl s "f" [] NotStrict $ [ Return s Nothing, ExprStmt s (INum 5) ] ]

    it "splits a statement on ++ if on a new line" $ do
      testParse "a=b\n++c" `shouldBe`
        Program NotStrict [ ExprStmt s (Assign (ReadVar "a") "=" (ReadVar "b")),
                  ExprStmt s (UnOp "++" (ReadVar "c")) ]

  describe "Unicode whitespace" $ do
    describe "Newline characters" $ do
      let expectedParse =
            Program NotStrict [ ExprStmt s (INum 1),
                                ExprStmt s (INum 2) ]

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

    describe "Line number counting" $ do
      it "treats \\n as a newline" $ do
        simpleParse "\n\n1" `shouldBe`
          Program NotStrict [ ExprStmt (SrcLoc "" 3 1 Nothing []) (INum 1) ]

      it "treats \\n\\r as a newline" $ do
        simpleParse "\r\n\r\n1" `shouldBe`
          Program NotStrict [ ExprStmt (SrcLoc "" 3 1 Nothing []) (INum 1) ]

      it "treats \\r as a newline" $ do
        simpleParse "\r\r1" `shouldBe`
          Program NotStrict [ ExprStmt (SrcLoc "" 3 1 Nothing []) (INum 1) ]

    it "understands other kinds of whitespace" $ do
      testParse "1\x0009\x000B\x000C\x0020\x00A0\x000A\x000D\x2028\x2029+\x0009\x000B\x000C\x0020\x00A0\x000A\x000D\x2028\x2029\&1" `shouldBe` testParse "1 + 1"
      testParse "Number\t.\tPI" `shouldBe` testParse "Number.PI"
      testParse "Number[\t'PI'\t]" `shouldBe` testParse "Number['PI']"

  describe "Strict mode" $ do
    it "disallows the with statement" $ do
      unparseableInStrictMode "with(obj) { }"

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

    it "disallows function params called eval and arguments" $ do
      unparseableInStrictMode "function f(eval) { }"
      unparseableInStrictMode "function g(arguments) { }"
      unparseableInStrictMode "var f = function(eval) {}"
      unparseableInStrictMode "var g = function(arguments) {}"

    it "disallows repeated parameter names" $ do
      unparseableInStrictMode "function f(a, a) { }"
      unparseableInStrictMode "var f = function(a, a) { }"

    it "disallows functions named eval or arguments" $ do
      unparseableInStrictMode "function eval() { }"
      unparseableInStrictMode "function arguments() { }"
      unparseableInStrictMode "var f = function eval() { }"
      unparseableInStrictMode "var g = function arguments() { }"

    it "disallows eval+argument as catch block param names" $ do
      unparseableInStrictMode "try {} catch(eval) {}"
      unparseableInStrictMode "try {} catch(arguments) {}"

    it "disallows var eval and var arguments" $ do
      unparseableInStrictMode "var eval = 42;"
      unparseableInStrictMode "var arguments = 42;"

    it "allows passing arguments to another function in strict mode" $ do
      testParseStrict "f(arguments)" `shouldBe`
        Program Strict [
          ExprStmt s (FunCall (ReadVar "f") [ReadVar "arguments"]) ]

  describe "with" $ do
    it "parses a with statement" $ do
      testParse "with(obj) { }" `shouldBe`
        Program NotStrict [
          WithStatement s (ReadVar "obj") (Block s []) ]

  describe "An ExprStmt" $ do
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

      testParse "var \\u0078 = 1" `shouldBe` testParse "var x = 1"
      testParse "var \\u0418 = 1" `shouldBe`
        Program NotStrict [ VarDecl s [("И", Just $ INum 1)] ]

  describe "Punctuation" $ do
    it "cannot be made of unicode" $ do
      unparseable "\\u007B\\u007D"

  describe "the switch statement" $ do
    it "can have cases" $ do
      testParse "switch (e) { case 1: a = 2; break; }" `shouldBe`
        Program NotStrict [
          SwitchStatement s (ReadVar "e") $
            ([ CaseClause (INum 1) [ ExprStmt s (Assign (ReadVar "a") "=" (INum 2)), BreakStatement s Nothing ] ], Nothing, [] ) ]

  describe "number parsing" $ do
    it "can parse a decimal int" $ do
      parseExpr "123" `shouldBe` INum 123

    it "can parse a decimal with a fraction" $ do
      parseExpr "123.456" `shouldBe` Num 123.456
      parseExpr "0.00" `shouldBe` Num 0

    it "can parse a decimal with only a fraction" $ do
      parseExpr ".000" `shouldBe` Num 0
      parseExpr ".001" `shouldBe` Num 0.001

    it "can parse a hex number" $ do
      map parseExpr [ "0x2e", "0x2E", "0X2e", "0X2E" ]
        `shouldBe`  [ INum 46, INum 46, INum 46, INum 46 ]

    it "does not parse a hex number without any digits" $ do
      unparseable "0x"
      unparseable "0X"

    it "can parse an octal number" $ do
      parseExpr "010" `shouldBe` INum 8

    it "refuses to parse a number in strict mode" $ do
      unparseableInStrictMode "010"

  describe "String parsing" $ do
    it "splits chars outside the BMP (> 0xFFFF) into two UTF-16 characters" $ do
      parseExpr "'\x104A0'" `shouldBe` Str "\xD801\xDCA0"

    it "parses a hex code" $ do
      parseExpr "'\\x00'" `shouldBe` Str "\0"
      parseExpr "'\\xFF'" `shouldBe` Str "\xff"
      parseExpr "'\\xff'" `shouldBe` Str "\xff"

  describe "Regular expression parsing" $ do
    it "parses regular expression literals" $ do
      parseExpr "/[a]/i" `shouldBe` RegExp "[a]" "i"

    it "does not allow a newline character in the first position" $ do
      unparseable "/\n/i"
      unparseable "/\r/i"
      unparseable "/\x2028/i"
      unparseable "/\x2029/i"

    it "does not allow an escaped newline character in first position" $ do
      unparseable "/\\\n/i"
      unparseable "/\\\r/i"
      unparseable "/\\\x2028/i"
      unparseable "/\\\x2029/i"

    it "does not allow a newline character" $ do
      unparseable "/a\n/i"
      unparseable "/a\r/i"
      unparseable "/a\x2028/i"
      unparseable "/a\x2029/i"
