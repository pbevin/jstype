module ParseSpec where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import GenExpr
import Expr
import Parse
import Eval


testParse :: String -> Program
testParse input = let Program stmts = simpleParse input
                      srcLoc = SrcLoc "" 0 0
                      overrideAll = map overrideSrcLoc
                      overrideSrcLoc stmt = case stmt of
                        Block _ stmts          ->  Block s $ overrideAll stmts
                        VarDecl _ a            ->  VarDecl s a
                        ExprStmt _ a           ->  ExprStmt s a
                        IfStatement _ a b c    ->  IfStatement s a (overrideSrcLoc b) (fmap overrideSrcLoc c)
                        WhileStatement _ a b   ->  WhileStatement s a $ overrideSrcLoc b
                        DoWhileStatement _ a b ->  DoWhileStatement s a $ overrideSrcLoc b
                        For _ a b              ->  For s a $ overrideSrcLoc b
                        ContinueStatement _    ->  ContinueStatement s
                        BreakStatement _       ->  BreakStatement s
                        Return _ a             ->  Return s a
                        ThrowStatement _ a     ->  ThrowStatement s a
                        TryStatement _ a b c   ->  TryStatement s a b c
                        EmptyStatement _       ->  EmptyStatement s
                        DebuggerStatement _    ->  DebuggerStatement s
                  in Program $ overrideAll stmts


spec = do
  describe "ParseExpr" $ do
    it "parses a number" $ do
      parseExpr "1" `shouldBe` Num (JSNum 1)

    it "parses a negative number" $ do
      parseExpr "-1" `shouldBe` UnOp "-" (Num (JSNum 1))

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

    it "parses object literals" $ do
      parseExpr "{}" `shouldBe` ObjectLiteral []
      parseExpr "{a: 1}" `shouldBe` ObjectLiteral [(IdentProp "a", Num 1)]
      parseExpr "{a: 1, b: 2}" `shouldBe` ObjectLiteral [(IdentProp "a", Num 1), (IdentProp "b", Num 2)]

    it "parses regular expression literals" $ do
      parseExpr "/[a]/i" `shouldBe` RegularExpression "[a]" "i"

    it "parses a unary operator" $ do
      parseExpr "++u" `shouldBe` UnOp "++" (ReadVar "u")

    it "parses a unop assignment" $ do
      parseExpr "e = -1" `shouldBe` Assign (ReadVar "e") "=" (UnOp "-" (Num (JSNum 1)))

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

    -- it "is the inverse of showExpr" $
    --   property prop_showExpr

  describe "Parsing programs" $ do
    it "parses a while statement" $ do
      testParse "while (a) { }" `shouldBe` Program [WhileStatement s (ReadVar "a") (Block s [])]

    it "parses a do-while statement" $ do
      testParse "do {} while (a);" `shouldBe` Program [DoWhileStatement s (ReadVar "a") (Block s [])]


    it "parses a var declaration" $ do
      testParse "var a;" `shouldBe` Program [VarDecl s [("a", Nothing)]]
      testParse "var a, b;" `shouldBe` Program [VarDecl s [("a", Nothing),
                                                           ("b", Nothing)]]
      testParse "var a = 1, b;" `shouldBe` Program [VarDecl s [("a", Just $ Num 1),
                                                               ("b", Nothing)]]
      testParse "var a = 1, b = a;" `shouldBe` Program [VarDecl s [("a", Just $ Num 1),
                                                                   ("b", (Just $ ReadVar "a"))]]

    it "ignores comments at the start of the file" $ do
      testParse "" `shouldBe` Program []
      testParse "// this is a comment\n" `shouldBe` Program []
      testParse "// a\n//b\n2\n" `shouldBe` Program [ExprStmt s (Num 2)]

    it "ignores white space before and after semicolons" $ do
      testParse "1 ; 2" `shouldBe` testParse "1;2"
      testParse "1; 2" `shouldBe` testParse "1;2"
      testParse "1 ;2" `shouldBe` testParse "1;2"

    it "parses a semicolon-terminated statement in a function" $ do
      testParse "function b() { return 3 }" `shouldBe` testParse "function b() { return 3; }"

    it "is OK with a semicolon in an if" $ do
      testParse "if (1) { return; }" `shouldBe` Program [IfStatement s (Num 1) (Return s Nothing) Nothing]

    it "treats semicolons as optional" $ do
      testParse "a()\nb()\n" `shouldBe` testParse "a(); b();"

    it "parses a return statement with a value" $ do
      testParse "return 4" `shouldBe`
        Program [ Return s $ Just $ Num 4 ]

    it "does not let a return statement break onto a newline" $ do
      testParse "return\n5\n" `shouldBe` Program [Return s Nothing, ExprStmt s (Num 5)]

    it "resolves the if-then-else ambiguity" $ do
      testParse "if (a) if (b) continue; else break" `shouldBe`
        Program [IfStatement s (ReadVar "a")
                               (IfStatement s (ReadVar "b")
                                            (ContinueStatement s)
                                            (Just $ BreakStatement s))
                               Nothing]

    it "parses empty statements" $ do
      testParse ";\n;\n" `shouldBe`
        Program [ EmptyStatement s, EmptyStatement s ]

    it "parses a for..in statement" $ do
      testParse "for (x in xs) return" `shouldBe`
        Program [For s (ForIn (ReadVar "x") (ReadVar "xs")) $ Return s Nothing]

    it "parses a for..var..in statement" $ do
      testParse "for (var x in xs) return" `shouldBe`
        Program [For s (ForInVar ("x", Nothing) (ReadVar "xs")) $ Return s Nothing]

    it "parses a new object" $ do
      testParse "new X()" `shouldBe`
        Program [ ExprStmt s $ NewExpr (ReadVar "X") [] ]

    it "doesn't get confused by variables starting with 'new'" $ do
      testParse "newx" `shouldBe` Program [ ExprStmt s $ ReadVar "newx" ]

    it "fails on ASI" $ do
      evaluate (testParse "{ 1 2 } 3") `shouldThrow` anyException

    -- it "is the inverse of showProg" $
    --   property prop_showProg
