module ParseSpec where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import GenExpr
import Expr
import Parse
import Eval

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

    it "parses array literals" $ do
      parseExpr "{}" `shouldBe` ObjectLiteral []
      parseExpr "{a: 1}" `shouldBe` ObjectLiteral [(IdentProp "a", Num 1)]

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
      parseExpr "f()()" `shouldBe` FunCall (FunCall (ReadVar "f") []) []

    it "parses a chained member access with a function call" $ do
      parseExpr "a.b.c()" `shouldBe` FunCall (MemberDot (MemberDot (ReadVar "a") "b") "c") []

    it "parses an 'in' expression" $ do
      parseExpr "x in xs" `shouldBe` BinOp "in" (ReadVar "x") (ReadVar "xs")

    it "parses a double-bang" $ do
      parseExpr "!!x" `shouldBe` UnOp "!" (UnOp "!" (ReadVar "x"))

    -- it "is the inverse of showExpr" $
    --   property prop_showExpr

  describe "simpleParse" $ do
    it "parses a while statement" $ do
      simpleParse "while (a) { }" `shouldBe` Program [WhileStatement (ReadVar "a") (Block [])]

    it "parses a do-while statement" $ do
      simpleParse "do {} while (a);" `shouldBe` Program [DoWhileStatement (ReadVar "a") (Block [])]


    it "parses a var declaration" $ do
      simpleParse "var a;" `shouldBe` Program [VarDecl [("a", Nothing)]]
      simpleParse "var a, b;" `shouldBe` Program [VarDecl [("a", Nothing),
                                                           ("b", Nothing)]]
      simpleParse "var a = 1, b;" `shouldBe` Program [VarDecl [("a", Just $ Num 1),
                                                               ("b", Nothing)]]
      simpleParse "var a = 1, b = a;" `shouldBe` Program [VarDecl [("a", Just $ Num 1),
                                                                   ("b", (Just $ ReadVar "a"))]]

    it "ignores comments at the start of the file" $ do
      simpleParse "" `shouldBe` Program []
      simpleParse "// this is a comment\n" `shouldBe` Program []
      simpleParse "// a\n//b\n2\n" `shouldBe` Program [ExprStmt (Num 2)]

    it "ignores white space before and after semicolons" $ do
      simpleParse "1 ; 2" `shouldBe` simpleParse "1;2"
      simpleParse "1; 2" `shouldBe` simpleParse "1;2"
      simpleParse "1 ;2" `shouldBe` simpleParse "1;2"

    it "parses a semicolon-terminated statement in a function" $ do
      simpleParse "function b() { return 3 }" `shouldBe` simpleParse "function b() { return 3; }"

    it "is OK with a semicolon in an if" $ do
      simpleParse "if (1) { return; }" `shouldBe` Program [IfStatement (Num 1) (Return Nothing) Nothing]

    it "treats semicolons as optional" $ do
      simpleParse "a()\nb()\n" `shouldBe` simpleParse "a(); b();"

    it "parses a return statement with a value" $ do
      simpleParse "return 4" `shouldBe`
        Program [ Return $ Just $ Num 4 ]

    it "does not let a return statement break onto a newline" $ do
      simpleParse "return\n5\n" `shouldBe` Program [Return Nothing, ExprStmt (Num 5)]

    it "resolves the if-then-else ambiguity" $ do
      simpleParse "if (a) if (b) continue; else break" `shouldBe`
        Program [IfStatement (ReadVar "a")
                             (IfStatement (ReadVar "b")
                                          ContinueStatement
                                          (Just BreakStatement))
                             Nothing]

    it "parses empty statements" $ do
      simpleParse ";\n;\n" `shouldBe`
        Program [ EmptyStatement, EmptyStatement ]

    it "parses a for..in statement" $ do
      simpleParse "for (x in xs) return" `shouldBe`
        Program [For (ForIn (ReadVar "x") (ReadVar "xs")) $ Return Nothing]

    it "parses a new object" $ do
      simpleParse "new X()" `shouldBe`
        Program [ ExprStmt $ NewExpr (ReadVar "X") [] ]

    it "doesn't get confused by variables starting with 'new'" $ do
      simpleParse "newx" `shouldBe` Program [ ExprStmt $ ReadVar "newx" ]

    -- it "is the inverse of showProg" $
    --   property prop_showProg
