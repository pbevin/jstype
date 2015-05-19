module ShowExpr (Code, code, ppcode, showProg, showExpr, showHeader) where

import Text.PrettyPrint hiding (Str)
import Data.List
import Expr
import Debug.Trace


class Code a where
  code :: a -> String
  ppdoc :: Int -> a -> Doc
  ppdoc _ = text . code
  ppcode :: a -> String
  ppcode = render . ppdoc 0

instance Code Program where
  code = showProg
  ppdoc = ppProg

instance Code Statement where
  code = showStatement
  ppdoc = ppStatement

instance Code Expr where
  code = showExpr
  ppdoc = ppExpr

instance Code ForHeader where
  code = showHeader
  ppdoc = ppHeader



showProg :: Program -> String
showProg (Program stmts) = intercalate ";" $ map code stmts

ppProg n (Program stmts) = vcat (map (ppdoc n) stmts)

showStatement :: Statement -> String
showStatement stmt = case stmt of
  WhileStatement _ expr stmt -> "while" ++ pparens (showExpr expr) ++ showStatement stmt
  Return _ Nothing -> "return"
  Return _ (Just expr) -> "return " ++ showExpr expr
  EmptyStatement _ -> ";"
  DebuggerStatement _ -> "debugger"
  BreakStatement _ _ -> "break"
  ContinueStatement _ _ -> "continue"
  VarDecl _ decls -> "var " ++ showVarDecls decls
  ThrowStatement _ expr -> "throw " ++ showExpr expr
  TryStatement _ block catch finally ->
    "try " ++ showStatement block
           ++ maybe "" showCatch catch
           ++ maybe "" showFinally finally

  ExprStmt _ expr -> parenObjectLiterals(showExpr expr)
    where parenObjectLiterals str =
            if head str == '{'
            then pparens str
            else str

  Block _ statements ->
    bbraces $ intercalate "; " $ map showStatement statements

  IfStatement _ test ifTrue ifFalse ->
    "if " ++ pparens(showExpr test) ++ " " ++ bbraces (showStatement ifTrue) ++
      case ifFalse of
        Nothing -> ""
        Just stmt -> " else " ++ bbraces (showStatement stmt)

  For _ header stmt ->
    "for " ++ showHeader header ++ showStatement stmt

ppStatement n stmt = case stmt of
  For _ header stmt ->
    vcat [ text "for" <+> ppdoc 0 header,
           nest 2 $ ppdoc n stmt ]

  IfStatement _ test ifTrue ifFalse ->
    case ifFalse of
      Just ifFalse' ->
        vcat [ ifPart, block ifTrue, text "} else {", block ifFalse', text "}" ]
      Nothing ->
        vcat [ ifPart, block ifTrue, text "}" ]

      where
        ifPart = text "if" <+> parens (ppdoc 0 test) <+> text "{"
        block stmt = nest 2 $ ppdoc (n+1) stmt

  _ -> text (code stmt)

showHeader header = pparens $ case header of
  For3 a b c ->
    intercalate ";" [ maybe "" showExpr a,
                      maybe "" showExpr b,
                      maybe "" showExpr c ]

  ForIn a b -> showExpr a ++ " in " ++ showExpr b
  _ -> "..."

ppHeader n header = case header of
  ForIn a b -> parens $ ppdoc 0 a <+> text "in" <+> ppdoc 1 b

  _ -> text (code header)


showCatch (Catch _ ident block) = " catch (" ++ ident ++ ") " ++ showStatement block
showFinally (Finally _ block) = " finally " ++ showStatement block


showExpr :: Expr -> String
showExpr expr = case expr of
  Str s -> show s
  ReadVar v -> v
  This -> "this"

  Num (JSNum n)
    | isInteger n -> show (round n)
    | otherwise   -> show n

  NewExpr cls args ->
    "new " ++ maybeParens cls ++ argList args

  ArrayLiteral exprs ->
    bbrackets (intercalate "," $ map (maybe "" showExpr) exprs)

  ObjectLiteral assignments ->
    bbraces (intercalate "," $ map showAssignment assignments)

  MemberDot e id ->
    maybeParens e ++ "." ++ id

  MemberGet a x ->
    maybeParens a ++ bbrackets (showExpr x)

  BinOp op e1 e2 ->
    maybeParens e1 ++ " " ++ op ++ " " ++ maybeParens e2

  UnOp op e -> case op of
    "delete" -> op ++ pparens (showExpr e)
    "void"   -> op ++ pparens (showExpr e)
    "typeof" -> op ++ pparens (showExpr e)
    _        -> op ++ maybeParens e

  PostOp op e ->
    maybeParens e ++ op

  Assign lhs op rhs ->
    showExpr lhs ++ op ++ showExpr rhs

  Cond test ifTrue ifFalse ->
    maybeParens test ++ " ? " ++ showExpr ifTrue ++ " : " ++ showExpr ifFalse

  FunCall fun args -> maybeParens fun ++ argList args

  FunDef Nothing params body ->
    "function" ++ pparens (intercalate "," params) ++ bbraces (mapshow ";" body)

  FunDef (Just name) params body ->
    "function " ++ name ++ pparens (intercalate "," params) ++ bbraces (mapshow ";" body)

  RegularExpression{} -> error "regex"

ppExpr n expr = case expr of
  Assign lhs op rhs ->
    parensIf (n>0) (ppdoc (n+1) lhs <+> text op <+> ppdoc (n+1) rhs)

  FunDef name params body ->
    ppheader <+> lbrace $$ ppbody $$ rbrace
      where ppheader = text "function"
                   <+> maybe empty text name
                    <> parens (commaList $ map text params)
            ppbody = nest 2 $ vcat $ map (ppdoc $ n+1) body

    --text "function" <+> maybe empty text name <> parens (commaList $ map text params) <+> lbrace $$ (nest 2 $ vcat $ map (ppdoc $ n+1) body) $$ rbrace


  NewExpr cls args ->
    text "new" <+> ppdoc (n+1) cls <> parens (commaList $ map (ppdoc $ n+1) args)

  _ -> text (code expr)

commaList :: [Doc] -> Doc
commaList [] = empty
commaList params = foldl1 (\d e -> d <> comma $$ e) params

parensIf :: Bool -> Doc -> Doc
parensIf False = id
parensIf True = parens

showVarDecls :: [(String, Maybe Expr)] -> String
showVarDecls = intercalate "," . map showDecl
  where showDecl (var, expr) = var ++ maybe "" (\e -> " = " ++ showExpr e) expr

showAssignment :: (PropertyName, Expr) -> String
showAssignment (n, v) = showPropertyName n ++ ": " ++ showExpr v

showPropertyName (IdentProp p) = p
showPropertyName (StringProp p) = show p
showPropertyName (NumProp (JSNum n)) = show n

argList :: [Expr] -> String
argList args = pparens (intercalate "," $ map showExpr args)


mapshow :: String -> [Statement] -> String
mapshow sep xs = intercalate sep $ map showStatement xs

pparens s = "(" ++ s ++ ")"
bbraces s = "{" ++ s ++ "}"
bbrackets s = "[" ++ s ++ "]"

isInteger :: RealFrac a => a -> Bool
isInteger x = x == fromIntegral (round x :: Integer)

maybeParens :: Expr -> String
maybeParens e = case e of
  Num _ -> showExpr e
  ReadVar _ -> showExpr e
  This -> showExpr e
  _ -> pparens (showExpr e)
