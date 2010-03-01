module Serialize (
    Serialize (..)
) where

import Text.PrettyPrint.HughesPJ

import Language.C.Pretty
import Language.C.Data
import Language.C.Syntax

class (Pretty s) => Serialize s where

    serialize :: s -> Doc
    serializePrec :: Int -> s -> Doc

    serialize = pretty
    serializePrec _ = serialize

szLine :: NodeInfo -> Doc
szLine ni = text "#" <+> int (posRow $ posOfNode ni) <+> (doubleQuotes $ text (posFile $ posOfNode ni))

-- serialize optional chunk
maybeP :: (p -> Doc) -> Maybe p -> Doc
maybeP = maybe empty

-- serialize when flag is true
ifP :: Bool -> Doc -> Doc
ifP flag doc = if flag then doc else empty

-- serialize _optional_ list, i.e. [] ~ Nothing and (x:xs) ~ Just (x:xs)
mlistP :: ([p] -> Doc) -> [p] -> Doc
mlistP pp xs = maybeP pp (if null xs then Nothing else Just xs)

-- serialize identifier
identP :: Ident -> Doc
identP = text . identToString

-- pretty print attribute annotations
attrlistP :: [CAttr] -> Doc
attrlistP [] = empty
attrlistP attrs = text "__attribute__" <> parens (parens (hcat . punctuate comma . map pretty $ attrs))

-- analogous to showParen
parenPrec :: Int -> Int -> Doc -> Doc
parenPrec prec prec2 t = if prec <= prec2 then t else parens t

-- indent a chunk of code
ii :: Doc -> Doc
ii = nest 4

-- Serialize instances
instance Serialize CTranslUnit where
    serialize c@(CTranslUnit edecls ni) = szLine ni $+$ vcat (map serialize edecls)

-- TODO: Check need of __extension__
instance Serialize CExtDecl where
    serialize (CDeclExt decl) = (szLine $ nodeInfo decl) $+$ serialize decl <> semi
    serialize (CFDefExt fund) = (szLine $ nodeInfo fund) $+$ serialize fund
    serialize (CAsmExt  asmStmt _) = text "asm" <> parens (pretty asmStmt) <> semi

instance Serialize CFunDef where
    serialize (CFunDef declspecs declr decls stat ni) =          -- Example:
	    szLine ni
        $+$  hsep (map pretty declspecs)                      -- __attribute__((noreturn)) static long
        <+> pretty declr                                     -- foo(b)
        $+$ (ii . vcat . map (<> semi) . map pretty) decls   --     register long b;
        $$ serializePrec (-1) stat                           -- {  ...
                                                             -- }

instance Serialize CStat where
    serialize (CLabel ident stat cattrs ni) = szLine ni $+$ identP ident <> text ":" <+> attrlistP cattrs $$ serialize stat
    serialize (CCase expr stat ni) =
        szLine ni $+$ text "case" <+> pretty expr <> text ":" $$ serialize stat
    serialize (CCases expr1 expr2 stat ni) =
        szLine ni $+$ text "case" <+> pretty expr1 <+> text "..."
                    <+> pretty expr2 <> text ":" $$ serialize stat
    serialize (CDefault stat ni) = szLine ni $+$ text "default:" $$ serialize stat
    serialize (CExpr expr ni) = szLine ni $+$ (ii $ maybeP pretty expr <> semi)
    serialize c@(CCompound _ _ _) = serializePrec 0 c
    serialize (CIf expr stat estat ni) =
	szLine ni $+$
        (ii $  text "if" <+> text "(" <> pretty expr <> text ")")
                $+$ serializePrec (-1) stat
              $$ maybeP szElse estat
      where
        szElse (CIf else_if_expr else_if_stat else_stat ni) =
	  szLine ni $+$ 
          text "else if" <+> text "(" <> pretty else_if_expr <> text ")"
            $+$ serializePrec (-1) else_if_stat
          $$ maybeP szElse else_stat
        szElse else_stmt =
          text "else" $+$ serializePrec (-1) else_stmt

    serialize (CSwitch expr stat ni) =
	szLine ni $+$ 
        (ii $ text "switch" <+> text "(" <> pretty expr <> text ")"
               $+$ serializePrec (-1) stat)
    serialize (CWhile expr stat False ni) =
	szLine ni $+$ 
        (ii $ text "while" <+> text "(" <> pretty expr <> text ")"
               $+$ serializePrec (-1) stat)
    serialize (CWhile expr stat True ni) =
	szLine ni $+$ 
        (ii $ text "do" $+$ serializePrec (-1) stat
               $$ text "while" <+> text "(" <> pretty expr <> text ");")
    serialize (CFor for_init cond step stat ni) =
	szLine ni $+$ 
        (ii $ text "for" <+> text "("
               <> either (maybeP pretty) pretty for_init <> semi
               <+> maybeP pretty cond <> semi
               <+> maybeP pretty step <> text ")" $+$ serializePrec (-1) stat)
    serialize (CGoto ident ni) = szLine ni $+$ (ii $ text "goto" <+> identP ident <> semi)
    serialize (CGotoPtr expr ni) = szLine ni $+$ (ii $ text "goto" <+> text "*" <+> prettyPrec 30 expr <> semi)
    serialize (CCont ni) = szLine ni $+$ (ii $ text "continue" <> semi)
    serialize (CBreak ni) = szLine ni $+$ (ii $ text "break" <> semi)
    serialize (CReturn Nothing ni) = szLine ni $+$ (ii $ text "return" <> semi)
    serialize (CReturn (Just e) ni) = szLine ni $+$ (ii $ text "return" <+> pretty e <> semi)
    serialize (CAsm asmStmt ni) = szLine ni $+$ pretty asmStmt
    serialize (CPWait stmt ni) =
	szLine ni $+$ 
    	(ii $ text "pwait" $+$ prettyPrec (-1) stmt)
    serialize (CPBranch stmt ni) = szLine ni $+$ (ii $ text "pbranch" $+$ prettyPrec (-1) stmt)
    serialize (CPBreak ni) = szLine ni $+$ (ii $ text "pbreak" <> semi)
    serializePrec p (CCompound localLabels bis ni) =
        let inner = szLine ni $+$ text "{" $+$ mlistP ppLblDecls localLabels $+$ vcat (map serialize bis) $$ text "}"
        in  if p == -1 then inner else ii inner
        where ppLblDecls =  vcat . map (\l -> text "__label__" <+> identP l <+> semi)
    serializePrec _ p = serialize p

-- TODO: Check need of __extension__
instance Serialize CBlockItem where
    serialize (CBlockStmt stat) = serialize stat
    serialize (CBlockDecl decl) = ii $ serialize decl <> semi
    serialize (CNestedFunDef fundef) = ii $ pretty fundef

instance Serialize CDecl where
    serialize c@(CDecl specs divs ni) = szLine ni $+$ pretty c
