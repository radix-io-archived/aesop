> module CParse where
> import Language.C
> import Language.C.Data.Node
> import Language.C.Data.Position
> import Language.C.Data.Ident
> import Language.C.System.GCC   -- preprocessor used
> import System.Environment
> import Data.Typeable
> import Data.Maybe
> import Data.Either
> import Data.HashTable
> import Data.Generics
> import Data.Generics.Schemes
> import Control.Monad.State
> import qualified Data.ByteString
> import qualified Data.ByteString.Char8
> import Control.Exception
> import Data.List
> import System.IO.Unsafe
> import Debug.Trace

Add a first function for triples

> tfst :: (a,b,c) -> a
> tfst ((,,) a _ _) = a

Get the line number and column string from a node info.  For example, a variable at line 123, column 34 will result
in the string:

_123_34_

> getPosVarStr :: NodeInfo -> String
> getPosVarStr (NodeInfo p _ _) = "_" ++ (show $ posRow p) ++ "_" ++ (show $ posColumn p) ++ "_"

This is useful for removing the names (and initializers) from a declaration, making it
a declaration without a name (good for return types, etc.).

> emptyDeclrList :: CDecl -> CDecl
> emptyDeclrList (CDecl specifiers _ ni) = CDecl specifiers [] ni

Get the declarators from a list of triples

> getCDeclrsFromList :: [(Maybe CDeclr, Maybe CInit, Maybe CExpr)] -> [CDeclr]
> getCDeclrsFromList cdeclrs =
>	concatMap (\cd -> if isJust $ tfst cd then [fromJust $ tfst cd] else []) cdeclrs
 
 Get the name from a declarator

> getCDeclrName :: CDeclr -> Ident
> getCDeclrName (CDeclr name _ _ _ _) = fromJust $ name
 
Get the names from a declaration.  For example:

int a, b, c = 3;

Will result in ["a", "b", "c"]

> getCDeclNames :: CDecl -> [Ident]
> getCDeclNames (CDecl _ declrs _) = map getCDeclrName $ getCDeclrsFromList declrs

> getCDeclName :: CDecl -> Ident
> getCDeclName (CDecl _ declrs _) = getCDeclrName $ d
>       where (d:dls) = getCDeclrsFromList declrs

> hasDeclrName :: CDecl -> Bool
> hasDeclrName (CDecl _ declrs _) = any (\(CDeclr name _ _ _ _) -> isJust name) $ getCDeclrsFromList declrs

> splitDecls :: CDecl -> [CDecl]
> splitDecls (CDecl specifiers initdecls ni) = map (\dlr -> (CDecl specifiers [dlr] ni)) initdecls
 
> isStructTypeSpec :: CDeclSpec -> Bool
> isStructTypeSpec (CTypeSpec (CSUType (CStruct _ _ _ _ _) _)) = True
> isStructTypeSpec _ = False

> isTypeDefSpec :: CDeclSpec -> Bool
> isTypeDefSpec (CStorageSpec (CTypedef _)) = True
> isTypeDefSpec _ = False

> isTypeDefTypeSpec :: CDeclSpec -> Bool
> isTypeDefTypeSpec (CTypeSpec (CTypeDef _ _)) = True
> isTypeDefTypeSpec _ = False

> isVoidTypeSpec :: CDeclSpec -> Bool
> isVoidTypeSpec (CTypeSpec (CVoidType _)) = True
> isVoidTypeSpec _ = False

> isStructDecl :: CDecl -> Bool
> isStructDecl (CDecl specs initdecls ni) = any isStructTypeSpec specs

> isTypeDefDecl :: CDecl -> Bool
> isTypeDefDecl (CDecl (s:specs) initdecls ni) = isTypeDefSpec s
> isTypeDefDecl _ = False

> structSpecHasName :: CDeclSpec -> Bool
> structSpecHasName (CTypeSpec (CSUType (CStruct _ (Just _) _ _ _) _)) = True
> structSpecHasName _ = False

> structSpecHasFields :: CDeclSpec -> Bool
> structSpecHasFields (CTypeSpec (CSUType (CStruct _ _ (Just fields) _ _) _)) = True
> structSpecHasFields _ = False

> isTypeDefOfPredefStruct :: CDecl -> Bool
> isTypeDefOfPredefStruct (CDecl (s1:s2:[]) initdecls ni) =
>	isTypeDefSpec s1 && isStructTypeSpec s2 && (not $ structSpecHasFields s2)
> isTypeDefOfPredefStruct _ = False

> isTypeDefOfInlineStruct :: CDecl -> Bool
> isTypeDefOfInlineStruct (CDecl (s1:s2:[]) initdecls ni) =
>	isTypeDefSpec s1 && isStructTypeSpec s2 && structSpecHasFields s2
> isTypeDefOfInlineStruct _ = False

> isTypeDefOfTypeDef :: CDecl -> Bool
> isTypeDefOfTypeDef (CDecl (s1:s2:[]) _ _) =
>       isTypeDefSpec s1 && isTypeDefTypeSpec s2
> isTypeDefOfTypeDef _ = False

> getStructName :: CDeclSpec -> Maybe String
> getStructName (CTypeSpec (CSUType (CStruct _ n _ _ _) _)) = if isJust n then Just $ identToString $ fromJust n else Nothing
> getStructName _ = Nothing

> getStructTypeDefInfo :: CDecl -> Maybe (Maybe String, String)
> getStructTypeDefInfo d@(CDecl (s1:s2:[]) initdecls ni)
>	| isTypeDefSpec s1 && isStructTypeSpec s2 =
>               let (i:ids) = initdecls
>		    ((Just (CDeclr (Just (Ident tname _ _)) _ _ _ _)), _, _) = i
>		in Just (getStructName s2, tname)
>	| otherwise = Nothing

> getTypeDefTypeDefInfo :: CDecl -> Maybe (String, String)
> getTypeDefTypeDefInfo d@(CDecl (s1:s2:[]) initdecls ni)
>       | isTypeDefSpec s1 && isTypeDefTypeSpec s2 =
>               let (CTypeSpec (CTypeDef (Ident pname _ _) _)) = s2
>                   (i:ids) = initdecls
>		    ((Just (CDeclr (Just (Ident tname _ _)) _ _ _ _)), _, _) = i
>               in Just (pname, tname)
>	| otherwise = Nothing

> getStructInfo :: CDecl -> Maybe (Maybe String, [CDecl])
> getStructInfo (CDecl s i n) = 
>	let (t:ts) = filter isStructTypeSpec s
>	in case t of
>		(CTypeSpec (CSUType (CStruct _ (Just n) (Just f) _ _) _)) -> Just (Just $ identToString n, f)
>		(CTypeSpec (CSUType (CStruct _ Nothing (Just f) _ _) _)) -> Just (Nothing, f)
>		_ -> Nothing

> getTypeName :: CDecl -> Maybe String
> getTypeName d@(CDecl s _ _) =
>       let (sp:sps) = s
>	in case sp of
>		(CTypeSpec (CSUType (CStruct _ (Just n) _ _ _) _)) -> Just $ identToString n
>	 	_ -> Nothing

> isVarDecl :: CDecl -> Bool
> isVarDecl d@(CDecl specs initdecls ni) =
>	(not $ isFunDecl d) && (not $ any isTypeDefSpec specs) && hasDeclrName d

> getVarName :: CDecl -> String
> getVarName d@(CDecl _ initdecls _) = vname
>	where (i:ids) = initdecls
>             ((Just (CDeclr (Just (Ident vname _ _)) _ _ _ _)), _, _) = i

For a compound block item (part of a compound block of statements), is the item a declaration?

> isFuncDecl :: CBlockItem -> Bool
> isFuncDecl (CBlockDecl decl) = True 
> isFuncDecl _ = False

For a compound block item (part of a compound block of statements), is the item a declaration?

> getFuncDecl :: CBlockItem -> CDecl
> getFuncDecl (CBlockDecl decl) = decl

For the array of compound block items, get the elements that are declarations

> findFuncDecls :: [CBlockItem] -> [CDecl]
> findFuncDecls decls = [ getFuncDecl d | d <- decls, isFuncDecl d ]

For the function derived declarator, get the function parameters

> getCFunDeclrParams :: CDerivedDeclr -> [CDecl]
> getCFunDeclrParams (CFunDeclr (Right (declarations, isVariadic)) _ ni) = declarations

> isTypeSpec :: CDeclSpec -> Bool
> isTypeSpec (CTypeSpec c) = True
> isTypeSpec _ = False

> getTypeSpecFromDeclSpec :: CDeclSpec -> CTypeSpec
> getTypeSpecFromDeclSpec (CTypeSpec c) = c

> getTypeSpec :: [CDeclSpec] -> CTypeSpec
> getTypeSpec declspecs = getTypeSpecFromDeclSpec $ fromJust $ find isTypeSpec declspecs

> getTypeSpecFromDecl :: CDecl -> CTypeSpec
> getTypeSpecFromDecl (CDecl dspecs _ _) = getTypeSpec dspecs

> isDerivedPtr :: CDerivedDeclr -> Bool
> isDerivedPtr (CPtrDeclr _ _) = True
> isDerivedPtr _ = False

> getDerivedDeclrs :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> [CDerivedDeclr]
> getDerivedDeclrs (Just (CDeclr _ derived _ _ _), _, _) = derived
> getDerivedDeclrs _ = []

> getDerivedPtrs :: CDeclr -> [CDerivedDeclr]
> getDerivedPtrs (CDeclr _ derivedDeclrs _ _ _) =
>	if any isDerivedFun derivedDeclrs then
>		tail $ snd $ break isDerivedFun derivedDeclrs
>	else filter (not . isDerivedPtr) derivedDeclrs

> getReturn :: CDecl -> (CTypeSpec, [CDerivedDeclr])
> getReturn d@(CDecl specs declarators ni) = 
>	let declrs = getCDeclrsFromList declarators 
>           (d:ds) = declrs
>	    derivedPtrs = if null declrs then [] else getDerivedPtrs $ d
>	in (getTypeSpec specs, derivedPtrs)

> isDerivedFun :: CDerivedDeclr -> Bool
> isDerivedFun (CFunDeclr (Right ((,) params variadic)) _ _) 
>	| otherwise = True
> isDerivedFun _ = False

> getDerivedFun :: CDeclr -> CDerivedDeclr
> getDerivedFun (CDeclr _ derivedDeclrs _ _ _) = fromJust $ find isDerivedFun derivedDeclrs

> getDerivedFunParams :: CDerivedDeclr -> [CDecl]
> getDerivedFunParams (CFunDeclr (Right ((,) params variadic)) _ _) = params

> getParams :: CDecl -> [CDecl]
> getParams (CDecl specs declarators ni) = 
>	let declrs = getCDeclrsFromList declarators
>           (d:ds) = declrs
>	in if null declrs then [] else getDerivedFunParams $ getDerivedFun $ d

split a function declaration into tuple: (function-name, (return-type, return-derived-list), function-parameters)

> splitFunDecl :: CDecl -> (String, (CTypeSpec, [CDerivedDeclr]), [CDecl])
> splitFunDecl decl  = (identToString $ dn, getReturn decl, getParams decl)
>       where (dn:dns) = getCDeclNames decl

> getFunInfo :: CFunDef -> (String, (CTypeSpec, [CDerivedDeclr]), [CDecl])
> getFunInfo f = (getFunDefName f, getFunDefReturn f, getFunDefParams f)

Get the name of a function definition

> getFunDefName :: CFunDef -> String
> getFunDefName (CFunDef _ (CDeclr name _ _ _ _) _ _ _) = identToString $ fromJust name

Get the parameter declarations of a function definition

> getFunDefParams :: CFunDef -> [CDecl]
> getFunDefParams (CFunDef _ (CDeclr _ derivedDeclrs _ _ _) _ _ _) =
>	let fundeclr = find isFunDeclr derivedDeclrs
>	in  getCFunDeclrParams (fromJust fundeclr)

Get the return type and derived declarators for a function definition

> getFunDefReturn :: CFunDef -> (CTypeSpec, [CDerivedDeclr])
> getFunDefReturn (CFunDef specs (CDeclr _ derivedDeclrs _ _ _) _ _ _) =
>	let (CTypeSpec tspec) = fromJust $ find isTypeSpec specs
>           derivedPtrs       = filter isDerivedPtr derivedDeclrs
> 	in (tspec, derivedPtrs)

> isBlockingSpec :: CDeclSpec -> Bool
> isBlockingSpec (CTypeQual (CBlocking _)) = True
> isBlockingSpec _ = False

> isPrivateSpec :: CDeclSpec -> Bool
> isPrivateSpec (CTypeQual (CPPrivate _)) = True
> isPrivateSpec _ = False

> isSharedSpec :: CDeclSpec -> Bool
> isSharedSpec (CTypeQual (CPShared _)) = True
> isSharedSpec _ = False

> isStdSpec :: CDeclSpec -> Bool
> isStdSpec c = (not $ isBlockingSpec c) && (not $ isPrivateSpec c) && (not $ isSharedSpec c)

> isRemoteSpec :: CDeclSpec -> Bool
> isRemoteSpec (CTypeQual (CRemote _)) = True
> isRemoteSpec _ = False

> getDeclSpecs :: CFunDef -> [CDeclSpec]
> getDeclSpecs (CFunDef declspecs _ _ _ _) = declspecs

> getAllStorageSpecs :: [CDeclSpec] -> [CStorageSpec]
> getAllStorageSpecs declspecs = map getSto $ filter isSto declspecs
>       where isSto (CStorageSpec s) = True
>             isSto _ = False
>             getSto (CStorageSpec s) = s

> getStorageSpecs :: CFunDef -> [CStorageSpec]
> getStorageSpecs (CFunDef declspecs _ _ _ _) = getStorageSpecsFromDeclSpecs declspecs

> getStorageSpecsFromDeclSpecs ::  [CDeclSpec] -> [CStorageSpec]
> getStorageSpecsFromDeclSpecs ((CStorageSpec s):specs)
>	| isStdSpec (CStorageSpec s) = s:(getStorageSpecsFromDeclSpecs specs)
>	| otherwise = getStorageSpecsFromDeclSpecs specs
> getStorageSpecsFromDeclSpecs (s:specs) = getStorageSpecsFromDeclSpecs specs
> getStorageSpecsFromDeclSpecs [] = []

> filterDecls :: (CDeclSpec -> Bool) -> (CDeclSpec -> Bool) -> (CDecl -> CDecl) -> [CDecl] -> [CDecl]
> filterDecls anyFunc allFunc transFunc decls = map transFunc $ filter isFilterDecl decls
>       where isFilterDecl (CDecl specs l ni) = any anyFunc specs && all allFunc specs

> isStorageSpec :: CDeclSpec -> Bool
> isStorageSpec (CStorageSpec _) = True
> isStorageSpec _ = False

> isTypeQual :: CDeclSpec -> Bool
> isTypeQual (CTypeQual _) = True
> isTypeQual _ = False

> isStdTypeQual :: CDeclSpec -> Bool
> isStdTypeQual (CTypeQual (CRemote _)) = False
> isStdTypeQual (CTypeQual (CBlocking _)) = False
> isStdTypeQual (CTypeQual (CPPrivate _)) = False
> isStdTypeQual (CTypeQual (CPShared _)) = False
> isStdTypeQual (CTypeQual _) = True
> isStdTypeQual _ = False

> filterStdDeclSpecs :: [CDeclSpec] -> [CDeclSpec]
> filterStdDeclSpecs declspecs = filter (\s -> isStorageSpec s || isStdTypeQual s) declspecs

> getStdDeclSpecs :: CFunDef -> [CDeclSpec]
> getStdDeclSpecs (CFunDef declspecs _ _ _ _) = filterStdDeclSpecs declspecs

> removeSharedSpecFromDecl :: CDecl -> CDecl
> removeSharedSpecFromDecl (CDecl specs l ni) = (CDecl rspecs l ni)
>    where rspecs = filter (not . isSharedSpec) specs

> removePrivateSpecFromDecl :: CDecl -> CDecl
> removePrivateSpecFromDecl (CDecl specs l ni) = (CDecl rspecs l ni)
>    where rspecs = filter (not . isPrivateSpec) specs

> removeRemoteSpecFromFunDef :: CExtDecl -> CExtDecl 
> removeRemoteSpecFromFunDef (CFDefExt (CFunDef specs declr decls stmt ni)) =
>       let newspecs = filter (not . isRemoteSpec) specs
>       in (CFDefExt (CFunDef newspecs declr decls stmt ni))

Get the local declarations for a function definition

> getFunLocalDeclarations :: CFunDef -> [CDecl]
> getFunLocalDeclarations (CFunDef _ _ _ (CCompound _ bitems _) _) = findFuncDecls bitems

> getLocalDeclarations :: CStat -> [CDecl]
> getLocalDeclarations (CPBranch (CCompound _ bitems _) _) = findFuncDecls bitems
> getLocalDeclarations (CPWait (CCompound _ bitems _) _) = findFuncDecls bitems
> getLocalDeclarations _ = error "Not handled!"

> getCallName :: CExpr -> String
> getCallName (CCall (CVar n _) _ _) = identToString n
> getCallName (CCall _ _ ni) = "fptr"

> getCallParams :: CExpr -> [CExpr]
> getCallParams (CCall _ p _) = p

> getCallNodeInfo :: CExpr -> NodeInfo
> getCallNodeInfo (CCall _ _ ni) = ni

> getBlockStmt :: CBlockItem -> Maybe CStat
> getBlockStmt (CBlockStmt stmt) = Just stmt
> getBlockStmt _  = Nothing

> getStmtsFromBlockItems :: [CBlockItem] -> [CStat]
> getStmtsFromBlockItems bitems = [ fromJust b | b <- map getBlockStmt bitems, isJust b ]

> getFunStmts :: CFunDef -> [CStat]
> getFunStmts (CFunDef _ _ _ (CCompound _ bitems _) _) = getStmtsFromBlockItems bitems

> getStmtList :: CStat -> [CStat]
> getStmtList (CCompound _ bitems _) = getStmtsFromBlockItems bitems
> getStmtList c = [c]

> getForExprs :: CStat -> (Maybe CExpr, Maybe CExpr, Maybe CExpr)
> getForExprs (CFor (Left init) expr1 expr2 _ _) = (init, expr1, expr2)

> isFunDeclr :: CDerivedDeclr -> Bool
> isFunDeclr (CFunDeclr _ _ _) = True
> isFunDeclr _ = False

> isFunDecl :: CDecl -> Bool
> isFunDecl (CDecl specs drs _) = or $ map (any isFunDeclr) $ map getDerivedDeclrs drs

> blockItemIsDecl :: CBlockItem -> Bool
> blockItemIsDecl (CBlockDecl _) = True
> blockItemIsDecl _ = False

> isCompound :: CStat -> Bool
> isCompound (CCompound _ _ _) = True
> isCompound _ = False

> stmtsHaveDecl :: CStat -> (Bool, NodeInfo)
> stmtsHaveDecl (CCompound i bitems ni) = (any blockItemIsDecl bitems, ni)
> stmtsHaveDecl c = (False, nodeInfo c)

> isVoidType :: CDecl -> Bool
> isVoidType (CDecl specs declarators ni) = 
>	let derivedDeclrs = concatMap getDerivedDeclrs declarators
>	in (not $ any isDerivedPtr derivedDeclrs) && (any isVoidTypeSpec specs)

> isVoidReturn :: (CTypeSpec, [CDerivedDeclr]) -> Bool
> isVoidReturn (CVoidType _, []) = True
> isVoidReturn _ = False

> isReturnStmt :: CStat -> Bool
> isReturnStmt (CReturn _ _) = True
> isReturnStmt _ = False

> justVoid :: [CDecl] -> Bool
> justVoid decls = ((length decls) < 2) && (any isVoidType decls)

> isBreak :: CStat -> Maybe CStat
> isBreak b@(CBreak _) = Just b
> isBreak _ = Nothing 

> isPBreak :: CStat -> Maybe CStat
> isPBreak b@(CPBreak _) = Just b
> isPBreak _ = Nothing
