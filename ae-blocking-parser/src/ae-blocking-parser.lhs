> module BlockingParser where
> import Language.C
> import Language.C.Data.Position
> import Language.C.System.GCC   -- preprocessor used
> import Language.C.Data.Ident
> import System.Environment
> import System.Console.GetOpt
> import System.Exit
> import Data.Typeable
> import Data.Maybe
> import Data.Either
> import Data.HashTable
> import Data.Generics
> import Data.Generics.Schemes
> import qualified Data.ByteString
> import qualified Data.ByteString.Char8
> import Control.Monad.State
> import Control.Exception
> import Data.List
> import CGen
> import CParse
> import CUtils
> import System.IO.Unsafe
> import Debug.Trace
> import Data.Tree
> import BlockingContext
> import Walker
> import ContextGen
> import Serialize
> import Header
> import System.Directory

> aesopCtlPrefix :: String
> aesopCtlPrefix = "__ae_ctl"

> aesopParamsPrefix :: String
> aesopParamsPrefix = "params"

> mkStmtsFromBlocking :: String -> [String] -> NodeInfo -> WalkerT [CStat]
> mkStmtsFromBlocking m p n = do
>       bp <- getBlockingParser
>       return $ mkStmtsFromCPPMacro bp n m p

> mkExprFromBlocking :: String -> [String] -> NodeInfo -> WalkerT (Maybe CExpr)
> mkExprFromBlocking m p n = do
>       bp <- getBlockingParser
>       return $ mkExprFromCPPMacro bp n m p

> mkDeclsFromBlocking :: String -> [String] -> NodeInfo -> WalkerT [CDecl]
> mkDeclsFromBlocking m p n = do
>       bp <- getBlockingParser
>       return $ mkDeclsFromCPPMacro bp n m p

> mkFunDefFromBlocking :: String -> [String] -> NodeInfo -> String -> [CStat] -> WalkerT [CExtDecl]
> mkFunDefFromBlocking m p ni insertName insertStmts = do
>       bp <- getBlockingParser
>       return $ mkFunDefFromCPPMacro bp ni m p insertName insertStmts

> mkBlockingParamsParallelFields :: String -> Bool -> NodeInfo -> WalkerT [CDecl]
> mkBlockingParamsParallelFields _ False _ = return []
> mkBlockingParamsParallelFields name True ni = mkDeclsFromBlocking "AE_MK_PARENT_POINTER_DECL" [name] ni

> mkBlockingParamsForStruct :: CFunDef -> WalkerT [CDecl]
> mkBlockingParamsForStruct fdef = do
>       let ni = nodeInfo fdef
>           ret = fromJust $ getFunDefReturnMaybe fdef
>       ds <- mkDeclsFromBlocking "AE_MK_BFUN_PARAMS_FOR_STRUCT_DECLS" [] ni
>       return $ ds
>                ++ (mkRetParam ret ni) -- optional return parameter:  [__ret_type __ae_ret]
>                ++ (mkCBParam ret ni) -- callback parameter:  void (*callback)(void *up [, __ret_type __ae_ret])

> mkBlockingParamsForFunction :: (CTypeSpec, [CDerivedDeclr]) -> NodeInfo -> WalkerT [CDecl]
> mkBlockingParamsForFunction retType ni = do
>       ds <- mkDeclsFromBlocking "AE_MK_BFUN_PARAMS_DECLS" [] ni
>       -- callback parameter:  void (*callback) (void *user_ptr, [, __ret_type __ae_ret])
>       let cbParam = mkCBParam retType ni
>           retParam = mkRetParam retType ni
>       return $ cbParam ++ ds ++ retParam
 
> mkWorkerParams :: String -> NodeInfo -> WalkerT [CDecl]
> mkWorkerParams n ni = do
>       ds <- mkDeclsFromBlocking "AE_MK_WORKER_PARAMS" [n] ni
>       return ds

> mkBlockingParamsFromFunPtrDecl :: CDecl -> WalkerT [CDecl]
> mkBlockingParamsFromFunPtrDecl c@(CDecl _ declrs ni) = do
>       let retType = getReturn c
>       ds <- mkDeclsFromBlocking "AE_MK_BFUN_PARAMS_FUN_PTR_DECLS" [returnToString retType] ni
>       return $ (mkCBParam retType ni) ++ ds

> mkBlockingRetType :: NodeInfo -> WalkerT CTypeSpec
> mkBlockingRetType ni = do
>       prd <- mkBlockingRetDecl ni
>       return $ fst $ getReturn prd

> mkBlockingRetDecl :: NodeInfo -> WalkerT CDecl
> mkBlockingRetDecl ni = liftM head $ mkDeclsFromBlocking "AE_MK_BFUN_RET_DECL" [] ni

> getLastStmtInCompounds :: CStat -> Maybe CStat
> getLastStmtInCompounds (CCompound _ bitems _)
>       | null bitems = Nothing
>       | isJust s = getLastStmtInCompounds $ fromJust s
>       | otherwise = s
>               where s = getBlockStmt $ last bitems
> getLastStmtInCompounds c = Just c

> checkForReturn :: CFunDef -> WalkerT CFunDef
> checkForReturn f@(CFunDef specs declarator decls c@(CCompound idents stmts cni) ni)

>       | isVoidReturn $ fromJust $ getFunDefReturnMaybe f =
>               if null stmts then addVoidReturn
>                 else if isNothing $ getBlockStmt $ last stmts then addVoidReturn
>                   else if not $ isReturnStmt $ fromJust $ getBlockStmt $ last stmts then addVoidReturn
>                     else return f

>       | otherwise = do
>               when (null stmts) $ errorNonVoidReturn
>               when (isNothing $ getLastStmtInCompounds c) $ errorNonVoidReturn
>               when (not $ isReturnStmt $ fromJust $ getLastStmtInCompounds c) $ errorNonVoidReturn
>               return f

>           where addVoidReturn = return $ (CFunDef specs declarator decls (CCompound idents (stmts ++ [CBlockStmt (CReturn Nothing ni)]) cni) ni)
>                 errorNonVoidReturn = invalid ("The blocking function: " ++ (getFunDefName f) ++ " does not have a final return statement\n") ni

> getLocals :: BlockingContext -> [CDecl]
> getLocals (FunContext funDef) = getFuncLocalDecls funDef
> getLocals b = getLocals $ getParent b

> isBlockingField :: CDecl -> Bool
> isBlockingField (CDecl s i n) = any isBlockingSpec s

> mkAnonStructName :: NodeInfo -> Ident
> mkAnonStructName ni = newIdent n ni
>       where n = "anonstruct_" ++ (show $ posRow $ posOfNode $ ni) ++ "_" ++
>                 (show $ posColumn $ posOfNode $ ni)

> lookupAndRegBlocking :: Maybe Ident -> CDecl -> WalkerT ()
> lookupAndRegBlocking sname d@(CDecl s i n) = do
>       let typeName = getTypeName d
>           t = if isJust typeName then fromJust typeName else mkAnonStructName $ nodeInfo d
>       assert (isJust sname) return ()
>       lookupAndRegisterBlockingStruct (fromJust sname)
>                                       (getCDeclName d)
>                                       t

> regBlockingStruct :: Maybe Ident -> CDecl -> WalkerT ()
> regBlockingStruct sname d = do
>       assert (isJust sname) return ()
>       registerBlockingStruct (fromJust sname)
>                              (getCDeclName d)
>                              (splitFunDecl d)

> registerStruct :: CDecl -> Maybe Ident -> WalkerT ()
> registerStruct d@(CDecl s i n) altSname = do
>     when (structSpecHasFields (head $ filter isStructTypeSpec s)) $ do
>         let si = getStructInfo d
>             (Just (sn, fields)) = si
>             sname = if isJust sn then sn else altSname
>         assert (isJust si) return ()
>         let blockingFields = filter isBlockingField fields
>             notBlockingFields = filter (\j -> isStructDecl j && (not $ isBlockingField j)) fields

>         mapM_ (regBlockingStruct sname) blockingFields

>         mapM_ (lookupAndRegBlocking sname) (filter isStructDecl notBlockingFields)

> registerTypedefStruct :: CDecl -> Maybe Ident -> WalkerT ()
> registerTypedefStruct d altSname = do
>       let t = getStructTypeDefInfo d
>           (Just (sn, tname)) = t
>           sname = if isJust sn then sn else altSname
>       assert (isJust t && isJust sname) return ()
>       lookupAndRegisterBlockingTypedef (fromJust sname) tname

> registerTypedefTypedef :: CDecl -> WalkerT ()
> registerTypedefTypedef d = do
>       let t = getTypeDefTypeDefInfo d
>           (Just (pn, tname)) = t
>       assert (isJust t) return ()
>       lookupAndRegisterBlockingTypedef pn tname

> registerBlockingDecl :: Bool -> CExtDecl -> WalkerT [CExtDecl]
> registerBlockingDecl ph e@(CDeclExt d@(CDecl specifiers initdecls ni))

>     | isTypeDefOfInlineStruct d = do
>           let name = mkAnonStructName (nodeInfo d)
>           registerStruct d (Just name)
>           registerTypedefStruct d (Just name)
>           if ph then
>               return [e]
>             else return []

>     | isTypeDefOfPredefStruct d = do
>           registerTypedefStruct d Nothing
>           if ph then
>               return [e]
>             else return []

>     | isTypeDefOfTypeDef d = do
>           registerTypedefTypedef d
>           if ph then
>               return [e]
>             else return []

Register structs containing blocking function pointers as fields:  struct a { __blocking int (*myfun) (); }
Or fields that are structs with blocking function pointers: struct b { struct a mya; }
etc. etc.

>     | isStructDecl d = do
>           addGlobals [d]
>           registerStruct d Nothing
>           if ph then
>               liftM ((:[]) . CDeclExt) $ everywhereM (mkM translateBlockingFunParam) d
>             else return []

Register global variable declarations.  This is necessary to map variable names to types (structs, typedefs, etc.)
allowing us to determine if a function pointer is being called within a particular variable.

>     | isVarDecl d = do
>               addGlobals [d]
>               let name = mkAnonStructName $ nodeInfo d
>               when (isStructDecl d) (registerStruct (removeVarFromDecl d) (Just name))
>               if ph then return [e] else return []

Register blocking function declarations:  __blocking int myfun();

>     | isFunDecl d && (any isBlockingSpec specifiers) = do
>         let function = splitFunDecl (CDecl specifiers initdecls ni)
>         registerBlocking function
>         pDecl <- mkBlockingDecl function specifiers
>         -- pPtrDecl <- mkBlockingPtrDecl True function specifiers
>         return $ [CDeclExt pDecl]

Transform function declarations that take blocking function pointers as parameters:  int myfun(__blocking int (*fnp)(void));
We only need to do the transformation here to get the type signature right, we don't generate anything else.

>     | isFunDecl d = do
>         if ph then
>             liftM ((:[]) . CDeclExt) $ everywhereM (mkM translateBlockingFunParam) d
>           else return []

         let (fname, (rtype, derived), ps) = splitFunDecl d
         newps <- mapM translateBlockingFunParam ps
         let newd = mkFunDeclWithDeclSpecs fname rtype derived specifiers newps
         if ph then return [CDeclExt newd]
           else return []
 
> registerBlockingDecl True c = do
>       return [c]
> registerBlockingDecl False c = do
>       return []
 
Registers all the blocking declarations present in the CTranslUnit

> registerBlockingFunDecls :: CTranslUnit -> WalkerT CTranslUnit
> registerBlockingFunDecls (CTranslUnit decls ni) = do
>       newdecls <- liftM concat $ sequence $ map (registerBlockingDecl True) decls
>       return $ CTranslUnit newdecls ni

> getBlockingHeaderDecls :: CTranslUnit -> WalkerT [([CExtDecl], CExtDecl)]
> getBlockingHeaderDecls (CTranslUnit decls ni) = do
>       newdecls <- sequence $ map (registerBlockingDecl False) decls
>       return $ zip newdecls decls

Registers blocking function pointers passed to a blocking function.

While translating a blocking function, we need to keep a registry of all the
variable names for blocking function pointers passed to the blocking function, so
that if one of them gets called, we can do translation at that point.  For example:

__blocking int myblockingfun(int a, __blocking int (*myblockingfunptr)(int b));

In this example, the myblockingfunptr variable refers to a blocking function somewhere, which
gets called in the blocking function myblockingfun.

> registerBlockingFunParam :: CDecl -> WalkerT ()
> registerBlockingFunParam (CDecl specifiers initdecls ni)
>       | (any isBlockingSpec specifiers) = do
>               let retType = getTypeSpec specifiers
>                   (Just declr, _, _) = head initdecls
>                   (CDeclr (Just f) derivedDeclrs _ _ _)  = declr
>                   (Just funDeclr) = find isFunDeclr derivedDeclrs
>                   deriveds = filter (not . (\d -> isFunDeclr d || isDerivedPtr d)) derivedDeclrs
>                   params = removeVoid $ getCFunDeclrParams funDeclr
>               registerLocalBlocking (f, (retType, deriveds), params)
>       | otherwise = return ()

> declHasBlockingFunPtrParam :: CDecl -> Bool
> declHasBlockingFunPtrParam d =
>       let ps = getParams d
>           specs (CDecl specs _ _)  = specs
>           isBlockingParam p = any isBlockingSpec (specs p)
>       in any isBlockingParam ps

> defHasBlockingFunPtrParam :: CFunDef -> Bool
> defHasBlockingFunPtrParam f =
>       let ps = getFunDefParams f
>           specs (CDecl specs _ _) = specs
>           isBlockingParam p = any isBlockingSpec (specs p)
>       in any isBlockingParam ps

> translateBlockingFunParam :: CDecl -> WalkerT CDecl
> translateBlockingFunParam fptr@(CDecl specifiers initdecls ni)
>   | isBlockingFunDecl fptr = do
>       bParams <- mkBlockingParamsFromFunPtrDecl fptr
>       ret <- mkBlockingRetType ni
>       let specs = filter (not . isBlockingSpec) specifiers
>           (Just declr, _, _) = head initdecls
>           (CDeclr (Just (Ident fname _ _)) _ _ _ _) = declr
>           funDeclr = getDerivedFun declr
>           params = removeVoid $ getCFunDeclrParams funDeclr
>           postParams = bParams ++ params
>       return $ mkFunPtrDecl fname ret postParams

>   | otherwise = return fptr

> removeVoid :: [CDecl] -> [CDecl]
> removeVoid decls = if justVoid decls then [] else decls

> isBlockingFunDef :: CFunDef  -> Bool
> isBlockingFunDef (CFunDef specifiers declarator decls stmt ni) = any isBlockingSpec specifiers
 
> isBlockingFunDecl :: CDecl -> Bool
> isBlockingFunDecl (CDecl specs _ _) = any isBlockingSpec specs

> containsReturn :: [CStat] -> Bool
> containsReturn stmts =
>       let hasReturn = everything (||) (mkQ False isReturnStmt)
>       in (any hasReturn stmts)

> getBINodeInfo :: CBlockItem -> NodeInfo
> getBINodeInfo (CBlockDecl (CDecl _ _ ni)) = ni

> invalid :: String -> NodeInfo -> WalkerT ()
> invalid msg ni = do
>       fstr <- getFilePosStr ni
>       error (fstr ++ ":  Invalid aesop usage: " ++ msg)

> gspWarn :: String -> NodeInfo -> WalkerT ()
> gspWarn msg ni = do
>       fstr <- getFilePosStr ni
>       putStrLnW $ fstr ++ ": warning: " ++ msg

> isValidBlockingFunDef :: CFunDef -> WalkerT ()
> isValidBlockingFunDef f@(CFunDef specs (CDeclr _ derivedDeclrs _ _ _) _ _ _) = do
>       let isVariadic (CFunDeclr (Right ((,) params variadic)) _ _) = variadic
>           isVariadic _ = False
>           ni = nodeInfo f
>       when (any isVariadic derivedDeclrs) $ invalid "Variadic functions are unsupported with the __blocking specifier" ni
>       return ()

> isValidBlockingFor :: CStat -> WalkerT ()
> isValidBlockingFor (CFor (Right _) _ _ _ ni) = invalid "declaration in blocking for loop" ni
> isValidBlockingFor (CFor (Left init) expr1 expr2 stmts ni) = do
>       binit <- containsBlockingCall (CExpr init ni)
>       when binit $ invalid "init expression in for loop cannot contain a blocking call" ni
>       bexpr1 <- containsBlockingCall (CExpr expr1 ni)
>       when bexpr1 $ invalid "conditional expression in for loop cannot contain a blocking call" ni
>       bexpr2 <- containsBlockingCall (CExpr expr2 ni)
>       when bexpr2 $ invalid "update expression in for loop cannot contain a blocking call" ni
>       when (not $ isCompound stmts)
>            $ invalid "blocking for loop requires curly brackets" ni
>       when (fst $ stmtsHaveDecl stmts)
>            $ invalid "blocking for loop cannot contain local declarations" (snd $ stmtsHaveDecl stmts)
>       return ()

> isValidBlockingWhile :: CStat -> WalkerT ()
> isValidBlockingWhile (CWhile expr stmts isDoWhile ni) = do
>       bexpr <- containsBlockingCall (CExpr (Just expr) ni)
>       when bexpr $ invalid "conditional expression in while loop cannot contain a blocking call" ni
>       when (not $ isCompound stmts)
>            $ invalid "blocking while loop requires curly brackets" ni
>       when (fst $ stmtsHaveDecl stmts)
>               $ invalid "blocking while loop cannot contain local declarations" (snd $ stmtsHaveDecl stmts)
>       return ()


> isValidBlockingIf :: CStat -> WalkerT ()
> isValidBlockingIf (CIf expr ifStmts elseStmts ni) = do
>       bexpr <- containsBlockingCall (CExpr (Just expr) ni)
>       when bexpr $ invalid "if expression cannot contain a blocking call" ni
>       when (not $ isCompound ifStmts)
>           $ invalid "blocking if requires curly brackets" ni
>       when (fst $ stmtsHaveDecl $ ifStmts)
>           $ invalid "if block with blocking calls cannot contain local declarations" (snd $ stmtsHaveDecl $ ifStmts)
>       when ((isJust elseStmts) && (isCompound $ fromJust elseStmts) && (fst $ stmtsHaveDecl $ fromJust elseStmts))
>           $ invalid "else block with blocking calls cannot contain local declarations" (snd $ stmtsHaveDecl $ fromJust elseStmts)
>       return ()

> isValidPWait :: CStat -> WalkerT ()
> isValidPWait (CPWait stmts ni) = do
>       when (containsReturn [stmts])
>           $ invalid "return not allowed within pwait/pbranch context" ni
>       return ()

> isValidLonePBranch :: CStat -> WalkerT ()
> isValidLonePBranch (CPBranch stmts ni) = do
>       when (containsReturn [stmts])
>           $ invalid "return not allowed within lone pbranch" ni
>       return ()

> getBlockingCallName :: CExpr -> WalkerT (Maybe Ident)
> getBlockingCallName (CCall e args _) =  do
>       res <- lookupBlocking e
>       case res of { (Just (bname, _, _)) -> return $ Just bname ; Nothing -> return Nothing }
> getBlockingCallName _ = return Nothing

> findBlockingCallName :: CStat -> WalkerT (Maybe Ident)
> findBlockingCallName stmt =
>       everything orElseMMaybe (mkQ (return Nothing) getBlockingCallName) $ stmt

> getBlockingCallExpr :: CExpr -> WalkerT (Maybe CExpr)
> getBlockingCallExpr call@(CCall e _ _) = do
>       res <- lookupBlocking e
>       if isJust res then return $ Just call else return Nothing
> getBlockingCallExpr _ = return Nothing

> findBlockingCallExpr :: CStat -> WalkerT (Maybe CExpr)
> findBlockingCallExpr stmt =
>       everything orElseMMaybe (mkQ (return Nothing) getBlockingCallExpr) $ stmt

> getBlockingCall :: CExpr -> WalkerT (Maybe (CExpr, FunDecl))
> getBlockingCall call@(CCall e _ _) = do
>       res <- lookupBlocking e
>       if isJust res then return $ Just (call, (fromJust res)) else return Nothing
> getBlockingCall _ = return Nothing

> findBlockingCall :: CStat -> WalkerT (Maybe (CExpr, FunDecl))
> findBlockingCall stmt =
>       everything orElseMMaybe (mkQ (return Nothing) getBlockingCall) $ stmt

Split a statement with a blocking function call into a triple of (return expression, function name, parameter expressions).  This
function replaces the blocking call with a variable "ret", and returns that as the return expression.

> replaceCallWithExpr :: CExpr -> CExpr -> WalkerT CExpr
> replaceCallWithExpr newe call@(CCall e _ ni) = do
>       res <- lookupBlocking e
>       if isJust res then return newe else return call
> replaceCallWithExpr _ c = return c

> findAndReplaceBlockingCall :: CExpr -> CStat -> WalkerT CStat

> -- if the statement is just the blocking call (the return value is ignored), we don't replace
> findAndReplaceBlockingCall newe (CExpr (Just call@(CCall e _ _)) ni) = do
>       res <- lookupBlocking e
>       if isJust res then return (mkCompoundStmt (Just $ "__ae_result_expr_" ++ (mkLineColStr ni)) [] ni) else do
>               expr <- replaceCallWithExpr newe call
>               return (CExpr (Just expr) ni)

> findAndReplaceBlockingCall newe stmt = everywhereM (mkM $ replaceCallWithExpr newe) stmt

From a blocking function definition, Construct an external declaration parameters struct

> mkStructCtlName :: String -> String
> mkStructCtlName name = name ++ "_ctl"

> mkStructParamsName :: String -> String
> mkStructParamsName name = name ++ "_params"

> mkStructRetParamsName :: String -> String
> mkStructRetParamsName name = name ++ "_return_params"

> mkStructPWaitName :: String -> String -> String
> mkStructPWaitName name id = id ++ "_params"

> mkStructPBranchName :: String -> String -> String -> String
> mkStructPBranchName name pwaitName id = id ++ "_params"

> mkPrivateParamPWaitName :: String -> String -> String
> mkPrivateParamPWaitName name id = id ++ "_private_params"

> mkSharedParamPWaitName :: String -> String -> String
> mkSharedParamPWaitName name id = id ++ "_shared_params"

> mkPWaitName :: String -> String
> mkPWaitName id = id ++ "_params"

> mkPrivateName :: String
> mkPrivateName = "private"

> mkPrivatePWaitName :: String -> String
> mkPrivatePWaitName id = (mkPWaitName id) ++ "." ++ mkPrivateName

> mkSharedName :: String
> mkSharedName = "shared"

> mkSharedPtrName :: String
> mkSharedPtrName = "shared_params"

> mkSharedPWaitName :: String -> String
> mkSharedPWaitName id = (mkPWaitName id) ++ "." ++ mkSharedPtrName

> mkParamPBranchName :: String -> String
> mkParamPBranchName id = id ++ "_params"

> mkBlockingParamsStruct :: CFunDef -> WalkerT CExtDecl
> mkBlockingParamsStruct funDef = do
>       let decls = getFunDefParams funDef
>           ni = nodeInfo funDef
>       bParams <- sequence $ map translateBlockingFunParam $ removeVoid decls
>       let params = bParams ++ (map removeInitFromDecl $ getFunLocalDeclarations funDef)
>       return $ genStructExtDecl (mkStructParamsName $ getFunDefName funDef) params ni

> mkBlockingRetParamDecl :: CExpr -> WalkerT [CDecl]
> mkBlockingRetParamDecl e = do
>       bfun <- lookupBlocking e
>       assert (isJust bfun) $ return ()
>       let (Just (bname, (retSpec, retDerives), params)) = bfun
>           ni = nodeInfo e
>       case (retSpec, retDerives) of
>           (CVoidType _, []) -> return []
>           _ -> return $ [mkCDecl retSpec retDerives (mkCallbackRetParam (getCallName e) ni) ni]

> mkBlockingRetParamsStruct :: CFunDef -> [CExpr] -> WalkerT CExtDecl
> mkBlockingRetParamsStruct funDef blockingCalls = do
>       params <- liftM concat $ sequence $ map mkBlockingRetParamDecl blockingCalls
>       return $ genStructExtDecl (mkStructRetParamsName $ getFunDefName funDef) params (nodeInfo funDef)
 
> removeInits :: (String, [CDecl]) -> (String, [CDecl])
> removeInits (pwaitName, pwaitDecls) = (pwaitName, (map removeInitFromDecl pwaitDecls))

> mkPWaitExtDecl :: (String -> String) -> (String, [CDecl]) -> NodeInfo -> CExtDecl
> mkPWaitExtDecl snamef (pwaitName, pwaitDecls) ni =
>       genStructExtDecl (snamef pwaitName) pwaitDecls ni

> mkBlockingPBranchStructs :: CFunDef -> (String, CStat) -> WalkerT [CExtDecl]
> mkBlockingPBranchStructs funDef (pwaitName, pwaitStmt) = do
>       let fname = getFunDefName funDef
>           ni = nodeInfo funDef
>           pbranches = getPBranches pwaitStmt
>           pbranchDecls = zip (map (getPBranchId funDef) pbranches) (map getPBranchDecls pbranches)
>           params = map removeInits pbranchDecls
>       return $ map (\p -> mkPBranchExtDecl fname pwaitName p ni) params

> addPBranchParams :: CFunDef -> CStat -> (String, [CDecl]) -> (String, [CDecl])
> addPBranchParams funDef pwait (pwaitId, pWaitDecls) =
>       let fname = getFunDefName funDef
>           ni = nodeInfo funDef
>           pbranchDecls =
>             map (\p -> mkStructDecl
>                          (mkStructPBranchName fname pwaitId (getPBranchId funDef p))
>                          (mkParamPBranchName (getPBranchId funDef p)) ni) $ getPBranches pwait
>       in (pwaitId, (pWaitDecls ++ pbranchDecls))

> containsPWait :: CFunDef -> Bool
> containsPWait funDef = everything (||) (mkQ False isPWaitStmt) funDef

> getPWaits :: CFunDef -> [CStat]
> getPWaits funDef = listify isPWaitStmt funDef

> getPWaitId :: CFunDef -> CStat -> String
> getPWaitId funDef pwStmt = "pwait_" ++ (mkLineColStr $ nodeInfo pwStmt)

> getPBranches :: CStat -> [CStat]
> getPBranches (CPWait stmts _) = listify isPBranchStmt stmts

> getPBranchId :: CFunDef -> CStat -> String
> getPBranchId funDef pbStmt = "pbranch_" ++ (mkLineColStr $ nodeInfo pbStmt)

> mkBlockingPWaitStructs :: CFunDef -> WalkerT [CExtDecl]
> mkBlockingPWaitStructs funDef
>    | containsPWait funDef = do
>        let fname = getFunDefName funDef
>            ni = nodeInfo funDef
>            pwaits = getPWaits funDef
>            ids = map (getPWaitId funDef) pwaits
>            privatePWaitDecls = zip ids (map getPWaitPrivateDecls pwaits) -- :: [(pwait_id, [private_decl])]
>            sharedPWaitDecls = zip ids (map getPWaitSharedDecls pwaits) -- :: [(pwait_id, [shared_decl])]
>            private = map removeInits privatePWaitDecls -- :: [(pwait_id, [private_decl_no_inits])]
>            shared = map removeInits sharedPWaitDecls -- :: [(pwait_id, [shared_decl_no_inits])]
>            privateWithPBranch = zipWith (addPBranchParams funDef) pwaits private -- :: [(pwait_id, [shared_decl_no_inits] ++ [pbranch_struct_decl])]
>            privatePWaitStructs = map (\p -> mkPWaitExtDecl (mkPrivateParamPWaitName fname) p ni) privateWithPBranch
>            sharedPWaitStructs = map (\p -> mkPWaitExtDecl (mkSharedParamPWaitName fname) p ni) shared
>            pwaitParams = map (\id -> (id, [mkStructDecl (mkPrivateParamPWaitName fname id) mkPrivateName ni,
>                                            mkStructDecl (mkSharedParamPWaitName fname id) mkSharedName ni,
>                                            mkStructPtrDecl (mkSharedParamPWaitName fname id) mkSharedPtrName ni])) ids  -- [(pwait_id, [private_struct, shared_struct])]
>            pwaitStructs = map (\p -> mkPWaitExtDecl (mkStructPWaitName fname) p ni) pwaitParams
>            pwaitNamePairs = zip (map (getPWaitId funDef) pwaits) pwaits
>        pbranchStructs <- liftM concat $ mapM (mkBlockingPBranchStructs funDef) pwaitNamePairs
>        return $ pbranchStructs ++ privatePWaitStructs ++ sharedPWaitStructs ++ pwaitStructs
>    | otherwise = return []

> mkPBranchExtDecl :: String -> String -> (String, [CDecl]) -> NodeInfo -> CExtDecl
> mkPBranchExtDecl fname pwaitName (pbranchName, pbranchDecls) ni =
>       genStructExtDecl (mkStructPBranchName fname pwaitName pbranchName) pbranchDecls ni

> mkBlockingCtlStruct :: CFunDef -> Bool -> WalkerT CExtDecl
> mkBlockingCtlStruct funDef pblock = do
>       let ni = nodeInfo funDef
>           fname = getFunDefName funDef
>           ctlName = mkStructCtlName fname
>           spName = mkStructParamsName fname
>           retSPName = mkStructRetParamsName fname
>           fieldsDecl = mkStructDecl spName "fields" ni
>           paramsDecl = mkStructPtrDecl spName "params" ni
>           returnsDecl = mkStructDecl retSPName "return_params" ni
>           pwaitDecls = map (\p -> mkStructDecl (mkStructPWaitName fname (getPWaitId funDef p))
>                                                (mkPWaitName (getPWaitId funDef p)) ni)
>                            $ getPWaits funDef

>           fret = fromJust $ getFunDefReturnMaybe funDef
>           retValDecl = if isVoidReturn fret then [] else [mkCDecl (fst fret) (snd fret) "return_value" ni]
>
>       bparams <- mkBlockingParamsForStruct funDef
>       pparams <- mkBlockingParamsParallelFields (getFunDefName funDef) pblock ni

>       return $ genStructExtDecl ctlName
>                                 (bparams ++ pparams ++
>                                  [fieldsDecl, paramsDecl, returnsDecl] ++ retValDecl ++ pwaitDecls) ni

> mkBlockingFunName :: String -> String -> String
> mkBlockingFunName ctlName name = name

Each callback function defined for a given blocking function must have a unique name.  We use:

<bfun>_<bcall>_<bcallrow>_<bcallcol>_callback

<bfun> is the name of the blocking function being translated
<bcall> is the name of the blocking call being made within the function
<bcallrow> is the row (line number in the file) where the blocking call is being made
<bcallcol> is the column in the file where the blocking call is being made

> mkLineColStr :: NodeInfo -> String
> mkLineColStr (NodeInfo p _ _) = (show $ posRow p) ++ "_" ++ (show $ posColumn p)

> mkCallbackFunName :: String -> String -> NodeInfo -> String
> mkCallbackFunName funName callName n@(NodeInfo p _ _) = funName ++ "_" ++ callName ++ "_" ++ (mkLineColStr n) ++ "_callback"

> mkCallbackRetParam :: String -> NodeInfo -> String
> mkCallbackRetParam callName ni = callName ++ "_" ++ (mkLineColStr ni) ++ "_ret"

> isVarIn :: [Ident] -> CExpr -> Bool
> isVarIn locals (CVar name _) = name `elem` locals
> isVarIn _ _ = False

> isCallIn :: [Ident] -> CExpr -> Bool
> isCallIn locals c@(CCall expr params ni) = (newIdent (getCallName c) ni) `elem` locals
> isCallIn _ _ = False

> addParamsPtrPrefixToExpr :: String -> String -> [Ident] -> CExpr -> CExpr
> addParamsPtrPrefixToExpr ctlPrefix paramsPrefix locals expr
>       | isVarIn locals expr = addStructPtrPrefix ctlPrefix paramsPrefix expr
>       | isCallIn locals expr = addStructPtrPrefix ctlPrefix paramsPrefix expr
>       | otherwise = expr

> addParamsPtrPrefixes :: String -> String -> [Ident] -> CStat -> CStat
> addParamsPtrPrefixes ctlPrefix paramsPrefix idents stmt = everywhere (mkT $ addParamsPtrPrefixToExpr ctlPrefix paramsPrefix idents) stmt

> addParamsPrefixToExpr :: String -> String -> [Ident] -> CExpr -> CExpr
> addParamsPrefixToExpr ctlPrefix paramsPrefix locals expr
>       | isVarIn locals expr = addStructPrefix ctlPrefix paramsPrefix expr
>       | isCallIn locals expr = addStructPrefix ctlPrefix paramsPrefix expr
>       | otherwise = expr

> addParamsPrefixes :: String -> String -> [Ident] -> CStat -> CStat
> addParamsPrefixes ctlPrefix paramsPrefix idents stmt = everywhere (mkT $ addParamsPrefixToExpr ctlPrefix paramsPrefix idents) stmt

> addParams2PrefixToExpr :: String -> String -> String -> [Ident] -> CExpr -> CExpr
> addParams2PrefixToExpr ctlPrefix p1Prefix p2Prefix locals expr
>       | isVarIn locals expr = addStructPrefixPrefix ctlPrefix p1Prefix p2Prefix expr
>       | isCallIn locals expr = addStructPrefixPrefix ctlPrefix p1Prefix p2Prefix expr
>       | otherwise = expr

> addParams2Prefixes :: String -> String -> String -> [Ident] -> CStat -> CStat
> addParams2Prefixes ctlPrefix p1Prefix p2Prefix idents stmt =
>       everywhere (mkT $ addParams2PrefixToExpr ctlPrefix p1Prefix p2Prefix idents) stmt

> addParams2PtrPrefixToExpr :: String -> String -> String -> [Ident] -> CExpr -> CExpr
> addParams2PtrPrefixToExpr ctlPrefix p1Prefix p2Prefix locals expr
>       | isVarIn locals expr = addStructPtrPrefixPrefix ctlPrefix p1Prefix p2Prefix expr
>       | isCallIn locals expr = addStructPtrPrefixPrefix ctlPrefix p1Prefix p2Prefix expr
>       | otherwise = expr

addParams2PtrPrefixes "foo" "bar" "baz" [locals] localvar

if localvar is in [locals]
results in:  foo->bar->baz.localvar

> addParams2PtrPrefixes :: String -> String -> String -> [Ident] -> CStat -> CStat
> addParams2PtrPrefixes ctlPrefix p1Prefix p2Prefix idents stmts =
>       everywhere (mkT $ addParams2PtrPrefixToExpr ctlPrefix p1Prefix p2Prefix idents) stmts

trLocals finds the local declarations for a blocking function context, and translates
the variables used in the statement, into variables prefixed with the ctl and parameter structures.
So for example, if a, b, c and d are local parameters, the statement:

runfun(a, b, c, d);

gets translated to:

runfun(ctl->fields.a, ctl->fields.b, ctl->fields.c, ctl->fields.d);

> trPBranchLocals :: BlockingContext -> CStat -> CStat
> trPBranchLocals bctx stmt
>    | isPBranchContext bctx =
>       let locals = join $ map getCDeclNames $ getPBranchDecls (pbranchStmt bctx)
>           pwaitCtx = parent bctx
>           fdef = funDef $ getParent bctx
>           pwaitName = mkPWaitName $ "pwait_" ++ (mkLineColStr $ nodeInfo $ pwaitStmt pwaitCtx)
>           pbranchName = mkParamPBranchName $ getPBranchId fdef $ pbranchStmt bctx

>       in if parentIsPWait bctx then
>
>              -- pbranch has a parameter struct in the pwait struct for the control struct
>              -- myvar becomes:  ctl->pwait_100_23.pbranch_123_10.myvar
>              addParams2Prefixes aesopCtlPrefix
>                                 (pwaitName ++ "." ++ mkPrivateName)
>                                 pbranchName
>                                 locals
>                                 stmt

>          else
>              -- a lone pbranch has a parameter struct in the control struct
>              -- myvar becomes:  ctl->pbranch_123_10.myvar
>              addParamsPtrPrefixes aesopCtlPrefix
>                                   pbranchName
>                                   locals
>                                   stmt

>    | otherwise = stmt

> trPWaitLocals :: BlockingContext -> CStat -> CStat
> trPWaitLocals bctx stmt
>    | isPWaitContext bctx || parentIsPWait bctx =
>       let pwaitCtx = if isPWaitContext bctx then bctx else parent bctx
>           mkLocals f = join $ map getCDeclNames $ f $ pwaitStmt pwaitCtx
>           fdef = funDef $ getParent bctx
>           privateLocals = mkLocals getPWaitPrivateDecls
>           sharedLocals = mkLocals getPWaitSharedDecls
>           privatePWaitName = mkPrivatePWaitName $ getPWaitId  fdef $ pwaitStmt pwaitCtx
>           sharedPWaitName = mkSharedPWaitName $ getPWaitId fdef $ pwaitStmt pwaitCtx
>           privatePrefixedStmt = addParamsPrefixes
>                                     aesopCtlPrefix
>                                     privatePWaitName
>                                     privateLocals
>                                     stmt
>           allPrefixedStmt = addParamsPtrPrefixes
>                                 aesopCtlPrefix
>                                 sharedPWaitName
>                                 sharedLocals
>                                 privatePrefixedStmt
>       in allPrefixedStmt
>    | otherwise = stmt

> trFunLocals :: BlockingContext -> CStat -> CStat
> trFunLocals bctx stmt =
>       let locals = join $ map getCDeclNames $ getLocals bctx
>       in (addParamsPtrPrefixes aesopCtlPrefix aesopParamsPrefix locals) stmt

> trLocals :: BlockingContext -> CStat -> CStat
> trLocals b = (trFunLocals b) . (trPWaitLocals b) . (trPBranchLocals b)

> trExpr :: BlockingContext -> Maybe CExpr -> Maybe CExpr
> trExpr bctx expr = if isJust expr then resExpr else expr
>       where ni = nodeInfo $ fromJust expr
>             (CExpr resExpr _) = trLocals bctx (CExpr expr ni)

> getFuncLocalDecls :: CFunDef -> [CDecl]
> getFuncLocalDecls funDef = (getFunDefParams funDef) ++ (getFunLocalDeclarations funDef)

Make the initialization statements for the post function:

params = malloc(sizeof(*params));
if(params == NULL)
{
    return -ENOMEM;
}
params->j = 0;
params->val = val;
...

> mkBlockingFunInitStmts :: BlockingContext -> [CDecl] -> [CDecl] -> WalkerT [CStat]
> mkBlockingFunInitStmts bctx fdecls pdecls = do
>       -- get the function parameters, and set the parameters in the
>       -- params struct to the values passed in
>       let withPrefix name ni = addStructPtrToAssign aesopCtlPrefix
>                                                     aesopParamsPrefix
>                                                     name
>                                                     (CVar name ni)
>           genParam decl = withPrefix (getCDeclName decl) (nodeInfo decl)
>           funParams = map genParam $ concat (map splitDecls fdecls)

>           -- get the init statements from local declarations, and add the params-> prefix
>           localDecls = (map (trLocals bctx) (getInitsFromDecls pdecls))

>       return $ funParams ++ localDecls

> mkPostCall :: BlockingContext -> String -> CExpr -> (CTypeSpec, [CDerivedDeclr]) -> WalkerT CStat
> mkPostCall b fname bcall@(CCall fexpr _ _) retType =
>       case retType of
>           (CVoidType _, []) -> do
>               [stmt] <- mkStmtsFromBlocking
>                               "AE_MK_POST_CALL"
>                               [fname, show $ pretty fexpr, getCallName bcall, posStr] ni
>               return $ addParamsToFunCall stmt callParams
>           _ -> do
>               [retParamStmt] <- mkStmtsFromBlocking "AE_MK_POST_RET_PARAM" [getCallName bcall, posStr] ni
>               let (CExpr (Just (CAssign CAssignOp _ retParam _)) _) = retParamStmt
>               [stmt] <- mkStmtsFromBlocking
>                               "AE_MK_POST_CALL"
>                               [fname, show $ pretty fexpr, getCallName bcall, posStr] ni
>               return $ addParamsToFunCall stmt (retParam : callParams)
>       where ni = nodeInfo bcall
>             posStr = mkLineColStr ni
>             callParams = map (fromJust . (trExpr b) . Just) $ getCallParams bcall

Construct the statements for the post function.  First the declarations, next the initialization parts,
and finally the code up-to the first blocking call.

> mkBlockingStmts :: BlockingContext -> CFunDef -> (CTypeSpec, [CDerivedDeclr]) -> NodeInfo -> WalkerT [CStat]


This is the case where the function returns void (no second paramter for the callback)

> mkBlockingStmts bctx funDef (CVoidType _, []) ni = do
>     let (fname, retType, params) = getFunInfo funDef
>         locals = getFunLocalDeclarations funDef
>     inits <- mkStmtsFromBlocking
>                  "AE_MK_BFUN_INIT_BLOCK" [identToString fname] ni
>     workerInvoke <- mkStmtsFromBlocking
>                         "AE_MK_BFUN_INVOKE_WORKER_VOIDFN" [identToString fname] ni
>     initsFromDecls <- mkBlockingFunInitStmts bctx params locals
>     return $ inits ++ initsFromDecls ++ workerInvoke

Normal non-void function

> mkBlockingStmts bctx funDef (ret, derived) ni = do
>     let (fname, retType, params) = getFunInfo funDef
>         locals = getFunLocalDeclarations funDef
>     inits <- mkStmtsFromBlocking
>                  "AE_MK_BFUN_INIT_BLOCK" [identToString fname] ni
>     workerInvoke <- mkStmtsFromBlocking
>                  "AE_MK_BFUN_INVOKE_WORKER" [identToString fname, returnToString retType] ni
>     initsFromDecls <- mkBlockingFunInitStmts bctx params locals
>     return $ inits ++ initsFromDecls ++ workerInvoke

> mkBlockingBody :: BlockingContext -> CFunDef -> WalkerT CStat
> mkBlockingBody bctx funDef = do

>     let ni = nodeInfo funDef
>         (fname, retType, params) = getFunInfo funDef

>     decls <- mkDeclsFromBlocking "AE_MK_BFUN_DECLS" [identToString fname] ni
>     stmts <- mkBlockingStmts bctx funDef retType ni
>     return $ mkCompoundWithDecls Nothing decls stmts ni

> mkRetParam :: (CTypeSpec, [CDerivedDeclr]) -> NodeInfo -> [CDecl]
> mkRetParam (CVoidType _, []) ni = []
> mkRetParam (ret, derived) ni = [mkCDecl ret ((CPtrDeclr [] ni) : derived) "__ae_retvalue" ni]

> mkCBParam :: (CTypeSpec, [CDerivedDeclr]) -> NodeInfo -> [CDecl]
> mkCBParam (CVoidType _, []) ni =
>       [mkVoidFunPtr "__ae_callback" [constructDeclFromC ni "void *user_ptr;"]]
> mkCBParam return ni =
>       [mkVoidFunPtr "__ae_callback"
>               [constructDeclFromC ni "void *user_ptr;", mkCDecl (fst return) (snd return) "__ae_ret" ni]]

> mkBlockingParams :: (Ident, (CTypeSpec, [CDerivedDeclr]), [CDecl]) -> WalkerT [CDecl]
> mkBlockingParams ((,,) fname ret params) = do
>       let ni = nodeInfo $ fst ret

>       txParams <- sequence $ map translateBlockingFunParam $ removeVoid params
>       bParams <- mkBlockingParamsForFunction ret ni

>       return $ bParams ++ txParams

> mkBlockingDecl :: (Ident, (CTypeSpec, [CDerivedDeclr]), [CDecl]) -> [CDeclSpec] -> WalkerT CDecl
> mkBlockingDecl f@((,,) fname ret params) declspecs = do
>       let ni = nodeInfo $ fst ret
>       fparams <- mkBlockingParams f
>       ret <- mkBlockingRetType ni
>       return $ mkFunDeclWithDeclSpecs
>                  (identToString fname)
>                  ret []
>                  (filterStdDeclSpecs declspecs)
>                  fparams

> mkBlockingPtrDecl :: Bool -> (Ident, (CTypeSpec, [CDerivedDeclr]), [CDecl]) -> [CDeclSpec] -> WalkerT CDecl
> mkBlockingPtrDecl extern f@((,,) fname ret params) declspecs = do
>       let ni = nodeInfo $ fst ret
>       fparams <- mkBlockingParams f
>       ret <- mkBlockingRetType ni
>       return $ mkFunPtrDeclWithDeclSpecs
>                   (identToString fname)
>                   ret []
>                   ((filterStdDeclSpecs declspecs) ++
>                    (if extern then [CStorageSpec $ CExtern ni] else []))
>                   fparams
>                   (if not extern then Just (identToString $ fname) else Nothing)

> mkBlockingFunction :: BlockingContext -> CFunDef -> WalkerT CExtDecl
> mkBlockingFunction bctx funDef = do
>       let decls = getFunDefParams funDef
>           ni = nodeInfo funDef
>       params <- sequence $ map translateBlockingFunParam $ removeVoid decls
>       blockingParams <- mkBlockingParamsForFunction (fromJust $ getFunDefReturnMaybe funDef) (nodeInfo funDef)
>       body <- mkBlockingBody bctx funDef
>       ret <- mkBlockingRetType ni
>       return $ mkFunDef (ret, []) -- return type
>                   (getStdDeclSpecs funDef) -- get the storage specifiers for the function
>                   (mkBlockingFunName aesopParamsPrefix $ getFunDefName funDef) -- function name
>                   (blockingParams ++ params)
>                   body -- statements

> removeDerivedFun :: [CDerivedDeclr] -> [CDerivedDeclr]
> removeDerivedFun (d:ds) | isDerivedFun d = ds
> removeDerivedFun (p:f:ds) | isDerivedPtr p && isDerivedFun f = ds
> removeDerivedFun ds = ds

> mkCallbackReturnParamDecl :: (CTypeSpec, [CDerivedDeclr]) -> String -> NodeInfo -> [CDecl]
>      -- no parameter if the return type is void
> mkCallbackReturnParamDecl (CVoidType _, []) _ _ = []
> mkCallbackReturnParamDecl (retType, derives) name ni =
>      [mkCDecl retType (removeDerivedFun derives) (mkCallbackRetParam name ni) ni]

> mkCallbackDecl :: BlockingContext -> CExpr -> (CTypeSpec, [CDerivedDeclr]) -> CExtDecl
> mkCallbackDecl bctx blockingCall returnType =
>      let fname  = getCallName blockingCall
>          params = getCallParams blockingCall
>          ni     = getCallNodeInfo blockingCall
>      in mkStaticFunDecl (CVoidType ni) -- return type
>                         (mkCallbackFunName (getParentName bctx) fname ni) -- function name
>                         ([constructDeclFromC ni "void *__ae_ptr;"] ++ -- parameter for control state
>                          (mkCallbackReturnParamDecl returnType fname ni)) -- parameter for return type of blocking function

> generateCallbackDeclForBlocking :: BlockingContext -> CExpr -> WalkerT CExtDecl
> generateCallbackDeclForBlocking b blockingCall = do

>       wdebug $ "[generateCallbackDeclForBlocking]: call: " ++ (show $ getCallName $ blockingCall)
>       blockingFun <- lookupBlocking $ blockingCall
>       assert (isJust blockingFun) $ return ()
>
>       let (Just ((,,) bname retType params)) = blockingFun
>       wdebug $ "[generateCallbackDeclForBlocking]: fun: " ++ (show bname)
>       return $ mkCallbackDecl b blockingCall retType

> generateCallbackDecls :: BlockingContext -> [CExpr] -> WalkerT [CExtDecl]
> generateCallbackDecls b blockingCalls = do
>        sequence $ map (generateCallbackDeclForBlocking b) blockingCalls

> generateCallbackForBlocking :: BlockingContext -> CExpr -> WalkerT [CExtDecl]
> generateCallbackForBlocking b blockingCall = do
>       -- lookup the declaration of the blocking function call
>       -- to get the return type and parameter types
>       blockingFun <- lookupBlocking $ blockingCall
>       assert (isJust blockingFun) $ return ()
>       let (Just ((,,) bname retType params)) = blockingFun
>           ni = getCallNodeInfo blockingCall
>           fname = getCallName blockingCall
>           cbFunName = mkCallbackFunName (getParentName b) fname ni
>           cbRetParam = mkCallbackRetParam fname ni
>           fRetType = fromJust $ getFunDefReturnMaybe $ funDef $ getParent b
>           fRetStr = returnToString fRetType
>           retStr = returnToString retType
>           bfunName = getParentName b
>           posStr = mkLineColStr ni
>           (macroName, macroParams) =
>               case (isVoidReturn retType, isVoidReturn fRetType) of
>                   (False, False) -> ("AE_MK_CALLBACK_FN",
>                                      [bfunName,
>                                       fname,
>                                       posStr,
>                                       fRetStr,
>                                       retStr,
>                                       cbRetParam])
>                   (False, True) -> ("AE_MK_CALLBACK_VOIDFN",
>                                     [bfunName,
>                                      fname,
>                                      posStr,
>                                      retStr,
>                                      cbRetParam])
>                   (True, False) -> ("AE_MK_CALLBACK_FN_VOIDCB",
>                                     [bfunName,
>                                      fname,
>                                      posStr,
>                                      fRetStr])
>                   (True, True) -> ("AE_MK_CALLBACK_VOIDFN_VOIDCB",
>                                    [bfunName, fname, posStr])

>       mkFunDefFromBlocking macroName macroParams ni "NOINJECTBLOCK" []

> generateCallbackDefs :: BlockingContext -> [CExpr] -> WalkerT [CExtDecl]
> generateCallbackDefs b blockingCalls = do
>        liftM concat $ sequence $ map (generateCallbackForBlocking b) blockingCalls

> swapReturn :: BlockingContext -> CStat -> WalkerT CStat
> swapReturn bctx (CReturn (Just retexpr) ni) = do
>       let retExprStr = show $ pretty retexpr
>       -- check that return is not in a pwait or pbranch
>       when (isPWaitContext bctx) $ invalid "return statements not allowed in pwait" ni
>       when (isPBranchContext bctx) $ invalid "return statements not allowed in pbranch" ni

>       [s] <- mkStmtsFromBlocking "AE_MK_WORKER_RETURN" [retExprStr] ni
>       return s

> swapReturn bctx (CReturn Nothing ni) = do
>       [s] <- mkStmtsFromBlocking "AE_MK_WORKER_VOID_RETURN" [] ni
>       return s

> swapReturn b r = return r

> swapPBreak :: BlockingContext -> CStat -> WalkerT CStat
> swapPBreak bctx (CPBreak ni) = do
>       when (not $ isPBranchContext bctx) $ invalid "pbreak statement not allowed outside of pbranch" ni
>       let pbranchPosStr = mkLineColStr $ nodeInfo $ pbranchStmt bctx
>       [s] <- mkStmtsFromBlocking "AE_MK_WORKER_PBREAK" [pbranchPosStr, mkLineColStr ni] ni
>       return s
> swapPBreak bctx s = return s

Could probably use generics here, but not sure how since we need to construct
a BlockingContext as we go along

> transformBlockingCall :: BlockingContext -> CStat -> WalkerT CStat
> transformBlockingCall b stmt
>       | isPWaitContext b = do
>           invalid "Blocking call made in pwait outside of pbranch:" (nodeInfo stmt)
>           return stmt
>       | otherwise = do
>               let bfunName = getParentName b
>                   ni = nodeInfo stmt
>               r@(Just (bexpr, (bcallIdent, bcallRet, bcallParams))) <- findBlockingCall stmt
>               assert (isJust r) return ()
>               let bcallName = getCallName bexpr
>                   lineColStr = mkLineColStr $ nodeInfo bexpr

>               ss <- case (isPBranchContext b, isPWaitContext $ parent b) of
>                       (True, _) -> mkStmtsFromBlocking "AE_MK_WORKER_BEFORE_POST_IN_PBRANCH"
>                                                               [bfunName,
>                                                                bcallName,
>                                                                lineColStr,
>                                                                mkLineColStr $ nodeInfo $ pbranchStmt b] ni
>                       (False, _) -> mkStmtsFromBlocking "AE_MK_WORKER_BEFORE_POST"
>                                                               [bfunName,
>                                                                bcallName,
>                                                                lineColStr] ni

>               se <- case (isPBranchContext b, isPWaitContext $ parent b) of
>                       (True, _) -> mkStmtsFromBlocking "AE_MK_WORKER_AFTER_POST_IN_PBRANCH"
>                                                               [bfunName,
>                                                                bcallName,
>                                                                lineColStr,
>                                                                mkLineColStr $ nodeInfo $ pbranchStmt b] ni
>                       (False, _) -> mkStmtsFromBlocking "AE_MK_WORKER_AFTER_POST"
>                                                               [bfunName,
>                                                                bcallName,
>                                                                lineColStr] ni

>               rexpr <- mkExprFromBlocking "AE_MK_WORKER_RETURN_VAR_EXPR" [bfunName, bcallName, lineColStr] ni
>               assert (isJust rexpr) $ return ()
>               retStmt <- findAndReplaceBlockingCall (fromJust rexpr) stmt
>               postStmt <- mkPostCall b bfunName bexpr bcallRet
>               return $ mkCompoundStmt Nothing (ss ++ [postStmt] ++ se ++ [retStmt]) ni

> transformCall :: BlockingContext -> CStat -> WalkerT CStat
> transformCall b stmt@(CExpr me ni) = do
>     r <- containsBlockingCall stmt
>     if r then transformBlockingCall b stmt else return stmt
> transformCall b rstmt@(CReturn expr ni) = do
>     r <- containsBlockingCall rstmt
>     if r then do
>         tx <- transformBlockingCall b rstmt
>         everywhereM (mkM $ swapReturn b) tx
>       else return rstmt
> transformCall _ s = return s

> addPBranchStmts :: BlockingContext -> CStat -> WalkerT CStat

> addPBranchStmts bctx pb@(CPBranch stmt ni) = do
>       -- special case for pbranches because we add start/end blocks

>       let bfunName = getParentName bctx
>           pbranchPosStr = mkLineColStr ni
>           decls = getLocalDeclarations pb
>           initStmts = map (trLocals bctx) $ concat $ map getInitStmtsFromDecl decls
>           (CCompound is bitems cni) = stmt
>           newCompound = (CCompound is ((map CBlockStmt initStmts) ++ (filter (isJust . getBlockStmt) bitems)) cni)

>       if not $ parentIsPWait bctx then do
>           -- lone pbranch
>           ds <- mkDeclsFromBlocking
>                       "AE_MK_WORKER_LONE_PBRANCH_DECLS"
>                       [bfunName, pbranchPosStr] ni
>           ss <- mkStmtsFromBlocking
>                       "AE_MK_WORKER_LONE_PBRANCH_START_STMTS"
>                       [bfunName, pbranchPosStr] ni
>           se <- mkStmtsFromBlocking
>                       "AE_MK_WORKER_LONE_PBRANCH_END_STMTS"
>                       [bfunName, pbranchPosStr] ni
>           return $ mkCompoundWithDecls Nothing ds (ss ++ [stmt] ++ se) ni
>         else do
>           let pwaitPosStr = mkLineColStr $ nodeInfo $ pwaitStmt $ parent bctx
>           ds <- mkDeclsFromBlocking
>                       "AE_MK_WORKER_PBRANCH_DECLS"
>                       [bfunName, pbranchPosStr] ni
>           ss <- mkStmtsFromBlocking
>                       "AE_MK_WORKER_PBRANCH_START_STMTS"
>                       [bfunName, pwaitPosStr, pbranchPosStr] ni
>           se <- mkStmtsFromBlocking
>                       "AE_MK_WORKER_PBRANCH_END_STMTS"
>                       [bfunName, pwaitPosStr, pbranchPosStr] ni
>           return $ mkCompoundWithDecls Nothing ds (ss ++ [newCompound] ++ se) ni

> addPBranchStmts bctx s = return s

> addPWaitStmts bctx pw@(CPWait stmts ni) = do
>       -- special case for pwaits because we add start/end blocks

>       let bfunName = getParentName bctx
>           lineColStr = mkLineColStr ni

>       -- pwait must be followed by a compound block
>       when (not $ isCompound stmts) $ invalid "pwait requires compound block:" ni

>       let decls = getLocalDeclarations pw
>           initStmts = map (trLocals bctx) $ concat $ map getInitStmtsFromDecl decls
>           (CCompound is bitems cni) = stmts
>           newCompound = (CCompound is ((map CBlockStmt initStmts) ++
>                                        (filter (isJust . getBlockStmt) bitems)) cni)
>       ds <- mkDeclsFromBlocking "AE_MK_WORKER_PWAIT_DECLS" [bfunName, lineColStr] ni
>       ss <- mkStmtsFromBlocking
>               "AE_MK_WORKER_PWAIT_START_STMTS" [bfunName, lineColStr] ni
>       se <- mkStmtsFromBlocking
>               "AE_MK_WORKER_PWAIT_END_STMTS"   [bfunName, lineColStr] ni
>       return $ mkCompoundWithDecls Nothing ds (ss ++ [newCompound] ++ se) ni

> addPWaitStmts bctx s = return s

> mkWorkerRetType :: NodeInfo -> WalkerT CTypeSpec
> mkWorkerRetType ni = do
>       prd <- mkWorkerRetDecl ni
>       return $ fst $ getReturn prd

> mkWorkerRetDecl :: NodeInfo -> WalkerT CDecl
> mkWorkerRetDecl ni = liftM head $ mkDeclsFromBlocking "AE_MK_WORKER_RET_DECL" [] ni

> transformBlockingStmt :: (BlockingContext -> CStat -> WalkerT CStat) -> BlockingContext -> CStat -> WalkerT CStat

> transformBlockingStmt tr b pw@(CPWait s ni) = do
>       news <- transformBlockingStmt tr (PWaitContext pw b) s
>       tr (PWaitContext pw b) (CPWait news ni)

> transformBlockingStmt tr b pb@(CPBranch s ni) = do
>       news <- transformBlockingStmt tr (PBranchContext pb b) s
>       tr (PBranchContext pb b) (CPBranch news ni)

> transformBlockingStmt tr b (CLabel i s as ni) = do
>       news <- transformBlockingStmt tr b s
>       tr b (CLabel i news as ni)
> transformBlockingStmt tr b (CCase e s ni) = do
>       news <- transformBlockingStmt tr b s
>       tr b (CCase e news ni)
> transformBlockingStmt tr b (CCases e1 e2 s ni) = do
>       news <- transformBlockingStmt tr b s
>       tr b (CCases e1 e2 news ni)
> transformBlockingStmt tr b (CDefault s ni) = do
>       news <- transformBlockingStmt tr b s
>       tr b (CDefault news ni)
> transformBlockingStmt tr b e@(CExpr me ni) = tr b e
> transformBlockingStmt tr b (CCompound is bitems ni) = do
>       let trStmt (CBlockStmt s) = liftM CBlockStmt $ transformBlockingStmt tr b s
>           trStmt bi = return bi
>       newbitems <- sequence $ map trStmt bitems
>       tr b (CCompound is newbitems ni)
> transformBlockingStmt tr b (CIf e is es ni) = do
>       newifs <- transformBlockingStmt tr b is
>       newelses <- if isJust es then
>                       liftM Just $ transformBlockingStmt tr b (fromJust es)
>                     else return Nothing
>       tr b (CIf e newifs newelses ni)
> transformBlockingStmt tr b (CSwitch e s ni) = do
>       news <- transformBlockingStmt tr b s
>       tr b (CSwitch e news ni)
> transformBlockingStmt tr b (CWhile e s d ni) = do
>       news <- transformBlockingStmt tr b s
>       tr b (CWhile e news d ni)
> transformBlockingStmt tr b (CFor e1 e2 e3 s ni) = do
>       news <- transformBlockingStmt tr b s
>       tr b (CFor e1 e2 e3 news ni)

> -- all other statement types don't contain sub-statements
> transformBlockingStmt tr b s = tr b s

> generateWorkerDecl :: String -> NodeInfo -> [CDeclSpec] -> WalkerT CDecl
> generateWorkerDecl fname ni declspecs = do
>       ret <- mkWorkerRetType ni
>       fparams <- mkWorkerParams fname ni
>       return $ mkFunDeclWithDeclSpecs
>                  ("__ae_worker_" ++ fname)
>                  ret []
>                  (filterStdDeclSpecs declspecs)
>                  fparams

> generateWorker :: CFunDef -> WalkerT CExtDecl
> generateWorker funDef@(CFunDef specs declr decls stmt ni) = do

>       let bctx = FunContext funDef
>           fname = getFunDefName funDef

>           -- filter decls out of inits so that just init statements remain
>           inits = concat $ map getInitStmtsFromDecl $ getDeclList stmt

>           -- replace decls with init statements
>           justStmts = inits ++ (getStmtList stmt)

>           -- make the transform locals function into a monadic function
>           trLocalsM b s = return $ trLocals b s

>           -- combinedTransform is a function that consists of the following
>           -- transforms:
>           --     1) transform all local variables and function parameters to
>           --        add the appropriate prefixes
>           --     2) for each blocking call found, add begin and end statements
>           --        and transform the blocking call into a post call
>           --     3) for each pbranch block, add begin and end statements
>           --        see (AE_MK_WORKER_PBRANCH_START_STMTS, AE_MK_WORKER_PBRANCH_END_STMTS)
>           --     4) for each pwait block, add begin and end statements
>           --        see (AE_MK_WORKER_PWAIT_START_STMTS, AE_MK_WORKER_PWAIT_END_STMTS)
>           --     5) swap all pbreaks with the transformed statements
>           --        see (AE_MK_WORKER_PBREAK)
>           --     6) swap the return statement with
>           --        the transformed statements (see AE_MK_WORKER_RETURN)

>       let combinedTransform b = (trLocalsM b) <=<
>                                 (swapReturn b) <=<
>                                 (swapPBreak b) <=<
>                                 (addPWaitStmts b) <=<
>                                 (addPBranchStmts b) <=<
>                                 (transformCall b)

>       -- now walk through the blocking function and do the above transformations
>       trStmts <- mapM (transformBlockingStmt combinedTransform bctx) justStmts

>       workerDecls <- mkDeclsFromBlocking "AE_MK_WORKER_DECLS" [] ni
>       workerStartStmts <- mkStmtsFromBlocking "AE_MK_WORKER_START_STMTS" [] ni
>       workerEndStmts <- mkStmtsFromBlocking "AE_MK_WORKER_END_STMTS" [] ni
>       let workerStmts = workerStartStmts ++ trStmts ++ workerEndStmts
>       ret <- mkWorkerRetType ni
>       params <- mkWorkerParams fname ni
>       return $ mkFunDef (ret, []) -- return type
>                         (getStdDeclSpecs funDef) -- get the storage specifiers for the original function
>                         ("__ae_worker_" ++ fname) -- worker function name
>                         params -- param to worker function
>                         (mkCompoundWithDecls Nothing workerDecls workerStmts ni)

> transformBlocking :: CExtDecl -> WalkerT [CExtDecl]
> transformBlocking fdefext@(CFDefExt funDef)

>   -- A blocking function definition, so we transform it
>   | isBlockingFunDef funDef = do

>       isValidBlockingFunDef funDef

>       wdebug $ "[transform start]: " ++ (getFunDefName funDef)

>       if (isNothing $ getFunDefReturnMaybe funDef) then do
>                invalid "function lacks return type" (nodeInfo fdefext)
>                return []
>       else do

>        -- register a blocking function with its parameters and return type
>               registerBlocking ((getFunDefIdent funDef),
>                           (fromJust $ getFunDefReturnMaybe funDef),
>                           (getFunDefParams funDef))

>       -- Find any blocking function pointers as parameters to the blocking function,
>       -- and register them
>               mapM_ registerBlockingFunParam $ getFunDefParams funDef

>       -- Register function parameters as local variables
>               addLocals $ getFunDefParams funDef

>       -- Register function local variables
>               addLocals $ getFunLocalDeclarations funDef

>       -- before generating the context, we add a return; statement at the end if its
>       -- a void function and doesn't have a return

>               fDefWReturn <- checkForReturn funDef

>               -- Generate the blocking tree of BlockingContext nodes
>               bcalls <- getAllBlockingCalls fDefWReturn

>               let bctx = FunContext fDefWReturn

>       -- Walk the tree, and generate the callback function declarations
>               callbackDecls <- generateCallbackDecls bctx bcalls

>       -- Walk the tree, and generate the callback function definitions
>               callbackDefs <- generateCallbackDefs bctx bcalls

>               workerDecl <- generateWorkerDecl
>                       (getFunDefName fDefWReturn)
>                       (nodeInfo fDefWReturn)
>                       (getDeclSpecs fDefWReturn)

>               workerDef <- generateWorker fDefWReturn

>       -- Make the blocking params structure declaration
>               paramsStruct <- mkBlockingParamsStruct fDefWReturn
>               returnParamsStruct <- mkBlockingRetParamsStruct fDefWReturn bcalls
>               pwaitStructs <- mkBlockingPWaitStructs fDefWReturn
>               ctlStruct <- mkBlockingCtlStruct fDefWReturn True

>               let ni = nodeInfo fDefWReturn
>                   fi = getFunInfo fDefWReturn
>                   specs = map CStorageSpec (getStorageSpecs fDefWReturn)

>               blockingFun <- mkBlockingFunction bctx fDefWReturn
>               blockingDecl <- mkBlockingDecl fi specs
>       -- pPtrDecl <- mkBlockingPtrDecl False fi specs

>       -- Construct the external declarations of parameters, callback functions, and post function
>               let transformedDecls = pwaitStructs ++ [paramsStruct, returnParamsStruct, ctlStruct] ++ callbackDecls ++
>                                      [CDeclExt workerDecl, CDeclExt blockingDecl] ++
>                                      callbackDefs ++ [workerDef, blockingFun]

>       -- Clear the blocking function parameters registry
>               resetLocals
>               clearLocalFunPtrRegistry

>               wdebug $ "[transform end]: " ++ (getFunDefName funDef)

>               return transformedDecls

   | defHasBlockingFunPtrParam funDef = do
       let (fname, (rtype, derived), ps) = getFunInfo funDef
           (CFunDef _ _ _ stmt _) = funDef
           specs = getDeclSpecs funDef
       newps <- mapM translateBlockingFunParam $ removeVoid ps
       let newDef = mkFunDef (rtype, derived) specs fname newps stmt
       return [newDef]

>   | otherwise =
>      liftM ((:[]) . CFDefExt) $ everywhereM (mkM translateBlockingFunParam) funDef

> transformBlocking c = return [c]

> transform :: CTranslUnit -> WalkerT CTranslUnit
> transform (CTranslUnit decls ni) = do
>       -- map transformBlockingDef decls returns: [WalkerT [CExtDecl]]
>       -- sequence of that returns: WalkerT [[CExtDecl]]
>       -- so we lift concat into a monad so that we end up with WalkerT [CExtDecl]
>       newdecls <- liftM concat $ sequence $ map transformBlocking decls
>       return $ CTranslUnit newdecls ni

> printBlocking :: Int -> FPType -> WalkerT ()
> printBlocking l (FPFun n f) = do
>       let (bname, (rettype, derives), params) = f
>           pstr = if null params then "" else (foldl1 ((++) . (++ ", ")) $ map (show . pretty) params)
>       liftIO $ print $ (take l $ repeat ' ') ++ (show bname) ++ "( " ++ pstr ++ " ) = " ++ (show . pretty $ rettype)
> printBlocking l (FPStruct n f i) = do
>       liftIO $ print $ (take l $ repeat ' ') ++ "( " ++ (show n) ++ " )->" ++ (show f)
>       printBlocking (l + 4) i

> printRegisteredBlockingCalls :: WalkerT ()
> printRegisteredBlockingCalls = do
>       blocking <- getAllBlocking
>       liftIO $ print "BLOCKING FUNCTIONS:"
>       mapM_ (printBlocking 0) blocking

> generateAST :: FilePath -> IO CTranslUnit
> generateAST input_file = do
>       input_stream <- readInputStream input_file
>       let parse_result = parseC input_stream (position 0 input_file 1 1)
>       case parse_result of
>         Left parse_err -> error $ "Parse failed for input file: " ++ input_file ++ ": " ++ (show parse_err)
>         Right ast      -> return ast

> parseHeader :: Bool -> String -> FilePath -> Maybe FilePath  -> FilePath -> [String]-> IO ()
> parseHeader debug compiler headerfile report outfile gccopts = do
>       let r = if isJust report then fromJust report else headerfile
>       ctu <- generateAST headerfile
>       let tdIdents = getTypeDefIdents ctu
>       w <- newWalkerState debug r compiler "aesop/ae-blocking-parser.h" gccopts tdIdents
>       (pairs, w) <- runStateT (getBlockingHeaderDecls ctu) w
>       writeFile outfile "\n\n/* This is an auto-generated file created by the ae-blocking-parser tool.  DO NOT MODIFY! */\n\n"
>       outputHeader r outfile pairs
>       appendFile outfile "\n\n"
>       assert (isJust $ blockingParser w) return ()
>       removeFile $ macheader $ fromJust $ blockingParser w
>       return ()

> isAesopInit :: CExtDecl -> Bool
> isAesopInit (CDeclExt d) = any ((== "aesop_init") . identToString) $ getCDeclNames d
> isAesopInit _ = False

> splitExtDeclsAtAesop :: CTranslUnit -> (CTranslUnit, CTranslUnit)
> splitExtDeclsAtAesop (CTranslUnit extDecls ni) =
>       let (before, after) = break isAesopInit extDecls
>       in (CTranslUnit before ni, CTranslUnit after ni)

> mergeCTUs :: CTranslUnit -> CTranslUnit -> CTranslUnit
> mergeCTUs (CTranslUnit das ni) (CTranslUnit dbs _) = (CTranslUnit (das ++ dbs) ni)

> parseFile :: Bool -> Bool -> String -> FilePath -> Maybe FilePath -> [String] -> FilePath -> IO ()
> parseFile debug p compiler outfile report gccopts f = do
>       let r = if isJust report then fromJust report else f
>       ctu <- generateAST f
>       let typeDefIdents = getTypeDefIdents ctu
>       w <- newWalkerState debug r compiler "aesop/ae-blocking-parser.h" gccopts typeDefIdents
>       let extDecls (CTranslUnit d _) = d
>       let (preBlockingCTU, mainCTU) = splitExtDeclsAtAesop ctu
>       let (CTranslUnit mainExtDecls _) = mainCTU
>       when (null mainExtDecls) $
>           error $ "Parse failed for " ++ f ++ ": missing aesop_init() function.  " ++
>                   "Must include aesop.h in all aesop source and header files.\n"
>       (ctuWithPostDecls, w) <- runStateT (registerBlockingFunDecls mainCTU) w
>       -- runStateT (printRegisteredBlockingCalls) w
>       (transCTU, w) <- runStateT (transform ctuWithPostDecls) w
>       writeFile outfile $ "\n\n\
>                           \/* This is an auto-generated source file create by the ae-blocking-parser tool.  DO NOT MODIFY! */\n\n"
>       if p then ((appendFile outfile) . show . pretty) (mergeCTUs preBlockingCTU transCTU)
>         else ((appendFile outfile) . show . serialize) (mergeCTUs preBlockingCTU transCTU)
>       appendFile outfile "\n\n"
>       assert (isJust $ blockingParser w) return ()
>       removeFile $ macheader $ fromJust $ blockingParser w
>       return ()

> data ParserOpts = Debug | Pretty | Help | Report String | Outfile String | Infile String | Header | Compiler String

> getReportFilename :: [ParserOpts] -> Maybe String
> getReportFilename ((Report s):ps) = Just s
> getReportFilename (_:ps) = getReportFilename ps
> getReportFilename [] = Nothing

> getOutfile :: [ParserOpts] -> Maybe String
> getOutfile ((Outfile s):ps) = Just s
> getOutfile (_:ps) = getOutfile ps
> getOutfile [] = Nothing

> getInfile :: [ParserOpts] -> Maybe String
> getInfile ((Infile s):ps) = Just s
> getInfile (_:ps) = getInfile ps
> getInfile [] = Nothing

> getCompiler :: [ParserOpts] -> String
> getCompiler ((Compiler s):ps) = s
> getCompiler (_:ps) = getCompiler ps
> getCompiler [] = "gcc"

> parserOpts :: [OptDescr ParserOpts]
> parserOpts =
>    [ Option ['p'] ["pretty"] (NoArg Pretty)
>               "output in pretty form without source line macros"
>    , Option ['h','?'] ["help"] (NoArg Help)
>               "help text"
>    , Option ['r'] ["report"] (ReqArg (\s -> Report s) "<report filename>")
>               "filename to use when reporting errors"
>    , Option ['o'] ["outfile"] (ReqArg (\s -> Outfile s) "<output file>")
>               "filename to write translated C code"
>    , Option ['i'] ["infile"] (ReqArg (\s -> Infile s) "<input file>")
>               "input filename to translate"
>    , Option ['c'] ["compiler"] (ReqArg (\s -> Compiler s) "<compiler>")
>               "compiler"
>    , Option ['j'] ["header"] (NoArg Header)
>               "parse header file instead of source"
>    , Option ['d'] ["debug"] (NoArg Debug)
>               "output debugging info (very verbose!)"
>    ]

> optPretty :: ParserOpts -> Bool
> optPretty (Pretty) = True
> optPretty _ = False

> optHelp :: ParserOpts -> Bool
> optHelp (Help) = True
> optHelp _ = False

> optDebug :: ParserOpts -> Bool
> optDebug (Debug) = True
> optDebug _ = False

> optHeader :: ParserOpts -> Bool
> optHeader (Header) = True
> optHeader _ = False

> main :: IO ()
> main = do
>   args <- getArgs
>   let (opts, gccopts, errs) = getOpt RequireOrder parserOpts args
>       pretty = any optPretty opts
>       help = any optHelp opts
>       report = getReportFilename opts
>       outfile = getOutfile opts
>       input = getInfile opts
>       pheader = any optHeader opts
>       debug = any optDebug opts
>       compiler = getCompiler opts
>       header = "Usage: ae-blocking-parser [OPTIONS...] files..."
>   when (not $ null errs) $ ioError $ userError ((concat errs) ++
>                                                 (usageInfo header parserOpts))
>   when help $ do { putStrLn $ usageInfo header parserOpts ; exitWith (ExitFailure 1) }
>   when (isNothing input) $ ioError $ userError "No input file specified."
>   when (isNothing outfile) $ ioError $ userError "No output file specified."

>   when pheader $ do
>       parseHeader debug compiler (fromJust input) report (fromJust outfile) gccopts
>       exitWith (ExitSuccess)

>   parseFile debug pretty compiler (fromJust outfile) report gccopts (fromJust input)
