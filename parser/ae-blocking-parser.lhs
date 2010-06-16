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
		
> mkStmtFromBlockingMacro :: Walker -> String -> [String] -> NodeInfo -> [CStat]
> mkStmtFromBlockingMacro w macro params ni = mkStmtFromCPPMacro includePaths defs "ae-blocking-parser.h" ni macro params
>       where includePaths = includes w
>             defs = defines w

> mkStmtFromBlocking :: String -> [String] -> NodeInfo -> WalkerT [CStat]
> mkStmtFromBlocking m p n = do
>       w <- get
>       return $ mkStmtFromBlockingMacro w m p n

> mkDeclsFromBlockingMacro :: Walker -> String -> [String] -> NodeInfo -> [CDecl]
> mkDeclsFromBlockingMacro w macro params ni = mkDeclsFromCPPMacro includePaths defs "ae-blocking-parser.h" ni macro params
>       where includePaths = includes w
>             defs = defines w

> mkDeclsFromBlocking :: String -> [String] -> NodeInfo -> WalkerT [CDecl]
> mkDeclsFromBlocking m p n = do
>       w <- get
>       return $ mkDeclsFromBlockingMacro w m p n

> mkErrorCBPostHandler :: Walker -> String -> ReturnType -> String -> NodeInfo -> [CStat]
> mkErrorCBPostHandler w fname retType ctlName ni =
>       let location = show $ posRow $ posOfNode ni
>       in mkStmtFromBlockingMacro w "AE_MK_POSTCB_STMT" [fname, (returnToString retType), ctlName, location] ni

> mkErrorPostHandler :: Walker -> String -> ReturnType -> String -> NodeInfo -> [CStat]
> mkErrorPostHandler w fname retType ctlName ni =
>       let location = show $ posRow $ posOfNode ni
>       in mkStmtFromBlockingMacro w "AE_MK_POST_STMT" [fname, (returnToString retType), ctlName, location] ni

> mkPBreakStmts :: String -> NodeInfo -> WalkerT ([CDecl], [CStat])
> mkPBreakStmts prefix ni = do
>       let location = show $ posRow $ posOfNode ni
>       fname <- getFileName
>       ds <- mkDeclsFromBlocking "AE_MK_PBREAK_DECLS" [] ni
>       ss <- mkStmtFromBlocking "AE_MK_PBREAK_STMTS" [prefix, fname, location] ni
>       return (ds, ss)

After a blocking operation within a pbranch, we need to check if the pbranch has been cancelled,
either from a pbreak, or from an external cancel call of the entire blocking function.
 
> mkPBranchCallbackStartStmts :: String -> String -> NodeInfo -> WalkerT ([CDecl], [CStat])
> mkPBranchCallbackStartStmts prefix funName ni = do
>     ds <- mkDeclsFromBlocking "AE_MK_PBRANCH_CB_START_DECLS" [prefix, (mkStructCtlName funName)] ni
>     ss <- mkStmtFromBlocking "AE_MK_PBRANCH_CB_START_STMTS" [prefix] ni
>     return (ds, ss)

> mkPBranchDeleteStmts :: NodeInfo -> WalkerT [CStat]
> mkPBranchDeleteStmts ni = mkStmtFromBlocking "AE_MK_PBRANCH_DELETE_STMTS" [] ni

> mkPBranchDoneStmts :: String -> String -> NodeInfo -> WalkerT [CStat]
> mkPBranchDoneStmts prefix funName ni =
>       mkStmtFromBlocking "AE_MK_PBRANCH_DONE_STMTS" [] ni

> mkPBranchCBDoneStmts :: Walker -> BlockingContext -> NodeInfo -> [CStat]
> mkPBranchCBDoneStmts w b ni =
>       mkStmtFromBlockingMacro w "AE_MK_PBRANCH_CB_DONE_STMTS" [] ni

> mkPBranchPostDoneStmts :: Walker -> BlockingContext -> NodeInfo -> [CStat]
> mkPBranchPostDoneStmts w pb ni = 
>       mkStmtFromBlockingMacro w "AE_MK_PBRANCH_POST_DONE_STMTS" [getPBranchId pb] ni

> mkDoneStmts :: NodeInfo -> WalkerT [CStat]
> mkDoneStmts ni = mkStmtFromBlocking "AE_MK_CB_DONE_STMTS" [] ni

> mkDoneCtlSetStmt :: String -> NodeInfo -> WalkerT [CStat]
> mkDoneCtlSetStmt p ni = mkStmtFromBlocking "AE_MK_CB_DONE_CTL_SET_STMTS" [p] ni

> mkPWaitInitStmts :: String -> String -> NodeInfo -> WalkerT [CStat]
> mkPWaitInitStmts pwaitParams prefix ni = 
>       mkStmtFromBlocking "AE_MK_PWAIT_INIT_STMTS" [pwaitParams, prefix] ni

> mkPWaitFinishStmts :: BlockingContext -> String -> WalkerT [CStat]
> mkPWaitFinishStmts pw prefix = do
>       let ni = getNI pw
>       mkStmtFromBlocking "AE_MK_PWAIT_FINISH_STMTS" [prefix, getPWaitId pw] ni

> mkPWaitNotDoneStmts :: BlockingContext -> String -> WalkerT [CStat]
> mkPWaitNotDoneStmts pw prefix = do
>       let ni = getNI pw
>       mkStmtFromBlocking "AE_MK_PWAIT_NOT_DONE_STMTS" [prefix, getPWaitId pw] ni

> mkPostFunInitStmts :: String -> NodeInfo -> WalkerT [CStat]
> mkPostFunInitStmts name ni =
>       mkStmtFromBlocking "AE_MK_POST_FUN_INIT_STMTS" [name] ni

> mkPBranchPostStmts :: BlockingContext -> String -> String -> String -> String -> [CStat] -> NodeInfo -> WalkerT [CStat]
> mkPBranchPostStmts pb pwaitName prefix parentPrefix fname stmts ni = do
>       let location = show $ posRow $ posOfNode ni
>       fileName <- getFileName
>       ds <- mkDeclsFromBlocking "AE_MK_PBRANCH_POST_DECLS" [mkStructCtlName fname, prefix] ni
>       ss <- mkStmtFromBlocking  "AE_MK_PBRANCH_POST_STMTS"
>                                 [pwaitName, prefix, parentPrefix, mkStructCtlName fname, fileName, location, getPBranchId pb] ni
>	return [mkCompoundWithDecls Nothing ds (ss ++ stmts) ni]

> mkBlockingParamsParallelFields :: String -> Bool -> NodeInfo -> WalkerT [CDecl]
> mkBlockingParamsParallelFields _ False _ = return []
> mkBlockingParamsParallelFields name True ni = mkDeclsFromBlocking "AE_MK_PARENT_POINTER_DECL" [name] ni

> mkBlockingParams :: ReturnType -> [CDecl] -> NodeInfo -> WalkerT [CDecl]
> mkBlockingParams ret params ni = mkDeclsFromBlocking "AE_MK_BLOCKING_PARAMS_DECLS" [returnToString $ ret] ni
 
> mkBlockingParamsForStruct :: CFunDef -> WalkerT [CDecl]
> mkBlockingParamsForStruct fdef = do
>	let ni = nodeInfo fdef
>           ret = getFunDefReturn fdef
>       ds <- mkDeclsFromBlocking "AE_MK_BLOCKING_PARAMS_FOR_STRUCT_DECLS" [] ni
>       return $ ds
>                ++ (mkRetParam ret ni) -- optional return parameter:  [__ret_type __ae_ret]
>                ++ (mkCBParam ret ni) -- callback parameter:  void (*callback)(void *up [, __ret_type __ae_ret])

> mkBlockingParamsForPost :: (CTypeSpec, [CDerivedDeclr]) -> NodeInfo -> WalkerT [CDecl]
> mkBlockingParamsForPost retType ni = do
>	ds <- mkDeclsFromBlocking "AE_MK_BLOCKING_PARAMS_FOR_POST_DECLS" [] ni
>       return $ (mkCBParam retType ni) -- callback parameter:  void (*callback) (void *user_ptr, [, __ret_type __ae_ret])
>                ++ ds
>                
> mkPostFunDecls :: String -> NodeInfo -> WalkerT [CDecl]
> mkPostFunDecls name ni = mkDeclsFromBlocking "AE_MK_POST_FUN_DECLS" [name] ni

> mkPostFunFinished :: NodeInfo -> WalkerT [CStat]
> mkPostFunFinished ni = mkStmtFromBlocking "AE_MK_POST_FUN_FINISHED_STMTS" [] ni

> mkBlockingParamsFromFunPtrDecl :: CDecl -> WalkerT [CDecl]
> mkBlockingParamsFromFunPtrDecl c@(CDecl _ declrs ni) = do
>	let retType = getReturn c
>	ds <- mkDeclsFromBlocking "AE_MK_BLOCKING_PARAMS_FUN_PTR_DECLS" [] ni
>       return $ (mkCBParam retType ni) ++ ds

> mkPostRetType :: NodeInfo -> WalkerT CTypeSpec
> mkPostRetType ni = do 
>       prd <- mkPostRetDecl ni
>       return $ fst $ getReturn prd

> mkPostRetDecl :: NodeInfo -> WalkerT CDecl
> mkPostRetDecl ni = liftM head $ mkDeclsFromBlocking "AE_MK_RET_DECL" [] ni

> getLastStmtInCompounds :: CStat -> CStat
> getLastStmtInCompounds (CCompound _ bitems _) = assert (isJust lastStmt) r
>       where lastStmt = getBlockStmt $ last bitems
>             r = getLastStmtInCompounds $ fromJust lastStmt
> getLastStmtInCompounds s = s

> checkForReturn :: CFunDef -> WalkerT CFunDef
> checkForReturn f@(CFunDef specs declarator decls c@(CCompound idents stmts cni) ni)
>	| (isVoidReturn $ getFunDefReturn f) && (isJust $ getBlockStmt $ last stmts) && (not $ isReturnStmt $ fromJust $ getBlockStmt $ last stmts) = 
>		return $ (CFunDef specs declarator decls (CCompound idents (stmts ++ [CBlockStmt (CReturn Nothing ni)]) cni) ni)
>	| (not $ isVoidReturn $ getFunDefReturn f) && (not $ isReturnStmt $ getLastStmtInCompounds c) = do
>		invalid ("The blocking function: " ++ (getFunDefName f) ++ " does not have a final return statement\n") ni
>		return f
>	| otherwise = return f

> getLocals :: BlockingContext -> [CDecl]
> getLocals (FunContext funDef _ _ _ _) = getFuncLocalDecls funDef
> getLocals b = getLocals $ getParent b

> addLoopBreakLabel :: BlockingContext -> [CStat] -> [CStat]
> addLoopBreakLabel b stmts = [mkLabel (mkLoopBreakLabelStr b) stmts $ getNI b]

> isBlockingField :: CDecl -> Bool
> isBlockingField (CDecl s i n) = any isBlockingSpec s

> mkAnonStructName :: NodeInfo -> String
> mkAnonStructName ni = 
>       "anonstruct_" ++ (show $ posRow $ posOfNode $ ni) ++ "_" ++
>	(show $ posColumn $ posOfNode $ ni)

> lookupAndRegBlocking :: Maybe String -> CDecl -> WalkerT ()
> lookupAndRegBlocking sname d@(CDecl s i n) = do
>       let typeName = getTypeName d
>           t = if isJust typeName then fromJust typeName else mkAnonStructName $ nodeInfo d
>       assert (isJust sname) return ()
>       lookupAndRegisterBlockingStruct (fromJust sname)
>			                (identToString $ getCDeclName d)
>			                t

> regBlockingStruct :: Maybe String -> CDecl -> WalkerT ()
> regBlockingStruct sname d = do
>       assert (isJust sname) return ()
>       registerBlockingStruct (fromJust sname)
>                              (identToString $ getCDeclName d)
>                              (splitFunDecl d)

> registerStruct :: CDecl -> Maybe String -> WalkerT ()
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

> registerTypedefStruct :: CDecl -> Maybe String -> WalkerT ()
> registerTypedefStruct d altSname = do
>	let t = getStructTypeDefInfo d
>	    (Just (sn, tname)) = t
>	    sname = if isJust sn then sn else altSname
>	assert (isJust t && isJust sname) return ()
> 	lookupAndRegisterBlockingTypedef (fromJust sname) tname

> registerTypedefTypedef :: CDecl -> WalkerT ()
> registerTypedefTypedef d = do
>       let t = getTypeDefTypeDefInfo d
>           (Just (pn, tname)) = t
>       assert (isJust t) return ()
>       lookupAndRegisterBlockingTypedef pn tname

> registerBlockingDecl :: Bool -> CExtDecl -> WalkerT [CExtDecl]
> registerBlockingDecl ph e@(CDeclExt d@(CDecl specifiers initdecls ni))

>     | isTypeDefOfInlineStruct d = do
>	    let name = mkAnonStructName (nodeInfo d)
>	    registerStruct d (Just name)
>	    registerTypedefStruct d (Just name)
>	    if ph then
>		return [e]
>	      else return []

>     | isTypeDefOfPredefStruct d = do
>	    registerTypedefStruct d Nothing
>	    if ph then
>		return [e]
>	      else return []

>     | isTypeDefOfTypeDef d = do
>           registerTypedefTypedef d
>           if ph then
>               return [e]
>             else return []

Register structs containing blocking function pointers as fields:  struct a { __blocking int (*myfun) (); }
Or fields that are structs with blocking function pointers: struct b { struct a mya; }
etc. etc.

>     | isStructDecl d = do
>	    registerStruct d Nothing
>	    if ph then 
>		liftM ((:[]) . CDeclExt) $ everywhereM (mkM translateBlockingFunParam) d
>	      else return []

Register global variable declarations.  This is necessary to map variable names to types (structs, typedefs, etc.)
allowing us to determine if a function pointer is being called within a particular variable.

>     | isVarDecl d = do
>		addGlobals [d]
>		let name = mkAnonStructName $ nodeInfo d
>		when (isStructDecl d) (registerStruct (removeVarFromDecl d) (Just name))
>		if ph then return [e] else return []

Register blocking function declarations:  __blocking int myfun();

>     | isFunDecl d && (any isBlockingSpec specifiers) = do 
>	  let function = splitFunDecl (CDecl specifiers initdecls ni)
>         registerBlocking function
>         pDecl <- mkPostDecl function specifiers
>         -- pPtrDecl <- mkPostPtrDecl True function specifiers
> 	  return $ [CDeclExt pDecl]

Transform function declarations that take blocking function pointers as parameters:  int myfun(__blocking int (*fnp)(void));
We only need to do the transformation here to get the type signature right, we don't generate anything else.

>     | isFunDecl d = do
>	  if ph then 
>	      liftM ((:[]) . CDeclExt) $ everywhereM (mkM translateBlockingFunParam) d
>	    else return []

         let (fname, (rtype, derived), ps) = splitFunDecl d
         newps <- mapM translateBlockingFunParam ps
         let newd = mkFunDeclWithDeclSpecs fname rtype derived specifiers newps
         if ph then return [CDeclExt newd]
           else return []
        
> registerBlockingDecl True c = return [c]
> registerBlockingDecl False c = return []
 
Registers all the blocking declarations present in the CTranslUnit

> registerBlockingFunDecls :: CTranslUnit -> WalkerT CTranslUnit
> registerBlockingFunDecls (CTranslUnit decls ni) = do
>	newdecls <- liftM concat $ sequence $ map (registerBlockingDecl True) decls
>	return $ CTranslUnit newdecls ni

> getBlockingHeaderDecls :: CTranslUnit -> WalkerT [([CExtDecl], CExtDecl)]
> getBlockingHeaderDecls (CTranslUnit decls ni) = do
>	newdecls <- sequence $ map (registerBlockingDecl False) decls
>	return $ zip newdecls decls

Registers blocking function pointers passed to a blocking function.

While translating a blocking function, we need to keep a registry of all the
variable names for blocking function pointers passed to the blocking function, so
that if one of them gets called, we can do translation at that point.  For example:

__blocking int myblockingfun(int a, __blocking int (*myblockingfunptr)(int b));

In this example, the myblockingfunptr variable refers to a blocking function somewhere, which
gets called in the blocking function myblockingfun.

> registerBlockingFunParam :: CDecl -> WalkerT ()
> registerBlockingFunParam (CDecl specifiers initdecls ni)
>	| (any isBlockingSpec specifiers) = do
>		let retType = getTypeSpec specifiers
>		    (Just declr, _, _) = head initdecls
>		    (CDeclr (Just f) derivedDeclrs _ _ _)  = declr
>		    fname = identToString f
>		    (Just funDeclr) = find isFunDeclr derivedDeclrs
>		    params = getCFunDeclrParams funDeclr
>	        registerLocalBlocking (fname, (retType, derivedDeclrs), params)
>	| otherwise = return ()

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
>       ret <- mkPostRetType ni
>	let specs = filter (not . isBlockingSpec) specifiers 	
>	    (Just declr, _, _) = head initdecls
>	    (CDeclr (Just (Ident fname _ _)) _ _ _ _) = declr
>	    funDeclr = getDerivedFun declr
>	    params = getCFunDeclrParams funDeclr
>	    postParams = params ++ bParams
>	return $ mkFunPtrDecl fname ret postParams

>   | otherwise = return fptr

> removeVoid :: [CDecl] -> [CDecl]
> removeVoid decls = if justVoid decls then [] else decls

> isBlockingFunDef :: CFunDef  -> Bool
> isBlockingFunDef (CFunDef specifiers declarator decls stmt ni) = any isBlockingSpec specifiers
 
> isBlockingFunDecl :: CDecl -> Bool
> isBlockingFunDecl (CDecl specs _ _) = any isBlockingSpec specs

> replaceBreak :: CStat -> CStat -> CStat
> replaceBreak replaceStmt bstmt 
>	| isJust $ isBreak bstmt = replaceStmt
>	| otherwise = bstmt

> swapBreak :: CStat -> [CStat] -> [CStat]
> swapBreak swapStmt stmts = map (everywhere (mkT $ replaceBreak swapStmt)) stmts

> replacePBreak :: CStat -> CStat -> CStat
> replacePBreak replaceStmt bstmt
>	| isJust $ isPBreak bstmt = replaceStmt
>	| otherwise = bstmt

> swapPBreak :: CStat -> [CStat] -> [CStat]
> swapPBreak swapStmt stmts = map (everywhere (mkT $ replacePBreak swapStmt)) stmts

> containsBreak :: [CStat] -> Bool
> containsBreak stmts = 
>	let hasBreak = everything orElse (mkQ Nothing isBreak)
>	in (any (isJust . hasBreak) stmts)

> containsPBreak :: [CStat] -> Bool
> containsPBreak stmts =
>	let hasPBreak = everything orElse (mkQ Nothing isPBreak)
>	in (any (isJust . hasPBreak) stmts)

> containsReturn :: [CStat] -> Bool
> containsReturn stmts =
>       let hasReturn = everything (||) (mkQ False isReturnStmt)
>       in (any  hasReturn stmts)

> getBINodeInfo :: CBlockItem -> NodeInfo
> getBINodeInfo (CBlockDecl (CDecl _ _ ni)) = ni

> invalid :: String -> NodeInfo -> WalkerT ()
> invalid msg ni = do
>	fstr <- getFilePosStr ni
>	error (fstr ++ ":  Invalid aesop usage: " ++ msg)

> gspWarn :: String -> NodeInfo -> WalkerT ()
> gspWarn msg ni = do
>	fstr <- getFilePosStr ni
>	putStrLnW $ fstr ++ ": warning: " ++ msg

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
>	binit <- containsBlockingCall (CExpr init ni)
>	when binit $ invalid "init expression in for loop cannot contain a blocking call" ni
>	bexpr1 <- containsBlockingCall (CExpr expr1 ni)
>	when bexpr1 $ invalid "conditional expression in for loop cannot contain a blocking call" ni
>	bexpr2 <- containsBlockingCall (CExpr expr2 ni)
>	when bexpr2 $ invalid "update expression in for loop cannot contain a blocking call" ni
>	when (not $ isCompound stmts)
>	     $ invalid "blocking for loop requires curly brackets" ni
>	when (fst $ stmtsHaveDecl stmts)
>	     $ invalid "blocking for loop cannot contain local declarations" (snd $ stmtsHaveDecl stmts)
>	return ()

> isValidBlockingWhile :: CStat -> WalkerT ()
> isValidBlockingWhile (CWhile expr stmts isDoWhile ni) = do
>	bexpr <- containsBlockingCall (CExpr (Just expr) ni)
>	when bexpr $ invalid "conditional expression in while loop cannot contain a blocking call" ni
>	when (not $ isCompound stmts)
>	     $ invalid "blocking while loop requires curly brackets" ni
>	when (fst $ stmtsHaveDecl stmts)
>		$ invalid "blocking while loop cannot contain local declarations" (snd $ stmtsHaveDecl stmts)
>	return ()


> isValidBlockingIf :: CStat -> WalkerT ()
> isValidBlockingIf (CIf expr ifStmts elseStmts ni) = do
>	bexpr <- containsBlockingCall (CExpr (Just expr) ni)
>	when bexpr $ invalid "if expression cannot contain a blocking call" ni
>	when (not $ isCompound ifStmts)
>	    $ invalid "blocking if requires curly brackets" ni
>	when (fst $ stmtsHaveDecl $ ifStmts)
>	    $ invalid "if block with blocking calls cannot contain local declarations" (snd $ stmtsHaveDecl $ ifStmts)
>	when ((isJust elseStmts) && (isCompound $ fromJust elseStmts) && (fst $ stmtsHaveDecl $ fromJust elseStmts))
>	    $ invalid "else block with blocking calls cannot contain local declarations" (snd $ stmtsHaveDecl $ fromJust elseStmts)
>	return ()

> isValidPWait :: CStat -> WalkerT ()
> isValidPWait (CPWait stmts ni) = do
>       when (containsReturn [stmts])
>           $ invalid "return not allowed within pwait/pbranch context" ni
>       return ()

> getBlockingCallName :: CExpr -> WalkerT (Maybe String)
> getBlockingCallName (CCall e args _) =  do
>	res <- lookupBlocking e 
>	case res of { (Just (bname, _, _)) -> return $ Just bname ; Nothing -> return Nothing }
> getBlockingCallName _ = return Nothing

> findBlockingCall :: CStat -> WalkerT (Maybe String)
> findBlockingCall stmt = 
> 	everything orElseMMaybe (mkQ (return Nothing) getBlockingCallName) $ stmt

> getBlockingCallExpr :: CExpr -> WalkerT (Maybe CExpr)
> getBlockingCallExpr call@(CCall e _ _) = do
>	res <- lookupBlocking e
>	if isJust res then return $ Just call else return Nothing
> getBlockingCallExpr _ = return Nothing

> findBlockingCallExpr :: CStat -> WalkerT (Maybe CExpr)
> findBlockingCallExpr stmt = 
>	everything orElseMMaybe (mkQ (return Nothing) getBlockingCallExpr) $ stmt

Split a statement with a blocking function call into a triple of (return expression, function name, parameter expressions).  This
function replaces the blocking call with a variable "ret", and returns that as the return expression.

> replaceCallWithVar :: String -> CExpr -> WalkerT CExpr
> replaceCallWithVar varname call@(CCall e _ ni) = do
>	res <- lookupBlocking e
>	if isJust res then return (mkVar varname ni) else return call
> replaceCallWithVar _ c = return c

> findAndReplaceBlockingCall :: String -> CStat -> WalkerT CStat

> -- if the statement is just the blocking call (the return value is ignored), we don't replace
> findAndReplaceBlockingCall s (CExpr (Just call@(CCall e _ _)) ni) = do
>	res <- lookupBlocking e 
>	if isJust res then return (mkCompoundStmt (Just "__ae_result_expr") [] ni) else do
>		expr <- replaceCallWithVar s call
>		return (CExpr (Just expr) ni)

> findAndReplaceBlockingCall varname stmt = everywhereM (mkM $ replaceCallWithVar varname) stmt

From a blocking function definition, Construct an external declaration parameters struct

> mkStructCtlName :: String -> String
> mkStructCtlName name = name ++ "_ctl"

> mkStructParamsName :: String -> String
> mkStructParamsName name = name ++ "_params"

> mkStructPWaitName :: String -> String -> String
> mkStructPWaitName name id = name ++ "_" ++ id ++ "_params"

> mkStructPBranchName :: String -> String -> String -> String
> mkStructPBranchName name pwaitName id = pwaitName ++ "_" ++ name ++ "_" ++ id ++ "_params"

> mkPrivateParamPWaitName :: String -> String -> String
> mkPrivateParamPWaitName name id = name ++ "_" ++ id ++ "_private_params"

> mkSharedParamPWaitName :: String -> String -> String
> mkSharedParamPWaitName name id = name ++ "_" ++ id ++ "_shared_params"

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
>	let decls = getFunDefParams funDef
>	    ni = nodeInfo funDef
>       bParams <- sequence $ map translateBlockingFunParam $ removeVoid decls
>	let params = bParams ++ (map removeInitFromDecl $ getFunLocalDeclarations funDef)
>	return $ genStructExtDecl (mkStructParamsName $ getFunDefName funDef) params ni

> removeInits :: (String, [CDecl]) -> (String, [CDecl])
> removeInits (pwaitName, pwaitDecls) = (pwaitName, (map removeInitFromDecl pwaitDecls))

> mkPWaitExtDecl :: (String -> String) -> (String, [CDecl]) -> NodeInfo -> CExtDecl
> mkPWaitExtDecl snamef (pwaitName, pwaitDecls) ni =
>	genStructExtDecl (snamef pwaitName) pwaitDecls ni

> mkBlockingPBranchStructs :: CFunDef -> (String, BlockingContext) -> WalkerT [CExtDecl]
> mkBlockingPBranchStructs funDef (pwaitName, pwaitCtx) = do
>	let fname = getFunDefName funDef
>	    ni = nodeInfo funDef
>	    pbranches = getPBranches pwaitCtx
>	    pbranchDecls = zip (map getPBranchId pbranches) (map getPBranchDecls pbranches)
>	    params = map removeInits pbranchDecls
>	return $ map (\p -> mkPBranchExtDecl fname pwaitName p ni) params

> addPBranchParams :: CFunDef -> BlockingContext -> (String, [CDecl]) -> (String, [CDecl])
> addPBranchParams funDef pwaitCtx (pwaitId, pWaitDecls) = 
>	let fname = getFunDefName funDef
>	    ni = nodeInfo funDef
>	    pbranchDecls = 
>             map (\p -> mkStructDecl
>		           (mkStructPBranchName fname pwaitId (getPBranchId p))
>			   (mkParamPBranchName (getPBranchId p)) ni) $ getPBranches pwaitCtx
>	in (pwaitId, (pWaitDecls ++ pbranchDecls))

> mkBlockingPWaitStructs :: CFunDef -> BlockingContext -> WalkerT [CExtDecl]
> mkBlockingPWaitStructs funDef bctx
>    | containsPWait bctx = do
>        let fname = getFunDefName funDef
>	     ni = nodeInfo funDef
>            pwaits = getPWaits bctx
>            ids = map getPWaitId pwaits
>            privatePWaitDecls = zip ids (map getPWaitPrivateDecls pwaits) -- :: [(pwait_id, [private_decl])]
>            sharedPWaitDecls = zip ids (map getPWaitSharedDecls pwaits) -- :: [(pwait_id, [shared_decl])]
>            private = map removeInits privatePWaitDecls -- :: [(pwait_id, [private_decl_no_inits])]
>            shared = map removeInits sharedPWaitDecls -- :: [(pwait_id, [shared_decl_no_inits])]
>	     sharedWithPBranch = zipWith (addPBranchParams funDef) pwaits shared -- :: [(pwait_id, [shared_decl_no_inits] ++ [pbranch_struct_decl])]
>            privatePWaitStructs = map (\p -> mkPWaitExtDecl (mkPrivateParamPWaitName fname) p ni) private
>            sharedPWaitStructs = map (\p -> mkPWaitExtDecl (mkSharedParamPWaitName fname) p ni) sharedWithPBranch
>            pwaitParams = map (\id -> (id, [mkStructDecl (mkPrivateParamPWaitName fname id) mkPrivateName ni,
>                                            mkStructDecl (mkSharedParamPWaitName fname id) mkSharedName ni,
>                                            mkStructPtrDecl (mkSharedParamPWaitName fname id) mkSharedPtrName ni])) ids  -- [(pwait_id, [private_struct, shared_struct])]
>            pwaitStructs = map (\p -> mkPWaitExtDecl (mkStructPWaitName fname) p ni) pwaitParams
>	     pwaitNamePairs = zip (map getPWaitId pwaits) pwaits
>	 pbranchStructs <- liftM concat $ mapM (mkBlockingPBranchStructs funDef) pwaitNamePairs
>	 return $ pbranchStructs ++ privatePWaitStructs ++ sharedPWaitStructs ++ pwaitStructs
>    | otherwise = return []

> mkPBranchExtDecl :: String -> String -> (String, [CDecl]) -> NodeInfo -> CExtDecl
> mkPBranchExtDecl fname pwaitName (pbranchName, pbranchDecls) ni =
>	genStructExtDecl (mkStructPBranchName fname pwaitName pbranchName) pbranchDecls ni

> mkBlockingCtlStruct :: CFunDef -> BlockingContext -> Bool -> WalkerT CExtDecl
> mkBlockingCtlStruct funDef bctx pblock = do
>	let ni = nodeInfo funDef
>	    fname = getFunDefName funDef
>	    ctlName = mkStructCtlName fname
>	    spName = mkStructParamsName fname
>	    fieldsDecl = mkStructDecl spName "fields" ni
>	    paramsDecl = mkStructPtrDecl spName "params" ni
>	    pwaitDecls = map (\p -> mkStructDecl (mkStructPWaitName fname (getPWaitId p))
>						 (mkPWaitName (getPWaitId p)) ni)
>			     $ getPWaits bctx

>	bparams <- mkBlockingParamsForStruct funDef
>	pparams <- mkBlockingParamsParallelFields (getFunDefName funDef) pblock ni

>	return $ genStructExtDecl ctlName 
>			          (bparams ++ pparams ++
>			           [fieldsDecl, paramsDecl] ++ pwaitDecls) ni

> mkPostFunName :: String -> String -> String
> mkPostFunName ctlName name = name

Each callback function defined for a given blocking function must have a unique name.  We use:

<bfun>_<bcall>_<bcallrow>_<bcallcol>_callback

<bfun> is the name of the blocking function being translated
<bcall> is the name of the blocking call being made within the function
<bcallrow> is the row (line number in the file) where the blocking call is being made
<bcallcol> is the column in the file where the blocking call is being made

> mkCallbackFunName :: String -> String -> NodeInfo -> String
> mkCallbackFunName funName callName (NodeInfo p _ _) = funName ++ "_" ++ callName ++ "_" ++ (show $ posRow p) ++ "_" ++ (show $ posColumn p) ++ "_callback"

> mkCallbackRetParam :: String -> NodeInfo -> String
> mkCallbackRetParam callName (NodeInfo p _ _) = callName ++ "_" ++ (show $ posRow p) ++ "_" ++ (show $ posColumn p) ++ "_ret"

> isVarIn :: [Ident] -> CExpr -> Bool
> isVarIn locals (CVar name _) = name `elem` locals
> isVarIn _ _ = False

> addParamsPtrPrefixToExpr :: String -> String -> [Ident] -> CExpr -> CExpr
> addParamsPtrPrefixToExpr ctlPrefix paramsPrefix locals expr
>	| isVarIn locals expr = addStructPtrPrefix ctlPrefix paramsPrefix expr
>	| otherwise = expr

> addParamsPtrPrefixes :: String -> String -> [Ident] -> CStat -> CStat
> addParamsPtrPrefixes ctlPrefix paramsPrefix idents stmt = everywhere (mkT $ addParamsPtrPrefixToExpr ctlPrefix paramsPrefix idents) stmt

> addParamsPrefixToExpr :: String -> String -> [Ident] -> CExpr -> CExpr
> addParamsPrefixToExpr ctlPrefix paramsPrefix locals expr
>	| isVarIn locals expr = addStructPrefix ctlPrefix paramsPrefix expr
>	| otherwise = expr

> addParamsPrefixes :: String -> String -> [Ident] -> CStat -> CStat
> addParamsPrefixes ctlPrefix paramsPrefix idents stmt = everywhere (mkT $ addParamsPrefixToExpr ctlPrefix paramsPrefix idents) stmt

> addParams2PrefixToExpr :: String -> String -> String -> [Ident] -> CExpr -> CExpr
> addParams2PrefixToExpr ctlPrefix p1Prefix p2Prefix locals expr
>	| isVarIn locals expr = addStructPrefixPrefix ctlPrefix p1Prefix p2Prefix expr
>	| otherwise = expr

> addParams2Prefixes :: String -> String -> String -> [Ident] -> CStat -> CStat
> addParams2Prefixes ctlPrefix p1Prefix p2Prefix idents stmt =
>	everywhere (mkT $ addParams2PrefixToExpr ctlPrefix p1Prefix p2Prefix idents) stmt

> addParams2PtrPrefixToExpr :: String -> String -> String -> [Ident] -> CExpr -> CExpr
> addParams2PtrPrefixToExpr ctlPrefix p1Prefix p2Prefix locals expr
>	| isVarIn locals expr = addStructPtrPrefixPrefix ctlPrefix p1Prefix p2Prefix expr
>	| otherwise = expr

> addParams2PtrPrefixes :: String -> String -> String -> [Ident] -> CStat -> CStat
> addParams2PtrPrefixes ctlPrefix p1Prefix p2Prefix idents stmts =
>       everywhere (mkT $ addParams2PtrPrefixToExpr ctlPrefix p1Prefix p2Prefix idents) stmts

trLocals finds the local declarations for a blocking function context, and translates
the variables used in the statement, into variables prefixed with the ctl and parameter structures.
So for example, if a, b, c and d are local parameters, the statement:

runfun(a, b, c, d);

gets translated to:

runfun(ctl->fields.a, ctl->fields.b, ctl->fields.c, ctl->fields.d);

> trPBranchLocals :: String -> BlockingContext -> CStat -> CStat
> trPBranchLocals ctlPrefix bctx stmt
>    | hasPBranchAncestor bctx =
>	let pbranchCtx = getPBranchAncestor bctx
>	    pwaitCtx = getPWaitAncestor bctx
>	    locals = join $ map getCDeclNames $ getPBranchDecls pbranchCtx
>	in (addParams2PtrPrefixes ctlPrefix
>			          ((mkPWaitName (getPWaitId pwaitCtx)) ++ "." ++ mkSharedPtrName)
>		                  (mkParamPBranchName (getPBranchId pbranchCtx))
>			          locals) stmt
>    | otherwise = stmt

> trPWaitLocals :: String -> BlockingContext -> CStat -> CStat
> trPWaitLocals ctlPrefix bctx stmt 
>    | hasPWaitAncestor bctx =
>	let pwaitCtx = getPWaitAncestor bctx
>	    privateLocals = join $ map getCDeclNames $ getPWaitPrivateDecls pwaitCtx
>           sharedLocals = join $ map getCDeclNames $ getPWaitSharedDecls pwaitCtx
>           privatePrefixedStmt = addParamsPrefixes ctlPrefix (mkPrivatePWaitName (getPWaitId pwaitCtx)) privateLocals stmt
>           allPrefixedStmt = addParamsPtrPrefixes ctlPrefix (mkSharedPWaitName (getPWaitId pwaitCtx)) sharedLocals privatePrefixedStmt
>	in allPrefixedStmt
>    | otherwise = stmt

> trFunLocals :: String -> BlockingContext -> CStat -> CStat
> trFunLocals ctlPrefix bctx stmt = 
> 	let locals = join $ map getCDeclNames $ getLocals bctx
>	in (addParamsPtrPrefixes ctlPrefix "params" locals) stmt

> trLocals :: String -> BlockingContext -> CStat -> CStat
> trLocals s b = (trFunLocals s b) . (trPWaitLocals s b) . (trPBranchLocals s b)

> trExpr :: String -> BlockingContext -> Maybe CExpr -> Maybe CExpr
> trExpr ctlPrefix bctx expr = 
>	let (CExpr resExpr _) = trLocals ctlPrefix bctx (CExpr expr $ getNI bctx)
>	in resExpr

translateForCB turns statements that use local variables to statements that use
fields within the appropriate parameter structure, and converts return statements
to calling the appropriate callback

> translateForCB :: BlockingContext -> [CStat] -> WalkerT [CStat]
> translateForCB b stmts = do
>	p <- getPrefix
>       endStmts <- mkStmtFromBlocking "AE_MK_END_OF_BLOCKING" [getParentName $ getFunContext b] (getNI b)
>	transformExits <- getTransExit
>	return $ map ((trLocals p b) . (transformExits (mkCompoundStmt Nothing endStmts (getNI b)))) stmts

> swapReturnWithCallback :: CStat -> CStat -> CStat
> swapReturnWithCallback endStmt (CReturn retexpr ni) = compoundStmt
>	where retparam = if isNothing retexpr then [] else [fromJust retexpr]
>	      compoundStmt = mkCompoundStmt Nothing (endStmt :
>               [CExpr (Just (CCall (CMember (mkVar "ctl" ni)
>				             (newIdent "callback" ni) True ni)
>		            	    ([constructExprFromC ni "ctl->user_ptr"] ++ retparam) ni)) ni,
>		 mkStmtFromC ni "free(ctl);",
>		 mkStmtFromC ni "return;"]) ni

> swapReturnWithCallback e c = c

> transformFuncReturnStmts :: CStat -> CStat -> CStat
> transformFuncReturnStmts endStmt stmt =
>	everywhere (mkT $ swapReturnWithCallback endStmt) stmt

> swapPostReturnWithCallback :: CStat -> CStat -> CStat
> swapPostReturnWithCallback endStmt (CReturn retexpr ni) = compoundStmt
>	where retparam = if isNothing retexpr then [] else [fromJust retexpr]
>	      compoundStmt = mkCompoundStmt Nothing (endStmt :
>                [(CExpr (Just (CCall (CMember (mkVar "ctl" ni)
>				     	      (newIdent "callback" ni) True ni)
>		            	     ([constructExprFromC ni "ctl->user_ptr"] ++ retparam) ni)) ni),
>		  mkStmtFromC ni "free(ctl);",
>		  mkStmtFromC ni "return TRITON_SUCCESS;"]) ni
> swapPostReturnWithCallback e c = c

> transformPostFuncReturnStmts :: CStat -> CStat -> CStat
> transformPostFuncReturnStmts endStmt stmt =
>	everywhere (mkT $ swapPostReturnWithCallback endStmt) stmt

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

> mkPostFunInit :: String -> BlockingContext -> [CDecl] -> [CDecl] -> WalkerT [CStat]
> mkPostFunInit pname bctx fdecls pdecls = do
>       -- default initialize statements for params
>	inits <- mkPostFunInitStmts pname (getNI bctx)
>       startStmts <- mkStmtFromBlocking "AE_MK_START_OF_BLOCKING" [getParentName $ getFunContext bctx] (getNI bctx)

>       -- get the function parameters, and set the parameters in the params struct to the values passed in
>       let funParams = (map ((\name -> addStructPtrToAssign "ctl" "params" name (CVar name $ getNI bctx)) . getCDeclName) $ 
>	                     concat (map splitDecls fdecls))

>       -- get the init statements from local declarations, and add the params-> prefix
>           localDecls = (map (trLocals pname bctx) (getInitsFromDecls pdecls))

>       return $ inits ++ funParams ++ localDecls ++ startStmts

> generateNextPostStmts :: Maybe BlockingContext -> WalkerT ([CStat])
> generateNextPostStmts b = do
>	generatePostStmts b []

> generateFirstPostStmts :: BlockingContext -> WalkerT ([CStat])
> generateFirstPostStmts b@(FunContext _ (first:_) _ _ _) = do
>	let initStmts = nbStmtsBefore first 
>	    ctxInitStmts = getCtxInitStmts first
>	tlInitStmts <- translateForCB b (initStmts ++ ctxInitStmts)
>	setErrorWriter mkErrorPostHandler
>	setPBDone mkPBranchPostDoneStmts
>	setTransExit transformPostFuncReturnStmts
>	postStmts <- generatePostStmts (Just first) []
>	return $ tlInitStmts ++ postStmts

> generateFirstPostStmts b@(FunContext f _ _ _ _) = do
>	let fname = getFunDefName f
>	    stmts = getFunStmts f
>	tlInitStmts <- translateForCB b stmts
>	setErrorWriter mkErrorPostHandler
>	setPBDone mkPBranchPostDoneStmts
>	setTransExit transformPostFuncReturnStmts
>	gspWarn
>	    ("The blocking function '" ++ fname ++ "' does not contain any blocking statements\n")
>	    (getNI b)
>	return tlInitStmts

> generateFirstPostStmts _ = error "Not a FunContext.  Invalid code path."

generatePostStmts makes the post call statements for a callback (or post function).
This is called on a blocking statement that will translate to a post call within a callback function (or post function).

> generatePostStmts :: Maybe BlockingContext -> [BlockingContext] -> WalkerT ([CStat])

> generatePostStmts (Just b@(BlockingStmt stmt nbBefore _ _ _ _)) _ = do

>	-- pull out just the blocking call from a complex expression
>	(Just blockingCall) <- findBlockingCallExpr $ stmt

>	-- generate the function name for the callback to pass to the post call
>	let callbackFunName = (mkCallbackFunName (getParentName b)
>						 (getCallName blockingCall) 
>						 (getCallNodeInfo blockingCall))

>	-- get the return type of the blocking call
>	let parentRetType = getFunDefReturn $ funDef (getParent b)

>	-- get the current params prefix
>	pname <- getPrefix
>	fname <- getFileName
>	errorWriter <- getErrorWriter
>	return (mkPostCall b callbackFunName pname blockingCall (\s -> errorWriter fname parentRetType s (getNI b)))

> generatePostStmts (Just (FunContext _ (first:stmts) _ _ _)) tracer = error "generatePostStmts(FunContext): Invalid"

> generatePostStmts (Just ifCtx@(IfContext ifDef (bif:ifStmts) (Just (belse:elseStmts)) nbBefore _ _ _ _)) tr = do
>	isValidBlockingIf ifDef
>	if (any (ifCtx ==) tr) then return []
>	  else do
>	    let tracer = ifCtx : tr
>	        (CIf expr _ _ ni) = ifDef
>	    ifPostStmts <- generatePostStmts (Just bif) tracer
>	    ifInitStmts <- translateForCB ifCtx (getCtxInitStmts bif)

>	    elsePostStmts <- generatePostStmts (Just belse) tracer
>	    elseInitStmts <- translateForCB ifCtx (getCtxInitStmts belse)

>	    ((CExpr tlExpr _):_) <- translateForCB ifCtx [(CExpr (Just expr) ni)]

>	    tlNBBeforeIf <- translateForCB ifCtx $ nbStmtsBefore bif
>	    tlNBBeforeElse <- translateForCB ifCtx $ nbStmtsBefore belse

>	    let ifStmts = tlNBBeforeIf ++ ifInitStmts ++ ifPostStmts
>	        elseStmts = tlNBBeforeElse ++ elseInitStmts ++ elsePostStmts
>	    return [mkIfElseStmt tlExpr ifStmts elseStmts]

Special case where the else has blocking call(s) but the if doesn't.  We know that the else
has blocking calls, because otherwise the IfContext wouldn't have been created.

> generatePostStmts (Just ifCtx@(IfContext ifDef [] (Just (belse:elseStmts)) nbBefore nbAfter _ next _)) tr = do
>	isValidBlockingIf ifDef
>	if (any (ifCtx ==) tr) then return []
>	  else do
> 	    let tracer = ifCtx : tr
>	        transCtx = findTransition ifCtx
>	        (CIf expr nbIfBlock _ ni) = ifDef

>	    elsePostStmts <- generatePostStmts (Just belse) tracer
>	    firstInitStmts <- translateForCB ifCtx (getCtxInitStmts belse)

>	    afterIfStmts <- generateAfterStmts ifCtx tracer

>           ((CExpr tlIfExpr _):_) <- translateForCB ifCtx [(CExpr (Just expr) ni)]

>	    tlNBIfStmts <- translateForCB ifCtx [nbIfBlock]

>	    tlNBBeforeElse <- translateForCB ifCtx $ nbStmtsBefore belse

>	    let ifStmts = tlNBIfStmts ++ afterIfStmts
>	        elseStmts = tlNBBeforeElse ++ firstInitStmts ++ elsePostStmts

>	    return [mkIfElseStmt tlIfExpr ifStmts elseStmts]

Special case where the if has blocking call(s), but the else doesn't (an else may not even be present).

> generatePostStmts (Just ifCtx@(IfContext ifDef (bif:ifStmts) (Just []) nbBefore nbAfter _ next _)) tr = do
>	isValidBlockingIf ifDef
>	if (any (ifCtx ==) tr) then return []
>	  else do
> 	    let tracer = ifCtx : tr
>	        transCtx = findTransition ifCtx
>		(CIf expr _ nbElse ni) = ifDef 

>	    ifPostStmts <- generatePostStmts (Just bif) tracer
>	    firstInitStmts <- translateForCB ifCtx (getCtxInitStmts bif)

>	    afterIfStmts <- generateAfterStmts ifCtx tracer

>	    ((CExpr tlIfExpr _):_) <- translateForCB ifCtx [(CExpr (Just expr) ni)]

>	    tlNBIfStmts <- translateForCB ifCtx $ nbStmtsBefore bif

>	    tlNBBeforeElse <- if isJust nbElse then translateForCB ifCtx [fromJust nbElse] else return []

>	    let ifStmts = tlNBIfStmts ++ firstInitStmts ++ ifPostStmts
>	        elseStmts = tlNBBeforeElse ++ afterIfStmts

>	    return [mkIfElseStmt tlIfExpr ifStmts elseStmts]

> generatePostStmts (Just ifCtx@(IfContext ifDef (bif:ifStmts) Nothing before after parent next prev)) tr = 
>	generatePostStmts (Just (IfContext ifDef (bif:ifStmts) (Just []) before after parent next prev)) tr

> generatePostStmts (Just forCtx@(ForContext forDef (firstB:forStmts) nbBefore nbAfter _ forNext _)) tr = do
>	isValidBlockingFor forDef

>	if (any (forCtx ==) tr) then return [mkGoto (mkLoopStartLabelStr forCtx) (getNI forCtx)]
>	  else do

>	    let (CFor init expr1 expr2 _ ni) = forDef
>	        loopStartLabel = \s -> mkLabel (mkLoopStartLabelStr forCtx) s $ getNI forCtx

>	    let tracer = forCtx : tr
>	    forPostStmts <- generatePostStmts (Just firstB) tracer
>	    forPostInitStmts <- translateForCB forCtx $ ((nbStmtsBefore firstB) ++ (getCtxInitStmts firstB))
>	    firstInitStmts <- translateForCB forCtx (getCtxInitStmts firstB)
>	    let forStmts = forPostInitStmts ++ forPostStmts

>	    afterForStmts <- generateAfterStmts forCtx tracer

>	    ((CExpr tlForExpr _):_) <- translateForCB forCtx [(CExpr expr1 ni)]

>	    return $ [loopStartLabel [mkIfElseStmt tlForExpr forStmts (addLoopBreakLabel forCtx afterForStmts)]]

> generatePostStmts (Just whileCtx@(WhileContext whileDef (bwhile:whileStmts) nbBefore nbAfter _ _ _)) tr = do
>	isValidBlockingWhile whileDef

>	if (any (whileCtx ==) tr) then return [mkGoto (mkLoopStartLabelStr whileCtx) (getNI whileCtx)]
>	  else do

>	    let (CWhile expr stmts isDoWhile ni) = whileDef
>	        loopStartLabel = \s -> mkLabel (mkLoopStartLabelStr whileCtx) s $ getNI whileCtx

>	    let tracer = whileCtx : tr
>	    whilePostStmts <- generatePostStmts (Just bwhile) tracer
>	    whilePostInitStmts <- translateForCB whileCtx $ ((nbStmtsBefore bwhile) ++ (getCtxInitStmts bwhile))
>	    firstInitStmts <- translateForCB whileCtx (getCtxInitStmts bwhile)
>	    let whileStmts = whilePostInitStmts ++ firstInitStmts ++ whilePostStmts

>	    afterWhileStmts <- generateAfterStmts whileCtx tracer

>	    ((CExpr tlWhileExpr _):_) <- translateForCB whileCtx [(CExpr (Just expr) ni)]

>	    return $ [loopStartLabel [mkIfElseStmt tlWhileExpr whileStmts (addLoopBreakLabel whileCtx afterWhileStmts)]]

> generatePostStmts (Just compoundCtx@(CompoundContext cDef (b:bs) nbBefore nbAfter _ _ _)) tr = do

>	if (any (compoundCtx ==) tr) then return [mkGoto (mkLoopStartLabelStr compoundCtx) (getNI compoundCtx)]
>	  else do

>	    let tracer = compoundCtx : tr
>	    compoundPostStmts <- generatePostStmts (Just b) tracer
>	    compoundPostInitStmts <- translateForCB compoundCtx $ ((nbStmtsBefore b) ++ (getCtxInitStmts b))
>	    firstInitStmts <- translateForCB compoundCtx (getCtxInitStmts b)
>	    return $ compoundPostInitStmts ++ firstInitStmts ++ compoundPostStmts

> generatePostStmts Nothing tr = return []

> generatePostStmts (Just next@(PWaitContext _ pwaitDef stmts before after _ _ _)) tr = do
>	isValidPWait pwaitDef
>	getParallelStmts next tr

> generatePostStmts (Just next@(PBranchContext _ _ _ _ _ _ _ _)) tr = return []

> generatePostStmts (Just c) _ = error $ "generatePostStmts: " ++ (getCtxName c) ++ " not handled yet"

> generateAfterStmts :: BlockingContext -> [BlockingContext] -> WalkerT [CStat]
> generateAfterStmts b tracer = do
>    let transCtx = findTransition b
>    afterLoopPostStmts <- generatePostStmts transCtx tracer
>    stmtsAfter <- getStmtsAfter b []
>    if isJust transCtx
>      then do
>	  initStmts <- translateForCB b ((getNextInitStmts b) ++ stmtsAfter)
>	  return $ initStmts ++ afterLoopPostStmts
>      else return stmtsAfter

> getParallelStmts :: BlockingContext -> [BlockingContext] -> WalkerT [CStat]

> getParallelStmts pb@(PBranchContext _ branchDef (b:branchStmts) _ _ _ _ _) tr = do
>	pp <- getPrefix
>	let ni = getNI pb
>           nbStmtsBeforePB = nbStmtsBefore pb
>	    nbStmts = nbStmtsBefore b
>	    pbDeclInits = getInitsFromDecls $ getLocalDeclarations branchDef
>	    ctxInitStmts = getCtxInitStmts b
>	    pwaitName = mkPWaitName (getPWaitId $ getPWaitAncestor pb)
>	    pbend = [mkLabel ("__ae_" ++ (getPBranchId pb) ++ "_end") [] ni]
>	afterPBranchNBStmts <- translateForCB pb (nbStmtsAfter pb)
>	pushPrefix "child_ctl"
>	initStmts <- translateForCB b $ (pbDeclInits ++ nbStmts ++ ctxInitStmts)
>       setPBDone mkPBranchPostDoneStmts
>	branchPostStmts <- generatePostStmts (Just b) [] 
>	cp <- getPrefix
>	branchStmts <- mkPBranchPostStmts pb pwaitName cp pp (getParentName pb) (initStmts ++ branchPostStmts) (nodeInfo branchDef)
>	popPrefix
>	return $ branchStmts ++ pbend ++ afterPBranchNBStmts

> getParallelStmts pw@(PWaitContext _ waitDef waitStmts@(w:ws) before after parent next prev) tr = do
>	let nbStmts = nbStmtsBefore w
>	    pwaitDeclInits = getInitsFromDecls $ getLocalDeclarations waitDef
>           pwParamsName = mkPWaitName (getPWaitId pw)
>	firstStmts <- translateForCB w (pwaitDeclInits ++ nbStmts)
>	pStmts <- liftM concat $ mapM (\b -> (getParallelStmts b tr)) waitStmts
>	p <- getPrefix
>	afterPWaitStmts <- generateAfterStmts pw tr
>       pwaitInitStmts <- mkPWaitInitStmts pwParamsName p $ getNI pw
>       pwaitFiniStmts <- mkPWaitFinishStmts pw p
>       pwaitNotDoneStmts <- mkPWaitNotDoneStmts pw p
>	return $ pwaitInitStmts ++ firstStmts ++ pStmts ++ pwaitFiniStmts ++ afterPWaitStmts ++ pwaitNotDoneStmts 

> getParallelStmts b@(ForContext forDef forStmts@(f:fs) nbBefore nbAfter parent next prev) tr = do
>	p <- getPrefix
>	pStmts <- liftM concat $ mapM (\f1 -> (getParallelStmts f1 tr)) forStmts
>	let (CFor (Left init) expr2 expr3 _ ni) = forDef
>	    trInit = trExpr p b init
>	    trExpr2 = trExpr p b expr2
>	    trExpr3 = trExpr p b expr3
>	trb <- translateForCB b (nbStmtsBefore f)
>	afterForNBStmts <- translateForCB b (nbStmtsAfter b)

>	return $ [(CFor (Left trInit) trExpr2 trExpr3 
>		        (mkCompoundStmt Nothing (trb ++ pStmts) ni) ni)] ++ afterForNBStmts

> getParallelStmts b@(WhileContext whileDef whileStmts@(w:ws) nbBefore nbAfter parent next prev) tr = do
>	p <- getPrefix
>	pStmts <- liftM concat $ mapM (\w1 -> (getParallelStmts w1 tr)) whileStmts
>	let (CWhile expr stmts isDoWhile ni) = whileDef
>	    (Just transExpr) = trExpr p b (Just expr)
>	trb <- translateForCB w (nbStmtsBefore w)
>	afterWhileNBStmts <- translateForCB b (nbStmtsAfter b)
>	return $ [(CWhile transExpr 
>		          (mkCompoundStmt Nothing (trb ++ pStmts) ni) isDoWhile ni)]
>		     ++ afterWhileNBStmts

> getParallelStmts b@(IfContext ifDef ifStmts@(if1:ifs) (Just (e1:elseStmts)) nbBefore nbAfter parent next prev) tr = do
>	p <- getPrefix
>	ifPStmts <- liftM concat $ mapM (\i -> (getParallelStmts i tr)) ifStmts
>	elsePStmts <- liftM concat $ mapM (\e -> (getParallelStmts e tr)) elseStmts
>	let (CIf expr _ _ ni) = ifDef
>	    (Just transExpr) = trExpr p b (Just expr)
>	iftrb <- translateForCB if1 (nbStmtsBefore if1)
>	elsetrb <- translateForCB e1 (nbStmtsBefore e1)

>	afterIfNBStmts <- translateForCB b (nbStmtsAfter b)
>	return $ [(CIf transExpr (mkCompoundStmt Nothing (iftrb ++ ifPStmts) ni) (Just (mkCompoundStmt Nothing (elsetrb ++ elsePStmts) ni)) ni)] ++ afterIfNBStmts

> getParallelStmts b@(IfContext ifDef ifStmts@(if1:ifs) Nothing nbBefore nbAfter parent next prev) tr = do
>	p <- getPrefix
>	ifPStmts <- liftM concat $ mapM (\i -> (getParallelStmts i tr)) ifStmts
>	let (CIf expr _ _ ni) = ifDef
>	    (Just transExpr) = trExpr p b (Just expr)
>	iftrb <- translateForCB if1 (nbStmtsBefore if1)

>	afterIfNBStmts <- translateForCB b (nbStmtsAfter b)
>	return $ [(CIf transExpr (mkCompoundStmt Nothing (iftrb ++ ifPStmts) ni) Nothing ni)] ++ afterIfNBStmts

> getParallelStmts b@(CompoundContext (CCompound _ _ ni) _ _ _ _ _ _) tr = do
>       invalid ("Compound statements are not allowed within pwait blocks\n") ni
>       return []

> getLoopParent :: BlockingContext -> Maybe BlockingContext
> getLoopParent b@(WhileContext _ _ _ _ _ _ _) = Just b
> getLoopParent b@(ForContext _ _ _ _ _ _ _) = Just b
> getLoopParent b = if isNothing $ parent b then Nothing else getLoopParent $ fromJust $ parent b

> getLoopNI :: BlockingContext -> NodeInfo
> getLoopNI (WhileContext wDef _ _ _ _ _ _) = nodeInfo wDef
> getLoopNI (ForContext fDef _ _ _ _ _ _) = nodeInfo fDef
> getLoopNI _ = assert False $ error "Not a loop context!"

> mkLoopBreakLabelStr :: BlockingContext -> String
> mkLoopBreakLabelStr b = assert (isJust $ getLoopParent b) s
>       where s = (getParentName b) ++ 
>                 (getPosVarStr $ getLoopNI $ fromJust $ getLoopParent b) ++ "break_after_loop"

> mkLoopStartLabelStr :: BlockingContext -> String
> mkLoopStartLabelStr b = (getParentName b) ++ (getPosVarStr $ getLoopNI b) ++ "start_of_loop"

> getStmtsAfter :: BlockingContext -> [CStat] -> WalkerT [CStat]
> getStmtsAfter b bcallReturn
>	| containsBreak $ nbStmtsAfter b = do
>		let loopCtx = findLoopParent b
>		    ni = getNI b
>		breakStmts <- generateAfterStmts loopCtx []
>	        tlAfter <- translateForCB b $ bcallReturn ++ (nbStmtsAfter b)
>		return $ swapBreak (mkCompoundStmt Nothing (breakStmts ++ [CReturn Nothing ni]) $ getNI b) tlAfter 

>	| (containsPBreak $ nbStmtsAfter b) && isLastContext b && parentIsPBranch b = do
>		prefix <- getPrefix

>		let pwaitCtx = getPWaitAncestor b
>		    pbranchCtx = getPBranchAncestor b
>		afterPWaitStmts <- generateAfterStmts pwaitCtx []
>		pbDone <- getPBDone

>		let fname = getParentName b
>		    ni = getNI b
>		    pbCheckDone = pbDone pbranchCtx ni
>               fileName <- getFileName
>		(pbreakDecls, pbreakStmts) <- mkPBreakStmts prefix ni
>		(pbranchDecls, pbranchStart) <- mkPBranchCallbackStartStmts prefix fname ni
>		pbranchDelete <- mkPBranchDeleteStmts ni
>		pbranchDone <- mkPBranchDoneStmts prefix fname ni
>		doneStmts <- mkDoneStmts ni

>		setDoneCtlStmt <- mkDoneCtlSetStmt prefix ni
>	        tlAfter <- translateForCB b $ bcallReturn ++ (nbStmtsAfter b)
>		let afterStmts = swapPBreak (mkCompoundWithDecls Nothing
>						         pbreakDecls
>					            	 (pbreakStmts ++ setDoneCtlStmt ++ pbranchDone ++ pbCheckDone ++
>							  afterPWaitStmts ++ doneStmts)
>							 $ getNI b) tlAfter

>		return $ [mkCompoundWithDecls Nothing pbranchDecls
>				(pbranchStart ++ afterStmts ++ setDoneCtlStmt ++
>				 pbranchDelete ++ pbranchDone ++ pbCheckDone ++ afterPWaitStmts) $ getNI b]



>	| containsPBreak $ nbStmtsAfter b = do
>		prefix <- getPrefix

>		let pwaitCtx = getPWaitAncestor b
>		    pbranchCtx = getPBranchAncestor b
>		afterPWaitStmts <- generateAfterStmts pwaitCtx []
>		pbDone <- getPBDone

>		let fname = getParentName b
>		    ni = getNI b
>		    pbCheckDone = pbDone pbranchCtx ni
>		setDoneCtlStmt <- mkDoneCtlSetStmt prefix ni
>		(pbreakDecls, pbreakStmts) <- mkPBreakStmts prefix ni
>		(pbranchDecls, pbranchStart) <- mkPBranchCallbackStartStmts prefix fname ni
>		pbranchDone <- mkPBranchDoneStmts prefix fname ni
>		doneStmts <- mkDoneStmts ni

>	        tlAfter <- translateForCB b $ bcallReturn ++ (nbStmtsAfter b)
>		let afterStmts = swapPBreak (mkCompoundWithDecls Nothing
>						         pbreakDecls
>					            	 (pbranchStart ++ pbreakStmts ++ setDoneCtlStmt ++ pbranchDone ++ pbCheckDone ++
>							  afterPWaitStmts ++ doneStmts)
>							 $ getNI b) tlAfter

>		return $ [mkCompoundWithDecls Nothing pbranchDecls afterStmts $ getNI b]


>	| isLastContext b && (parentIsIf b || parentIsCompound b) = do
>		prefix <- getPrefix
>               assert (isJust $ parent b) return ()
>		parentStmts <- getStmtsAfter (fromJust $ parent b) []
>		tlAfter <- translateForCB b $ bcallReturn ++ (nbStmtsAfter b)
>		return $ tlAfter ++ parentStmts 

>	| isLastContext b && parentIsPBranch b = do

>		prefix <- getPrefix

>		let pn = getParentName b
>		    ni = getNI b

>		-- get statements after last blocking call in pbranch
>		tlAfterLastBlocking <- translateForCB b $ bcallReturn ++ (nbStmtsAfter b)

>		-- get statements after pwait context
>		let pwaitCtx = getPWaitAncestor b
>		    pbranchCtx = getPBranchAncestor b
>		afterPWaitStmts <- getStmtsAfter pwaitCtx []
>		pbDone <- getPBDone

>		pbDoneStmts <- liftM concat $ sequence [mkPBranchDeleteStmts ni,
>				                        mkPBranchDoneStmts prefix pn ni]
>               let doneStmts = pbDone pbranchCtx ni
>		setDoneCtlStmt <- mkDoneCtlSetStmt prefix ni
>		(cbDecls, cbStmts) <- mkPBranchCallbackStartStmts prefix pn ni 
>	        return $ [mkCompoundWithDecls Nothing 
>					      cbDecls 
>					      (cbStmts ++ tlAfterLastBlocking ++ setDoneCtlStmt ++
>					       pbDoneStmts ++ doneStmts ++ afterPWaitStmts) $ getNI b]

>	| otherwise = translateForCB b $ bcallReturn ++ (nbStmtsAfter b)

> getNextInitStmts :: BlockingContext -> [CStat]
> getNextInitStmts b 
>	| isLastContext b && parentIsFor b =
>		let (Just (ForContext (CFor (Left init) expr1 expr2 _ ni) _ _ _ _ _ _)) = parent b
>		in [(CExpr expr2 ni)]

>	| isLastContext b && parentIsIf b = 
>               let nis = getNextInitStmts $ fromJust $ parent b
>               in assert (isJust $ parent b) nis

>	| (isJust $ next b) && (isForContext $ fromJust $ next b) = 
>		let (Just (ForContext (CFor (Left init) expr1 expr2 _ ni) _ _ _ _ _ _)) = next b
>               in [CExpr init ni]
>	| otherwise = []

> getCtxInitStmts :: BlockingContext -> [CStat]
> getCtxInitStmts b
>	| isForContext b = 
>		let (ForContext (CFor (Left init) expr1 expr2 _ ni) _ _ _ _ _ _) = b
>		in [(CExpr init ni)]
>	| otherwise = []

> findTransition :: BlockingContext -> Maybe BlockingContext 

> findTransition b

>	-- When the blocking context is the last context in a chain and the parent is a loop,
>	-- we transition back to the loop

>	| isLastContext b && parentIsFun b = Nothing
>	| isLastContext b && parentIsFor b = parent b
>	| isLastContext b && parentIsWhile b = parent b
>	| isLastContext b && parentIsIf b = 
>               let res = findTransition $ fromJust $ parent b
>               in assert (isJust $ parent b) res
>	| isLastContext b && parentIsPBranch b = findTransition $ getPWaitAncestor b
>       | isLastContext b && parentIsCompound b =
>               let res = findTransition $ fromJust $ parent b
>               in assert (isJust $ parent b) res
>	| otherwise = next b

> mkPostCall :: BlockingContext -> String -> String -> CExpr -> (String -> [CStat]) -> [CStat]
> mkPostCall b fname ctlName bcall@(CCall fexpr _ _) errorHandler =
>	let ni = nodeInfo bcall
>	in [mkFunCall (mkVar "__ae_postret" ni)
>		   fexpr
>		   ((map (fromJust . (trExpr ctlName b) . Just) $ getCallParams bcall)
>                      ++ [mkVar fname ni, mkVar ctlName ni, 
>			   addStructPtr ctlName $ mkVar "hints" ni,
>			   addStructPtr ctlName $ mkVar "context" ni,
>			   addAddrOp $ addStructPtr ctlName $ mkVar "current_op_id" ni])] ++
>	    (errorHandler ctlName)

Construct the statements for the post function.  First the declarations, next the initialization parts,
and finally the code up-to the first blocking call.

> mkPostStmts :: String -> BlockingContext -> CFunDef -> [CStat] -> WalkerT CStat
> mkPostStmts prefix bctx funDef blockingStmts = do

>     -- declarations
>     decls <- mkPostFunDecls (getParentName bctx) $ getNI bctx

>     -- initializer statements
>     inits <- mkPostFunInit prefix bctx (getFunDefParams funDef) (getFunLocalDeclarations funDef)
>     finis <- mkPostFunFinished (getNI bctx)

>     return $ mkCompoundWithDecls Nothing decls (inits ++ (map (trLocals prefix bctx) blockingStmts) ++ finis) $ getNI bctx

> mkRetParam :: (CTypeSpec, [CDerivedDeclr]) -> NodeInfo -> [CDecl]
> mkRetParam (CVoidType _, []) ni = []
> mkRetParam (ret, derived) ni = [mkCDecl ret derived "__ae_ret" ni]

> mkCBParam :: (CTypeSpec, [CDerivedDeclr]) -> NodeInfo -> [CDecl]
> mkCBParam (CVoidType _, []) ni = 
>	[mkVoidFunPtr "callback" [constructDeclFromC ni "void *user_ptr;"]]
> mkCBParam return ni = 
>	[mkVoidFunPtr "callback"
>		[constructDeclFromC ni "void *user_ptr;", mkCDecl (fst return) (snd return) "__ae_ret" ni]]

> mkPostParams :: (String, (CTypeSpec, [CDerivedDeclr]), [CDecl]) -> WalkerT [CDecl]
> mkPostParams ((,,) fname ret params) = do
>	let ni = nodeInfo $ fst ret

>       txParams <- sequence $ map translateBlockingFunParam $ removeVoid params 
>       bParams <- mkBlockingParamsForPost ret ni

>	return $ txParams ++ bParams

> mkPostDecl :: (String, (CTypeSpec, [CDerivedDeclr]), [CDecl]) -> [CDeclSpec] -> WalkerT CDecl
> mkPostDecl f@((,,) fname ret params) declspecs = do
>	let ni = nodeInfo $ fst ret
>	fparams <- mkPostParams f
>       ret <- mkPostRetType ni
>	return $ mkFunDeclWithDeclSpecs
>		   fname
>		   ret []
>		   (filterStdDeclSpecs declspecs)
>	           fparams

> mkPostPtrDecl :: Bool -> (String, (CTypeSpec, [CDerivedDeclr]), [CDecl]) -> [CDeclSpec] -> WalkerT CDecl
> mkPostPtrDecl extern f@((,,) fname ret params) declspecs = do
>	let ni = nodeInfo $ fst ret
>	fparams <- mkPostParams f
>       ret <- mkPostRetType ni
>	return $ mkFunPtrDeclWithDeclSpecs
>		    fname
>		    ret []
>		    ((filterStdDeclSpecs declspecs) ++
>		     (if extern then [CStorageSpec $ CExtern ni] else []))
>		    fparams
>		    (if not extern then Just fname else Nothing)

> mkPostFunction :: String -> BlockingContext -> CFunDef -> [CStat] -> WalkerT CExtDecl
> mkPostFunction prefix bctx funDef postStmts = do
>	let decls = getFunDefParams funDef
>	    ni = getNI bctx
>       params <- sequence $ map translateBlockingFunParam $ removeVoid decls
>       blockingParams <- mkBlockingParamsForPost (getFunDefReturn funDef) (nodeInfo funDef) 
>       stmts <- mkPostStmts prefix bctx funDef postStmts
>       ret <- mkPostRetType ni
>	return $ mkFunDef (ret, []) -- return type
>		    (getStdDeclSpecs funDef) -- get the storage specifiers for the function
>	            (mkPostFunName "params" $ getFunDefName funDef) -- function name
>	            (params ++ blockingParams)
>	            stmts -- statements

> mkCallbackInitStmts :: String -> BlockingContext -> [CStat]
> mkCallbackInitStmts prefix bctx =
>	let ni = getNI bctx
>	in [CExpr (Just (CAssign
>			 	CAssignOp
>				(mkVar prefix ni)
>				(CCast (mkAnonStructPtrDecl (mkStructCtlName $ getParentName bctx) ni)
>				(mkVar "__ae_ptr" ni) ni) ni)) ni]

> mkCallbackStmts :: String -> BlockingContext -> [CStat] -> [CStat] -> WalkerT CStat
> mkCallbackStmts prefix bctx stmtsAfter nextBlockingPostStmts =  do
>	let ni = getNI bctx
>       ret <- mkPostRetDecl ni
>       return $ mkCompoundWithDecls Nothing

>		-- declarations
>		[(mkStructPtrDecl (mkStructCtlName $ getParentName bctx) "ctl" ni),
>		 ret]

>	 	 -- initializer stmts
>	 	((mkCallbackInitStmts prefix bctx)

>		 -- callback statements
>	 	 ++ stmtsAfter

>	 	 -- the next blocking call
>	 	 ++ nextBlockingPostStmts
>	         ++ [mkLabel "__ae_callback_end" [] $ getNI bctx]) $ getNI bctx

> mkCallbackReturnParamDecl :: (CTypeSpec, [CDerivedDeclr]) -> String -> NodeInfo -> [CDecl]
>	-- no parameter if the return type is void
> mkCallbackReturnParamDecl (CVoidType _, []) _ _ = []
> mkCallbackReturnParamDecl (retType, derives) name ni =
>	[mkCDecl retType (filter (not . isDerivedFun) derives) (mkCallbackRetParam name ni) ni]

function definition, blocking call, return expression, statements after blocking call up to next blocking, optional next blocking call

> mkCallbackDecl :: BlockingContext -> CExpr -> (CTypeSpec, [CDerivedDeclr]) -> CExtDecl
> mkCallbackDecl bctx blockingCall returnType =
>	let fname  = getCallName blockingCall
>	    params = getCallParams blockingCall
>	    ni     = getCallNodeInfo blockingCall
>	in mkStaticFunDecl (CVoidType ni) -- return type
>	                   (mkCallbackFunName (getParentName bctx) fname ni) -- function name
>	                   ([constructDeclFromC ni "void *__ae_ptr;"] ++ -- parameter for control state
>			    (mkCallbackReturnParamDecl returnType fname ni)) -- parameter for return type of blocking function

> mkCallback :: String -> BlockingContext -> CExpr -> (CTypeSpec, [CDerivedDeclr]) -> [CStat] -> [CStat] -> WalkerT CExtDecl
> mkCallback prefix bctx blockingCall returnType stmts nextBlockingPostStmts = do
>	let fname  = getCallName blockingCall
>	    params = getCallParams blockingCall
>	    ni     = getCallNodeInfo blockingCall
>       cbStmts <- mkCallbackStmts prefix bctx stmts nextBlockingPostStmts
>	return $ mkStaticFunDef (CVoidType ni) -- return type
>	                  (mkCallbackFunName (getParentName bctx) fname ni) -- function name
>	                  ([constructDeclFromC ni "void *__ae_ptr;"] ++  -- parameter for control state
>			   (mkCallbackReturnParamDecl returnType fname ni)) -- parameter for return type of blocking function
>	                  cbStmts -- statements

Take a blocking statement and a list of statements that follow
(possibly containing other blocking statements), and return a list
of external declarations that are linked sequence of callback function
definitions for the blocking statements in the list.

Parameters:  

BlockingContext:  The blocking context
CStat:  The blocking statement
[[CStat]]:  A list of statement lists.  Each internal list contains nonblocking statements,
	    with an optional blocking statement as the last element.

> generateCallbackForBlocking :: BlockingContext -> WalkerT CExtDecl
> generateCallbackForBlocking b@(BlockingStmt _ _ _ _ _ _) = do

>	-- get the actual blocking function call from the statement containing the call
>	blockingCall <- findBlockingCallExpr $ stmt b

>	-- verify that the blocking call exists
>	assert (isJust blockingCall) return ()

>	-- create a callback parameter that matches the return type of the blocking call
>	let callbackRetParam = mkCallbackRetParam (getCallName $ fromJust blockingCall) (getCallNodeInfo $ fromJust blockingCall)

>	-- make a statement that matches the one with the blocking call, but with the blocking call replaced
>	-- with a return variable
>	blockingCallReturnStmt <- findAndReplaceBlockingCall callbackRetParam $ stmt b

>	-- lookup the declaration of the blocking function call to get the return type and parameter types
>	blockingFun <- lookupBlocking $ fromJust blockingCall
>	assert (isJust blockingFun) return ()
>	let (Just ((,,) bname retType params)) = blockingFun

>	setErrorWriter mkErrorCBPostHandler
>	setTransExit transformFuncReturnStmts
>	setPBDone mkPBranchCBDoneStmts

>	-- get the nonblocking statements 
>	prefix <- getPrefix
>	nonBlockingStmts <- getStmtsAfter b [blockingCallReturnStmt]
>	nextInitStmts <- translateForCB b $ getNextInitStmts b

>	-- figure out if there's another blocking call following this one, and get the call statements if so
>	nextBlockingPostStmts <- generateNextPostStmts (findTransition b)

>	prefix <- getPrefix

>	-- finally, generate the actual callback function definition
>	mkCallback prefix
>		   b 
>		   (fromJust blockingCall)
>		   retType
>                  (nonBlockingStmts ++ nextInitStmts)
>	           nextBlockingPostStmts

> generateCallbackDeclForBlocking :: BlockingContext -> WalkerT CExtDecl
> generateCallbackDeclForBlocking b = do
>	blockingCall <- findBlockingCallExpr $ stmt b
>	-- verify that the blocking call exists
>	assert (isJust blockingCall) return ()
>	
>	blockingFun <- lookupBlocking $ fromJust blockingCall
>	assert (isJust blockingFun) return ()
>
>	let (Just ((,,) bname retType params)) = blockingFun
>	return $ mkCallbackDecl b (fromJust blockingCall) retType

> generateCallbackDefs :: BlockingContext -> WalkerT [CExtDecl]
> generateCallbackDefs = generateCallbacks generateCallbackForBlocking

> generateCallbackDecls :: BlockingContext -> WalkerT [CExtDecl]
> generateCallbackDecls = generateCallbacks generateCallbackDeclForBlocking

> generateCallbacks :: (BlockingContext -> WalkerT CExtDecl) -> BlockingContext -> WalkerT [CExtDecl]

> generateCallbacks transFun (FunContext funDef stmts _ _ _) = liftM concat $ mapM (generateCallbacks transFun) stmts

> generateCallbacks transFun b@(BlockingStmt stmt before after _ _ _) = do
>	liftM (:[]) $ transFun b

> generateCallbacks transFun b@(IfContext ifDef ifStmts elseStmts before after _ _ _) = do
>	isValidBlockingIf ifDef
>	ifCallbacks <- mapM (generateCallbacks transFun) ifStmts
>	elseCallbacks <- if isJust elseStmts then mapM (generateCallbacks transFun) $ fromJust elseStmts else return []
>	return $ concat ifCallbacks ++ concat elseCallbacks

> generateCallbacks transFun b@(ForContext forDef forStmts before after _ _ _) =
>	liftM concat $ mapM (generateCallbacks transFun) forStmts

> generateCallbacks transFun b@(WhileContext whileDef whileStmts before after _ _ _) =
>	liftM concat $ mapM (generateCallbacks transFun) whileStmts

> generateCallbacks transFun b@(CompoundContext cDef cStmts before after _ _ _) =
>       liftM concat $ mapM (generateCallbacks transFun) cStmts

> generateCallbacks transFun b@(PWaitContext _ pwaitDef stmts before after _ _ _) =
>	liftM concat $ mapM (generateCallbacks transFun) stmts

> generateCallbacks transFun b@(PBranchContext _ pwaitDef stmts before after _ _ _) =
>	liftM concat $ mapM (generateCallbacks transFun) stmts

> transformBlocking :: CExtDecl -> WalkerT [CExtDecl]
> transformBlocking fdefext@(CFDefExt funDef)

>   -- A blocking function definition, so we transform it
>   | isBlockingFunDef funDef = do 

>       isValidBlockingFunDef funDef

>	-- register a blocking function with its parameters and return type
>	registerBlocking ((getFunDefName funDef), (getFunDefReturn funDef), (getFunDefParams funDef))

>	-- Find any blocking function pointers as parameters to the blocking function,
>	-- and register them
>	mapM_ registerBlockingFunParam $ getFunDefParams funDef

>	-- Register function parameters as local variables
>	addLocals $ getFunDefParams funDef

>	-- Register function local variables
>	addLocals $ getFunLocalDeclarations funDef

>	-- before generating the context, we add a return; statement at the end if its
>	-- a void function and doesn't have a return
>	fDefWReturn <- checkForReturn funDef

>	-- Generate the blocking tree of BlockingContext nodes
>	ctx <- generateContext fDefWReturn

>	-- Debugging:  display the blocking tree	
>	-- putStrLnW $ drawTree $ contextToTree ctx

>	-- Walk the tree, and generate the callback function declarations
>	callbackDecls <- generateCallbackDecls ctx

>	-- Walk the tree, and generate the callback function definitions
>       callbackDefs <- generateCallbackDefs ctx

>	setErrorWriter mkErrorPostHandler
>	setPBDone mkPBranchPostDoneStmts
>	setTransExit transformPostFuncReturnStmts

>	-- Generate the post statements for the post function from the first blocking context
>	postStmts <- generateFirstPostStmts ctx

>	-- Make the blocking params structure declaration
>	paramsStruct <- mkBlockingParamsStruct funDef
>	pwaitStructs <- mkBlockingPWaitStructs funDef ctx
>	ctlStruct <- mkBlockingCtlStruct funDef ctx True

>	prefix <- getPrefix

>	let ni = getNI ctx
>	    fi = getFunInfo funDef
>	    specs = map CStorageSpec (getStorageSpecs funDef)

>       postFun <- mkPostFunction prefix ctx funDef postStmts
>       pDecl <- mkPostDecl fi specs
>       -- pPtrDecl <- mkPostPtrDecl False fi specs 

>	let postDecls = []

>	    -- Construct the external declarations of parameters, callback functions, and post function
>	    transformedDecls = pwaitStructs ++ [paramsStruct, ctlStruct] ++ callbackDecls ++ callbackDefs ++
>			       [CDeclExt pDecl] ++
>			       [postFun]

>	-- Clear the blocking function parameters registry
>	resetLocals
>	clearLocalFunPtrRegistry
>	return transformedDecls

   | defHasBlockingFunPtrParam funDef = do
       let (fname, (rtype, derived), ps) = getFunInfo funDef
           (CFunDef _ _ _ stmt _) = funDef
           specs = getDeclSpecs funDef
       newps <- mapM translateBlockingFunParam ps
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
>	newdecls <- liftM concat $ sequence $ map transformBlocking decls
>	return $ CTranslUnit newdecls ni

> printBlocking :: Int -> FPType -> WalkerT ()
> printBlocking l (FPFun n f) = do
>	let (bname, (rettype, derives), params) = f
>           pstr = if null params then "" else (foldl1 ((++) . (++ ", ")) $ map (show . pretty) params)
>	liftIO $ print $ (take l $ repeat ' ') ++ bname ++ "( " ++ pstr ++ " ) = " ++ (show . pretty $ rettype)
> printBlocking l (FPStruct n f i) = do
>	liftIO $ print $ (take l $ repeat ' ') ++ "( " ++ n ++ " )->" ++ f
>	printBlocking (l + 4) i

> printRegisteredBlockingCalls :: WalkerT ()
> printRegisteredBlockingCalls = do
> 	blocking <- getAllBlocking
> 	liftIO $ print "BLOCKING FUNCTIONS:"
> 	mapM_ (printBlocking 0) blocking

> generateAST :: FilePath -> IO CTranslUnit
> generateAST input_file = do
>	input_stream <- readInputStream input_file
>	let parse_result = parseC input_stream (position 0 input_file 1 1)
>       case parse_result of
>         Left parse_err -> error (show parse_err)
>         Right ast      -> return ast

> generateASTWithCPP :: FilePath -> [String] -> IO CTranslUnit
> generateASTWithCPP input_file includes = do
>	parseResult <- parseCFile (newGCC "gcc") Nothing (("-x c"):includes) input_file
>	case parseResult of
>		Left p -> error (show p)
>		Right ast -> return ast

> parseHeader :: FilePath -> [String] -> [(String, String)] -> Maybe FilePath -> FilePath -> IO ()
> parseHeader headerfile includes defs report outfile = do
>	let r = if isJust report then fromJust report else headerfile
>	w <- newWalkerState r includes defs mkErrorPostHandler mkPBranchPostDoneStmts transformFuncReturnStmts
>	ctu <- generateAST headerfile
>	(pairs, w) <- runStateT (getBlockingHeaderDecls ctu) w
>	writeFile outfile "\n\n/* This is an auto-generated file created by the ae-blocking-parser tool.  DO NOT MODIFY! */\n\n"
>	outputHeader r outfile pairs
>	appendFile outfile "\n\n"
>	return ()

> parseFile :: Bool -> [String] -> [(String, String)] -> FilePath -> Maybe FilePath -> FilePath -> IO ()
> parseFile p includes defs outfile report f = do
>	let r = if isJust report then fromJust report else f
> 	w <- newWalkerState r includes defs mkErrorPostHandler mkPBranchPostDoneStmts transformFuncReturnStmts
> 	ctu <- generateAST f
>	(ctuWithPostDecls, w) <- runStateT (registerBlockingFunDecls ctu) w
>       -- runStateT (printRegisteredBlockingCalls) w
>	(transCTU, w) <- runStateT (transform ctuWithPostDecls) w
>       writeFile outfile $ "\n\n\
>                           \/* This is an auto-generated source file create by the ae-blocking-parser tool.  DO NOT MODIFY! */\n\n"
>	if p then ((appendFile outfile) . show . pretty) transCTU
>	  else ((appendFile outfile) . show . serialize) transCTU
>	appendFile outfile "\n\n"
> 	return ()

> data ParserOpts = Pretty | Help | Include String | Report String | Outfile String | Header | Define (String, String)

> getIncludes :: [ParserOpts] -> [String]
> getIncludes ((Include s):ps) = s:(getIncludes ps)
> getIncludes (_:ps) = getIncludes ps
> getIncludes [] = []

> getReportFilename :: [ParserOpts] -> Maybe String
> getReportFilename ((Report s):ps) = Just s
> getReportFilename (_:ps) = getReportFilename ps
> getReportFilename [] = Nothing

> getOutfile :: [ParserOpts] -> Maybe String
> getOutfile ((Outfile s):ps) = Just s
> getOutfile (_:ps) = getOutfile ps
> getOutfile [] = Nothing

> newDefine :: String -> ParserOpts
> newDefine s = Define $ parseDef ([], []) s

> parseDef :: (String, String) -> String -> (String, String)
> parseDef (k,v) [] = (k,[])
> parseDef (k,v) ('=':ds) = (k,ds)
> parseDef (k,v) (s:ds) = parseDef (k++[s], []) ds

> getDefs :: [ParserOpts] -> [(String, String)]
> getDefs ((Define s):ps) = s:(getDefs ps)
> getDefs (_:ps) = getDefs ps
> getDefs [] = []

> parserOpts :: [OptDescr ParserOpts]
> parserOpts =
>    [ Option ['p'] ["pretty"] (NoArg Pretty)
>		"output in pretty form without source line macros"
>    , Option ['I'] ["include"] (ReqArg (\s -> Include s) "<include path>")
>		"include path for preprocessor"
>    , Option ['h','?'] ["help"] (NoArg Help)
>		"help text"
>    , Option ['r'] ["report"] (ReqArg (\s -> Report s) "<report filename>")
>		"filename to use when reporting errors"
>    , Option ['o'] ["outfile"] (ReqArg (\s -> Outfile s) "<output file>")
>		"filename to write translated C code"
>    , Option ['j'] ["header"] (NoArg Header)
>		"parse header file instead of source"
>    , Option ['D'] ["define"] (ReqArg (\s -> newDefine s) "<cpp def>")
>               "define a CPP macro or variable"
>    ]

> optPretty :: ParserOpts -> Bool
> optPretty (Pretty) = True
> optPretty _ = False

> optHelp :: ParserOpts -> Bool
> optHelp (Help) = True
> optHelp _ = False

> optHeader :: ParserOpts -> Bool
> optHeader (Header) = True
> optHeader _ = False

> main :: IO ()
> main = do
>   args <- getArgs
>   let (opts, files, errs) = getOpt RequireOrder parserOpts args 
>	pretty = any optPretty opts
>	help = any optHelp opts
>	includes = getIncludes opts
>	report = getReportFilename opts
>	outfile = getOutfile opts
>	pheader = any optHeader opts
>       defines = getDefs opts
>	header = "Usage: ae-blocking-parser [OPTIONS...] files..."
>   when (not $ null errs) $ ioError $ userError ((concat errs) ++
>			     	                  (usageInfo header parserOpts))
>   when help $ do { putStrLn $ usageInfo header parserOpts ; exitWith (ExitFailure 1) } 
>   when (isNothing outfile) $ ioError $ userError "No output file specified."
>   when pheader $ do { mapM_ (\f -> parseHeader f includes defines report (fromJust outfile)) files ; exitWith (ExitSuccess) }
>   mapM_ (parseFile pretty includes defines (fromJust outfile) report) files

