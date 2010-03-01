> module ContextGen where
> import Language.C
> import Language.C.System.GCC   -- preprocessor used
> import System.Environment
> import Data.Typeable
> import Data.Maybe
> import Data.Either
> import Data.HashTable
> import Control.Exception
> import Control.Monad.State
> import Data.List
> import Debug.Trace
> import CParse
> import CUtils
> import Walker
> import Data.Generics
> import Data.Generics.Schemes
> import Data.Tree
> import BlockingContext

> mkBlockingContext :: CStat -> [CStat] -> [CStat] -> Maybe BlockingContext -> 
>                      Maybe BlockingContext -> Maybe BlockingContext -> WalkerT BlockingContext

> mkBlockingContext b@(CIf expr ifStmts elseStmts _) nbBefore nbAfter parent next prev = do
>	ifCtxStmts <- mkBlockingList <=< splitAtBlocking $ getStmtList ifStmts 
>	elseCtxStmts <- if (isJust elseStmts)
>			  then 
>			    liftM Just $ mkBlockingList <=< splitAtBlocking $ 
>								getStmtList $ fromJust elseStmts 
>			  else return Nothing
>	return $ IfContext b ifCtxStmts elseCtxStmts nbBefore nbAfter parent next prev

> mkBlockingContext b@(CFor init expr2 expr3 stmts _) nbBefore nbAfter parent next prev = do
>	forCtxStmts <- mkBlockingList <=< splitAtBlocking $ getStmtList stmts
>	return $ ForContext b forCtxStmts nbBefore nbAfter parent next prev

> mkBlockingContext b@(CWhile expr stmts isDoWhile _) nbBefore nbAfter parent next prev = do
>	whileStmtsCtx <- mkBlockingList <=< splitAtBlocking $ getStmtList stmts
>	return $ WhileContext b whileStmtsCtx nbBefore nbAfter parent next prev

> mkBlockingContext b@(CCompound _ bitems _) nbBefore nbAfter parent next prev = do
>       cCtx <- mkBlockingList <=< splitAtBlocking $ getStmtList b
>       return $ CompoundContext b cCtx nbBefore nbAfter parent next prev

> mkBlockingContext b@(CPWait stmts _) nbBefore nbAfter parent next prev = do
>	waitCtx <- mkBlockingList <=< splitAtBlocking $ getStmtList stmts
>	let (NodeInfo p _ _) = nodeInfo b
>	    pwaitId = "pwait_" ++ (show $ posRow p) ++ "_" ++ (show $ posColumn p)
>	return $ PWaitContext pwaitId b waitCtx nbBefore nbAfter parent next prev

> mkBlockingContext b@(CPBranch stmts _) nbBefore nbAfter parent next prev = do
>	branchCtx <- mkBlockingList <=< splitAtBlocking $ getStmtList stmts
>	let (NodeInfo p _ _) = nodeInfo b
>	    pbranchId = "pbranch_" ++ (show $ posRow p) ++ "_" ++ (show $ posColumn p)
>	return $ PBranchContext pbranchId b branchCtx nbBefore nbAfter parent next prev

> mkBlockingContext b nbBefore nbAfter parent next prev =
>	return $ BlockingStmt b nbBefore nbAfter parent next prev


> mkBlockingList :: [Either CStat [CStat]] -> WalkerT [BlockingContext]

> mkBlockingList ((Right nbl):(Left b):(Right nbr):es) =  do
>	ctx <- mkBlockingContext b nbl nbr Nothing Nothing Nothing
>	rest <- mkBlockingList $ (Right nbr):es
>	return $ ctx:rest

> mkBlockingList ((Left b):(Right nbr):es) = do
>	ctx <- mkBlockingContext b [] nbr Nothing Nothing Nothing
>	rest <- mkBlockingList $ (Right nbr):es
>	return $ ctx:rest

> mkBlockingList ((Right nbl):(Left b):es) = do
>	ctx <- mkBlockingContext b nbl [] Nothing Nothing Nothing
>	rest <- mkBlockingList es
> 	return $ ctx:rest

> mkBlockingList ((Left b):es) = do
>	ctx <- mkBlockingContext b [] [] Nothing Nothing Nothing
>	rest <- mkBlockingList es
>	return $ ctx:rest

> mkBlockingList ((Right _):[]) = return []
> mkBlockingList [] = return []

> generateContext :: CFunDef -> WalkerT BlockingContext
> generateContext funDef = do
>	ctxStmts <- mkBlockingList <=< splitAtBlocking $ getFunStmts funDef
>	let fctx = FunContext funDef ctxStmts Nothing Nothing Nothing
>	return $ mkContextTree Nothing Nothing Nothing fctx

> isBlockingCall :: CExpr -> WalkerT Bool
> isBlockingCall c@(CCall v args _) =  do 
>	res <- lookupBlocking v
>	if (isJust res) then (return True) else (return False)
> isBlockingCall _ = return False

> containsBlockingCall :: CStat -> WalkerT Bool 
> containsBlockingCall stmt = do
>	anyBlocking <- everything orElseMBool (mkQ (return False) isBlockingCall) $ stmt
>	if not anyBlocking then
>	  case stmt of
>	    (CPBranch stmts ni) -> do
>	        fstr <- getFilePosStr ni
>		error $ fstr ++ ": The pbranch block does not contain a blocking call"
>	    (CPWait stmts ni) -> do
>		fstr <- getFilePosStr ni
>		error $ fstr ++ ": The pwait block does not contain a blocking call"
>	    _ -> return anyBlocking
>		  
>	  else return $ anyBlocking 

> splitAtBlocking :: [CStat] -> WalkerT [Either CStat [CStat]]
> splitAtBlocking ls = do
>	split <- splitM ((liftM not) . containsBlockingCall) ls
>	return split

> countBlocking :: CExpr -> WalkerT Int
> countBlocking (CCall e _ _) = do
>	res <- lookupBlocking e 
>	if (isJust res) then (return 1) else (return 0)
> countBlocking _ = return 0

> countBlockingCalls :: CStat -> WalkerT Int
> countBlockingCalls stmt = do
>	everything orElseMAdd (mkQ (return 0) countBlocking) $ stmt
