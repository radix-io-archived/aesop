> module BlockingContext where
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
> import Data.Generics
> import Data.Generics.Schemes
> import Data.Tree

> data BlockingContext = FunContext {     funDef :: CFunDef ,
>				          stmts :: [BlockingContext] ,
>					  parent :: Maybe BlockingContext ,
>				 	  next :: Maybe BlockingContext ,
>					  prev :: Maybe BlockingContext }

>		       | IfContext {   	  ifDef :: CStat ,
>				       	  ifStmts :: [BlockingContext] ,
>				          elseStmts :: Maybe [BlockingContext] ,
>				          nbStmtsBefore :: [CStat] ,
>					  nbStmtsAfter :: [CStat] ,
>					  parent :: Maybe BlockingContext ,
>				 	  next :: Maybe BlockingContext ,
>					  prev :: Maybe BlockingContext }

>		       | ForContext {     forDef :: CStat ,
>				   	  forStmts  :: [BlockingContext] ,
>				          nbStmtsBefore :: [CStat] ,
>					  nbStmtsAfter :: [CStat] ,
>					  parent :: Maybe BlockingContext ,
>					  next :: Maybe BlockingContext ,
>					  prev :: Maybe BlockingContext }

>		       | WhileContext {   whileDef :: CStat ,
>		 			  whileStmts :: [BlockingContext] ,
>				          nbStmtsBefore :: [CStat] ,
>					  nbStmtsAfter :: [CStat] ,
>					  parent :: Maybe BlockingContext ,
>					  next :: Maybe BlockingContext ,
>					  prev :: Maybe BlockingContext }

>		       | BlockingStmt {   stmt :: CStat ,
>				          nbStmtsBefore :: [CStat] ,
>					  nbStmtsAfter :: [CStat] ,
>					  parent :: Maybe BlockingContext ,
>					  next :: Maybe BlockingContext ,
>					  prev :: Maybe BlockingContext }

>                      | CompoundContext { 
>                                         compoundDef :: CStat ,
>                                         compoundStmts :: [BlockingContext] ,
>                                         nbStmtsBefore :: [CStat] ,
>                                         nbStmtsAfter :: [CStat] ,
>                                         parent :: Maybe BlockingContext ,
>                                         next :: Maybe BlockingContext ,
>                                         prev :: Maybe BlockingContext }

>		       | PWaitContext {   id :: String,
>					  pwaitDef :: CStat ,
>					  stmts :: [BlockingContext] ,
>					  nbStmtsBefore :: [CStat] ,
>					  nbStmtsAfter :: [CStat] ,
>					  parent :: Maybe BlockingContext ,
>					  next :: Maybe BlockingContext ,
>					  prev :: Maybe BlockingContext }

>		       | PBranchContext { id :: String,
>					  pbranchDef :: CStat ,
>					  stmts :: [BlockingContext] ,
>					  nbStmtsBefore :: [CStat] ,
>					  nbStmtsAfter :: [CStat] ,
>					  parent :: Maybe BlockingContext ,
>					  next :: Maybe BlockingContext ,
>					  prev :: Maybe BlockingContext }

> getNI :: BlockingContext -> NodeInfo
> getNI (PBranchContext _ d _ _ _ _ _ _) = nodeInfo d
> getNI (PWaitContext _ d _ _ _ _ _ _) = nodeInfo d
> getNI (BlockingStmt s _ _ _ _ _) = nodeInfo s
> getNI (WhileContext d _ _ _ _ _ _) = nodeInfo d
> getNI (CompoundContext c _ _ _ _ _ _) = nodeInfo c
> getNI (ForContext d _ _ _ _ _ _) = nodeInfo d
> getNI (IfContext d _ _ _ _ _ _ _) = nodeInfo d
> getNI (FunContext d _ _ _ _) = nodeInfo d

> instance Eq BlockingContext where
>     (==) (FunContext fDef1 _ _ _ _) (FunContext fDef2 _ _ _ _) = (nodeInfo fDef1) == (nodeInfo fDef2)
>     (==) (IfContext ifDef1 _ _ _ _ _ _ _) (IfContext ifDef2 _ _ _ _ _ _ _) = (nodeInfo ifDef1) == (nodeInfo ifDef2)
>     (==) (ForContext fDef1 _ _ _ _ _ _) (ForContext fDef2 _ _ _ _ _ _) = (nodeInfo fDef1) == (nodeInfo fDef2)
>     (==) (WhileContext wDef1 _ _ _ _ _ _) (WhileContext wDef2 _ _ _ _ _ _) = (nodeInfo wDef1) == (nodeInfo wDef2)
>     (==) (CompoundContext cDef1 _ _ _ _ _ _) (CompoundContext cDef2 _ _ _ _ _ _) = (nodeInfo cDef1) == (nodeInfo cDef2)
>     (==) (BlockingStmt s1 _ _ _ _ _) (BlockingStmt s2 _ _ _ _ _) = (nodeInfo s1) == (nodeInfo s2)
>     (==) (PWaitContext _ pw1 _ _ _ _ _ _) (PWaitContext _ pw2 _ _ _ _ _ _) = (nodeInfo pw1) == (nodeInfo pw2)
>     (==) (PBranchContext _ pb1 _ _ _ _ _ _) (PBranchContext _ pb2 _ _ _ _ _ _) = (nodeInfo pb1) == (nodeInfo pb2)
>     (==) _ _ = False

> isLastContext :: BlockingContext -> Bool
> isLastContext b = isNothing $ next b

> isLastContextInFun :: BlockingContext -> Bool
> isLastContextInFun b
>	| isLastContext b && parentIsFun b = True
>	| isLastContext b && (isJust $ parent b) = isLastContextInFun $ fromJust $ parent b
>	| otherwise = False

> parentIsFun :: BlockingContext -> Bool
> parentIsFun b = case (parent b) of { (Just (FunContext _ _ _ _ _)) -> True ; _ -> False }

> parentIsFor :: BlockingContext -> Bool
> parentIsFor b = (isJust $ parent b) && (isForContext $ fromJust $ parent b)

> parentIsWhile :: BlockingContext -> Bool
> parentIsWhile b = (isJust $ parent b) && (isWhileContext $ fromJust $ parent b)

> parentIsIf :: BlockingContext -> Bool
> parentIsIf b = (isJust $ parent b) && (isIfContext $ fromJust $ parent b)

> parentIsCompound :: BlockingContext -> Bool
> parentIsCompound b = (isJust $ parent b) && (isCompoundContext $ fromJust $ parent b)

> parentIsPBranch :: BlockingContext -> Bool
> parentIsPBranch b = (isJust $ parent b) && (isPBranchContext $ fromJust $ parent b)

> getPBranchAncestor :: BlockingContext -> BlockingContext
> getPBranchAncestor b | isPBranchContext b = b
>		       | otherwise = getPBranchAncestor $ fromJust $ parent b

> hasPBranchAncestor :: BlockingContext -> Bool
> hasPBranchAncestor b 
>	| (isJust $ parent b) && (isPBranchContext $ fromJust $ parent b) = True
>	| (isJust $ parent b) = hasPBranchAncestor $ fromJust $ parent b
>	| otherwise = False

> getPWaitAncestor :: BlockingContext -> BlockingContext
> getPWaitAncestor b | isPWaitContext b = b
>		     | otherwise = getPWaitAncestor $ fromJust $ parent b

> hasPWaitAncestor :: BlockingContext -> Bool 
> hasPWaitAncestor b | isPWaitContext b = True
>		     | isJust $ parent b = hasPWaitAncestor $ fromJust $ parent b
>		     | otherwise = False

> isIfContext :: BlockingContext -> Bool
> isIfContext (IfContext _ _ _ _ _ _ _ _) = True
> isIfContext _ = False

> isForContext :: BlockingContext -> Bool
> isForContext (ForContext _ _ _ _ _ _ _) = True
> isForContext _ = False

> isWhileContext :: BlockingContext -> Bool
> isWhileContext (WhileContext _ _ _ _ _ _ _) = True
> isWhileContext _ = False

> isCompoundContext :: BlockingContext -> Bool
> isCompoundContext (CompoundContext _ _ _ _ _ _ _) = True
> isCompoundContext _ = False

> isPBranchContext :: BlockingContext -> Bool
> isPBranchContext (PBranchContext _ _ _ _ _ _ _ _) = True
> isPBranchContext _ = False

> isPWaitContext :: BlockingContext -> Bool
> isPWaitContext (PWaitContext _ _ _ _ _ _ _ _) = True
> isPWaitContext _ = False

> isBlockingStmt :: BlockingContext -> Bool
> isBlockingStmt (BlockingStmt _ _ _ _ _ _) = True
> isBlockingStmt _ = False

> getCtxName :: BlockingContext -> String
> getCtxName (BlockingStmt _ _ _ _ _ _) = "BlockingStmt"
> getCtxName (FunContext _ _ _ _ _) = "FunContext"
> getCtxName (ForContext _ _ _ _ _ _ _) = "ForContext"
> getCtxName (WhileContext _ _ _ _ _ _ _) = "WhileContext"
> getCtxName (CompoundContext _ _ _ _ _ _ _) = "CompoundContext"
> getCtxName (PWaitContext _ _ _ _ _ _ _ _) = "PWaitContext"
> getCtxName (PBranchContext _ _ _ _ _ _ _ _) = "PBranchContext"
> getCtxName (IfContext _ _ _ _ _ _ _ _) = "IfContext"

> instance Show BlockingContext where
>	show (BlockingStmt stmt _ _ parent prev next) = "BlockingStmt: " ++ (show . pretty $ stmt) ++ "\n"
>	show (FunContext funDef _ _ _ _) = "FunContext: " ++ (getFunDefName $ funDef) ++ "\n"
>	show (ForContext forDef forStmts _ _ _ _ _) = "ForContext"
>	show (WhileContext whileDef whileStmts _ _ _ _ _) = "WhileContext"
>	show (CompoundContext cDef cStmts _ _ _ _ _) = "CompoundContext"
>	show (PWaitContext _ _ stmts _ _ _ _ _) = "PWaitContext"
>	show (PBranchContext _ _ stmts _ _ _ _ _) = "PBranchContext"
>	show (IfContext ifDef ifCtx elseCtx nbBefore nbAfter parent next prev) =
>		"IfContext: IF: " ++ (concat (map (("IFNODE: " ++) . (drawTree . contextToTree)) ifCtx)) ++ 
>		(if isJust elseCtx then ("\n\n\tELSE: " ++ 
>					(concat (map (("ELSENODE: " ++) . (drawTree . contextToTree)) $ fromJust elseCtx)))
>		 else "")

> mkTreeNode :: BlockingContext -> (String, [BlockingContext])
> mkTreeNode b@(BlockingStmt stmt _ _ _ _ _) = 
>	let nodeName = show b
>	in (nodeName, [])

> mkTreeNode b@(FunContext funDef stmts _ _ _) =
>	let nodeName = show b
>       in (nodeName, stmts)

> mkTreeNode b@(ForContext forDef forStmts _ _ _ _ _) =
>	let nodeName = show b
>	in (nodeName, forStmts)

> mkTreeNode b@(WhileContext whileDef whileStmts nbStmtsBefore nbStmtsAfter parent next prev) =
>	let nodeName = show b
>	in (nodeName, whileStmts)

> mkTreeNode b@(CompoundContext cDef cStmts nbStmtBefore nbStmtsAfter parent next prev) =
>       let nodeName = show b
>       in (nodeName, cStmts)

> mkTreeNode b@(IfContext ifDef ifCtx elseCtx nbBefore nbAfter parent next prev) =
>	let nodeName = show b
>	in (nodeName, [])

> mkTreeNode b@(PWaitContext _ _ stmts _ _ _ _ _) =
>	let nodeName = show b
>	in (nodeName, stmts)

> mkTreeNode b@(PBranchContext _ _ stmts _ _ _ _ _) =
>	let nodeName = show b
>	in (nodeName, stmts)

> contextToTree :: BlockingContext -> Tree String
> contextToTree b = unfoldTree mkTreeNode b

> getFunContext :: BlockingContext -> BlockingContext
> getFunContext b@(FunContext _ _ _ _ _) = b
> getFunContext b = getFunContext $ fromJust $ parent b

> getParent :: BlockingContext -> BlockingContext
> getParent b@(FunContext _ _ _ _ _) = b
> getParent b = getParent $  fromJust $ parent b

> findLoopParent :: BlockingContext -> Maybe BlockingContext
> findLoopParent b
>	| parentIsWhile b = parent b
>	| parentIsFor b = parent b
>       | isNothing $ parent b = Nothing
>	| otherwise = findLoopParent $ fromJust $ parent b

> getParentName :: BlockingContext -> String
> getParentName (FunContext funDef _ _ _ _) = getFunDefName funDef
> getParentName b = getParentName $ assert (isJust $ parent b) fromJust $ parent b

> mkContextTree :: Maybe BlockingContext -> Maybe BlockingContext -> Maybe BlockingContext -> BlockingContext -> BlockingContext

> mkContextTree parent next prev (FunContext funDef stmts _ _ _) = this
>	where this = FunContext funDef (fixupContextList (Just this) Nothing stmts) Nothing Nothing Nothing

> mkContextTree parent next prev (IfContext ifDef ifStmts elseStmts nbBefore nbAfter _ _ _) = this
>	where newElseStmts = if isJust elseStmts then 
>				Just (fixupContextList (Just this) Nothing (fromJust  elseStmts)) else Nothing
>	      this = IfContext ifDef
>		               (fixupContextList (Just this) Nothing ifStmts)
>			       newElseStmts
>		               nbBefore
>			       nbAfter
>			       parent
>			       next
>		 	       prev

> mkContextTree parent next prev (ForContext forDef forStmts nbBefore nbAfter _ _ _) = this
>	where this = ForContext forDef (fixupContextList (Just this) Nothing forStmts) nbBefore nbAfter parent next prev

> mkContextTree parent next prev (WhileContext whileDef whileStmts nbBefore nbAfter _ _ _) = this
>	where this = WhileContext whileDef (fixupContextList (Just this) Nothing whileStmts) nbBefore nbAfter parent next prev

> mkContextTree parent next prev (CompoundContext cDef cStmts nbBefore nbAfter _ _ _) = this
>       where this = CompoundContext cDef (fixupContextList (Just this) Nothing cStmts) nbBefore nbAfter parent next prev

> mkContextTree parent next prev (BlockingStmt stmt nbBefore nbAfter _ _ _) = this
>	where this = BlockingStmt stmt nbBefore nbAfter parent next prev

> mkContextTree parent next prev (PWaitContext id waitDef stmts nbBefore nbAfter _ _ _) = this
>	where this = PWaitContext id waitDef (fixupContextList (Just this) Nothing stmts) nbBefore nbAfter parent next prev

> mkContextTree parent next prev (PBranchContext id branchDef stmts nbBefore nbAfter _ _ _) = this
>	where this = PBranchContext id branchDef (fixupContextList (Just this) Nothing stmts) nbBefore nbAfter parent next prev

> fixupContextList :: Maybe BlockingContext -> Maybe BlockingContext -> [BlockingContext] -> [BlockingContext]

> fixupContextList parent prev [] = []
> fixupContextList parent prev (b:[]) = (mkContextTree parent Nothing prev b):[]

> fixupContextList parent prev (b:bs) = newb:next:rest
>	where newb = mkContextTree parent (Just next) prev b
>	      next:rest = fixupContextList parent (Just newb) bs

> getPWaitSharedDecls :: BlockingContext -> [CDecl]
> getPWaitSharedDecls (PWaitContext id (CPWait (CCompound _ bitems _) _) _ _ _ _ _ _) =
>	filterDecls (\_ -> True) (not . isPrivateSpec) removeSharedSpecFromDecl $ findFuncDecls bitems
> getPWaitSharedDecls _ = []

> getPWaitPrivateDecls :: BlockingContext -> [CDecl]
> getPWaitPrivateDecls (PWaitContext id (CPWait (CCompound _ bitems _) _) _ _ _ _ _ _) =
>       filterDecls isPrivateSpec (not . isSharedSpec) removePrivateSpecFromDecl $ findFuncDecls bitems

> getPWaitId :: BlockingContext -> String
> getPWaitId (PWaitContext id _ _ _ _ _ _ _) = id
> getPWaitId _ = error "Not a pwait context"

> containsPWait :: BlockingContext -> Bool
> containsPWait (PWaitContext _ _ _ _ _ _ _ _) = True
> containsPWait (PBranchContext _ _ stmts _ _ _ _ _) = any containsPWait stmts
> containsPWait (BlockingStmt _ _ _ _ _ _) = False
> containsPWait (WhileContext _ stmts _ _ _ _ _) = any containsPWait stmts
> containsPWait (ForContext _ stmts _ _ _ _ _) = any containsPWait stmts
> containsPWait (CompoundContext _ stmts _ _ _ _ _) = any containsPWait stmts
> containsPWait (IfContext _ ifStmts elseStmts _ _ _ _ _) = 
>	(any containsPWait ifStmts) ||
>	(if isJust elseStmts then any containsPWait $ fromJust elseStmts else False)
> containsPWait (FunContext _ stmts _ _ _) = any containsPWait stmts

> foldPWaits :: [BlockingContext] -> [BlockingContext]
> foldPWaits [] = []
> foldPWaits bs = foldl1 (++) $ map getPWaits bs

> getPWaits :: BlockingContext -> [BlockingContext]
> getPWaits b@(PWaitContext _ _ _ _ _ _ _ _) = [b]
> getPWaits (PBranchContext _ _ stmts _ _ _ _ _) = foldPWaits stmts
> getPWaits (BlockingStmt _ _ _ _ _ _) = []
> getPWaits (WhileContext _ stmts _ _ _ _ _) = foldPWaits stmts
> getPWaits (ForContext _ stmts _ _ _ _ _) = foldPWaits stmts
> getPWaits (CompoundContext _ stmts _ _ _ _ _) = foldPWaits stmts
> getPWaits (IfContext _ ifStmts elseStmts _ _ _ _ _) = 
>	(foldPWaits ifStmts) ++
>	(if isJust elseStmts then foldPWaits $ fromJust elseStmts else [])
> getPWaits (FunContext _ stmts _ _ _) = foldPWaits stmts

> getPBranchDecls :: BlockingContext -> [CDecl]
> getPBranchDecls (PBranchContext id (CPBranch (CCompound _ bitems _) _) _ _ _ _ _ _) =
>	findFuncDecls bitems
> getPBranchDecls _ = []

> getPBranchId :: BlockingContext -> String
> getPBranchId (PBranchContext id _ _ _ _ _ _ _) = id
> getPBranchId _ = error "Not a pbranch context"

> foldPBranches :: [BlockingContext] -> [BlockingContext]
> foldPBranches [] = []
> foldPBranches bs = foldl1 (++) $ map getPBranches bs

> getPBranches :: BlockingContext -> [BlockingContext]
> getPBranches (PWaitContext _ _ stmts _ _ _ _ _) = foldPBranches stmts
> getPBranches b@(PBranchContext _ _ _ _ _ _ _ _) = [b]
> getPBranches (BlockingStmt _ _ _ _ _ _) = []
> getPBranches (WhileContext _ stmts _ _ _ _ _) = foldPBranches stmts
> getPBranches (ForContext _ stmts _ _ _ _ _) = foldPBranches stmts
> getPBranches (CompoundContext _ stmts _ _ _ _ _) = foldPBranches stmts
> getPBranches (IfContext _ ifStmts elseStmts _ _ _ _ _) = 
>	(foldPBranches ifStmts) ++
>	(if isJust elseStmts then foldPBranches $ fromJust elseStmts else [])
