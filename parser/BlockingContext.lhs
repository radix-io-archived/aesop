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

> data BlockingContext = FunContext {      funDef :: CFunDef }

>                      | PWaitContext {    pwaitStmt :: CStat ,
>                                          parent :: BlockingContext }

>                      | PBranchContext {  pbranchStmt :: CStat ,
>                                          parent :: BlockingContext }

> isFunContext :: BlockingContext -> Bool
> isFunContext (FunContext _) = True
> isFunContext _ = False

> isPWaitContext :: BlockingContext -> Bool
> isPWaitContext (PWaitContext _ _) = True
> isPWaitContext _ = False

> isPBranchContext :: BlockingContext -> Bool
> isPBranchContext (PBranchContext _ _) = True
> isPBranchContext _ = False

> parentIsPWait :: BlockingContext -> Bool
> parentIsPWait (PBranchContext _ parent)
>       | isPWaitContext parent = True
>       | otherwise = False
> parentIsPWait _ = False

> getParent :: BlockingContext -> BlockingContext
> getParent b@(FunContext _) = b
> getParent b = getParent $ parent b

> getParentName :: BlockingContext -> String
> getParentName (FunContext funDef) = getFunDefName funDef
> getParentName b = getParentName $ parent b

> getPWaitSharedDecls :: CStat -> [CDecl]
> getPWaitSharedDecls (CPWait (CCompound _ bitems _) _) =
>	filterDecls (\_ -> True) (not . isPrivateSpec) removeSharedSpecFromDecl $ findFuncDecls bitems
> getPWaitSharedDecls _ = []

> getPWaitPrivateDecls :: CStat -> [CDecl]
> getPWaitPrivateDecls (CPWait (CCompound _ bitems _) _) =
>       filterDecls isPrivateSpec (not . isSharedSpec) removePrivateSpecFromDecl $ findFuncDecls bitems

> getPBranchDecls :: CStat -> [CDecl]
> getPBranchDecls (CPBranch (CCompound _ bitems _) _) =
>      findFuncDecls bitems
> getPBranchDecls _ = []
