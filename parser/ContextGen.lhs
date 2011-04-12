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

> isBlockingCall :: CExpr -> WalkerT Bool
> isBlockingCall c@(CCall v args _) =  do 
>	res <- lookupBlocking v
>	if (isJust res) then (return True) else (return False)
> isBlockingCall _ = return False

> getBlockingExpr :: CExpr -> WalkerT [CExpr]
> getBlockingExpr c@(CCall v args _) = do
>       b <- isBlockingCall c
>       if b then return [c] else return []
> getBlockingExpr _ = return []

> accumM :: (Monad m) => (a -> a -> a) -> m a -> m a -> m a
> accumM acc mx my = do x <- mx; y <- my; return $ acc x y

> getAllBlockingCalls :: CFunDef -> WalkerT [CExpr]
> getAllBlockingCalls funDef = do
>       everything (accumM (++)) (mkQ (return []) getBlockingExpr) funDef

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
