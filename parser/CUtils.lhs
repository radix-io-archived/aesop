> module CUtils (orElseM,
>		 takeJust,
>		 orElseMMaybe,
>		 orElseMBool,
>		 orElseMAdd,
>		 spanM,
>		 takeWhileM,
>		 spliceM,
>		 splitM) where
> import Language.C
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


> orElseM :: (Monad m) => (a -> a -> a) -> m a -> m a -> m a
> orElseM compare x y = do mx <- x; my <- y; return $ compare mx my

> takeJust :: Maybe a -> Maybe a -> Maybe a
> takeJust a b = if isJust a then a else b

> orElseMMaybe :: (Monad m) => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
> orElseMMaybe = orElseM takeJust

> orElseMBool :: (Monad m) => m Bool -> m Bool -> m Bool
> orElseMBool = orElseM (||)

> orElseMAdd :: (Monad m) => m Int -> m Int -> m Int
> orElseMAdd = orElseM (+)

> spanM                 :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
> spanM _ []            =  return ([], [])
> spanM p (x:xs)        =  do
>	res <- p x
>	if res then (do (ys,zs) <- spanM p xs ; return (x:ys,zs)) else (return ([], x:xs))

> takeWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
> takeWhileM _ []          =  return []
> takeWhileM p (x:xs) = do
>	res <- p x
>	if res then (do as <- takeWhileM p xs ; return $ x:as) else (return [])

Take a monad predicate and a list, for each element of the list evaluate with the predicate
and split the list at that point if the predicate returns False.  So the result is a
monad that contains a list of lists.  Each internal list contains elements from the previous
list up-to the failed element.  For example, the list:

[1, 2, 2, 1, 2, 3, 1, 1, 2, 3, 2, 3, 1, 2]

and a predicate: (== 1)

will return:

[[1], [2, 2, 1] [2, 3, 1], [1], [2, 3, 2, 3, 1], [2]]

> spliceM		:: (Monad m) => (a -> m Bool) -> [a] -> m [[a]]
> spliceM _ []		= return [[]]
> spliceM p (x:xs)	= do
>	res <- p x
>	l:ls <- spliceM p xs
>	if res then return $ (x:l):ls else return $ [x]:l:ls

> splitM		:: (Monad m) => (a -> m Bool) -> [a] -> m [Either a [a]]
> splitM _ []		= return []
> splitM p (a:[])	= do
>	res <- p a
>	if res then return [Right [a]] else return [Left a]
> splitM p (x:xs)	= do
>	res <- p x
>	l:ls <- splitM p xs
>	if res then case l of { Right as -> return $ (Right (x:as)):ls ; _ -> return $ (Right [x]):(l:ls) } else return $ (Left x):(l:ls)


DEBUGGING OUTPUT

Toggle abp_trace:

> abp_debug_enabled = True

-- > abp_debug_enabled = False

Aesop Blocking Parser debug:

> abp_debug :: String -> IO ()
> abp_debug s =
>       if abp_debug_enabled then putStrLn ("aecc: debug: " ++ s) 
>       else putStr ""
