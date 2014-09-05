> module Header where
> import Language.C
> import Language.C.Data.Position
> import Language.C.Data.Node
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

> data HeaderS = HeaderS { header :: [String], trans :: Bool, pos :: Position }

> newHeaderState :: FilePath -> IO HeaderS
> newHeaderState f = do
>	s <- readFile f
>	return $ HeaderS (lines s) False (position 0 f 0 0)

> type HeaderT = StateT HeaderS IO

> getTrans :: HeaderT Bool
> getTrans = do { h <- get ; return $ trans h }

> setTrans :: Bool -> HeaderT ()
> setTrans b = do { (HeaderS h l p) <- get ; put (HeaderS h b p) }

> getLastPos :: HeaderT Position
> getLastPos = do { h <- get ; return $ pos h }

> setLastPos :: Position -> HeaderT ()
> setLastPos np = do { (HeaderS h l p) <- get ; put (HeaderS h l np) }

> getHeaderLines :: Int -> HeaderT [String]
> getHeaderLines i = do 
>	(HeaderS h l p) <- get
>	let (n, rest) = splitAt i $ h 
>	put (HeaderS rest l p)
>	return n

> getRemainingLines :: HeaderT [String]
> getRemainingLines = do
>	(HeaderS h l p) <- get
>	return h

> skipBytes :: Int -> HeaderT ()
> skipBytes i = do
>	(HeaderS h l p) <- get
>	let (n, rest) = splitAt i $ unlines h
>	put (HeaderS (lines rest) l p)

> outputHeader :: FilePath -> FilePath -> [([CExtDecl], CExtDecl)] -> IO ()
> outputHeader hf outf pairs = do
>	hs <- newHeaderState hf
>	runStateT (outputPairs hf outf pairs) $ hs
>	return ()

> outputPairs :: FilePath -> FilePath -> [([CExtDecl], CExtDecl)] -> HeaderT ()
> outputPairs hf outf pairs = do
>	mapM_ (outputPair hf outf) pairs
>	l <- getRemainingLines
>	liftIO $ appendFile outf $ unlines l

> outputPair :: FilePath -> FilePath -> ([CExtDecl], CExtDecl) -> HeaderT ()
> outputPair hf outf ([], c) | isInFile c hf = return ()
> outputPair hf outf (ds, c) | isInFile c hf = do
>	lastPos <- getLastPos
>	let currentPos = posOfNode $ nodeInfo c
>	    n = (posRow currentPos) - (posRow lastPos) - 1
>	    ltp = fst $ getLastTokenPos $ nodeInfo c
>	hl <- getHeaderLines n
>	getHeaderLines $ (posRow $ ltp) - (posRow currentPos) + 1
>	liftIO $ appendFile outf $ unlines hl 
>	liftIO $ appendFile outf $ unlines $ map (show . pretty) ds
>	setLastPos $ ltp 
> outputPair hf outf (ds, c) = do
>	return ()

> isInFile :: CExtDecl -> String -> Bool
> isInFile c f = (posFile $ posOfNode $ nodeInfo c) == f
