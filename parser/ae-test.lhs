> module Main where
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
> import Language.C.Data.Node
> import Language.C.Data.Position

> main :: IO ()
> main = do
>       let decls = mkDeclsFromCPPMacro "/radix-homes/slang/dev/grayskull/code/src/gsl/" "common/triton-blocking.h" (mkNodeInfoOnlyPos (initPos "dummy")) "GSTESTDECLMACRO" ["blah"]
>       writeFile "/tmp/out1" ""
>       mapM_ ((appendFile "/tmp/out1") . show. pretty) (map CDeclExt decls)
