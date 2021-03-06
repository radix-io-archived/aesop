> module Walker where
> import Language.C
> import Language.C.System.GCC   -- preprocessor used
> import Language.C.Pretty
> import Language.C.Data.Ident
> import System.Environment
> import Data.Typeable
> import Data.Maybe
> import Data.Either
> import qualified Data.HashTable.IO as H
> import Data.Hashable
> import Control.Exception
> import Data.Generics
> import Data.Generics.Schemes
> import Data.List
> import Debug.Trace
> import Control.Monad.State
> import BlockingContext
> import CParse
> import CGen
> import Control.Monad.Error

> type HashTable k v = H.BasicHashTable k v

Registry for blocking operations.  Once we see a function declaration
with the __blocking specifier, we add it to the registry.
 
> type FunDecl = (Ident, ReturnType, [CDecl])

> funDeclName :: FunDecl -> Ident
> funDeclName (n, _, _) = n

> funDeclReturnType :: FunDecl -> ReturnType
> funDeclReturnType (_, r, _) = r

> funDeclParams :: FunDecl -> [CDecl]
> funDeclParams (_, _, p) = p

The FPType is either a function or a struct.  The FPFun is the
function pointer, declarated within a struct or simply declarated locally.

The struct (FPStruct) is a struct with a function pointer as one of its fields.

> data FPType = FPFun     { name :: Ident , ftype :: FunDecl }
>	      | FPStruct  { name :: Ident , field :: Ident, inner :: FPType }

> instance Show FPType where
>	show (FPFun n _) = "Fun: " ++ (show n)
>	show (FPStruct n f t) = "Struct: " ++ (show n) ++ ":"  ++ (show f) ++ "[" ++ (show t) ++ "]"

> isFPStruct :: FPType -> Bool
> isFPStruct (FPStruct _ _ _) = True
> isFPStruct _ = False

> getInner :: FPType -> Maybe FPType
> getInner (FPStruct _ _ t) = Just t
> getInner _ = Nothing

> isFPFun :: FPType -> Bool
> isFPFun (FPFun _ _) = True
> isFPFun _ = False

> getFunDecl :: FPType -> Maybe FunDecl
> getFunDecl (FPFun _ f) = Just f
> getFunDecl _ = Nothing

> type FPTypeRegistry = [FPType]

> type VarMap = HashTable Ident CDecl

> data VarRegistry = VarRegistry { locals :: VarMap , globals :: VarMap }

> newVarRegistry :: IO VarRegistry
> newVarRegistry = do
>	l <- H.new
>	g <- H.new
>	return $ VarRegistry l g

> resetLocalVarsInRegistry :: VarRegistry -> IO VarRegistry
> resetLocalVarsInRegistry v = do
>	l <- H.new
>	return $ VarRegistry l (globals v)

> insertGlobalDeclsToVarRegistry :: [CDecl] -> VarRegistry -> IO ()
> insertGlobalDeclsToVarRegistry decls v = do
>	let alldecls = concatMap splitDecls decls
>	mapM_ (\d -> H.insert (globals v) (getCDeclName $ d) d) alldecls

> insertLocalDeclsToVarRegistry :: [CDecl] -> VarRegistry -> IO ()
> insertLocalDeclsToVarRegistry decls v = do
>	let alldecls = concatMap splitDecls decls
>	    insertD d = H.insert (locals v) (getCDeclName $ d) d
>	mapM_ insertD alldecls

> lookupInVarRegistry :: Ident -> VarRegistry -> IO (Maybe CDecl)
> lookupInVarRegistry i v = do
>       res <- H.lookup (locals v) i
>	if isJust res
>	  then return res
>	  else H.lookup (globals v) i

The monad transformer that we thread through our functions

The Walker is the state that we pass around our functions as we walk the AST.  It primarily holds
the blocking call information, but is actually a tuple of:
filename, blocking call registry, and blocking function pointer registry

> data Walker = Walker {
>       debug :: Bool,
>	filename :: String,
>       compiler :: String,
>	fpTypeReg :: FPTypeRegistry,
>	fpTypeLocalReg :: FPTypeRegistry,
>	varReg :: VarRegistry,
>       blockingParser :: Maybe MacroParser
> }

> newWalkerState :: Bool ->
>                   String ->
>                   FilePath ->
>                   FilePath ->
>                   [String] ->
>                   [Ident] ->
>                   IO Walker

> newWalkerState debug fname compiler macroHeader gccopts tdIdents = do
>	varReg <- newVarRegistry
>       bp <- mkParser compiler macroHeader gccopts
>       let nbp = addTypeIdents tdIdents bp
>	return $ Walker debug fname compiler [] [] varReg (Just nbp)

> setBlockingParser :: MacroParser -> WalkerT ()
> setBlockingParser b = do
>       w <- get
>       put $ w { blockingParser = Just b }

> getBlockingParser :: WalkerT MacroParser
> getBlockingParser = do
>       w <- get
>       let bp = blockingParser w
>       return $ assert (isJust bp) (fromJust bp)

> type WalkerT = StateT Walker IO

> wdebug :: String -> WalkerT ()
> wdebug msg = do
>       w <- get
>       when (debug w) $ putStrLnW msg
>       return ()

> getFileName :: WalkerT String
> getFileName = do
>	w <- get
>	return $ filename w

> getFilePosStr :: NodeInfo -> WalkerT (String)
> getFilePosStr (NodeInfo p _ _)  = do
> 	fname <- getFileName
>	return (fname ++ ":" ++ (show $ posRow p) ++ ":" ++ (show $ posColumn p))

> clearLocalFunPtrRegistry :: WalkerT ()
> clearLocalFunPtrRegistry = do
>	w <- get
>	put $ w { fpTypeLocalReg = [] }

> resetLocals :: WalkerT ()
> resetLocals = do
>	w <- get
>	newvr <- liftIO $ resetLocalVarsInRegistry $ varReg w
>	put $ w { varReg = newvr }

> addLocals :: [CDecl] -> WalkerT ()
> addLocals ds = do
>	w <- get
>	liftIO $ insertLocalDeclsToVarRegistry ds $ varReg w

> addGlobals :: [CDecl] -> WalkerT ()
> addGlobals ds = do
>	w <- get
>	liftIO $ insertGlobalDeclsToVarRegistry ds $ varReg w

Get the chain of variable references starting
at the outermost. i.e.

struct a a1;
struct b b1;
typedef struct c ct;
ct c1;
struct d d1;

a1->b1->c1->d1

Turns into:

(a, [b1, c1, d1])

> lookupVar :: CExpr -> WalkerT (Maybe [Ident])
> lookupVar e@(CVar name _) = do
>       wdebug $ "[lookupVar]: CVar: " ++ (show name)
>	w <- get
>	d <- liftIO $ lookupInVarRegistry name $ varReg w
>	if isNothing d
>	    then return Nothing
>	    else do
>	        let t = getTypeSpecFromDecl $ fromJust d
>		    r = case t of
>			  (CTypeDef tname _) -> Just [tname]
>			  (CSUType (CStruct _ (Just tname) _ _ _) _) -> Just [tname]
>			  _ -> Nothing
>		if isFunDecl $ fromJust d
>		    then return $ Just [getCDeclName $ fromJust d]
>		    else if isJust r
>		             then return r
>		             else return Nothing

> lookupVar (CMember expr name _ _) = do
>       wdebug $ "[lookupVar]: CMember: " ++ (show name)
>	w <- get
>	inner <- lookupVar expr
>	if isNothing inner
>		then do
>                       return Nothing
>		else return $ Just $ (fromJust inner) ++ [name]

> lookupVar e = do
>	return Nothing

> insertFP :: FPType -> WalkerT ()
> insertFP fptype = do
>	w <- get
>	put $ w { fpTypeReg = fptype : (fpTypeReg w) }

Register a blocking function pointer.

> registerBlocking :: FunDecl -> WalkerT ()
> registerBlocking fd = do
>	let (n, (rt, rd), p) =  fd
>	insertFP $ FPFun n fd
>       let d = mkIdentFunDecl n rt rd p
>	addGlobals [d]

Register a blocking struct based on the struct type (structName), the field
with the function pointer, and type of the function pointer (function signature).

> registerBlockingStruct :: Ident -> Ident -> FunDecl -> WalkerT ()
> registerBlockingStruct structName fieldName funDecl = do
>	let (n, (rt, rd), p) = funDecl
>	insertFP $ FPStruct structName fieldName (FPFun n funDecl)

We have to deal with structs that have members that are structs that have blocking
function pointers.  I.e:

struct inner
{
    __blocking void (*blockingfun)(void);
};

struct outer
{
    struct inner i;
};

So we need a way to lookup if a field within a struct is a blocking struct, and
if so, register the outer struct too.

> lookupAndRegisterBlockingStruct :: Ident -> Ident -> Ident -> WalkerT ()
> lookupAndRegisterBlockingStruct structName fieldName fieldType = do
>	w <- get
>	let reg = fpTypeReg w
>	    matching = filter (\f -> (isFPStruct f && ((name f) == fieldType))) reg
>	if not $ null matching
>	    then do
>		mapM_ (\m -> insertFP $ FPStruct structName fieldName m) matching
>	    else return ()

> lookupAndRegisterBlockingTypedef :: Ident -> Ident -> WalkerT ()
> lookupAndRegisterBlockingTypedef structName typeName = do
>	w <- get
>	let reg = fpTypeReg w
>	    matching = filter (\f -> (isFPStruct f && ((name f) == structName))) reg
>	if not $ null matching
>	    then do
>               -- when ((length matching) /= 1) $ do
>                   -- putStrLnW $ identToString structName
>                   -- mapM_ (putStrLnW . show) matching
>		-- assert ((length matching) == 1) return ()
>		let mkFP (FPStruct s f fpt) = FPStruct typeName f fpt
>		mapM_ (insertFP . mkFP) matching
>	  else return ()

> registerLocalBlocking :: FunDecl -> WalkerT ()
> registerLocalBlocking fd = do
>	w <- get
>	let (n, _, _) = fd
>	put $ w { fpTypeLocalReg = (FPFun n fd) : (fpTypeLocalReg w) }

> getAllBlocking :: WalkerT [FPType]
> getAllBlocking = do
>	w <- get
>	let bs = fpTypeReg w
>	    bls = fpTypeLocalReg w
>	return $ bs ++ bls

> showAllBlocking :: WalkerT ()
> showAllBlocking = do
>	all <- getAllBlocking
>	putStrLnW $ unlines $ map show all

> tr :: [String] -> FPType -> String -> a -> a
> tr ss st a = trace $ a ++ "::" ++ (show ss) ++ "::" ++ (show st)

> matchFP :: [Ident] -> FPType -> Maybe FunDecl
> matchFP ss@(s:p:seqs) st@(FPStruct n f t)
>	| s == n && p == f && null seqs && isFPFun t = getFunDecl t
>	| s == n && p == f = matchiFP seqs (fromJust (getInner t))
>	| otherwise = Nothing

> matchFP s f = matchiFP s f

> matchiFP :: [Ident] -> FPType -> Maybe FunDecl
> matchiFP [] _ = Nothing

> matchiFP ss@(s:[]) st@(FPFun n t) = if s == n then Just t else Nothing
> matchiFP ss@(s:seqs) st@(FPFun n t) = Nothing
> matchiFP ss@(s:seqs) st@(FPStruct n f t) =
>	if s == f then matchFP seqs t else Nothing

> lookupBlocking :: CExpr -> WalkerT (Maybe FunDecl)
> lookupBlocking (CCall e _ _) = do
>     lookupBlocking e

> lookupBlocking c = do
>       wdebug $ "[lookupBlocking]: " ++ (show $ pretty c)
>	v <- lookupVar c
>	if isNothing v
>	    then do
>               wdebug $ "[lookupBlocking]: returning Nothing"
>               return Nothing
>	    else do
>		all <- getAllBlocking
>		let fs = mapMaybe (matchFP $ fromJust v) $ all
>	        if null fs then do
>                       wdebug $ "[lookupBlocking]: returning Nothing"
>                       return Nothing 
>                 else do
>                       wdebug $ "[lookupBlocking]: returning " ++ (identToString $ funDeclName $ head fs)
>                       return $ Just $ head fs

Utility lift the print function into the WalkerT monad transformer

> putStrLnW :: String -> WalkerT ()
> putStrLnW = liftIO . putStrLn
 
> putStrW :: String -> WalkerT ()
> putStrW = liftIO . putStr
