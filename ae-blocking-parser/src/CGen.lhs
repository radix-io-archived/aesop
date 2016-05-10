> module CGen where
> import Language.C
> import Language.C.System.GCC   -- preprocessor used
> import Language.C.System.Preprocess
> import Language.C.Data.Position
> import Language.C.Data.Name
> import Language.C.Data.InputStream
> import System.Environment
> import Data.Typeable
> import Data.Maybe
> import Data.Either
> import Data.Generics
> import Data.Generics.Schemes
> import Control.Monad.State
> import qualified Data.ByteString
> import qualified Data.ByteString.Char8
> import Control.Exception
> import Data.List
> import CParse
> import System.IO
> import System.IO.Unsafe
> import System.Exit
> import System.Directory
> import System.Process
> import Debug.Trace
> import System.Time

> tmpFile :: String -> IO FilePath
> tmpFile s = do
>       (fp, h) <- openTempFile "/tmp" "__aesop_cgen_tmp.c"
>       hPutStrLn h s
>       hClose h
>       return fp

> newTmpFile :: IO FilePath
> newTmpFile = do
>       (fp, h) <- openTempFile "/tmp" "__aesop_cgen_tmp.c"
>       hClose h
>       return fp

> tmpHandle :: IO (FilePath, Handle)
> tmpHandle = do
>       (fp, h) <- openTempFile "/tmp" "__aesop_cgen_tmp.c"
>       return (fp, h)

> getTimeDiff :: ClockTime -> ClockTime -> Float
> getTimeDiff (TOD c1s c1p) (TOD c2s c2p) =
>       let u1 = (fromInteger c1s) * (1e6 :: Float) + (fromInteger c1p) * (1e-6 :: Float)
>           u2 = (fromInteger c2s) * (1e6 :: Float) + (fromInteger c2p) * (1e-6 :: Float)
>       in u2 - u1

> myCPP :: String -> Maybe FilePath -> [String] -> (Either String FilePath) -> IO FilePath
> myCPP compiler incheader options input = do
>       inputFile <- either (\s -> tmpFile s) (\fp -> return fp) input
>       outFile <- newTmpFile
>       let opts = ["-E"] ++ mkIncHeader ++ options ++ ["-o", outFile] ++ [inputFile]
>           mkIncHeader = if isJust incheader then ["-include", fromJust incheader] else []
>       (errorFP, errorH) <- tmpHandle
>       -- st <- getClockTime
>       pid <- runProcess compiler opts Nothing Nothing Nothing Nothing (Just errorH)
>       ec <- waitForProcess pid
>       -- et <- getClockTime
>       -- let td = getTimeDiff st et
>       -- putStrLn $ (show td) ++ "us"
>       either (\_ -> removeFile inputFile) (\_ -> return ()) input
>       case ec of
>               ExitSuccess -> do
>                       hClose errorH
>                       removeFile errorFP
>                       return outFile
>               ExitFailure e -> do
>                       isOpen <- hIsOpen errorH
>                       errorH <- if not isOpen then openFile errorFP ReadMode else return errorH
>                       errs <- hGetContents errorH
>                       removeFile errorFP
>                       trace ("Preprocessor failed with return code: " ++ (show e) ++ "\n" ++ errs ++ "\n\n") $ assert False $ return "dummy_file.c"
>                       hClose errorH
>                       return "dummy_file.c"
                       
> type ReturnType = (CTypeSpec, [CDerivedDeclr])

> returnToString :: ReturnType -> String
> returnToString (t, ds) = (show $ pretty t) ++ (join (map derivedToString ds))

> derivedToString :: CDerivedDeclr -> String
> derivedToString (CPtrDeclr _ _) = " * "
> derivedToString (CFunDeclr (Right ([], _)) _ _) = ""
> derivedToString (CFunDeclr (Right (params, _)) _ _) = foldl1 (++) $ map (show . pretty) params

Construct a new node info:

> newIdent :: String -> NodeInfo -> Ident
> newIdent name ni = mkIdent (posOfNode ni) name (Name 0)

Functions to generate AST objects from C template code

> mkInputStream :: String -> InputStream
> mkInputStream s = inputStreamFromString s

> constructDeclFromC :: NodeInfo -> String -> CDecl
> constructDeclFromC ni s =
>	let result = (execParser_ extDeclP (mkInputStream s) $ posOfNode ni) in
>		case result of
>		        -- parse succeeded, so we return the declaration
>			(Right (CDeclExt decl)) -> decl
>		        -- parse failed, so we assert fail for now.  The CDecl empty constructor is needed to match types
>			(Left pe) -> trace ("Parse Error in:\n\n" ++ ("mkStmtFromC failed: " ++ s) ++ (show pe)) $ assert False (CDecl [] [] ni)

> mkStmtFromC :: NodeInfo -> String -> CStat
> mkStmtFromC ni s =
>	let result = (execParser_ statementP (mkInputStream s) $ posOfNode ni) in
>		case result of
>			-- parse succeeded, so we return the statement
>			(Right stmt) -> stmt
>			(Left pe) -> trace ("Parse Error in:\n\n" ++
>					    ("mkStmtFromC failed: " ++ s) ++
>					    (show pe)) $ assert False (CBreak ni)

> mkStmtsFromCLines :: NodeInfo -> String -> [CStat]
> mkStmtsFromCLines ni lines = [mkStmtFromC ni lines]

> data MacroParser = MacroParser { compiler :: String, macheader :: FilePath, typedefs :: [Ident] }

> addTypeIdents :: [Ident] -> MacroParser -> MacroParser
> addTypeIdents idents m = m { typedefs = idents ++ (typedefs m) }

> getTypeIdents :: String -> FilePath -> [String] -> IO [Ident]
> getTypeIdents compiler macheader gccopts = do
>       rfile <- myCPP compiler (Just macheader) gccopts (Left "\n")
>       result <- readFile rfile
>       removeFile rfile
>       -- run the C parser on the result stream
>       let parsed = execParser_ translUnitP (mkInputStream result) $ initPos macheader
>       -- now get the typedefs as idents from the external declarations
>       case parsed of
>               (Right (CTranslUnit [] _)) -> return []
>               (Right (CTranslUnit decls _)) -> return $ getTypedefIdentsFromDecls decls
>               (Left pe) -> trace ("Error parsing declarations from header " ++ macheader ++ ": " ++ (show pe)) (assert False (return []))

> mkParser :: String -> FilePath -> [String] -> IO MacroParser
> mkParser compiler macheader gccopts = do
>       types <- getTypeIdents compiler macheader gccopts
>       result <- myCPP compiler Nothing (["-imacros", macheader, "-dM", "-P"] ++ gccopts) (Left "\n")
>       (errorFP, errorH) <- tmpHandle
>       (outFile, outH) <- tmpHandle
>       pid <- runProcess "grep" ["-v", "__STDC", result] Nothing Nothing Nothing (Just outH) (Just errorH)
>       ec <- waitForProcess pid
>       case ec of
>               ExitSuccess -> do
>                       hClose errorH
>                       removeFile errorFP
>                       return outFile
>               ExitFailure e -> do
>                       isOpen <- hIsOpen errorH
>                       errorH <- if not isOpen then openFile errorFP ReadMode else return errorH
>                       errs <- hGetContents errorH
>                       removeFile errorFP
>                       trace ("Preprocessor failed with return code: " ++ (show e) ++ "\n" ++ errs ++ "\n\n") $ assert False $ return "dummy_file.c"
>                       hClose errorH
>                       return "dummy_file.c"
>       return $ MacroParser compiler outFile types

> doCPPWithParser :: MacroParser -> NodeInfo -> String -> IO String
> doCPPWithParser p ni input = do
>       fp <- myCPP (compiler p) (Just $ macheader p) ["-P"] (Left input) 
>       s <- readFile fp
>       removeFile fp
>       return s

> doStmtPP :: MacroParser -> NodeInfo -> String -> [String] -> IO String
> doStmtPP p ni macro params = do
>     let paramsStr [] = ""
>         paramsStr pp = foldl1 ((++) . (++ ",")) pp
>         macBody = "void tmp_triton_macro_fun(void) {\n" ++ macro ++ "(" ++ (paramsStr params) ++ ")\n}\n"
       
>     -- putStrLn $ join $ map (\(a,b) -> "-D" ++ a ++ "=" ++ b) defines
>     doCPPWithParser p ni macBody

> swapNodeInfo :: NodeInfo -> NodeInfo -> NodeInfo
> swapNodeInfo newni oldni = newni

> swapNodeInfoInStmts :: NodeInfo -> [CStat] -> [CStat]
> swapNodeInfoInStmts ni stmts = map (everywhere (mkT $ swapNodeInfo ni)) stmts

> mkStmtsFromCPPMacro :: MacroParser -> NodeInfo -> String -> [String] -> [CStat]
> mkStmtsFromCPPMacro p ni macro params = swapNodeInfoInStmts ni stmts
>       where preproc = unsafePerformIO $ doStmtPP p ni macro params
>             parsedResult = fmap fst $ (execParser translUnitP (mkInputStream preproc) (posOfNode ni) (typedefs p) newNameSupply)
>	      stmts = case parsedResult of
>		        -- parse succeeded, so we return the statement
>                       (Right (CTranslUnit [] _)) -> []
>		        (Right (CTranslUnit edecls _)) -> case (last edecls) of
>                                                             (CFDefExt (CFunDef _ _ _ (CCompound _ bstmts _) _)) -> getStmtsFromBlockItems bstmts
>                                                             _ -> []
>			(Left pe) -> trace ("Error generating function definitions from macro '" ++
>                                           macro ++ "': " ++ preproc ++ (show pe))
>                                          (assert False [])

> mkExprFromCPPMacro :: MacroParser -> NodeInfo -> String -> [String] -> Maybe CExpr
> mkExprFromCPPMacro p ni macro params = e
>       where [s] = mkStmtsFromCPPMacro p ni macro params
>             (CExpr e _) = s

> declDummyName :: String
> declDummyName = "__dummy_decl"

> doDeclPP :: MacroParser -> NodeInfo -> String -> [String] -> IO String 
> doDeclPP p ni macro params = do
>     let paramsStr [] = ""
>         paramsStr pp = foldl1 ((++) . (++ ",")) pp
>         macBody = "int " ++ declDummyName ++ ";\n" ++
>                   macro ++ "(" ++ (paramsStr params) ++ ")\n"
>     doCPPWithParser p ni macBody

> getDeclsAfterDummy :: [CExtDecl] -> [CDecl]
> getDeclsAfterDummy edecls = map getd $ reverse $ takeWhile dname (reverse edecls)
>       where getd (CDeclExt d) = d
>             dname (CDeclExt d) = (/=) declDummyName $ identToString (getCDeclName d)
>             dname _ = False 

> swapNodeInfoInDecls :: NodeInfo -> [CDecl] -> [CDecl]
> swapNodeInfoInDecls ni decls = map (everywhere (mkT $ swapNodeInfo ni)) decls

> mkDeclsFromCPPMacro :: MacroParser -> NodeInfo -> String -> [String] -> [CDecl]
> mkDeclsFromCPPMacro p ni macro params = swapNodeInfoInDecls ni decls
>       where preproc = unsafePerformIO $ doDeclPP p ni macro params
>             parsedResult = fmap fst $ (execParser translUnitP (mkInputStream preproc) (posOfNode ni) (typedefs p) newNameSupply)
>	      decls = case parsedResult of
>		        -- parse succeeded, so we return the statement
>                       (Right (CTranslUnit [] _)) -> []
>		        (Right (CTranslUnit edecls _)) -> getDeclsAfterDummy edecls
>			(Left pe) -> trace ("Error generating function definitions from macro '" ++ macro ++ "': " ++ preproc ++ (show pe))
>                                          (assert False [])

> doFunDefPP :: MacroParser -> NodeInfo -> String -> [String] -> IO String
> doFunDefPP p ni macro params = do
>     let paramsStr [] = ""
>         paramsStr pp = foldl1 ((++) . (++ ",")) pp
>         macBody = "int " ++ declDummyName ++ ";\n" ++
>                   macro ++ "(" ++ (paramsStr params) ++ ")\n"
>     doCPPWithParser p ni macBody

> getFunDefDeclsAfterDummy :: [CExtDecl] -> [CExtDecl]
> getFunDefDeclsAfterDummy edecls = reverse $ takeWhile dname (reverse edecls)
>       where dname (CDeclExt d) = (/=) declDummyName $ identToString (getCDeclName d)
>             dname _ = True 

> swapStmts :: String -> [CStat] -> CStat -> CStat
> swapStmts stmtName insert a@(CLabel name _ _ ni)
>       | (identToString name) == "__AESOP_BLOCK_INSERT_HERE__" = mkCompoundStmt (Just stmtName) insert ni
>       | otherwise = a
> swapStmts _ _ a = a

> insertStmtBlock :: (Data a) => String -> [CStat] -> a -> a
> insertStmtBlock name insert a = everywhere (mkT $ swapStmts name insert) a

> swapNodeInfoInExtDecls :: NodeInfo -> [CExtDecl] -> [CExtDecl]
> swapNodeInfoInExtDecls ni edecls = map (everywhere (mkT $ swapNodeInfo ni)) edecls

> mkFunDefFromCPPMacro :: MacroParser -> NodeInfo -> String -> [String] -> String -> [CStat] -> [CExtDecl]
> mkFunDefFromCPPMacro p ni macro params insertName insertStmts = swapNodeInfoInExtDecls ni [completedFunDef]
>       where preproc = unsafePerformIO $ doFunDefPP p ni macro params
>             parsedResult = fmap fst $ (execParser translUnitP (mkInputStream preproc)  (posOfNode ni) (typedefs p) newNameSupply)
>	      decls = case parsedResult of
>		        -- parse succeeded, so we return the statement
>                       (Right (CTranslUnit [] _)) -> []
>		        (Right (CTranslUnit edecls _)) -> getFunDefDeclsAfterDummy edecls
>			(Left pe) -> trace ("Error generating function definitions from macro '" ++ macro ++ "': " ++ preproc ++ (show pe))
>                                          (assert False [])
            
>             [fdef] = assert ((length decls) == 1) $ decls
>             completedFunDef = insertStmtBlock insertName insertStmts fdef

> mkCompoundStmt :: Maybe String -> [CStat] -> NodeInfo -> CStat
> mkCompoundStmt (Just label) stmts ni = CLabel (newIdent label ni) (CCompound [] (map CBlockStmt stmts) ni) [] ni
> mkCompoundStmt Nothing stmts ni = CCompound [] (map CBlockStmt stmts) ni

> mkCompoundWithDecls :: Maybe String -> [CDecl] -> [CStat] -> NodeInfo -> CStat
> mkCompoundWithDecls (Just label) decls stmts ni = 
>	CCompound [newIdent label ni] ((map CBlockDecl decls) ++ (map CBlockStmt stmts)) ni
> mkCompoundWithDecls Nothing decls stmts ni =
>	CCompound [] ((map CBlockDecl decls) ++ (map CBlockStmt stmts)) ni

> constructExprFromC :: NodeInfo -> String -> CExpr
> constructExprFromC ni s = 
>	let result = (execParser_ expressionP (mkInputStream s) $ posOfNode ni) in
>		case result of
>			-- parse succeeded, so we return the expression
>			(Right expr) -> expr
>			(Left pe) -> trace ("Error constructing expression:\n" ++ s ++ (show pe)) (assert False (CVar (newIdent "blah" ni) ni))

funDefDecl creates the return type declaration for a given function.  The parameters to funDefDecl are
the function definition and the name of the return type.  For example, a function defined as:

int ** myFunc(int a);

would generate a CDecl like this:

int ** ret;

> mkRetDecl :: (CTypeSpec, [CDerivedDeclr]) -> String -> NodeInfo -> [CDecl]
> mkRetDecl ((CVoidType _), []) name ni = []
> mkRetDecl rtype name ni = [CDecl [(CTypeSpec (fst rtype))] 
>                   [(Just (CDeclr (Just $ newIdent name ni) (snd rtype) Nothing [] ni),
>		     Nothing,
>		     Nothing)]
>                   ni]

> mkRetDeclFromFunDef :: CFunDef -> String -> [CDecl]
> mkRetDeclFromFunDef f name =
>	mkRetDecl rtype name (nodeInfo f)
>	where rtype = fromJust $ getFunDefReturnMaybe f

Functions for building struct members from scratch

Make a declarator: <derived> name;
Examples:

**foo;

mkCDeclr "foo" [CPtrDeclr, CPtrDeclr] ...

(*myFunPtr)(int bar, char *baz);

mkCDeclr "myFunPtr" [CPtrDeclr, (CFunDeclr (Right [(CDecl [CTypeSpec CIntType] [("bar" ...)]), ...]) ...)]

> mkCDeclr :: String -> [CDerivedDeclr] -> CDeclr
> mkCDeclr name derived = 
>	let ni = nodeInfo $ head derived
>	in CDeclr (Just (newIdent name ni)) derived Nothing [] ni

> mkIdentCDeclr :: Ident -> [CDerivedDeclr] -> CDeclr
> mkIdentCDeclr name derived = 
>	let ni = nodeInfo $ head derived
>	in CDeclr (Just name)  derived Nothing [] ni

> mkAnonCDeclr :: [CDerivedDeclr] -> CDeclr
> mkAnonCDeclr derived = 
>	let ni = nodeInfo $ head derived
>	in CDeclr Nothing derived Nothing [] ni

Make a declaration: type <derived> name;
Examples:

int **foo;

mkCDecl CIntType [CPtrDeclr, CPtrDeclr] "foo"

struct myStruct bar;

mkCDecl (CSUType (CStruct CStructTag "myStruct" Nothing [] ...) ...) [] "bar"

myType *baz;

mkCDecl (CTypeDef "myType") [CPtrDeclr] "baz"

> mkCDecl :: CTypeSpec -> [CDerivedDeclr] -> String -> NodeInfo -> CDecl
> mkCDecl mtype derives name ni = CDecl [CTypeSpec mtype] [(Just $ mkCDeclr name derives, Nothing, Nothing)] ni

> mkIdentCDecl :: CTypeSpec -> [CDerivedDeclr] -> Ident -> NodeInfo -> CDecl
> mkIdentCDecl mtype derives name ni = CDecl [CTypeSpec mtype] [(Just $ mkIdentCDeclr name derives, Nothing, Nothing)] ni

> mkAnonCDecl :: CTypeSpec -> [CDerivedDeclr] -> NodeInfo -> CDecl
> mkAnonCDecl mtype derives ni = CDecl [CTypeSpec mtype] [(Just $ mkAnonCDeclr derives, Nothing, Nothing)] ni

> mkAnonFromDecl :: CDecl -> CDecl
> mkAnonFromDecl (CDecl specs inits ni) = (CDecl specs (map stripName inits) ni)
>       where stripName ((Just (CDeclr (Just n) deriveds strlit attrs sni)), init, ex) =
>                       ((Just (CDeclr Nothing  deriveds strlit attrs sni)), init, ex)

> mkConstCDecl :: CTypeSpec -> [CDerivedDeclr] -> String -> NodeInfo -> CDecl
> mkConstCDecl mtype derives name ni = CDecl [CTypeQual (CConstQual ni), CTypeSpec mtype] [(Just $ mkCDeclr name derives, Nothing, Nothing)] ni

> genCDecl :: String -> String -> NodeInfo -> CDecl
> genCDecl typedef name ni = mkCDecl (CTypeDef (newIdent typedef ni) ni) [] name ni

> addBlockingTQ :: CDecl -> CDecl
> addBlockingTQ (CDecl specs inits ni) = CDecl (CTypeQual ((CBlocking ni)):specs) inits ni

> mkFunPtrsStruct :: String -> String -> [(String, String)] -> Bool -> NodeInfo -> CExtDecl
> mkFunPtrsStruct stype sname fields static ni = CDeclExt decl
>     where decl = CDecl (staticSpec ++ [tspec]) declrs ni
>           tspec = CTypeSpec (CSUType (CStruct CStructTag (Just $ newIdent stype ni) Nothing [] ni) ni)
>           staticSpec = if static then [CStorageSpec $ CStatic ni] else []
>           declrs = [(Just vname, Just inits, Nothing)]
>           vname = CDeclr (Just (newIdent sname ni)) [] Nothing [] ni
>           inits = CInitList [ ( [CMemberDesig (newIdent field ni) ni], CInitExpr (CVar (newIdent fun ni) ni) ni ) | (field, fun) <- fields ] ni

Functions for building a struct from the name and array of members

param1Decl can be created with mkCDecl.

> mkStructDef :: Ident -> [CDecl] -> NodeInfo -> CTypeSpec
> mkStructDef name members ni = CSUType (CStruct CStructTag (Just name) (Just members) [] ni) ni 

> mkStructDefDecl :: String -> [CDecl] -> NodeInfo -> CDecl
> mkStructDefDecl name members ni = 
>	CDecl [CTypeSpec $ mkStructDef (newIdent name ni) members ni] [(Nothing, Nothing, Nothing)] ni 

> mkStructDecl :: String -> String -> NodeInfo -> CDecl
> mkStructDecl tname name ni = mkCDecl (CSUType (CStruct CStructTag (Just $ newIdent tname ni)
>						   		 Nothing 
>						   		 []
>						   		 ni) ni) [] name ni

> mkStructPtrDecl :: String -> String -> NodeInfo -> CDecl
> mkStructPtrDecl tname name ni = mkCDecl (CSUType (CStruct CStructTag (Just $ newIdent tname ni)
>								    Nothing
>							            []
>								    ni) ni) [(CPtrDeclr [] ni)] name ni

> addPtrToDeclarators :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
> addPtrToDeclarators (Just (CDeclr name d asm attrs ni), i, e) =
>       (Just (CDeclr name (d ++ [CPtrDeclr [] ni]) asm attrs ni), i, e)
> addPtrToDeclarators a = a

> addPtrDeclr :: CDecl -> CDecl
> addPtrDeclr (CDecl specs declarators ni) = (CDecl specs ((addPtrToDeclarators (head declarators)):(tail declarators)) ni)

> mkAnonStructPtrDecl :: String -> NodeInfo -> CDecl
> mkAnonStructPtrDecl tname ni = mkAnonCDecl (CSUType (CStruct CStructTag (Just $ newIdent tname ni) Nothing [] ni) ni)
>		                             [(CPtrDeclr [] ni)] ni
> mkIntDecl :: String -> NodeInfo -> CDecl
> mkIntDecl name ni = mkCDecl (CIntType ni) [] name ni

From a name list of parameters, and return type, construct an external declaration struct

Example:

struct myStruct
{
   param1type param1;
   param2type param2;
};

genStructExtDecl "myStruct" [param1Decl, param2Decl]

> genStructExtDecl :: String -> [CDecl] -> NodeInfo -> CExtDecl
> genStructExtDecl name members ni = CDeclExt $ mkStructDefDecl name members ni

Make a function pointer declarator: (*) (params);

> mkFunPtrDeclr :: [CDecl] -> [CDerivedDeclr]
> mkFunPtrDeclr params = 
>	let ni = nodeInfo $ head params
>	in [CPtrDeclr [] ni, CFunDeclr (Right (params, False)) [] ni]

Make the function pointer: void (* name) (params);

> mkFunPtrDecl :: String -> CTypeSpec -> [CDecl] -> CDecl
> mkFunPtrDecl name retType params =
>     let ni = nodeInfo retType 
>     in mkCDecl retType (mkFunPtrDeclr params) name ni

> mkFunPtrDerivedDecl :: String -> CTypeSpec -> [CDerivedDeclr] -> [CDecl] -> CDecl
> mkFunPtrDerivedDecl name retType retDerived params =
>	let ni = nodeInfo retType
>	in mkCDecl retType ((mkFunPtrDeclr params) ++ retDerived) name ni

> mkVoidFunPtr :: String -> [CDecl] -> CDecl
> mkVoidFunPtr name params = 
>	let ni = nodeInfo $ head params
>	in mkFunPtrDecl name (CVoidType ni) params

> mkFunDeclr :: [CDecl] -> NodeInfo -> CDerivedDeclr
> mkFunDeclr params ni = 
>	CFunDeclr (Right (params, False)) [] ni

> mkFunDecl :: String -> CTypeSpec -> [CDerivedDeclr] -> [CDecl] -> CDecl
> mkFunDecl fname returnType returnDerived params = 
>	let ni = nodeInfo returnType 
>	in mkCDecl returnType ([mkFunDeclr params ni] ++ returnDerived) fname ni

> mkIdentFunDecl :: Ident -> CTypeSpec -> [CDerivedDeclr] -> [CDecl] -> CDecl
> mkIdentFunDecl fname returnType returnDerived params = 
>	let ni = nodeInfo returnType 
>	in mkIdentCDecl returnType ([mkFunDeclr params ni] ++ returnDerived) fname ni

> mkFunDeclWithDeclSpecs :: String -> CTypeSpec -> [CDerivedDeclr] -> [CDeclSpec] -> [CDecl] -> CDecl
> mkFunDeclWithDeclSpecs fname rType rDerived declSpecs params =
>	let ni = nodeInfo rType
>	in CDecl (declSpecs ++ [CTypeSpec rType])
>		 [(Just $ mkCDeclr fname ([mkFunDeclr params ni] ++ rDerived),
>		   Nothing, Nothing)] ni

> mkFunPtrDeclWithDeclSpecs :: String -> CTypeSpec -> [CDerivedDeclr] -> [CDeclSpec] -> [CDecl] -> Maybe String -> CDecl
> mkFunPtrDeclWithDeclSpecs fname rType rDerived declSpecs params initName =
>	let ni = nodeInfo rType
>	in CDecl (declSpecs ++ [CTypeSpec rType])
>		 [(Just $ mkCDeclr fname ([CPtrDeclr [] ni, mkFunDeclr params ni] ++ rDerived),
>		   (if isJust initName
>			then Just (CInitExpr (CVar (newIdent (fromJust initName) ni) ni) ni)
>		        else Nothing), Nothing)] ni

Remove the initializer from the declarator.  For example:

int a = 10;

becomes:

int a;

We don't remove the initialization list, so:

int a[2] = {1, 2};

remains.

> removeInitFromDeclr :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
> removeInitFromDeclr ((,,) declr (Just (CInitExpr expr _)) cexpr) = (declr, Nothing, cexpr)
> removeInitFromDeclr t = t

> removeInitFromDecl :: CDecl -> CDecl
> removeInitFromDecl (CDecl declspecs declrs ni) = (CDecl declspecs (map removeInitFromDeclr declrs) ni)

> getInitFromDeclr :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> Maybe CExpr
> getInitFromDeclr ((,,) _ (Just (CInitExpr initexpr _)) _) = Just initexpr
> getInitFromDeclr _ = Nothing

> getNameFromDeclr :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> Maybe Ident
> getNameFromDeclr ((,,) (Just (CDeclr name _ _ _ _)) _ _) = name
> getNameFromDeclr _ = Nothing

Get the init statements from a declaration.  As an example, the declaration:

int a, b = 3, c = 4;

results in the statements:

b = 3;
c = 4;

> getInitStmtsFromDecl :: CDecl -> [CStat]
> getInitStmtsFromDecl d@(CDecl declspecs declrs ni) = do
>	declr <- declrs
>	let name = getNameFromDeclr declr
>	    init = getInitFromDeclr declr
>	    ni = nodeInfo d 
>	if isJust init && isJust name then (return $ mkAssignStmt (CVar (fromJust name) ni) (fromJust init)) else [] 

> getInitsFromDecls :: [CDecl] -> [CStat]
> getInitsFromDecls ds = join $ map getInitStmtsFromDecl ds

> removeVarNameFromDeclr :: CDeclr -> CDeclr
> removeVarNameFromDeclr (CDeclr name indirections asm attrs ni) =
>	(CDeclr Nothing indirections asm attrs ni) 

> removeVarFromDecl :: CDecl -> CDecl
> removeVarFromDecl (CDecl specs indirs ni) = (CDecl specs (map stripVar indirs) ni) where 
>	stripVar i@(cd, ci, ce) =
>		if isJust cd
>			then (Just (removeVarNameFromDeclr $ fromJust $ cd), Nothing, Nothing)
>			else i

Given a lhs expression and rhs expression, construct the statement:  lhs = rhs;

> mkAssignStmt :: CExpr -> CExpr -> CStat
> mkAssignStmt lexpr rexpr = 
>	let ni = nodeInfo lexpr
>	in (CExpr (Just (CAssign CAssignOp lexpr rexpr ni)) ni)

> mkFunCall :: CExpr -> CExpr -> [CExpr] -> CStat
> mkFunCall ret call params = 
>	let ni = nodeInfo ret
>	in (CExpr (Just (CAssign CAssignOp ret (CCall call params ni) ni)) ni)

> addParamsToFunCall :: CStat -> [CExpr] -> CStat
> addParamsToFunCall
>     (CExpr (Just (CAssign
>                     CAssignOp
>                     ret 
>                     (CCall call params ni1) ni2)) ni3) addParams =
>         (CExpr (Just (CAssign
>                         CAssignOp
>                         ret
>                         (CCall call (params ++ addParams) ni1) ni2)) ni3)
> addParamsToFunCall
>     (CExpr (Just (CCall call params ni1)) ni2) addParams =
>         (CExpr (Just (CCall call (params ++ addParams) ni1)) ni2)

> mkVar :: String -> NodeInfo -> CExpr
> mkVar s ni = (CVar (newIdent s ni) ni)

Tack on a struct pointer variable to an expression (assuming its a CVar).  For example, with an expression of "blah"

addStructPtr "params" expr

will make:

params->blah

> addStructPtr :: String -> CExpr -> CExpr
> addStructPtr sname (CVar name ni) = CMember (CVar (newIdent sname ni) ni) name True ni
> addStructPtr _ expr = expr

> addAddrOp :: CExpr -> CExpr
> addAddrOp c = (CUnary CAdrOp c (nodeInfo c))

addStructPtrPrefix "ctl" "params" (CVar "blah")

will make:

ctl->params->blah

> addStructPtrPrefix :: String -> String -> CExpr -> CExpr
> addStructPtrPrefix sname pname (CVar name ni) = CMember (CMember (CVar (newIdent sname ni) ni) (newIdent pname ni) True ni) name True ni
> addStructPtrPrefix _ _ e = e

Same as above, but without the second pointer deref:

ctl->params.blah

> addStructPrefix :: String -> String -> CExpr -> CExpr
> addStructPrefix sname pname (CVar name ni) =
>	CMember (CMember (CVar (newIdent sname ni) ni)
>		 	 (newIdent pname ni) True ni) name False ni

ctl->params1.params2.blah

> addStructPrefixPrefix :: String -> String -> String -> CExpr -> CExpr
> addStructPrefixPrefix sname p1name p2name (CVar name ni) =
>	CMember (CMember (CMember (CVar (newIdent sname ni) ni)
>			 	  (newIdent p1name ni) True ni)
>			 (newIdent p2name ni) False ni) name False ni

ctl->params1->params2.blah

> addStructPtrPrefixPrefix :: String -> String -> String -> CExpr -> CExpr
> addStructPtrPrefixPrefix sname p1name p2name (CVar name ni) =
>       CMember (CMember (CMember (CVar (newIdent sname ni) ni) (newIdent p1name ni) True ni) (newIdent p2name ni) True ni) name False ni

Take an assign statement, and insert a struct pointer prefix to the  lhs.  For example:

addStructToAssign "params" "mymem" "funcall(a, 123)"

results in the statement:  params.mymem = funcall(a, 123);

> addStructToAssign :: String -> String -> Ident -> CExpr -> CStat
> addStructToAssign sname pname lvar rexpr = 
>	let ni = nodeInfo rexpr
>	in (CExpr (Just (CAssign CAssignOp (addStructPrefix sname pname (CVar lvar ni)) rexpr ni)) ni)

Take an assign statement, and insert a struct pointer prefix to the  lhs.  For example:

addStructPtrToAssign "params" "mymem" "funcall(a, 123)"

results in the statement:  params->mymem = funcall(a, 123);

> addStructPtrToAssign :: String -> String -> Ident -> CExpr -> CStat
> addStructPtrToAssign sname pname lvar rexpr = 
>	let ni = nodeInfo rexpr
>	in (CExpr (Just (CAssign CAssignOp (addStructPtrPrefix sname pname (CVar lvar ni)) rexpr ni)) ni)

> mkFunDef :: (CTypeSpec, [CDerivedDeclr]) -> [CDeclSpec] -> String -> [CDecl] -> CStat -> CExtDecl
> mkFunDef (rtype, derivedPtrs) dspecs fname params stmts = 
>	let ni = if null params then nodeInfo stmts else nodeInfo $ head params
>	in CFDefExt (CFunDef (dspecs ++ [(CTypeSpec rtype)])
>			  (CDeclr (Just $ newIdent fname ni)
>                                 (derivedPtrs ++ 
>			           [(CFunDeclr (Right (params, False)) [] ni)])
>			          Nothing [] ni)
>                         [] stmts ni)

> addDeclSpecs :: CExtDecl -> [CDeclSpec] -> CExtDecl
> addDeclSpecs (CFDefExt (CFunDef s d dls stmt ni)) newspecs = CFDefExt $ CFunDef (newspecs ++ s) d dls stmt ni

> mkStaticFunDecl :: CTypeSpec -> String -> [CDecl] -> CExtDecl
> mkStaticFunDecl return fname params =
>	let ni = nodeInfo return
>	in CDeclExt (CDecl [(CStorageSpec (CStatic ni)),
>		            (CTypeSpec return)]
>	                   [((Just $ mkCDeclr fname [mkFunDeclr params ni]), Nothing, Nothing)] ni)

> mkStaticFunDef :: CTypeSpec -> String -> [CDecl] -> CStat -> CExtDecl
> mkStaticFunDef return fname params stmts =
>	let ni = if null params then nodeInfo stmts else nodeInfo $ head params
>	in CFDefExt (CFunDef [(CStorageSpec (CStatic ni)), (CTypeSpec return)]
>			  (CDeclr (Just $ newIdent fname ni)
>			          [(CFunDeclr (Right (params, False)) [] ni)]
>			          Nothing [] ni)
>                         [] stmts ni)

> mkIfCondition :: Maybe CExpr -> CExpr
> mkIfCondition mexpr = if isJust mexpr then fromJust mexpr else CConst (CIntConst (cInteger 1) $ nodeInfo $ fromJust mexpr)

> mkIfStmt :: Maybe CExpr -> [CStat] -> CStat
> mkIfStmt cond stmts = CIf (mkIfCondition cond) (CCompound [] (map CBlockStmt stmts) $ nodeInfo $ fromJust cond) Nothing $ nodeInfo $ fromJust cond

> mkIfElseStmt :: Maybe CExpr -> [CStat] -> [CStat] -> CStat
> mkIfElseStmt cond ifstmts elsestmts =
>	CIf (mkIfCondition cond)
>	    (CCompound [] (map CBlockStmt ifstmts) $ nodeInfo $ fromJust cond)
>	    (Just (CCompound [] (map CBlockStmt elsestmts) $ nodeInfo $ fromJust cond)) $ nodeInfo $ fromJust cond

> mkIntConst :: Integer -> NodeInfo -> CExpr
> mkIntConst v ni = CConst (CIntConst (cInteger v) ni)

> mkGoto :: String -> NodeInfo -> CStat
> mkGoto s ni = (CGoto (newIdent s ni) ni)

> mkLabel :: String -> [CStat] -> NodeInfo -> CStat
> mkLabel s stmts ni = (CLabel (newIdent s ni) (CCompound [] (map CBlockStmt stmts) ni) [] ni)

> canonCDecl :: CDecl -> String
> canonCDecl (CDecl declspecs derived _) = 
>       let t = getTypeSpec declspecs
>           tstr = case t of
>                      (CSUType (CStruct CStructTag (Just n) _ _ _) _) -> "struct_" ++ (identToString n)
>                      (CTypeDef n _) -> identToString n
>                      (CSUType (CStruct CUnionTag (Just n) _ _ _) _) -> "union_" ++ (identToString n)
>                      (CEnumType (CEnum (Just n) _ _ _) _) -> "enum_" ++ (identToString n)
>                      _ -> show $ pretty t
>           ptrs = filter isDerivedPtr (join $ map getDerivedDeclrs derived)
>           ptrStr = concat [ "_ptr" | p <- ptrs ]
>       in tstr ++ ptrStr

> removeAPtr :: CDecl -> CDecl
> removeAPtr (CDecl sp derived n) = (CDecl sp nd n)
>       where nd = map remP derived
>             remP (declr, init, expr) = (rem declr, init, expr)
>             rem Nothing = Nothing
>             rem (Just (CDeclr name deriveds lit attrs ni)) = (Just (CDeclr name (remd deriveds) lit attrs ni))
>             remd (d:ds) = if isDerivedPtr d then ds else (d:(remd ds))
>             remd [] = []

> mkCastStmt :: String -> CDecl -> String -> NodeInfo -> CStat
> mkCastStmt l t r ni = CExpr (Just (CAssign CAssignOp (CVar (newIdent l ni) ni) (CCast t (CVar (newIdent r ni) ni) ni) ni)) ni

