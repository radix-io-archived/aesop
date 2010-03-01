> module RemoteParser where
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
> import Serialize
> import CParse
> import CGen
> import Header
> import Text.Regex.PCRE
> import Array

Each type in the registry is the name and a set of truth values each function
required:  (encode, decode, encode_size, init, destroy)
Once one of those function declarations is found, the appropriate boolean value
is set to true.

> type RemoteTypeRegistry = HashTable String (Bool, Bool, Bool, Bool, Bool)

> newRemoteTypeRegistry :: IO RemoteTypeRegistry
> newRemoteTypeRegistry = Data.HashTable.new (==) Data.HashTable.hashString

> data Remote = Remote {
>       filename :: FilePath,
>       typeReg :: RemoteTypeRegistry,
>       funs :: [String],
>       includes :: [FilePath],
>       defines :: [(String, String)]
> }

> type RemoteT = StateT Remote IO

> invalid :: String -> NodeInfo -> RemoteT ()
> invalid msg ni = do
>	fstr <- getFilePosStr ni
>	error (fstr ++ ":  Invalid aesop usage: " ++ msg)

> registerRemoteFun :: String -> RemoteT ()
> registerRemoteFun name = do
>       (Remote fname treg funs is d) <- get
>       put (Remote fname treg (name:funs) is d)

> registerEncodeFun :: String -> RemoteT ()
> registerEncodeFun e = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) e
>       if isJust res
>         then do
>             let (en, de, es, ini, des) = fromJust res
>             liftIO $  Data.HashTable.insert (typeReg r) e (True, de, es, ini, des)
>         else
>             liftIO $  Data.HashTable.insert (typeReg r) e (True, False, False, False, False)
>       return ()

> registerDecodeFun :: String -> RemoteT ()
> registerDecodeFun d = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) d
>       if isJust res
>         then do
>             let (en, de, es, ini, des) = fromJust res
>             liftIO $  Data.HashTable.insert (typeReg r) d (en, True, es, ini, des)
>         else
>             liftIO $  Data.HashTable.insert (typeReg r) d (False, True, False, False, False)
>       return ()

> registerEncodeSizeFun :: String -> RemoteT ()
> registerEncodeSizeFun d = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) d
>       if isJust res
>         then do
>             let (en, de, es, ini, des) = fromJust res
>             liftIO $ Data.HashTable.insert (typeReg r) d (en, de, True, ini, des)
>         else
>             liftIO $ Data.HashTable.insert (typeReg r) d (False, False, True, False, False)
>       return ()

> registerInitFun :: String -> RemoteT ()
> registerInitFun d = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) d
>       if isJust res
>         then do
>             let (en, de, es, ini, des) = fromJust res
>             liftIO $  Data.HashTable.insert (typeReg r) d (en, de, es, True, des)
>         else
>             liftIO $  Data.HashTable.insert (typeReg r) d (False, False, False, True, False)
>       return ()

> registerDestroyFun :: String -> RemoteT ()
> registerDestroyFun d = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) d
>       if isJust res
>         then do
>             let (en, de, es, ini, des) = fromJust res
>             liftIO $  Data.HashTable.insert (typeReg r) d (en, de, es, ini, True)
>         else
>             liftIO $  Data.HashTable.insert (typeReg r) d (False, True, False, False, False)
>       return ()

> isFullEncoding :: String -> RemoteT Bool
> isFullEncoding d = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) d
>       let (Just (enc, dec, siz, ini, des)) = res
>       if isJust res && enc && dec && siz && ini && des
>         then return True
>         else return False

> registerFullEncoding :: String -> NodeInfo -> RemoteT ()
> registerFullEncoding s ni = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) s
>       when (isJust res) $ invalid (s ++ " already has encoding functions defined.\n") ni
>       liftIO $ Data.HashTable.insert (typeReg r) s (True, True, True, True, True)

> newRemoteState :: String -> [FilePath] -> [(String, String)] -> IO Remote
> newRemoteState fname includes defs = do
>       r <- Data.HashTable.new (==) Data.HashTable.hashString
>       return $ Remote fname r [] includes defs

> getRemoteTypeName :: CTypeSpec -> Maybe String
> getRemoteTypeName (CSUType (CStruct CStructTag (Just n) _ _ _) _) = Just $ "struct_" ++ (identToString n)
> getRemoteTypeName (CSUType (CStruct CUnionTag (Just n) _ _ _) _) = Just $ "union_" ++ (identToString n)
> getRemoteTypeName (CEnumType (CEnum (Just n) _ _ _) _) = Just $ "enum_" ++ (identToString n)
> getRemoteTypeName (CTypeDef n _) = Just $ identToString n
> getRemoteTypeName _ = Nothing

> getIncludes :: RemoteT [FilePath]
> getIncludes = liftM includes $ get

> getDefines :: RemoteT [(String, String)]
> getDefines = liftM defines $ get

> mkRemoteDecl :: CDecl -> [CDerivedDeclr] -> String -> Maybe CDecl
> mkRemoteDecl d@(CDecl specs inits ni) ders v =
>       let s = getTypeSpec specs
>       in case s of
>               (CSUType (CStruct CStructTag name _ _ ni2) ni3) -> Just $ mkCDecl (CSUType (CStruct CStructTag name Nothing [] ni2) ni3) ders v ni
>               (CSUType (CStruct CUnionTag name _ _ ni2) ni3) -> Just $ mkCDecl (CSUType (CStruct CUnionTag name Nothing [] ni2) ni3) ders v ni
>               (CEnumType (CEnum name _ _ ni2) ni3) -> Just $ mkCDecl (CEnumType (CEnum name Nothing [] ni2) ni3) ders v ni
>               (CTypeDef name ni2) -> Just $ mkCDecl (CTypeDef name ni2) ders v ni
>               _ -> Nothing

> uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
> uncurry3 f (a, b, c) = f a b c

> getFieldInfo :: CDecl -> RemoteT (String, String, Bool)
> getFieldInfo d@(CDecl specs declrs ni) = do
>       let t = getTypeSpec specs
>           ds = getCDeclrsFromList declrs
>           ptrs = if null ds then [] else getDerivedPtrs $ head ds
>           tname = getRemoteTypeName t
>           isPtr = not $ null ptrs
>           varName = getVarName d
>       when (length ptrs > 1) $ invalid ("parameter '" ++ (show $ pretty d) ++ "' has too many pointers, only one pointer supported") ni
>       when (isNothing tname) $ invalid ("parameter '" ++ (show $ pretty d) ++ "' does not have a known encoding type") ni
>       return (fromJust tname, varName, isPtr)

typeof(expr)

CTypeOfType CDecl NodeInfo      

> mkDeclsFromRemote :: String -> [String] -> NodeInfo -> RemoteT [CDecl]
> mkDeclsFromRemote macro params ni = do
>       includes <- getIncludes
>       defs <- getDefines
>       return $ mkDeclsFromCPPMacro includes defs "ae-remote-parser.h" ni macro params

> mkStmtFromRemote :: String -> [String] -> NodeInfo -> RemoteT [CStat]
> mkStmtFromRemote macro params ni = do
>       includes <- getIncludes
>       defs <- getDefines
>       return $ mkStmtFromCPPMacro includes defs "ae-remote-parser.h" ni macro params

> mkFunDefFromRemote :: String -> [CStat] -> String -> [String] -> NodeInfo -> RemoteT [CExtDecl]
> mkFunDefFromRemote name block macro params ni = do
>       includes <- getIncludes
>       defs <- getDefines
>       return $ mkFunDefFromCPPMacro includes defs "ae-remote-parser.h" ni macro params name block

> mkEncodeBlock :: NodeInfo -> String -> String -> Bool -> RemoteT [CStat]
> mkEncodeBlock ni typeName fieldName isPtr = do
>       let encodeMacro = if isPtr then "AER_MK_ENCODE_TYPE_PTR" else "AER_MK_ENCODE_TYPE"
>       mkStmtFromRemote encodeMacro [typeName, fieldName] ni

> mkEncodeStmts :: CDecl -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkEncodeStmts stype fields ni = do
>       fieldsInfo <- mapM getFieldInfo fields
>       let anonSType = mkAnonFromDecl stype
>       encodeBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkEncodeBlock ni) fieldsInfo
>       encodeDecls <- mkDeclsFromRemote "AER_MK_ENCODE_DECLS" [show $ pretty anonSType] ni
>       startStmts <- mkStmtFromRemote "AER_MK_ENCODE_STMTS_START" [show $ pretty anonSType] ni
>       endStmts <- mkStmtFromRemote "AER_MK_ENCODE_STMTS_END" [] ni
>       return $ mkCompoundWithDecls Nothing encodeDecls (startStmts ++ encodeBlocks ++ endStmts) ni

> mkEncodeFun :: CDecl -> String -> [CDecl] -> NodeInfo -> RemoteT CExtDecl
> mkEncodeFun decl sname fields ni = do
>     let tparam = mkRemoteDecl decl [(CPtrDeclr [] ni)] "x"
>     when (isNothing tparam) $ invalid "not a known encoding type" ni
>     stmts <- mkEncodeStmts (fromJust tparam) fields ni
>     return $ mkFunDef ((CTypeDef (newIdent "triton_ret_t" ni) ni), [])
>                       [(CStorageSpec (CStatic ni)), (CTypeQual (CInlineQual ni))]
>                       ("aer_encode_" ++ sname)
>                       [mkCDecl (CTypeDef (newIdent "triton_buffer_t" ni) ni) [CPtrDeclr [] ni] "buf" ni,
>                        mkConstCDecl (CCharType ni) [CPtrDeclr [] ni] "varname" ni,
>                        mkCDecl (CVoidType ni) [CPtrDeclr [] ni] "vx" ni]
>                       stmts

> mkDecodeBlock :: NodeInfo -> String -> String -> Bool -> RemoteT [CStat]
> mkDecodeBlock ni typeName fieldName isPtr = do 
>       let decodeMacro = if isPtr then "AER_MK_DECODE_TYPE_PTR" else "AER_MK_DECODE_TYPE"
>       mkStmtFromRemote decodeMacro [typeName, fieldName] ni

> mkDecodeStmts :: CDecl -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkDecodeStmts stype fields ni = do
>       fieldsInfo <- mapM getFieldInfo fields
>       let anonSType = mkAnonFromDecl stype
>       decodeBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkDecodeBlock ni) fieldsInfo 
>       decodeDecls <- mkDeclsFromRemote "AER_MK_DECODE_DECLS" [show $ pretty anonSType] ni
>       startStmts <- mkStmtFromRemote "AER_MK_DECODE_STMTS_START" [show $ pretty anonSType] ni
>       endStmts <- mkStmtFromRemote "AER_MK_DECODE_STMTS_END" [] ni
>       return $ mkCompoundWithDecls Nothing decodeDecls (startStmts ++ decodeBlocks ++ endStmts) ni

> mkDecodeFun :: CDecl -> String -> [CDecl] -> NodeInfo -> RemoteT CExtDecl
> mkDecodeFun decl sname fields ni = do
>     let tparam = mkRemoteDecl decl [(CPtrDeclr [] ni)] "x"
>     when (isNothing tparam) $ invalid "not a known encoding type" ni
>     stmts <- mkDecodeStmts (fromJust tparam) fields ni
>     return $ mkFunDef ((CTypeDef (newIdent "triton_ret_t" ni) ni), [])
>                       [(CStorageSpec (CStatic ni)), (CTypeQual (CInlineQual ni))]
>                       ("aer_decode_" ++ sname)
>                       [mkCDecl (CTypeDef (newIdent "triton_buffer_t" ni) ni) [CPtrDeclr [] ni] "buf" ni,
>                        mkCDecl (CCharType ni) [CPtrDeclr [] ni, CPtrDeclr [] ni] "varname" ni,
>                        mkCDecl (CVoidType ni) [CPtrDeclr [] ni] "vx" ni]
>                       stmts

> mkSizeBlock :: NodeInfo -> String -> String -> Bool -> RemoteT [CStat]
> mkSizeBlock ni typeName fieldName isPtr = do
>       let sizeMacro = if isPtr then "AER_MK_SIZE_TYPE_PTR" else "AER_MK_SIZE_TYPE"
>       mkStmtFromRemote sizeMacro [typeName, fieldName] ni

> mkSizeStmts :: CDecl -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkSizeStmts stype fields ni = do
>       fieldsInfo <- mapM getFieldInfo fields
>       let anonSType = mkAnonFromDecl stype
>       sizeBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkSizeBlock ni) fieldsInfo
>       sizeDecls <- mkDeclsFromRemote "AER_MK_SIZE_DECLS" [show $ pretty anonSType] ni
>       startStmts <- mkStmtFromRemote "AER_MK_SIZE_STMTS_START" [show $ pretty anonSType] ni
>       endStmts <- mkStmtFromRemote "AER_MK_SIZE_STMTS_END" [] ni
>       return $ mkCompoundWithDecls Nothing sizeDecls (startStmts ++ sizeBlocks ++ endStmts) ni

> mkSizeFun :: CDecl -> String -> [CDecl] -> NodeInfo -> RemoteT CExtDecl
> mkSizeFun decl sname fields ni = do
>       let tparam = mkRemoteDecl decl [(CPtrDeclr [] ni)] "x"
>       when (isNothing tparam) $ invalid "not a known encoding type" ni
>       stmts <- mkSizeStmts (fromJust tparam) fields ni
>       return $ mkFunDef ((CTypeDef (newIdent "uint64_t" ni) ni), [])
>                         [(CStorageSpec (CStatic ni)), (CTypeQual (CInlineQual ni))]
>                         ("aer_encode_size_" ++ sname)
>                         [mkConstCDecl (CCharType ni) [CPtrDeclr [] ni] "varname" ni,
>                          mkCDecl (CVoidType ni) [CPtrDeclr [] ni] "vx" ni]
>                         stmts

> mkInitBlock :: NodeInfo -> String -> String -> String -> Bool -> RemoteT [CStat]
> mkInitBlock ni baseTypeName typeName fieldName isPtr = do
>       let initMacro = if isPtr then "AER_MK_INIT_PTR_TYPE" else "AER_MK_INIT_TYPE"
>       mkStmtFromRemote initMacro [baseTypeName, typeName, fieldName] ni

> mkInitNullBlock :: NodeInfo -> String -> String -> Bool -> RemoteT [CStat]
> mkInitNullBlock ni tName fName isPtr = do
>       if isPtr then mkStmtFromRemote "AER_MK_INIT_PTR_NULL" [tName, fName] ni
>                else return []

> mkInitStmts :: CDecl -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkInitStmts stype fields ni = do
>       fieldsInfo <- mapM getFieldInfo fields
>       let getDeriveds (CDecl _ declrs _) = concatMap getDerivedDeclrs declrs
>           ptrVars = map getVarName $ filter (\d -> any isDerivedPtr (getDeriveds d)) fields
>           initPtr s = "x->" ++ s ++ " = 0;"
>           initPtrs = mkStmtsFromCLines ni ("{ " ++ (join $ map initPtr ptrVars) ++ "}")
>           anonSType = mkAnonFromDecl stype
>           baseTypeName = getRemoteTypeName $ getTypeSpecFromDecl anonSType
>       when (isNothing baseTypeName) $ invalid ("type '" ++ (show $ pretty stype) ++ "does not have a known encoding type") ni
>       nullBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkInitNullBlock ni) fieldsInfo
>       initBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkInitBlock ni $ fromJust baseTypeName) fieldsInfo
>       initDecls <- mkDeclsFromRemote "AER_MK_INIT_DECLS" [show $ pretty anonSType] ni
>       startStmts <- mkStmtFromRemote "AER_MK_INIT_STMTS_START" [show $ pretty anonSType] ni
>       endStmts <- mkStmtFromRemote "AER_MK_INIT_STMTS_END" [] ni
                    
>       return $ mkCompoundWithDecls Nothing initDecls (startStmts ++ nullBlocks ++ initBlocks ++ endStmts) ni

> mkInitFun :: CDecl -> String -> [CDecl] -> NodeInfo -> RemoteT CExtDecl
> mkInitFun decl sname fields ni = do
>     let tparam = mkRemoteDecl decl [(CPtrDeclr [] ni)] "x"
>     when (isNothing tparam) $ invalid "not a known encoding type" ni
>     stmts <- mkInitStmts (fromJust tparam) fields ni
>     return $ mkFunDef ((CTypeDef (newIdent "triton_ret_t" ni) ni), [])
>                       [(CStorageSpec (CStatic ni)), (CTypeQual (CInlineQual ni))]
>                       ("aer_init_" ++ sname)
>                       [mkCDecl (CVoidType ni) [CPtrDeclr [] ni] "vx" ni]
>                       stmts

> mkDestroyBlock :: NodeInfo -> String -> String -> String -> Bool -> RemoteT [CStat]
> mkDestroyBlock ni baseTypeName typeName fieldName isPtr = do
>       let destroyMacro = if isPtr then "AER_MK_DESTROY_PTR_TYPE" else "AER_MK_DESTROY_TYPE"
>       mkStmtFromRemote destroyMacro [baseTypeName, typeName, fieldName] ni

> mkDestroyStmts :: CDecl -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkDestroyStmts stype fields ni = do
>       fieldsInfo <- mapM getFieldInfo fields
>       let anonSType = mkAnonFromDecl stype
>       destroyBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkDestroyBlock ni (show $ pretty anonSType)) fieldsInfo
>       destroyDecls <- mkDeclsFromRemote "AER_MK_DESTROY_DECLS" [show $ pretty anonSType] ni
>       startStmts <- mkStmtFromRemote "AER_MK_DESTROY_STMTS_START" [show $ pretty anonSType] ni
>       endStmts <- mkStmtFromRemote "AER_MK_DESTROY_STMTS_END" [] ni

>       return $ mkCompoundWithDecls Nothing destroyDecls (startStmts ++ destroyBlocks ++ endStmts) ni

> mkDestroyFun :: CDecl -> String -> [CDecl] -> NodeInfo -> RemoteT CExtDecl
> mkDestroyFun decl sname fields ni = do
>     let tparam = mkRemoteDecl decl [(CPtrDeclr [] ni)] "x"
>     when (isNothing tparam) $ invalid "not a known encoding type" ni
>     stmts <- mkDestroyStmts (fromJust tparam) fields ni
>     return $ mkFunDef ((CVoidType ni), [])
>                       [(CStorageSpec (CStatic ni)), (CTypeQual (CInlineQual ni))]
>                       ("aer_destroy_" ++ sname)
>                       [mkCDecl (CVoidType ni) [CPtrDeclr [] ni] "vx" ni]
>                       stmts

> mkEncodingStruct :: CDecl -> String -> NodeInfo -> RemoteT CExtDecl
> mkEncodingStruct decl sname ni = do
>       let ffs = zip fields funs
>           fields = ["encode", "decode", "encode_size", "init", "destroy"]
>           funs = ["aer_encode_"++sname, "aer_decode_"++sname, "aer_encode_size_"++sname, "aer_init_"++sname, "aer_destroy_"++sname]
>       return $ mkFunPtrsStruct "aer_encoder" ("aer_encoder_"++sname) ffs ni

> tripleFST :: (a,b,c) -> a
> tripleFST (a,b,c) = a

> registerTypedefTypedef :: CDecl -> RemoteT [CExtDecl]
> registerTypedefTypedef t@(CDecl specs inits ni) = do
>       let ti@(Just (t1, t2)) = getTypeDefTypeDefInfo t
>       when (isNothing ti) $ invalid ("cannot register a remote type without a name\n") ni
>       return []
      

> registerStruct :: CDecl -> RemoteT [CExtDecl]
> registerStruct d@(CDecl specs inits ni) = do
>       let si = getStructInfo d
>           (Just (structName, fields)) = si
>           s = getRemoteTypeName (getTypeSpec specs)
>           sname = fromJust s
>           allFields = join $ map splitDecls fields

>       -- check that struct has a name
>       when (isNothing s) $ invalid ("cannot define a remote struct without a name\n") ni

>       -- check that fields all have encoding functions
>       finfos <- mapM getFieldInfo fields
>       let types = map tripleFST finfos
>       bools <- mapM isFullEncoding types
>       let nInfos = map nodeInfo fields
>           tbns = zip4 types bools nInfos fields
>       let checkRemoteFields (t, b, n, f) = when (not b) $ 
>               invalid ("struct '" ++  (fromJust structName) ++ "' with parameter '" ++ 
>                        (identToString $ getCDeclName $ f) ++ "' is not a valid remote type\n") n
>       mapM_ checkRemoteFields tbns
>       
>       let (s:_) = filter isStructTypeSpec specs
>       if (structSpecHasFields s)
>          then do
>               let newS = CDeclExt $ CDecl (filterOutRemoteSpec specs) inits ni
>               enc <- mkEncodeFun d sname allFields ni
>               size <- mkSizeFun d sname allFields ni 
>               dec <- mkDecodeFun d sname allFields ni
>               init <- mkInitFun d sname allFields ni
>               dest <- mkDestroyFun d sname allFields ni
>               encStruct <- mkEncodingStruct d sname ni
>               registerFullEncoding sname ni
>               return [newS, dest, init, size, enc, dec, encStruct]
>          else return []

> transDecl :: CExtDecl -> RemoteT [CExtDecl]
> transDecl e@(CDeclExt d@(CDecl specs inits ni))
>       | any isRemoteSpec specs && isStructDecl d = registerStruct d
>       | any isRemoteSpec specs = do
>               invalid "__remote specifiers can only be placed on struct declarations.\n" ni
>               return []
>       | otherwise = return []

> getFileName :: RemoteT String
> getFileName = do
>       w <- get
>       return $ filename w

> setFileName :: FilePath -> RemoteT ()
> setFileName f = do
>       (Remote _ t s is d) <- get
>       put (Remote f t s is d)

> getFilePosStr :: NodeInfo -> RemoteT String
> getFilePosStr (NodeInfo p _ _) = do
>       fname <- getFileName
>       return (fname ++ ":" ++ (show $ posRow p) ++ ":" ++ (show $ posColumn p))

> inRemoteStructs :: CDecl -> Bool
> inRemoteStructs _ = True

> isRemoteFunDef :: CFunDef -> Bool
> isRemoteFunDef (CFunDef specs declr decls stmt ni) = any isRemoteSpec specs

> filterOutRemoteSpec :: [CDeclSpec] -> [CDeclSpec]
> filterOutRemoteSpec = filter (not . isRemoteSpec)

> filterOutTypeSpec :: [CDeclSpec] -> [CDeclSpec]
> filterOutTypeSpec = filter (not . isTypeSpec)

> mkStubDecl :: String -> ReturnType -> [CDecl] -> NodeInfo -> RemoteT CExtDecl
> mkStubDecl fname r@(retType, retDerived) params ni = do
>       [decl] <- liftM (map CDeclExt) $ mkDeclsFromRemote "AER_MK_STUB_DECL" ([(returnToString r), fname] ++ (map (show . pretty) params)) ni
>       return decl

> mkGSRetType :: NodeInfo -> CTypeSpec
> mkGSRetType ni = (CTypeDef (newIdent "triton_ret_t" ni) ni)

> mkGSBufferType :: NodeInfo -> CTypeSpec
> mkGSBufferType ni = (CTypeDef (newIdent "triton_buffer_t" ni) ni)

> checkRemoteParams :: NodeInfo -> [CDecl] -> RemoteT ()
> checkRemoteParams ni [inputdecl, outputdecl] = do
>       let inNI = nodeInfo inputdecl
>           inName = identToString $ getCDeclName inputdecl
>           outNI = nodeInfo outputdecl
>           outName = identToString $ getCDeclName outputdecl
>       when (not $ inRemoteStructs inputdecl) $ invalid ("input parameter '"++inName++"' cannot be encoded, must use a __remote type") inNI
>       when (not $ inRemoteStructs outputdecl) $ invalid ("output parameter '"++outName++"' cannot be encoded, must use a __remote type") outNI
>       let (CDecl specs initdeclrs ni) = outputdecl
>           ptrs = filter isDerivedPtr $ join $ map getDerivedDeclrs initdeclrs
>       when ((length ptrs) /= 1) $ invalid ("output parameter '"++outName++"' must be a pointer to type") outNI
>       return ()

> checkRemoteParams ni _ = do
>       invalid "only two parameters allowed for a __remote declaration" ni 
>       return ()

> isAEReturnType :: CTypeSpec -> Bool
> isAEReturnType (CTypeDef (Ident name _ _) _) = name == "triton_ret_t"

> checkRemoteReturn :: (CTypeSpec, [CDerivedDeclr]) -> RemoteT ()
> checkRemoteReturn (retType, retDerived) = do
>       when (not $ isAEReturnType retType) $ invalid "__remote function must have a return type of triton_ret_t" (nodeInfo retType)
>       return ()

> registerEFun :: String -> RemoteT ()
> registerEFun fname = do
>       let encRE = "aer_encode_(.*)$"
>           decRE = "aer_decode_(.*)$"
>           sizeRE = "aer_encode_size_(.*)$"
>           initRE = "aer_init_(.*)$"
>           destRE = "aer_destroy_(.*)$"
>           getT r = fst ((head r) ! 1)

>       let encResult = (fname =~ encRE) :: [MatchText String]
>       when (not $ null encResult) $ registerEncodeFun $ getT encResult

>       let decResult = (fname =~ decRE) :: [MatchText String]
>       when (not $ null decResult) $ registerDecodeFun $ getT decResult

>       let sizeResult = (fname =~ sizeRE) :: [MatchText String]
>       when (not $ null sizeResult) $ registerEncodeSizeFun $ getT sizeResult

>       let initResult = (fname =~ initRE) :: [MatchText String]
>       when (not $ null initResult) $ registerInitFun $ getT initResult

>       let destResult = (fname =~ destRE) :: [MatchText String]
>       when (not $ null destResult) $ registerDestroyFun $ getT destResult

> registerRemoteDecl :: CExtDecl -> RemoteT [CExtDecl]
> registerRemoteDecl e@(CDeclExt d@(CDecl specs inits ni))

>      | isStructDecl d && (any isRemoteSpec specs) = transDecl e
>      | isFunDecl d && (any isRemoteSpec specs) = do
>           let (fname, returnType, params) = splitFunDecl d
>               newspecs = filterOutRemoteSpec specs
>               ni = nodeInfo e

>           -- only two params allowed right now: input and pointer to output
>           checkRemoteParams ni params

>           checkRemoteReturn returnType

>           let [inputdecl, outputdecl] = params
>               origDecl = CDeclExt $ (CDecl newspecs inits ni)
>           stubDecl <- mkStubDecl fname returnType params ni
>           serviceDecl <- mkServiceDecl ni fname 

>           return [origDecl, stubDecl, serviceDecl]

>       | isFunDecl d = do
>               let (fname, _, _) = splitFunDecl d
>               registerEFun fname
>               return [e]

> registerRemoteDecl e@(CFDefExt fd) = do
>       registerEFun $ getFunDefName fd
>       return [e]      

> registerRemoteDecl e = return [e]

> registerRemoteDecls :: CTranslUnit -> RemoteT CTranslUnit
> registerRemoteDecls (CTranslUnit decls ni) = do
>       newdecls <- liftM concat $ sequence $ map registerRemoteDecl decls
>       return $ CTranslUnit newdecls ni

> data ParserOpts = Pretty | Help | Include String | Report String | Outfile String | Header | ServiceName String | RegistryDir String | Define (String, String)

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

> getReportFilename :: [ParserOpts] -> Maybe String
> getReportFilename ((Report s):ps) = Just s
> getReportFilename (_:ps) = getReportFilename ps
> getReportFilename [] = Nothing

> getOutfile :: [ParserOpts] -> Maybe String
> getOutfile ((Outfile s):ps) = Just s
> getOutfile (_:ps) = getOutfile ps
> getOutfile [] = Nothing

> getServiceName :: [ParserOpts] -> Maybe String
> getServiceName ((ServiceName s):ps) = Just s
> getServiceName (_:ps) = getServiceName ps
> getServiceName [] = Nothing

> getRegistryDir :: [ParserOpts] -> Maybe String
> getRegistryDir ((RegistryDir s):ps) = Just s
> getRegistryDir (_:ps) = getRegistryDir ps
> getRegistryDir [] = Nothing

> getIncludeOpts :: [ParserOpts] -> [FilePath]
> getIncludeOpts ((Include f):ps) = f:(getIncludeOpts ps)
> getIncludeOpts (_:ps) = getIncludeOpts ps
> getIncludeOpts [] = []
                             
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
>    , Option ['n'] ["name"] (ReqArg (\s -> ServiceName s) "<service name>")
>               "service name to use for remote functions"
>    , Option ['d'] ["regdir"] (ReqArg (\s -> RegistryDir s) "<registry dir>")
>               "registry directory path to use for generated service files"
>    , Option ['D'] ["define"] (ReqArg (\s -> newDefine s) "<cpp define>")
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

> parseRemoteHeader :: FilePath -> Maybe FilePath -> FilePath -> IO ()
> parseRemoteHeader headerfile report outfuile = return ()

> mkStubBlock :: String -> String -> String -> Bool -> String -> String -> NodeInfo -> RemoteT [CStat]
> mkStubBlock fname inTypeName inName isInPtr outTypeName outName ni = do
>       let macro = if isInPtr then "AER_MK_STUB_PTR_BLOCK" else "AER_MK_STUB_BLOCK"
>       mkStmtFromRemote macro [fname, inTypeName, inName, outTypeName, outName] ni

> mkStubStmts :: String -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkStubStmts fname params ni = do
>       let [inparam, outparam] = params
>           (Ident inParamName _ _) = getCDeclName inparam
>           (Ident outParamName _ _) = getCDeclName outparam
>           inTypeName = canonCDecl (removeAPtr inparam)
>           outTypeName = canonCDecl (removeAPtr outparam)
>           (CDecl _ inDerived _) = inparam
>           isInPtr = any isDerivedPtr (join $ map getDerivedDeclrs inDerived)
>       stubBlock <- mkStubBlock fname inTypeName inParamName isInPtr outTypeName outParamName ni
>       stubDecls <- mkDeclsFromRemote "AER_MK_STUB_DECLS" [fname] ni
>       return $ mkCompoundWithDecls Nothing 
>           -- declarations
>           stubDecls
>           stubBlock
>           ni

> mkStubFun :: CFunDef -> RemoteT CExtDecl
> mkStubFun fundef = do
>      let fname = getFunDefName fundef
>          params = getFunDefParams fundef
>          ni = nodeInfo fundef
>          noremote = filterOutRemoteSpec $ getDeclSpecs fundef
>          newspecs = filterOutTypeSpec $ noremote
>      stubStmts <- mkStubStmts fname params ni
>      return $ mkFunDef ((CTypeDef (newIdent "triton_ret_t" ni) ni), []) -- return type
>                  newspecs -- get the storage specs for the function
>                  ("remote_" ++ fname) -- the function name for the stub
>                  ((genCDecl "triton_node_t" "id" ni) : params) -- parameters
>                  stubStmts

> mkServiceBlock :: String -> String -> String -> Bool -> String -> String -> NodeInfo -> RemoteT [CStat]
> mkServiceBlock fname inTypeName inName isInPtr outTypeName outName ni = do
>       let macro = if isInPtr then "AER_MK_SERVICE_PTR_BLOCK" else "AER_MK_SERVICE_BLOCK"
>       mkStmtFromRemote macro [fname, inTypeName, inName, outTypeName, outName] ni

> mkServiceStmts :: String -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkServiceStmts fname params ni = do
>       let [inparam, outparam] = params
>           (Ident inParamName _ _) = getCDeclName inparam
>           (Ident outParamName _ _) = getCDeclName outparam
>           inTypeName = canonCDecl (removeAPtr inparam)
>           outTypeName = canonCDecl (removeAPtr outparam)
>           (CDecl _ inDerived _) = inparam
>           isInPtr = any isDerivedPtr (join $ map getDerivedDeclrs inDerived)
>       serviceBlock <- mkServiceBlock fname inTypeName inParamName isInPtr outTypeName outParamName ni

>       return $ mkCompoundWithDecls Nothing [inparam, (removeAPtr outparam), genCDecl "triton_ret_t" "ret" ni]
>                                    serviceBlock ni

> mkServiceFun :: CFunDef -> RemoteT CExtDecl
> mkServiceFun fundef = do
>       let fname = getFunDefName fundef
>           [inparam, outparam]= getFunDefParams fundef
>           ni = nodeInfo fundef
>           noremote = filterOutRemoteSpec $ getDeclSpecs fundef
>           newspecs = filterOutTypeSpec $ noremote
>           (Ident inParamName _ _) = getCDeclName inparam
>           (Ident outParamName _ _) = getCDeclName outparam
>           inTypeName = canonCDecl (removeAPtr inparam)
>           outTypeName = canonCDecl (removeAPtr outparam)
>           (CDecl _ inDerived _) = inparam
>           isInPtr = any isDerivedPtr (join $ map getDerivedDeclrs inDerived)
>           macro = if isInPtr then "AER_MK_SERVICE_FNDEF_INPTR" else "AER_MK_SERVICE_FNDEF"
>       [fdef] <- mkFunDefFromRemote (fname ++ "_service_block") [] macro [fname, inTypeName, inParamName, outTypeName, outParamName] ni
>       return $ addDeclSpecs fdef newspecs

> mkOpStmts :: String -> NodeInfo -> RemoteT [CStat]
> mkOpStmts fname ni = mkStmtFromRemote "AER_MK_OP_STMTS" [fname] ni

> mkOpFun :: CFunDef -> RemoteT CExtDecl
> mkOpFun fundef = do
>       let fname = getFunDefName fundef
>           ni = nodeInfo fundef
>       opfunStmts <- mkOpStmts fname ni
>       return $ mkFunDef ((CTypeDef (newIdent "uint64_t" ni) ni), [])
>                   []
>                   ("__get_op_id_" ++ fname)
>                   []
>                   (mkCompoundStmt Nothing (opfunStmts) ni)

> transformRemote :: CExtDecl -> RemoteT [CExtDecl]
> transformRemote e@(CFDefExt funDef)
>       | isRemoteFunDef funDef = do
>               let re = removeRemoteSpecFromFunDef e 
>                   ni = nodeInfo funDef
>               stub <- mkStubFun funDef
>               servF <- mkServiceFun funDef
>               let rstub = removeRemoteSpecFromFunDef stub
>                   rservF = removeRemoteSpecFromFunDef servF
>                   opfundecl = mkOpIdDecl (nodeInfo funDef) (getFunDefName funDef)
>               opfun <- mkOpFun funDef
>               sop <- liftM (map CDeclExt) $ mkDeclsFromRemote "AER_MK_STATIC_OP_DECL" [getFunDefName funDef] ni
>               registerRemoteFun $ getFunDefName funDef
>               return $ sop ++ [opfundecl, opfun, re, rstub, rservF]
>       | otherwise = return [e]
> transformRemote e = return [e]

> transformR :: CTranslUnit -> RemoteT CTranslUnit
> transformR (CTranslUnit decls ni) = do
>       newdecls <- liftM concat $ sequence $ map transformRemote decls
>       return $ CTranslUnit newdecls ni

> mkRegBlock :: NodeInfo -> String -> RemoteT [CStat]
> mkRegBlock ni s = do
>       mkStmtFromRemote "AER_MK_REG_BLOCK" [s] ni

> mkRegFun :: String -> NodeInfo -> RemoteT CExtDecl
> mkRegFun sname ni = do
>       r <- get
>       decls <- mkDeclsFromRemote "AER_MK_REG_DECLS" [sname] ni
>       stmts <- liftM concat $ sequence $ map (mkRegBlock ni) (funs r)
>       endStmts <- mkStmtFromRemote "AER_MK_REG_END" [sname] ni
>       return $ mkFunDef ((CTypeDef (newIdent "triton_ret_t" ni) ni), [])
>                         []
>                         ("aer_remote_register_" ++ sname)
>                         [mkAnonCDecl (CVoidType ni) [] ni]
>                         (mkCompoundWithDecls Nothing 
>                                              decls
>                                              (stmts ++ endStmts)
>                                              ni)

> mkOpIdDecl :: NodeInfo -> String -> CExtDecl
> mkOpIdDecl ni s =
>       CDeclExt $ mkFunDeclWithDeclSpecs
>               ("__get_op_id_" ++ s)
>               (CTypeDef (newIdent "uint64_t" ni) ni)
>               []
>               [CStorageSpec $ CExtern ni]
>               [mkAnonCDecl (CVoidType ni) [] ni]

> mkOpIdDecls :: NodeInfo -> RemoteT [CExtDecl]
> mkOpIdDecls ni = do
>       r <- get
>       return $ map (mkOpIdDecl ni) (funs r)

> mkServiceDecl :: NodeInfo -> String -> RemoteT CExtDecl
> mkServiceDecl ni s = do
>       [decl] <- liftM (map CDeclExt) $ mkDeclsFromRemote "AER_MK_SERVICE_DECL" [s] ni
>       return $ decl

> mkServiceDecls :: NodeInfo -> RemoteT [CExtDecl]
> mkServiceDecls ni = do
>       r <- get
>       sequence $ map (mkServiceDecl ni) (funs r)

> addRegisterFun :: String -> NodeInfo -> RemoteT CTranslUnit
> addRegisterFun sname ni = do
>       opidDecls <- mkOpIdDecls ni
>       sDecls <- mkServiceDecls ni
>       regf <- mkRegFun sname ni
>       return $ CTranslUnit (opidDecls ++ sDecls ++ [regf]) ni

> generateAST :: FilePath -> IO CTranslUnit
> generateAST input_file = do
>	input_stream <- readInputStream input_file
>	let parse_result = parseC input_stream (position 0 input_file 1 1)
>       case parse_result of
>         Left parse_err -> error (show parse_err)
>         Right ast      -> return ast

> getRegistryDecl :: String -> NodeInfo -> CTranslUnit
> getRegistryDecl s ni = CTranslUnit [ CDeclExt $ (mkFunDecl ("aer_remote_register_" ++ s) (CTypeDef (newIdent "triton_ret_t" ni) ni) []
>                                                            [mkAnonCDecl (CVoidType ni) [] ni]) ] ni

> writeRegistryFiles :: String -> Maybe String -> RemoteT ()
> writeRegistryFiles s r = do
>       let ni = mkNodeInfoOnlyPos $ initPos s
>       regCTU <- addRegisterFun s ni
>       let sourcefile = (if (isJust r) then ((fromJust r) ++ "/") else "") ++ s ++ "_module.ae"
>           headerfile = (if (isJust r) then ((fromJust r) ++ "/") else "") ++ s ++ "_module.h"
>       liftIO $ (writeFile sourcefile) $ "\
>       \\n\n\
>       \/* This is an auto-generated source file created by the ae-remote-parser tool.  DO NOT MODIFY! */\n\n\
>       \#include \"" ++ headerfile ++ "\"\n\n"
>       liftIO $ ((appendFile sourcefile) . show . pretty) regCTU
>       liftIO $ appendFile sourcefile "\n\n"

>       liftIO $ (writeFile headerfile) $ "\
>       \\n\n\
>       \/* This is an auto-generated header created by the ae-remote-parser tool.  DO NOT MODIFY! */\n\n\
>       \#ifndef ___" ++ s ++ "_h__ \n\
>       \#define ___" ++ s ++ "_h__ \n\
>       \#include \"src/aesop/aesop.h\" \n\
>       \#include \"src/remote/service.h\" \n\
>       \\n\n"
>       liftIO $ ((appendFile headerfile) . show . pretty) (getRegistryDecl s ni)
>       liftIO $ appendFile headerfile $ " \n\
>       \\n\n\
>       \#endif \
>       \\n\n"

> parseRemote :: Bool -> [String] -> Maybe FilePath -> Maybe FilePath -> FilePath -> RemoteT ()
> parseRemote p includes outfile report f = do
>	let r = if isJust report then fromJust report else f
>           outf = fromJust outfile
>       setFileName r
>       ctu <- liftIO $ generateAST f
>       ctuWithDecls <- registerRemoteDecls ctu
>       transCTU <- transformR ctuWithDecls
>       when (isJust outfile) $ do
>           if p then liftIO $ ((writeFile outf) . show . pretty) transCTU
>             else liftIO $ ((writeFile outf) . show . serialize) transCTU
>           liftIO $ appendFile (fromJust outfile) "\n\n"
>       return ()

> main :: IO ()
> main = do
>       args <- getArgs
>       let (opts, files, errs) = getOpt RequireOrder parserOpts args
>           pretty = any optPretty opts
>           help = any optHelp opts
>           includes = getIncludeOpts opts
>           outfile = getOutfile opts
>           pheader = any optHeader opts
>           report = getReportFilename opts
>           sname = getServiceName opts
>           regdir = getRegistryDir opts
>           defs = getDefs opts
>           header = "Usage: ae-remote-parser [OPTIONS...] files..."
>       
>       when (not $ null errs) $ ioError $ userError ((concat errs) ++
>                                                     (usageInfo header parserOpts))
>
>       when help $ do { putStrLn $ usageInfo header parserOpts ; exitWith (ExitFailure 1) }
>       when (isNothing outfile && isNothing sname) $ ioError $ userError "No output file specified."

>       when pheader $ do { mapM_ (\f -> parseRemoteHeader f report (fromJust outfile)) files ; exitWith (ExitSuccess) }
>       w <- newRemoteState "" includes defs -- empty string here because the parseRemote function sets the filename for each file
>       let remotes = map (\f -> parseRemote pretty includes outfile report f) files :: [RemoteT ()]
>       w <- foldM (flip execStateT) w remotes
>       when (isJust sname) $ do { runStateT (writeRegistryFiles (fromJust sname) regdir) w; return () }

vim: ts=8 sts=4 sw=4 expandtab
