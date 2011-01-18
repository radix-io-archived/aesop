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
> import System.Directory

Each type in the registry is the name and a set of truth values each function
required:  (encode, decode, encode_size, init, destroy)
Once one of those function declarations is found, the appropriate boolean value
is set to true.

> data RemoteType = RemoteType {
>       encode :: Bool,
>       decode :: Bool,
>       encodeSize :: Bool,
>       initNull :: Bool,
>       destroy :: Bool,
>       copy :: Bool
> }

> remoteTypeAllFalse = RemoteType False False False False False False
> remoteTypeAllTrue = RemoteType True True True True True True

> type RemoteTypeRegistry = HashTable String RemoteType

> newRemoteTypeRegistry :: IO RemoteTypeRegistry
> newRemoteTypeRegistry = Data.HashTable.new (==) Data.HashTable.hashString

> data Remote = Remote {
>       filename :: FilePath,
>       typeReg :: RemoteTypeRegistry,
>       decls :: [CExtDecl],
>       funs :: [(String, (CTypeSpec, [CDerivedDeclr]), [CDecl])],
>       includes :: [FilePath],
>       defines :: [(String, String)],
>       remoteParser :: Maybe MacroParser
> }

> getRemotes :: RemoteT [(String, (CTypeSpec, [CDerivedDeclr]), [CDecl])]
> getRemotes = do
>       r <- get
>       return $ funs r

> getRemoteNames :: RemoteT [String]
> getRemoteNames = do
>       ts <- getRemotes
>       return $ map (\((,,) f _ _) -> f) ts

> type RemoteT = StateT Remote IO

> invalid :: String -> NodeInfo -> RemoteT ()
> invalid msg ni = do
>	fstr <- getFilePosStr ni
>	error (fstr ++ ":  Invalid aesop usage: " ++ msg)

> registerRemoteFun :: String -> (CTypeSpec, [CDerivedDeclr]) -> [CDecl] -> RemoteT ()
> registerRemoteFun name retType params = do
>       r <- get
>       put $ r { funs = (name, retType, params) : (funs r) }

> registerEncodeFun :: String -> RemoteT ()
> registerEncodeFun e = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) e
>       if isJust res
>         then do
>             let rtype = fromJust res
>             liftIO $  Data.HashTable.insert (typeReg r) e $ rtype { encode = True }
>         else
>             liftIO $  Data.HashTable.insert (typeReg r) e $ remoteTypeAllFalse { encode = True }
>       return ()

> registerDecodeFun :: String -> RemoteT ()
> registerDecodeFun d = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) d
>       if isJust res
>         then do
>             let rtype = fromJust res
>             liftIO $  Data.HashTable.insert (typeReg r) d $ rtype { decode = True }
>         else
>             liftIO $  Data.HashTable.insert (typeReg r) d $ remoteTypeAllFalse { decode = True }
>       return ()

> registerEncodeSizeFun :: String -> RemoteT ()
> registerEncodeSizeFun d = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) d
>       if isJust res
>         then do
>             let rtype = fromJust res
>             liftIO $ Data.HashTable.insert (typeReg r) d $ rtype { encodeSize = True }
>         else
>             liftIO $ Data.HashTable.insert (typeReg r) d $ remoteTypeAllFalse { encodeSize = True }
>       return ()

> registerCopyFun :: String -> RemoteT ()
> registerCopyFun d = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) d
>       if isJust res
>         then do
>             let rtype = fromJust res
>             liftIO $  Data.HashTable.insert (typeReg r) d $ rtype { copy = True }
>         else
>             liftIO $  Data.HashTable.insert (typeReg r) d $ remoteTypeAllFalse { copy = True }
>       return ()

> registerInitNullFun :: String -> RemoteT ()
> registerInitNullFun d = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) d
>       if isJust res
>         then do
>             let rtype = fromJust res
>             liftIO $  Data.HashTable.insert (typeReg r) d $ rtype { initNull = True }
>         else
>             liftIO $  Data.HashTable.insert (typeReg r) d $ remoteTypeAllFalse { initNull = True }
>       return ()

> registerDestroyFun :: String -> RemoteT ()
> registerDestroyFun d = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) d
>       if isJust res
>         then do
>             let rtype = fromJust res
>             liftIO $  Data.HashTable.insert (typeReg r) d $ rtype { destroy = True }
>         else
>             liftIO $  Data.HashTable.insert (typeReg r) d $ remoteTypeAllFalse { destroy = True }
>       return ()

> isFullEncoding :: String -> RemoteT Bool
> isFullEncoding d = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) d
>       let rtype = fromJust res
>       if isJust res && encode rtype && decode rtype && encodeSize rtype
>                     && initNull rtype && copy rtype && destroy rtype
>         then return True
>         else return False

> registerFullEncoding :: String -> NodeInfo -> RemoteT ()
> registerFullEncoding s ni = do
>       r <- get
>       res <- liftIO $ Data.HashTable.lookup (typeReg r) s
>       when (isJust res) $ invalid (s ++ " already has encoding functions defined.\n") ni
>       liftIO $ Data.HashTable.insert (typeReg r) s remoteTypeAllTrue

> addRemoteDecl :: CExtDecl -> NodeInfo -> RemoteT ()
> addRemoteDecl d ni = do
>       r <- get
>       put $ r { decls = d : (decls r) }

> newRemoteState :: String -> [FilePath] -> [(String, String)] -> FilePath -> [String] -> IO Remote
> newRemoteState fname includes defs macroHeader gccopts = do
>       r <- Data.HashTable.new (==) Data.HashTable.hashString
>       bp <- mkParser includes defs macroHeader gccopts
>       return $ Remote fname r [] [] includes defs (Just bp)

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
>       r <- get
>       let mp = assert (isJust $ remoteParser r) (fromJust $ remoteParser r)
>       return $ mkDeclsFromCPPMacro mp ni macro params

> mkStmtFromRemote :: String -> [String] -> NodeInfo -> RemoteT [CStat]
> mkStmtFromRemote macro params ni = do
>       r <- get
>       let mp = assert (isJust $ remoteParser r) (fromJust $ remoteParser r)
>       return $ mkStmtFromCPPMacro mp ni macro params

> mkFunDefFromRemote :: String -> [CStat] -> String -> [String] -> NodeInfo -> RemoteT [CExtDecl]
> mkFunDefFromRemote name block macro params ni = do
>       r <- get
>       let mp = assert (isJust $ remoteParser r) (fromJust $ remoteParser r)
>       return $ mkFunDefFromCPPMacro mp ni macro params name block

> mkEncodeBlock :: NodeInfo -> String -> String -> Bool -> RemoteT [CStat]
> mkEncodeBlock ni typeName fieldName isPtr = do
>       let ptrParam = if isPtr then "" else "&"
>       mkStmtFromRemote "AER_MK_ENCODE_TYPE" [typeName, fieldName, ptrParam] ni

> mkEncodeStmts :: CDecl -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkEncodeStmts stype fields ni = do
>       fieldsInfo <- mapM getFieldInfo fields
>       let anonSType = mkAnonFromDecl stype
>       encodeBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkEncodeBlock ni) fieldsInfo
>       encodeDecls <- mkDeclsFromRemote "AER_MK_ENCODE_DECLS" [show $ pretty anonSType] ni
>       let typeStr = show $ pretty anonSType
>           canon ' ' = '_'
>           canon '*' = 'p'
>           canon '{' = '_';
>           canon '}' = '_';
>           canon a = a
>           canonTypeStr = map canon typeStr
>       startStmts <- mkStmtFromRemote "AER_MK_ENCODE_STMTS_START" [typeStr, canonTypeStr] ni
>       endStmts <- mkStmtFromRemote "AER_MK_ENCODE_STMTS_END" [] ni
>       return $ mkCompoundWithDecls Nothing encodeDecls (startStmts ++ encodeBlocks ++ endStmts) ni

> mkEncodeFun :: CDecl -> String -> [CDecl] -> NodeInfo -> RemoteT CExtDecl
> mkEncodeFun decl sname fields ni = do
>     let tparam = mkRemoteDecl decl [(CPtrDeclr [] ni)] "x"
>     when (isNothing tparam) $ invalid "not a known encoding type" ni
>     stmts <- mkEncodeStmts (fromJust tparam) fields ni
>     return $ mkFunDef ((CTypeDef (newIdent "triton_ret_t" ni) ni), [])
>                       --[(CStorageSpec (CStatic ni)), (CTypeQual (CInlineQual ni))]
>                       []
>                       ("aer_encode_" ++ sname)
>                       [mkCDecl (CTypeDef (newIdent "triton_buffer_t" ni) ni) [CPtrDeclr [] ni] "buf" ni,
>                        mkConstCDecl (CCharType ni) [CPtrDeclr [] ni] "varname" ni,
>                        mkCDecl (CVoidType ni) [CPtrDeclr [] ni] "vx" ni]
>                       stmts

> mkDecodeBlock :: NodeInfo -> String -> String -> Bool -> RemoteT [CStat]
> mkDecodeBlock ni typeName fieldName isPtr = do 
>       let ptrParam = if isPtr then "" else "&"
>       mkStmtFromRemote "AER_MK_DECODE_TYPE" [typeName, fieldName, ptrParam] ni

> mkDecodeStmts :: CDecl -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkDecodeStmts stype fields ni = do
>       fieldsInfo <- mapM getFieldInfo fields
>       let anonSType = mkAnonFromDecl stype
>       decodeBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkDecodeBlock ni) fieldsInfo 
>       decodeDecls <- mkDeclsFromRemote "AER_MK_DECODE_DECLS" [show $ pretty anonSType] ni
>       let typeStr = show $ pretty anonSType
>           canon ' ' = '_'
>           canon '*' = 'p'
>           canon '{' = '_';
>           canon '}' = '_';
>           canon a = a
>           canonTypeStr = map canon typeStr
>       startStmts <- mkStmtFromRemote "AER_MK_DECODE_STMTS_START" [typeStr, canonTypeStr] ni
>       endStmts <- mkStmtFromRemote "AER_MK_DECODE_STMTS_END" [] ni
>       return $ mkCompoundWithDecls Nothing decodeDecls (startStmts ++ decodeBlocks ++ endStmts) ni

> mkDecodeFun :: CDecl -> String -> [CDecl] -> NodeInfo -> RemoteT CExtDecl
> mkDecodeFun decl sname fields ni = do
>     let tparam = mkRemoteDecl decl [(CPtrDeclr [] ni)] "x"
>     when (isNothing tparam) $ invalid "not a known encoding type" ni
>     stmts <- mkDecodeStmts (fromJust tparam) fields ni
>     return $ mkFunDef ((CTypeDef (newIdent "triton_ret_t" ni) ni), [])
>                       --[(CStorageSpec (CStatic ni)), (CTypeQual (CInlineQual ni))]
>                       []
>                       ("aer_decode_" ++ sname)
>                       [mkCDecl (CTypeDef (newIdent "triton_buffer_t" ni) ni) [CPtrDeclr [] ni] "buf" ni,
>                        mkCDecl (CCharType ni) [CPtrDeclr [] ni, CPtrDeclr [] ni] "varname" ni,
>                        mkCDecl (CVoidType ni) [CPtrDeclr [] ni] "vx" ni]
>                       stmts

> mkSizeBlock :: NodeInfo -> String -> String -> Bool -> RemoteT [CStat]
> mkSizeBlock ni typeName fieldName isPtr = do
>       let ptrParam = if isPtr then "" else "&"
>       mkStmtFromRemote "AER_MK_SIZE_TYPE" [typeName, fieldName, ptrParam] ni

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
>                         --[(CStorageSpec (CStatic ni)), (CTypeQual (CInlineQual ni))]
>                         []
>                         ("aer_encode_size_" ++ sname)
>                         [mkConstCDecl (CCharType ni) [CPtrDeclr [] ni] "varname" ni,
>                          mkCDecl (CVoidType ni) [CPtrDeclr [] ni] "vx" ni]
>                         stmts

> mkPtrInitNullBlock :: NodeInfo -> String -> String -> String -> Bool -> RemoteT [CStat]
> mkPtrInitNullBlock ni _ typeName fieldName _ = do
>       mkStmtFromRemote "AER_MK_INIT_PTR_NULL" [typeName, fieldName] ni

> mkInitNullBlock :: NodeInfo -> String -> String -> String -> Bool -> RemoteT [CStat]
> mkInitNullBlock ni baseTypeName typeName fieldName isPtr = do
>       let ptrParam = if isPtr then "" else "&"
>       mkStmtFromRemote "AER_MK_INIT_NULL_TYPE" [baseTypeName, typeName, fieldName, ptrParam] ni

> mkInitNullStmts :: CDecl -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkInitNullStmts stype fields ni = do
>       fieldsInfo <- mapM getFieldInfo fields
>       let getDeriveds (CDecl _ declrs _) = concatMap getDerivedDeclrs declrs
>           ptrFields = filter (\d -> any isDerivedPtr (getDeriveds d)) fields
>           nonPtrFields = filter (\d -> not $ any isDerivedPtr (getDeriveds d)) fields
>           anonSType = mkAnonFromDecl stype
>           baseTypeName = getRemoteTypeName $ getTypeSpecFromDecl anonSType
>       ptrFieldInfo <- mapM getFieldInfo ptrFields
>       nonPtrFieldsInfo <- mapM getFieldInfo nonPtrFields
>       when (isNothing baseTypeName) $ invalid ("type '" ++ (show $ pretty stype) ++ "does not have a known encoding type") ni
>       nullBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkPtrInitNullBlock ni $ fromJust baseTypeName) ptrFieldInfo
>       initBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkInitNullBlock ni $ fromJust baseTypeName) nonPtrFieldsInfo
>       initDecls <- mkDeclsFromRemote "AER_MK_INIT_DECLS" [show $ pretty anonSType] ni
>       startStmts <- mkStmtFromRemote "AER_MK_INIT_STMTS_START" [show $ pretty anonSType] ni
>       endStmts <- mkStmtFromRemote "AER_MK_INIT_STMTS_END" [] ni

>       return $ mkCompoundWithDecls Nothing initDecls (startStmts ++ nullBlocks ++ initBlocks ++ endStmts) ni

> mkInitNullFun :: CDecl -> String -> [CDecl] -> NodeInfo -> RemoteT CExtDecl
> mkInitNullFun decl sname fields ni = do
>     fieldsInfo <- mapM getFieldInfo fields
>     let tparam = mkRemoteDecl decl [(CPtrDeclr [] ni)] "x"
>     when (isNothing tparam) $ invalid "not a known encoding type" ni
>     stmts <- mkInitNullStmts (fromJust tparam) fields ni
>     return $ mkFunDef ((CTypeDef (newIdent "triton_ret_t" ni) ni), [])
>                       --[(CStorageSpec (CStatic ni)), (CTypeQual (CInlineQual ni))]
>                       []
>                       ("aer_init_null_" ++ sname)
>                       ([mkCDecl (CVoidType ni) [CPtrDeclr [] ni] "vx" ni])
>                       stmts

> mkPtrInitBlock :: NodeInfo -> String -> String -> String -> Bool -> RemoteT [CStat]
> mkPtrInitBlock ni baseTypeName typeName fieldName isPtr = do
>       let ptrParam = if isPtr then "" else "&"
>       mkStmtFromRemote "AER_MK_PTR_INIT_TYPE" [baseTypeName, typeName, fieldName, ptrParam] ni

> mkInitBlock :: NodeInfo -> String -> String -> String -> Bool -> RemoteT [CStat]
> mkInitBlock ni baseTypeName typeName fieldName isPtr = do
>       let ptrParam = if isPtr then "" else "&"
>       mkStmtFromRemote "AER_MK_INIT_TYPE" [baseTypeName, typeName, fieldName, ptrParam] ni

> mkInitStmts :: CDecl -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkInitStmts stype fields ni = do
>       fieldsInfo <- mapM getFieldInfo fields
>       let getDeriveds (CDecl _ declrs _) = concatMap getDerivedDeclrs declrs
>           anonSType = mkAnonFromDecl stype
>           baseTypeName = getRemoteTypeName $ getTypeSpecFromDecl anonSType
>           ptrFields = filter (\d -> any isDerivedPtr (getDeriveds d)) fields
>           nonPtrFields = filter (\d -> not $ any isDerivedPtr (getDeriveds d)) fields
>       ptrFieldInfo <- mapM getFieldInfo ptrFields
>       nonPtrFieldsInfo <- mapM getFieldInfo nonPtrFields
>       when (isNothing baseTypeName) $ invalid ("type '" ++ (show $ pretty stype) ++ "does not have a known encoding type") ni
>       nullBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkPtrInitBlock ni $ fromJust baseTypeName) ptrFieldInfo
>       initBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkInitBlock ni $ fromJust baseTypeName) nonPtrFieldsInfo
>       initDecls <- mkDeclsFromRemote "AER_MK_INIT_DECLS" [show $ pretty anonSType] ni
>       startStmts <- mkStmtFromRemote "AER_MK_INIT_STMTS_START" [show $ pretty anonSType] ni
>       endStmts <- mkStmtFromRemote "AER_MK_INIT_STMTS_END" [] ni
                    
>       return $ mkCompoundWithDecls Nothing initDecls (startStmts ++ nullBlocks ++ initBlocks ++ endStmts) ni

> mkInitFun :: CDecl -> String -> [CDecl] -> NodeInfo -> RemoteT CExtDecl
> mkInitFun decl sname fields ni = do
>     fieldsInfo <- mapM getFieldInfo fields
>     let tparam = mkRemoteDecl decl [(CPtrDeclr [] ni)] "x"
>         getDeriveds (CDecl _ declrs _) = concatMap getDerivedDeclrs declrs
>         params = map (\d -> if (any isDerivedPtr (getDeriveds d)) then d else addPtrDeclr d) fields
>     when (isNothing tparam) $ invalid "not a known encoding type" ni
>     stmts <- mkInitStmts (fromJust tparam) fields ni
>     return $ mkFunDef ((CTypeDef (newIdent "triton_ret_t" ni) ni), [])
>                       []
>                       ("aer_init_" ++ sname)
>                       ([mkCDecl (CVoidType ni) [CPtrDeclr [] ni] "vx" ni] ++ params)
>                       stmts

> mkPtrCopyBlock :: NodeInfo -> String -> String -> String -> Bool -> RemoteT [CStat]
> mkPtrCopyBlock ni baseTypeName typeName fieldName isPtr = do
>       let ptrParam = if isPtr then "" else "&"
>       mkStmtFromRemote "AER_MK_PTR_COPY_TYPE" [baseTypeName, typeName, fieldName, ptrParam] ni

> mkCopyBlock :: NodeInfo -> String -> String -> String -> Bool -> RemoteT [CStat]
> mkCopyBlock ni baseTypeName typeName fieldName isPtr = do
>       let ptrParam = if isPtr then "" else "&"
>       mkStmtFromRemote "AER_MK_COPY_TYPE" [baseTypeName, typeName, fieldName, ptrParam] ni

> mkCopyStmts :: CDecl -> [CDecl] -> NodeInfo -> RemoteT CStat
> mkCopyStmts stype fields ni = do
>       fieldsInfo <- mapM getFieldInfo fields
>       let getDeriveds (CDecl _ declrs _) = concatMap getDerivedDeclrs declrs
>           anonSType = mkAnonFromDecl stype
>           baseTypeName = getRemoteTypeName $ getTypeSpecFromDecl anonSType
>           ptrFields = filter (\d -> any isDerivedPtr (getDeriveds d)) fields
>           nonPtrFields = filter (\d -> not $ any isDerivedPtr (getDeriveds d)) fields
>       ptrFieldInfo <- mapM getFieldInfo ptrFields
>       nonPtrFieldsInfo <- mapM getFieldInfo nonPtrFields
>       when (isNothing baseTypeName) $ invalid ("type '" ++ (show $ pretty stype) ++ "does not have a known encoding type") ni
>       ptrInitBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkPtrCopyBlock ni $ fromJust baseTypeName) ptrFieldInfo
>       initBlocks <- liftM concat $ sequence $ map (uncurry3 $ mkCopyBlock ni $ fromJust baseTypeName) nonPtrFieldsInfo
>       initDecls <- mkDeclsFromRemote "AER_MK_COPY_DECLS" [show $ pretty anonSType] ni
>       startStmts <- mkStmtFromRemote "AER_MK_COPY_STMTS_START" [show $ pretty anonSType] ni
>       endStmts <- mkStmtFromRemote "AER_MK_COPY_STMTS_END" [] ni
                    
>       return $ mkCompoundWithDecls Nothing initDecls (startStmts ++ ptrInitBlocks ++ initBlocks ++ endStmts) ni

> mkCopyFun :: CDecl -> String -> [CDecl] -> NodeInfo -> RemoteT CExtDecl
> mkCopyFun decl sname fields ni = do
>     fieldsInfo <- mapM getFieldInfo fields
>     let tparam = mkRemoteDecl decl [(CPtrDeclr [] ni)] "x"
>         mkVoidParam n = mkCDecl (CVoidType ni) [CPtrDeclr [] ni] n ni
>     when (isNothing tparam) $ invalid "not a known encoding type" ni
>     stmts <- mkCopyStmts (fromJust tparam) fields ni
>     return $ mkFunDef ((CTypeDef (newIdent "triton_ret_t" ni) ni), [])
>                       --[(CStorageSpec (CStatic ni)), (CTypeQual (CInlineQual ni))]
>                       []
>                       ("aer_copy_" ++ sname)
>                       [mkVoidParam "vx", mkVoidParam "vv"]
>                       stmts

> mkDestroyBlock :: NodeInfo -> String -> String -> String -> Bool -> RemoteT [CStat]
> mkDestroyBlock ni baseTypeName typeName fieldName isPtr = do
>       let ptrParam = if isPtr then "" else "&"
>       mkStmtFromRemote "AER_MK_DESTROY_TYPE" [baseTypeName, typeName, fieldName, ptrParam] ni

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
>                       --[(CStorageSpec (CStatic ni)), (CTypeQual (CInlineQual ni))]
>                       []
>                       ("aer_destroy_" ++ sname)
>                       [mkCDecl (CVoidType ni) [CPtrDeclr [] ni] "vx" ni]
>                       stmts

> mkEncodingStruct :: CDecl -> String -> NodeInfo -> RemoteT CExtDecl
> mkEncodingStruct decl sname ni = do
>       let ffs = zip fields funs
>           fields = ["encode", "decode", "encode_size", "init_null", "copy", "destroy"]
>           funs = ["aer_encode_"++sname, "aer_decode_"++sname, "aer_encode_size_"++sname, "aer_init_null_"++sname, "aer_copy_"++sname, "aer_destroy_"++sname]
>       return $ mkFunPtrsStruct "aer_encoder" ("aer_encoder_"++sname++" __attribute__ ((unused))") ffs True ni

> tripleFST :: (a,b,c) -> a
> tripleFST (a,b,c) = a

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
>               invalid ("struct '" ++  (show $ fromJust structName) ++ "' with parameter '" ++ 
>                        (show $ getCDeclName $ f) ++ "' is not a valid remote type\n") n
>       mapM_ checkRemoteFields tbns
>       
>       let (s:_) = filter isStructTypeSpec specs
>       if (structSpecHasFields s)
>          then do
>               let newS = CDeclExt $ CDecl (filterOutRemoteSpec specs) inits ni
>               enc <- mkEncodeFun d sname allFields ni
>               size <- mkSizeFun d sname allFields ni 
>               dec <- mkDecodeFun d sname allFields ni
>               initNull <- mkInitNullFun d sname allFields ni
>               init <- mkInitFun d sname allFields ni
>               dest <- mkDestroyFun d sname allFields ni
>               copy <- mkCopyFun d sname allFields ni
>               encStruct <- mkEncodingStruct d sname ni
>               decls <- mkStructFunDecls (CDeclExt d)
>               registerFullEncoding sname ni
>               addRemoteDecl newS ni
>               return $ [newS] ++ decls ++ [dest, initNull, copy, size, enc, dec, init, encStruct]
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
>       r <- get
>       put $ r { filename = f }

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
>           initNullRE = "aer_init_null_(.*)$"
>           copyRE = "aer_copy_(.*)$"
>           destRE = "aer_destroy_(.*)$"
>           getT r = fst ((head r) ! 1)

>       let encResult = (fname =~ encRE) :: [MatchText String]
>       when (not $ null encResult) $ registerEncodeFun $ getT encResult

>       let decResult = (fname =~ decRE) :: [MatchText String]
>       when (not $ null decResult) $ registerDecodeFun $ getT decResult

>       let sizeResult = (fname =~ sizeRE) :: [MatchText String]
>       when (not $ null sizeResult) $ registerEncodeSizeFun $ getT sizeResult

>       let initNullResult = (fname =~ initNullRE) :: [MatchText String]
>       when (not $ null initNullResult) $ registerInitNullFun $ getT initNullResult

>       let copyResult = (fname =~ copyRE) :: [MatchText String]
>       when (not $ null copyResult) $ registerCopyFun $ getT copyResult

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
>           stubDecl <- mkStubDecl (identToString fname) returnType params ni
>           serviceDecl <- mkServiceDecl ni (identToString fname)

>           return [origDecl, stubDecl, serviceDecl]

>       | isFunDecl d = do
>               let (fname, _, _) = splitFunDecl d
>               registerEFun (identToString fname)
>               return [e]

> registerRemoteDecl e@(CFDefExt fd) = do
>       registerEFun $ getFunDefName fd
>       return [e]      

> registerRemoteDecl e = return [e]

> registerRemoteDecls :: CTranslUnit -> RemoteT CTranslUnit
> registerRemoteDecls (CTranslUnit decls ni) = do
>       newdecls <- liftM concat $ sequence $ map registerRemoteDecl decls
>       return $ CTranslUnit newdecls ni

> data ParserOpts = Pretty | Help | Include String | Report String | Outfile String | Header | ServiceName String | RegistryDir String | Define (String, String) | GCCOpt String

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
                             
> getGCCOpts :: [ParserOpts] -> [String]
> getGCCOpts ((GCCOpt o):ps) = o:(getGCCOpts ps)
> getGCCOpts (_:ps) = getGCCOpts ps
> getGCCOpts [] = []

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
>    , Option ['g'] ["gccopt"] (ReqArg (\s -> GCCOpt s) "<gcc option>")
>               "pass this gcc option when gcc is invoked"
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
>       let ptrParam = if isInPtr then "" else "&"
>       mkStmtFromRemote "AER_MK_STUB_BLOCK" [fname, inTypeName, inName, outTypeName, outName, ptrParam] ni

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
>                  ((genCDecl "aer_remote_ctx_t" "ctx" ni) :
>                  ((genCDecl "triton_addr_t" "id" ni) : params)) -- parameters
>                  stubStmts

> mkServiceFun :: CFunDef -> RemoteT CExtDecl
> mkServiceFun fundef = do
>       let fname = getFunDefName fundef
>           [inparam, outparam]= getFunDefParams fundef
>           ni = nodeInfo fundef
>           noremote = filterOutRemoteSpec $ getDeclSpecs fundef
>           newspecs = filterOutTypeSpec $ noremote
>           (Ident inParamName _ _) = getCDeclName inparam
>           (Ident outParamName _ _) = getCDeclName outparam
>           inTypeName = show $ pretty (removeAPtr $ emptyDeclrList inparam)
>           outTypeName = show $ pretty (removeAPtr $ emptyDeclrList outparam)
>           canonInType = getRemoteTypeName $ getTypeSpecFromDecl inparam 
>           canonOutType = getRemoteTypeName $ getTypeSpecFromDecl outparam
>           (CDecl _ inDerived _) = inparam
>           isInPtr = any isDerivedPtr (join $ map getDerivedDeclrs inDerived)
>           ptrParam = if isInPtr then "&" else ""
>       when (not $ isJust canonInType) $
>               invalid ("'" ++ inTypeName ++ "'" ++ 
>                        " is not a recognized encoding type for input parameter '" ++ inParamName ++ "' to function: " ++ fname) ni
>       when (not $ isJust canonOutType) $
>               invalid ("'" ++ outTypeName ++ "'" ++
>                        " is not a recognized encoding type for output parameter '" ++ outParamName ++ "' to function: " ++ fname) ni
>       [fdef] <- mkFunDefFromRemote (fname ++ "_service_block") [] "AER_MK_SERVICE_FNDEF"
>                                    [fname,
>                                     inTypeName,
>                                     fromJust canonInType,
>                                     inParamName,
>                                     outTypeName,
>                                     fromJust canonOutType,
>                                     outParamName,
>                                     ptrParam] ni
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

> mkRegCtorFun :: String -> NodeInfo -> RemoteT CExtDecl
> mkRegCtorFun serviceName ni = do
>       [fdef] <- mkFunDefFromRemote "" [] "AER_MK_REG_CTOR_FNDEF" [serviceName] ni
>       return fdef

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
>               registerRemoteFun (getFunDefName funDef) (getFunDefReturn funDef) (getFunDefParams funDef)
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
>       decls <- mkDeclsFromRemote "AER_MK_REG_DECLS" [sname] ni
>       rs <- getRemoteNames 
>       stmts <- liftM concat $ sequence $ map (mkRegBlock ni) rs 
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
>       rs <- getRemoteNames
>       return $ map (mkOpIdDecl ni) rs 

> mkServiceDecl :: NodeInfo -> String -> RemoteT CExtDecl
> mkServiceDecl ni s = do
>       [decl] <- liftM (map CDeclExt) $ mkDeclsFromRemote "AER_MK_SERVICE_DECL" [s] ni
>       return $ decl

> mkServiceDecls :: NodeInfo -> RemoteT [CExtDecl]
> mkServiceDecls ni = do
>       rs <- getRemoteNames
>       sequence $ map (mkServiceDecl ni) rs 

> addRegisterFun :: String -> NodeInfo -> RemoteT CTranslUnit
> addRegisterFun sname ni = do
>       opidDecls <- mkOpIdDecls ni
>       sDecls <- mkServiceDecls ni
>       regf <- mkRegFun sname ni
>       ctor <- mkRegCtorFun sname ni
>       return $ CTranslUnit (opidDecls ++ sDecls ++ [regf, ctor]) ni
>       -- return $ CTranslUnit (opidDecls ++ sDecls ++ [regf]) ni

> mkInitDeclParamStr :: NodeInfo -> String -> String -> Bool -> String
> mkInitDeclParamStr ni typeName fieldName isPtr =
>       let ptrParam = if isPtr then " * " else ""
>       in (typeName ++ " " ++ ptrParam ++ " " ++ fieldName)

> mkStructFunDecls :: CExtDecl -> RemoteT [CExtDecl]
> mkStructFunDecls (CDeclExt d@(CDecl specs inits ni)) = do
>       let si = getStructInfo d
>           (Just (structName, fields)) = si
>           s = getRemoteTypeName (getTypeSpec specs)
>           sname = fromJust s
>           getDeriveds (CDecl _ declrs _) = concatMap getDerivedDeclrs declrs
>           params = map (\d -> if (any isDerivedPtr (getDeriveds d)) then d else addPtrDeclr d) fields

>       mDecls <- mkDeclsFromRemote "AER_MK_STRUCT_DECLS" (sname : (map (show . pretty) fields)) ni
>       initDecl <- mkDeclsFromRemote "AER_MK_STRUCT_INIT_DECL" (sname : (map (show . pretty) params)) ni
>       return $ map CDeclExt (mDecls ++ initDecl)

> mkStubs :: NodeInfo -> RemoteT CTranslUnit
> mkStubs ni = do
>       r <- get
>       let rdecls = reverse $ decls r
>       funDecls <- mapM mkStructFunDecls rdecls
>       let allDecls  = concat $ zipWith (\a b -> a:b) rdecls funDecls
>       rs <- getRemotes
>       stubs <- sequence $ map (\((,,) f r p) -> mkStubDecl f r p ni) rs 
>       return $ CTranslUnit (allDecls ++ stubs) ni

> generateAST :: FilePath -> IO CTranslUnit
> generateAST input_file = do
>	input_stream <- readInputStream input_file
>	let parse_result = parseC input_stream (position 0 input_file 1 1)
>       case parse_result of
>         Left parse_err -> error $ "Parse failed for input file: " ++ input_file ++ ": " ++ (show parse_err)
>         Right ast      -> return ast

> getRegistryDecl :: String -> NodeInfo -> CTranslUnit
> getRegistryDecl s ni = CTranslUnit [ 
>                            CDeclExt $ (mkFunDecl ("aer_remote_register_" ++ s)
>                                                  (CTypeDef (newIdent "triton_ret_t" ni) ni) []
>                                                  [mkAnonCDecl (CVoidType ni) [] ni]) ] ni

> writeRegistryFiles :: String -> Maybe String -> RemoteT ()
> writeRegistryFiles s r = do
>       let ni = mkNodeInfoOnlyPos $ initPos s
>       let sourcefile = (if (isJust r) then ((fromJust r) ++ "/") else "") ++ s ++ "_module.ae"
>           headerfile = (if (isJust r) then ((fromJust r) ++ "/") else "") ++ s ++ "_module.h"
>           stubfile = (if (isJust r) then ((fromJust r) ++ "/") else "") ++ s ++ "_stubs.hae"

>       regCTU <- addRegisterFun s ni

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
>       \#include \"src/remote/service.hae\" \n\
>       \\n\n"
>       liftIO $ ((appendFile headerfile) . show . pretty) (getRegistryDecl s ni)
>       liftIO $ appendFile headerfile $ " \n\
>       \\n\n\
>       \#endif \
>       \\n\n"

>       stubCTU <- mkStubs ni
>       liftIO $ (writeFile stubfile) $ "\
>       \\n\n\
>       \/* This is an auto-generated header file create by the ae-remote-parser tool.  DO NOT MODIFY! */\n\n\
>       \#ifndef ___" ++ s ++ "_stubs_hae__ \n\
>       \#define ___" ++ s ++ "_stubs_hae__ \n\
>       \#include \"src/aesop/aesop.h\" \n\
>       \#include \"src/remote/service.hae\" \n\
>       \\n\n"
>       liftIO $ ((appendFile stubfile) . show . pretty) stubCTU
>       liftIO $ appendFile stubfile $ " \n\
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
>           gccopts = getGCCOpts opts
>           header = "Usage: ae-remote-parser [OPTIONS...] files..."
>       
>       when (not $ null errs) $ ioError $ userError ((concat errs) ++
>                                                     (usageInfo header parserOpts))
>
>       when help $ do { putStrLn $ usageInfo header parserOpts ; exitWith (ExitFailure 1) }
>       when (isNothing outfile && isNothing sname) $ ioError $ userError "No output file specified."

>       when pheader $ do { mapM_ (\f -> parseRemoteHeader f report (fromJust outfile)) files ; exitWith (ExitSuccess) }
>       -- empty string here because the parseRemote function sets the filename for each file
>       r <- newRemoteState "" includes defs "src/aesop/ae-remote-parser.h" gccopts
>       let remotes = map (\f -> parseRemote pretty includes outfile report f) files :: [RemoteT ()]
>       r <- foldM (flip execStateT) r remotes
>       when (isJust sname) $ do { runStateT (writeRegistryFiles (fromJust sname) regdir) r; return () }
>       assert (isJust $ remoteParser r) return ()
>       removeFile $ macheader $ fromJust $ remoteParser r

vim: ts=8 sts=4 sw=4 expandtab
