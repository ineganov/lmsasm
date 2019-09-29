import System.Environment
import Data.Char
import Data.Word
import Data.Bits (rotateR, (.&.), (.|.))
import Data.Map (fromList, member, (!))
import System.FilePath (replaceExtension)
import Control.Monad.State.Lazy (State, get, put, runState, evalState, execState)
import qualified Data.ByteString as BS
import InstnEncodings

data Token = Comment  String |
             Ident    String |
             Instn    String |
             Lit_S    String |
             Lit_I    Int    |
             LParen | RParen |
             LBrace | RBrace |
             KFunction | KIn | KOut | KConst | KWord | KHalf | KByte | KString |
             Comma | Semi | Colon | Equal | EOF deriving (Show, Eq);

data Statement = ConstDecl String Int |
                 VarDecl   String Int |
                 LabelDecl String     |
                 Operation String [OpParam] deriving (Show)

data FuncParam = InParam  String Int |
                 OutParam String Int deriving (Show)

data OpParam = Para_Lit_I Int    |
               Para_Lit_S String |
               Para_Ident String deriving (Show)

data Symbol = SymConst   String Int     |
              SymGlobVar String Int Int |
              SymLocVar  String Int Int |
              SymLabel   String Int     |
              SymFunName String Int deriving (Show)

data Function = Function {fname :: String, fpars :: [FuncParam], fcode :: [Statement]} deriving (Show)

data ObjectFile = ObjectFile [Statement] [Function] deriving (Show)

type SymMapping = (String, Int)
type Xltd_Inst  = (Int, String, [Word8])
data TranslationState = TranslationState { syms    :: [Symbol],
                                           instns  :: [Xltd_Inst],
                                           go      :: Int, -- next global variable offset
                                           lo      :: Int, -- next local variable offset
                                           pc      :: Int } deriving (Show)

lookup_sym :: [Symbol] -> String -> Maybe Symbol
lookup_sym [] _                          = Nothing
lookup_sym ((SymConst   nm i):rest)    s = if nm == s then Just $ SymConst   nm i    else lookup_sym rest s
lookup_sym ((SymGlobVar nm i sz):rest) s = if nm == s then Just $ SymGlobVar nm i sz else lookup_sym rest s
lookup_sym ((SymLocVar  nm i sz):rest) s = if nm == s then Just $ SymLocVar  nm i sz else lookup_sym rest s
lookup_sym ((SymLabel   nm i):rest)    s = if nm == s then Just $ SymLabel   nm i    else lookup_sym rest s
lookup_sym ((SymFunName nm i):rest)    s = if nm == s then Just $ SymFunName nm i    else lookup_sym rest s

locals_size :: [Symbol] -> Int
locals_size ss = sum $ map sz_map ss
                 where sz_map (SymLocVar _ _ sz) = sz
                       sz_map _                  = 0

globals_size :: [Symbol] -> Int
globals_size ss = sum $ map sz_map ss
                  where sz_map (SymGlobVar _ _ sz) = sz
                        sz_map _                   = 0


init_xlat_state = TranslationState [] [] 0 0 0

keyw_map = fromList [ ("function", KFunction ),
                      ("in",       KIn       ),
                      ("out",      KOut      ),
                      ("const",    KConst    ),
                      ("word",     KWord     ),
                      ("half",     KHalf     ),
                      ("byte",     KByte     ),
                      ("string",   KString   ) ]

isIdentAlpha :: Char -> Bool
isIdentAlpha c = (isAlphaNum c) || (c == '_') 

tokenize :: String -> [Token]
tokenize []           = [EOF]
tokenize (' ':xs)     = tokenize xs
tokenize ('\n':xs)    = tokenize xs
tokenize ('(':xs)     = LParen   : tokenize xs
tokenize (')':xs)     = RParen   : tokenize xs
tokenize ('{':xs)     = LBrace   : tokenize xs
tokenize ('}':xs)     = RBrace   : tokenize xs
tokenize (';':xs)     = Semi     : tokenize xs
tokenize (':':xs)     = Colon    : tokenize xs
tokenize (',':xs)     = Comma    : tokenize xs
tokenize ('=':xs)     = Equal    : tokenize xs
tokenize ('/':'/':xs) = Comment (takeWhile (/= '\n') xs)                   : tokenize (dropWhile (/= '\n') xs)
tokenize ('"':xs)     = Lit_S   (takeWhile (/= '"')  xs)                   : tokenize (tail (dropWhile (/= '"' ) xs))
tokenize (x:xs) | isDigit x      = Lit_I (read (takeWhile isDigit (x:xs))) : tokenize (dropWhile isDigit xs)
                | isIdentAlpha x = if member anum keyw_map 
                                   then (keyw_map ! anum)                  : tokenize (dropWhile isIdentAlpha xs)
                                   else if member anum opcode_map
                                   then Instn anum                         : tokenize (dropWhile isIdentAlpha xs)
                                   else Ident anum                         : tokenize (dropWhile isIdentAlpha xs)
                | otherwise = error $ "Unexpected token: " ++ take 10 (x:xs)
                where anum = takeWhile isIdentAlpha (x:xs)

no_comments :: [Token] -> [Token]
no_comments []             = []
no_comments (Comment _:xs) = no_comments xs
no_comments (x:xs)         = x:(no_comments xs)

-----------------------------------------------------------------------------------------------------------------------

expect :: Token -> State [Token] ()
expect t = get >>= (\(x:xs) -> if x == t then put xs else error $ "Expected token <" ++ show t ++ ">, but got: " ++ show x)

take_int :: State [Token] Int
take_int = do (x:rest) <- get
              case x of
                 (Lit_I i) -> put rest >> return i
                 otherwise -> error $ "Expected integer literal, but got: " ++ show x

take_prm :: State [Token] OpParam
take_prm = do (x:rest) <- get
              case x of
                 (Lit_I i) -> put rest >> return (Para_Lit_I i)
                 (Lit_S i) -> put rest >> return (Para_Lit_S i)
                 (Ident i) -> put rest >> return (Para_Ident i)
                 otherwise -> error $ "Expected integer, string or identifier, but got: " ++ show x

take_idn :: State [Token] String
take_idn = do (x:rest) <- get
              case x of
                 (Ident i) -> put rest >> return i
                 otherwise -> error $ "Expected an identifier, but got: " ++ show x

consume :: Int -> State [Token] ()
consume i = get >>= put . (drop i)

parse_statement :: State [Token] Statement
parse_statement = do (tkn:scnd:rest) <- get
                     case (tkn, scnd) of
                        ((Ident i), Colon ) -> put rest  >> return (LabelDecl i)
                        (KConst, (Ident i)) -> put rest  >> expect Equal >> take_int >>= (\d -> expect Semi >> return (ConstDecl i d))
                        (KByte,  (Ident i)) -> put rest  >> expect Semi  >> return (VarDecl i 1)
                        (KHalf,  (Ident i)) -> put rest  >> expect Semi  >> return (VarDecl i 2)
                        (KWord,  (Ident i)) -> put rest  >> expect Semi  >> return (VarDecl i 4)
                        (KString,(Ident i)) -> put rest  >> expect Colon >> take_int >>= (\d -> expect Semi >> return (VarDecl i d))
                        ((Instn i),  Semi ) -> consume 2 >> return (Operation i [])
                        ((Instn i),      _) -> consume 1 >> parse_params [] >>= (\l -> return (Operation i l))

parse_statements :: [Statement] -> State [Token] [Statement]
parse_statements acc = do (nxt:rest) <- get
                          if is_stmt_start nxt then parse_statement >>= (\s -> parse_statements (acc ++ [s]))
                                               else return acc
                          where is_stmt_start  KConst   = True
                                is_stmt_start  KByte    = True
                                is_stmt_start  KHalf    = True
                                is_stmt_start  KWord    = True
                                is_stmt_start  KString  = True
                                is_stmt_start (Instn _) = True
                                is_stmt_start (Ident _) = True                                
                                is_stmt_start _         = False

parse_globals :: [Statement] -> State [Token] [Statement]
parse_globals acc = do (nxt:rest) <- get
                       if is_decl_start nxt then parse_statement >>= (\s -> parse_globals (acc ++ [s]))
                                            else return acc
                       where is_decl_start  KConst   = True
                             is_decl_start  KByte    = True
                             is_decl_start  KHalf    = True
                             is_decl_start  KWord    = True
                             is_decl_start  KString  = True
                             is_decl_start _         = False

parse_params :: [OpParam] -> State [Token] [OpParam]
parse_params acc = do p          <- take_prm
                      (nxt:rest) <- get
                      if nxt == Comma then put rest >> parse_params (acc ++ [p])
                                      else (expect Semi) >> return (acc ++ [p])

parse_function :: State [Token] Function
parse_function = do expect KFunction
                    nm <- take_idn
                    expect LParen
                    fpars <- parse_func_params []
                    expect RParen
                    expect LBrace
                    stmts <- parse_statements []
                    expect RBrace
                    return $ Function nm fpars stmts

parse_func_params :: [FuncParam] -> State [Token] [FuncParam]
parse_func_params acc = do r <- get
                           case r of
                              (RParen:rest) -> return acc
                              (Comma :rest) -> put rest >> parse_func_param >>= (\pp -> parse_func_params (acc ++ [pp]))
                              otherwise     -> parse_func_param >>= (\pp -> parse_func_params (acc ++ [pp]))

parse_func_param :: State [Token] FuncParam
parse_func_param = do r <- get
                      case r of
                        (KIn :KByte  :(Ident i):rest) -> put rest >> return (InParam i 1)
                        (KIn :KHalf  :(Ident i):rest) -> put rest >> return (InParam i 2)
                        (KIn :KWord  :(Ident i):rest) -> put rest >> return (InParam i 4)
                        (KIn :KString:(Ident i):rest) -> put rest >> expect Colon >> take_int >>= (\d -> return (InParam i d))
                        (KOut:KByte  :(Ident i):rest) -> put rest >> return (OutParam i 1)
                        (KOut:KHalf  :(Ident i):rest) -> put rest >> return (OutParam i 2)
                        (KOut:KWord  :(Ident i):rest) -> put rest >> return (OutParam i 4)
                        (KOut:KString:(Ident i):rest) -> put rest >> expect Colon >> take_int >>= (\d -> return (OutParam i d))
                        otherwise                     -> error $ "Expected function argument declaration, but got: " ++ (show $ take 5 r)

parse_functions :: [Function] -> State [Token] [Function]
parse_functions acc = do f <- parse_function
                         (tkn:rest) <- get
                         if tkn == KFunction then parse_functions (acc ++ [f])
                                             else return (acc ++ [f])

parse_file :: State [Token] ObjectFile
parse_file = do g <- parse_globals []
                f <- parse_functions []
                expect EOF
                return $ ObjectFile g f

parse_file_io :: FilePath -> IO ObjectFile
parse_file_io s = do f <- readFile s
                     let tk = no_comments $ tokenize f
                     return $ evalState parse_file tk

-----------------------------------------------------------------------------------------------------------------------
getByte :: Int -> Word32 -> Word8
getByte i v = fromIntegral $ rotateR v (8*i)

integer_enc_size :: Int -> Int
integer_enc_size i | (i <    32) && (i >    -33) = 1
                   | (i <   128) && (i >   -129) = 2
                   | (i < 32768) && (i > -32769) = 3
                   | otherwise                   = 5

integer_enc_1 :: Int -> [Word8]
integer_enc_1 x = [fromIntegral x]

integer_enc_2 :: Int -> [Word8]
integer_enc_2 x = [0x81, fromIntegral x]

integer_enc_3 :: Int -> [Word8]
integer_enc_3 x = [0x82, getByte 0 w32, getByte 1 w32]
                  where w32 = fromIntegral x

integer_enc_5 :: Int -> [Word8]
integer_enc_5 x = [0x83, getByte 0 w32, getByte 1 w32, getByte 2 w32, getByte 3 w32]
                  where w32 = fromIntegral x

integer_enc_4 :: Int -> [Word8]
integer_enc_4 x = [getByte 0 w32, getByte 1 w32, getByte 2 w32, getByte 3 w32]
                  where w32 = fromIntegral x

integer_enc :: Int -> [Word8]
integer_enc x = case (integer_enc_size x) of
                     1 -> integer_enc_1 x
                     2 -> integer_enc_2 x
                     3 -> integer_enc_3 x
                     5 -> integer_enc_5 x
                     otherwise -> error "Encoding size specced incorrectly"

string_enc :: String -> [Word8]
string_enc s = [0x80] ++ (map (fromIntegral . fromEnum) s) ++ [0x00]


op_size :: Statement -> State TranslationState Int
op_size (ConstDecl _ _ )     = return 0
op_size (VarDecl   _ _ )     = return 0
op_size (LabelDecl _ )       = return 0
op_size (Operation _ params) = (mapM param_size params) >>= (\pm -> return $ sum pm + 1)


param_size :: OpParam -> State TranslationState Int
param_size (Para_Lit_I i) = return (integer_enc_size i)
param_size (Para_Lit_S s) = return $ length s + 2
param_size (Para_Ident s) = do st <- get
                               case lookup_sym (syms st) s of
                                 Just (SymGlobVar _ i _) -> return $ integer_enc_size i
                                 Just (SymLocVar  _ i _) -> return $ integer_enc_size i
                                 Just (SymConst   _ i  ) -> return $ integer_enc_size i
                                 Just (SymLabel   _ i  ) -> return 3
                                 Just (SymFunName _ i  ) -> return $ integer_enc_size i
                                 Nothing                 -> error  $ "Undeclared identifier (param size): " ++ s

param_enc :: Int -> OpParam -> State TranslationState [Word8]
param_enc _ (Para_Lit_I i) = return $ integer_enc i
param_enc _ (Para_Lit_S s) = return $ string_enc s
param_enc w (Para_Ident s) = do st <- get
                                case lookup_sym (syms st) s of
                                  Just (SymGlobVar _ i _) -> return $ set_head 0x60 $ integer_enc i
                                  Just (SymLocVar  _ i _) -> return $ set_head 0x40 $ integer_enc i
                                  Just (SymConst   _ i  ) -> return $ integer_enc i
                                  Just (SymLabel   _ i  ) -> return $ integer_enc_3 $ i - ((pc st) + w) -- ref to next pc
                                  Just (SymFunName _ i  ) -> return $ integer_enc i
                                  Nothing                 -> error  $ "Undeclared identifier: " ++ s
                             where set_head bits (x:rest) = (bits .|. x):rest

func_params_enc :: [FuncParam] -> [Word8]
func_params_enc fp = [fromIntegral $ length fp] ++ (concat $ map pmap fp)
                     where pmap (OutParam _ 1) = [0x40]
                           pmap (OutParam _ 2) = [0x41]
                           pmap (OutParam _ 4) = [0x42]
                        -- pmap (OutParam _ 4) = [0x43] -- FIXME: floating point, add to parser as well
                           pmap (OutParam _ x) = [0x44, fromIntegral x] -- FIXME: up to 255 byte strings?
                           pmap (InParam  _ 1) = [0x80]
                           pmap (InParam  _ 2) = [0x81]
                           pmap (InParam  _ 4) = [0x82]
                           pmap (InParam  _ x) = [0x84, fromIntegral x] -- FIXME: up to 255 byte strings?

-----------------------------------------------------------------------------------------------------------------------

reset_pc :: State TranslationState ()
reset_pc = get >>= (\st -> put st{pc = 0})

xlate_globals :: [Statement] -> State TranslationState ()

xlate_globals []                     = return ()

xlate_globals ((LabelDecl s):_)      = error $ "Labels are not allowed in global space: " ++ s

xlate_globals ((Operation s pp):_)   = error $ "Instructions are not allowed in global space: " ++ show (Operation s pp)

xlate_globals ((ConstDecl s i):rest) = do st <- get
                                          put st{syms = (SymConst s i):syms st}
                                          xlate_globals rest 

xlate_globals ((VarDecl s i):rest)   = do st <- get
                                          put st{syms = (SymGlobVar s (go st) i):syms st, go = (go st) + i}
                                          xlate_globals rest 

xlate_func_par :: [FuncParam] -> State TranslationState ()

xlate_func_par [] = return ()
xlate_func_par ((OutParam s i):rest) = xlate_func_par ((InParam s i):rest)
xlate_func_par ((InParam  s i):rest) = do st <- get
                                          put st{syms = (SymLocVar s (lo st) i):syms st, lo = (lo st) + i}
                                          xlate_func_par rest



xlate_flist :: [Function] -> State TranslationState ()
xlate_flist ff = do st <- get
                    let fnlist = map (\(a,b) -> SymFunName a b) $ zip (map fname ff) [1..] -- functions start at index 1
                    put st{syms = syms st ++ fnlist}
                    return ()

xlate_func_fst :: [Statement] -> State TranslationState ()

xlate_func_fst []                      = return ()

xlate_func_fst ((LabelDecl s):rest)    = do st <- get
                                            put st{syms = (SymLabel s (pc st)):syms st}
                                            xlate_func_fst rest

xlate_func_fst ((Operation s pp):rest) = do st <- get -- only increment pc on the first pass
                                            par_sizes <- if is_jump s then mapM param_size (init pp) -- never lookup jump target symbol
                                                                      else mapM param_size pp
                                            if is_jump s then put st{pc = (pc st) + (sum par_sizes) + 4}
                                                         else put st{pc = (pc st) + (sum par_sizes) + 1}
                                            xlate_func_fst rest

xlate_func_fst ((ConstDecl s i):rest)  = do st <- get
                                            put st{syms = (SymConst s i):syms st}
                                            xlate_func_fst rest 

xlate_func_fst ((VarDecl s i):rest)    = do st <- get
                                            put st{syms = (SymLocVar s (lo st) i):syms st, lo = (lo st) + i}
                                            xlate_func_fst rest


xlate_func_snd :: [Statement] -> State TranslationState ()
xlate_func_snd []                      = return ()
xlate_func_snd ((LabelDecl _):rest)    = xlate_func_snd rest
xlate_func_snd ((ConstDecl _ _):rest)  = xlate_func_snd rest
xlate_func_snd ((VarDecl _ _):rest)    = xlate_func_snd rest
xlate_func_snd ((Operation s pp):rest) = do st <- get
                                            par_sizes <- mapM param_size pp
                                            par_encs  <- mapM (param_enc (sum par_sizes + 1)) pp
                                            let new_inst = (pc st, s, [instn_lookup s] ++ concat par_encs)
                                            put st{instns = new_inst:instns st, pc = (pc st) + (sum par_sizes) + 1}
                                            xlate_func_snd rest

xlate_function :: Function -> State TranslationState ()
xlate_function fn = xlate_func_par (fpars fn) >> xlate_func_fst (fcode fn) >> reset_pc >> xlate_func_snd (fcode fn)

xlate_obj :: ObjectFile -> ([[Word8]], [TranslationState])
xlate_obj (ObjectFile globs fns) = let global_state = execState (xlate_globals globs >> xlate_flist fns) init_xlat_state in
                                       ( (map (\f -> func_params_enc $ fpars f) fns), (map (\f -> execState (xlate_function f) global_state) fns) )


-----------------------------------------------------------------------------------------------------------------------

serialize_obj_header :: Bool -> Int -> TranslationState -> [Word8]
serialize_obj_header is_main ofst st = (integer_enc_4 ofst)              ++ 
                                       [0x00, 0x00, trigger_count, 0x00] ++ 
                                       (integer_enc_4 $ locals_size $ syms st)
                                       where trigger_count = if is_main then 0x00 else 0x01

serialize_obj_code :: [Word8] -> TranslationState -> [Word8]
serialize_obj_code pfx st = pfx ++ (concat $ map (\(_, _, x) -> x) $ reverse $ instns st)


serialize_obj :: ([[Word8]], [TranslationState]) -> [Word8]
serialize_obj (pfxs, sts) = image_header ++ first_header ++ (concat other_headers) ++ (concat code_blocks)
                            where code_pfxs    = []:(tail pfxs)
                                  code_blocks  = map (\(pfx, st) -> serialize_obj_code pfx st) (zip code_pfxs sts)
                                  image_size   = 16 + 12 * (length sts) + (sum $ map length code_blocks)
                                  code_offsets = scanl (+) (16 + 12 * (length sts)) (init $ map length code_blocks)
                                  image_header   = img_signature ++ image_size_enc ++ version_info ++ num_objects ++ glob_size
                                  img_signature  = [0x4c, 0x45, 0x47, 0x4f]
                                  image_size_enc = integer_enc_4 image_size
                                  version_info   = [0x68, 0x00]
                                  num_objects    = [fromIntegral $ length sts, 0x00]
                                  glob_size      = integer_enc_4 $ globals_size $ syms $ head sts
                                  first_header   = serialize_obj_header True (head code_offsets) (head sts)
                                  other_headers  = map (\(ofst, st) -> serialize_obj_header False ofst st) (zip (tail code_offsets) (tail sts))


write_rbf :: FilePath -> [Word8] -> IO ()
write_rbf fname code = BS.writeFile fname $ BS.pack $ code

-----------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do fname  <- fmap head getArgs
          parsed <- parse_file_io fname
          write_rbf (replaceExtension fname ".rbf") (serialize_obj $ xlate_obj parsed)
