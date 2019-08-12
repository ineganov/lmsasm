import System.Environment
import Data.Char
import Data.Map (fromList, member, (!))
import Control.Monad.State.Lazy (State, get, put, runState, evalState)
import InstnEncodings

data Token = Comment  String |
             Ident    String |
             Label    String |
             Instn    String |
             Lit_S    String |
             Lit_I    Int    |
             LParen | RParen |
             LBrace | RBrace |
             KFunction | KConst | KWord | KHalf | KByte | 
             Comma | Semi | Colon | Equal | EOF deriving (Show, Eq);

data Statement = ConstDecl String Int | 
                 VarDecl   String Int | 
                 LabelDecl String     |
                 Operation String [Token] deriving (Show)

data Function = Function String [Statement] deriving (Show)

data ObjectFile = ObjectFile [Statement] [Function] deriving (Show)

keyw_map = fromList [ ("function", KFunction ),
                      ("const",    KConst    ),
                      ("word",     KWord     ),
                      ("half",     KHalf     ),
                      ("byte",     KByte     ) ]

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
tokenize ('@':xs)     = Label   (takeWhile isIdentAlpha xs)                : tokenize (dropWhile isIdentAlpha xs)
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

take_prm :: State [Token] Token
take_prm = do (x:rest) <- get
              case x of
                 (Lit_I i) -> put rest >> return (Lit_I i)
                 (Lit_S i) -> put rest >> return (Lit_S i)
                 (Ident i) -> put rest >> return (Ident i)
                 (Label i) -> put rest >> return (Label i)
                 otherwise -> error $ "Expected integer, identifier, string or a label, but got: " ++ show x

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
                        ((Instn i),      _) -> consume 1 >> parse_params [] >>= (\l -> return (Operation i l))

parse_statements :: [Statement] -> State [Token] [Statement]
parse_statements acc = do (nxt:rest) <- get
                          if is_stmt_start nxt then parse_statement >>= (\s -> parse_statements (acc ++ [s]))
                                               else return acc
                          where is_stmt_start  KConst   = True
                                is_stmt_start  KByte    = True
                                is_stmt_start  KHalf    = True
                                is_stmt_start  KWord    = True
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
                             is_decl_start _         = False

parse_params :: [Token] -> State [Token] [Token]
parse_params acc = do p          <- take_prm
                      (nxt:rest) <- get
                      if nxt == Comma then put rest >> parse_params (acc ++ [p])
                                      else (expect Semi) >> return (acc ++ [p])

parse_function :: State [Token] Function
parse_function = do expect KFunction
                    nm <- take_idn
                    expect LParen >> expect RParen >> expect LBrace
                    stmts <- parse_statements []
                    expect RBrace
                    return $ Function nm stmts

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

main :: IO ()
main = getArgs >>= readFile . head >>= return . no_comments. tokenize >>= mapM_ (putStrLn . show)