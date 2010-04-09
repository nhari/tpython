module TPython.Parser where

import Language.Python.Common
import Language.Python.Version3.Parser
import Data.Char(isSpace)
import qualified Data.Map as M
import Control.Arrow
import Control.Monad
import System

type Identifier = String
type TypeAnnotation = (Identifier, Type)

data Type = T deriving (Eq, Read, Show, Ord)

-- PARSING

annotTable :: [Token] -> M.Map (Int, Identifier) Type
annotTable [] = M.empty
annotTable (c:cs) = let annots = parseComment $ token_literal c
                        linenum = span_row $ token_span c
                    in foldr (\(i,t) -> M.insert (linenum, i) t) (annotTable cs) annots

parseComment :: String -> [TypeAnnotation]
parseComment ('#':':':ss) = let nospaces = filter (not . isSpace) ss
                                annots = tokenize (==',') nospaces
                            in map (\a -> let (x,y) = span (/=':') a in (x,read $ tail y))
                                   annots
parseComment _ = []

tokenize    :: (Char -> Bool) -> String -> [String]
tokenize p s = case dropWhile p s of
                  "" -> []
                  s' -> t : tokenize p s''
                      where (t, s'') = break p s'

-- TYPECHECKING

type SymTable = M.Map Identifier Type
data Environment = Env {local :: SymTable,
                        global :: SymTable,
                        table :: M.Map (Int, Identifier) Type} deriving (Eq, Ord, Show)

extend :: [(Identifier, Type)] -> Environment -> Environment
extend tas (Env local global table) = if M.null local
                                      then Env local (foldr (uncurry M.insert) global tas) table
                                      else Env (foldr (uncurry M.insert) local tas) global table

extendLocal :: [(Identifier, Type)] -> Environment -> Environment
extendLocal tas (Env local global t) = Env (foldr (uncurry M.insert) local tas) global t

builtins = M.fromList [("range", T), ("print", T)]


--typeStmt :: Environment -> Statement -> Environment
typeStmt e (While cnd bdy els _) = if typeExpr e cnd == T 
                                   then let e' = typeSuite e bdy
                                        in typeSuite e els `seq` e'
                                   else e
typeStmt e (For trg gen bdy els ann) = if all ((== T) . typeExpr e) (gen:trg)
                                       then let linenum = span_row $ token_span ann
                                                names = map (ident_string . var_ident) trg
                                                Just types = mapM (\t -> M.lookup (linenum, t) (table e)) names
                                                e' = extendLocal (zip names types) e
                                                e'' = typeSuite e' bdy
                                            in typeSuite e' els `seq` e''
                                       else e
typeStmt e (Fun name args res bdy _) = let params = map (ident_string . param_name &&& const T) $ 
                                                    filter (\p -> case p of
                                                                    EndPositional _ -> False
                                                                    _             -> True) args
                                           fnType = T
                                           tas = (ident_string name, fnType) : params
                                           e' = extendLocal tas e
                                           bodyT = typeSuite e' bdy
                                       in bodyT `seq` extend [(ident_string name, fnType)] e
typeStmt e (Assign to expr ann) = let tbl = table e
                                      linenum = span_row $ token_span ann
                                      name = var_ident $ head to
                                      annot = M.lookup (linenum, ident_string name) tbl
                                      e' = case annot of
                                             Just t  -> extend [(ident_string name, t)] e
                                             Nothing -> e
                                  in if typeExpr e' (head to) == typeExpr e expr
                                     then e'
                                     else error $ "Mismatched assignment on line " ++ show linenum
typeStmt e (StmtExpr expr _) = typeExpr e expr `seq` e
typeStmt e (Global vars _) = let res = mapM (\v -> M.lookup (ident_string v) (global e)) vars
                                 e' = case res of 
                                        Just types -> extendLocal (zip (map ident_string vars) types) e
                                        Nothing    -> error "Global variable not found"
                             in e'
typeStmt e _ = e

typeSuite e stmts = foldr (flip typeStmt) e stmts

typeExpr e (Var v ann) = case M.lookup (ident_string v) (local e) of
                         Just t  -> T
                         Nothing -> case M.lookup (ident_string v) (global e) of
                                      Just t  -> T
                                      Nothing -> case M.lookup (ident_string v) builtins of
                                                   Just t  -> T
                                                   Nothing -> error $ "Identifier \'" ++ ident_string v ++ "\' not found on line " ++ 
                                                                       show (span_row $ token_span ann)
typeExpr e (Call fn args _) = head $ map (typeExpr e) (fn : (map arg_expr args))
typeExpr e (SlicedExpr s _ _) = typeExpr e s
typeExpr e (CondExpr t c f _) = typeExpr e t `seq` typeExpr e c `seq` typeExpr e f
typeExpr e (BinaryOp op arg1 arg2 _) = typeExpr e arg1 `seq` typeExpr e arg2
typeExpr e (UnaryOp op arg _) = typeExpr e arg
typeExpr e (Lambda args bdy _) = let params = map (ident_string . param_name &&& const T) $ 
                                                    filter (\p -> case p of
                                                                    EndPositional  _ -> False
                                                                    _             -> True) args
                                     e' = extendLocal params e
                                 in typeExpr e' bdy
typeExpr e (Tuple es _) = last $ map (typeExpr e) es
typeExpr e (List es _) = last $ map (typeExpr e) es
typeExpr e (Set es _) = last $ map (typeExpr e) es
typeExpr e (Dictionary es _) = last $ map (typeExpr e . fst) es





-- RUNTIME

main = do args <- System.getArgs
          unless (length args == 1) (putStrLn "Wrong number of arguments" >> System.exitFailure)
          let filename = head args
          fileText <- readFile filename
          let (ast, comments) = case parseModule fileText filename of
                                  Left err -> error $ prettyText err
                                  Right res -> res
          let table = annotTable comments
          return ()