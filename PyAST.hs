module TPython.PyAST where

import Language.Python.Common
import Language.Python.Version3.Parser
import Data.Char(isSpace)
import qualified Data.Map as M

type Identifier = String
type TypeAnnotation = (Identifier, Type)

data Type = T deriving (Eq, Read, Show)

data PyExpr annot
    = Var  {
        var_ident :: Ident annot
        pyexpr_annot :: annot
      }
    | Literal {
        expr_literal :: String
        literal_type :: Type
        pyexpr_annot :: annot
      }
    | Call {
        call_fun :: Expr annot
        call_args :: [Argument annot]
        expr_annot :: annot
      }
    | Subscript {
        subscriptee :: Expr annot
        subscript_exprs :: [Expr annot]
        expr_annot :: annot
      }
    | SlicedExpr {
        slicee :: Expr annot
        slices :: [Slice annot]
        expr_annot :: annot
      }
    | CondExpr {
        ce_true_branch :: Expr annot
        ce_condition :: Expr annot
        ce_false_branch :: Expr annot
        expr_annot :: annot
      }
    | BinaryOp {
        operator :: Op annot
        left_op_arg :: Expr annot
        right_op_arg :: Expr annot
        expr_annot :: annot
      }
    | UnaryOp {
        operator :: Op annot
        op_arg :: Expr annot
        expr_annot :: annot
      }
    | Lambda {
        lambda_args :: [Parameter annot]
        lambda_body :: Expr annot
        expr_annot :: annot
      }
    | Tuple {
        tuple_exprs :: [Expr annot]
        expr_annot :: annot
      }
    | Yield {
        yield_expr :: Maybe (Expr annot)
        expr_annot :: annot
      }
    | Generator {
        gen_comprehension :: Comprehension (Expr annot) annot
        expr_annot :: annot
      }
    | ListComp {
        list_comprehension :: Comprehension (Expr annot) annot
        expr_annot :: annot
      }
    | List {
        list_exprs :: [Expr annot]
        expr_annot :: annot
      }
    | Dictionary {
        dict_mappings :: [(Expr annot, Expr annot)]
        expr_annot :: annot
      }
    | DictComp {
        dict_comprehension :: Comprehension (Expr annot, Expr annot) annot
        expr_annot :: annot
      }
    | Set {
        set_exprs :: [Expr annot]
        expr_annot :: annot
      }
    | SetComp {
        set_comprehension :: Comprehension (Expr annot) annot
        expr_annot :: annot
      }
    | Starred {
        starred_expr :: Expr annot
        expr_annot :: annot
      }
    | Paren {
        paren_expr :: Expr annot
        expr_annot :: annot
      }
    | StringConversion {
        backquoted_expr :: Expr annot
        expr_anot :: annot
      }