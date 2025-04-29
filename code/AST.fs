namespace AST

type Expr =
    | Number of float
    | Seq of Expr list

// A program is a sequence of expressions

type Program =
    Expr list