# variational-data-structures

This project is an experiment on automatically lifting analyses to the variational domain.
Given a data structure representing an AST and functions operating on this type,
our goal is to **automatically** transform the data structure and functions to handle
variability.

## Examples

We extract the data type and functions operating on this type.

```haskell
exprD :: DecsQ
exprD = [d|
  data Expr = Num Int
            | Add Expr Expr
          deriving (Show)
  |]

evalD :: DecsQ
evalD = [d|
  eval :: Expr -> Int
  eval (Num x) = x
  eval (Add x y) = (+) (eval x) (eval y)
  |]
```

We then apply some transformations and splice
the code so that we can use the automatically-generated
variational versions.

```haskell
$(liftType exprD)

$(liftFun evalD)
```

(This snippet is illustrative; the actual code
has some implementation details)

We can then use the automatically generated
functions and datatype.

```haskell
vast :: V Expr
vast =
  Obj
  $ Add
    (Add (Num 1) (Add (Num 2) (Num 3)))
    (VExpr $
      Chc "A"
          (Obj (Add (Add (Num 2) (Num 4)) (Num 6)))
          (Obj (Add (Add (Num 3) (Num 5)) (Num 7))))

veval vast
```

For details on how this is done, check out `src/Metalift.hs`

## Related Work

- [Choice Calculus](https://dl.acm.org/doi/10.1145/2063239.2063245)
- [Variational Data Structures](https://dl.acm.org/doi/10.1145/2661136.2661143)
- [Automatic and Efficient Variability-Aware Lifting](https://dl.acm.org/doi/pdf/10.1145/3428225)
