-- https://youtu.be/44Z2buIu9cE

data a = Nil |
         Cons a (List a)

data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr

data SExpr = Atom String |
             List [SExpr]

eval :: Expr -> Int
eval (Lit a) = a
eval (Add a b) = (eval a) + (eval b)
eval (Sub a b) = (eval a) - (eval b)

count :: Expr -> Int
count (Lit a) = 1
count (Add a b) = (count a) + (count b) + 1
count (Sub a b) = (count a) + (count b) + 1

data ExprF = LitF Int |
             AddF a a |
             SubF a a

count_alg :: ExprF -> Int
count_alg (LitF a) = 1
count_alg (AddF a b) = a + b + 1
count_alg (SubF a b) = a + b + 1

cata_expr :: ((ExprF -> a) -> a) -> Expr -> a
cata_expr alg (LitF a) = alg (LitF a)
cata_expr alg (AddF a b) = alg (AddF (cata_expr alg a) (cata_expr alg b))
cata_expr alg (SubF a b) = alg (SubF (cata_expr alg a) (cata_expr alg b))

newtype Fix f = In ( f (Fix f))
type Expr = Fix ExprF

unFix (In x) = x


{-
cata_expr :: ((ExprF -> a) -> a) -> Expr -> a
cata_expr alg (In (LitF a)) = alg (LitF a)
cata_expr alg (In (AddF a b)) = alg (AddF (cata_expr alg a) (cata_expr alg b))
cata_expr alg (In (SubF a b)) = alg (SubF (cata_expr alg a) (cata_expr alg b))
-}

instance Functor ExprF where
    fmap f (LitF a) = LitF a
    fmap f (AddF a b) = AddF (f a) (f b)
    fmap f (SubF a b) = SubF (f a) (f b)

cata alg (In f) = alg (fmap (cata alg) f)
cata alg = alg.(fmap (cata alg)).unFix
