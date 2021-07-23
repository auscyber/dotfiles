import Data.Foldable

data List a = Cons a (List a) | Nil deriving (Ord, Eq)

instance Functor List where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
    fmap f Nil = Nil

instance Semigroup (List a) where
    Cons x xs <> ys = Cons x (xs <> ys)
    Nil <> ys = ys

instance Monoid (List a) where
    mempty = Nil

instance Foldable List where
    foldMap f (Cons x xs) = f x <> foldMap f xs
    foldMap f Nil = mempty

fromList :: [a] -> List a
fromList = foldr Cons Nil

instance Show a => Show (List a) where
    show (Cons x xs) = "[|" ++ show x ++ foldr (\y acc -> "," ++ show y <> acc) [] xs ++ "|]"
