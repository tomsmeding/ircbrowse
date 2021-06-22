module Data.List.Reverse where


newtype Reverse a = Reverse [a]

instance Semigroup (Reverse a) where
    Reverse a <> Reverse b = Reverse (b <> a)

instance Monoid (Reverse a) where
    mempty = Reverse []
