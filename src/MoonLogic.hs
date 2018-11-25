{-|
Module     : MoonLogic
Description: Implements "Dismal Arithmetic"
License    : MIT
Maintainer : Ryan McNamara <gn341ram@gmail.com>
Portability: Linux

For more information on dismal arithmetic, see:
    https://www.numberphile.com/videos/lunar-arithmetic
    https://arxiv.org/abs/1107.1130

I'm  using  dismal  /  moon  arithmetic  because  a  single  hash
algorithm of a  document is a dismal or moon  number, the minhash
of two moon numbers is moon  addition and what we ultimately want
to do is concat a document's moon numbers.
|-}

module MoonLogic where

newtype MoonInt = MoonInt [Int]

-- |Find the minimum of each component of an array of ints.
minwise :: MoonInt -> MoonInt -> MoonInt
minwise (MoonInt a) (MoonInt b) = MoonInt (zipWith min a b)

instance Semigroup MoonInt where
    a <> b = minwise a b

instance Monoid MoonInt where
    mempty  = MoonInt $ repeat maxBound
    mappend = minwise

digits :: MoonInt -> Int
digits (MoonInt xs) = length xs

fromMoonInt :: MoonInt -> [Int]
fromMoonInt (MoonInt xs) = xs

jaccard :: MoonInt -> MoonInt -> Double
jaccard (MoonInt xs) (MoonInt ys) = numer / denom
    where similar = zipWith (==) xs ys
          numer   = fromIntegral . sum $ fromEnum <$> similar
          denom   = fromIntegral $ length xs
