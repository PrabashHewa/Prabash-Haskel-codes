module Eq3 (Eq3((===))) where

import Bool3

class Eq3 a where
    (===) :: a -> a -> Bool3

instance Eq3 Bool3 where
    (===) x y
        | x == True3 && y == True3 = True3
        | x == False3 && y == False3 = True3
        | x == Unk3 || y == Unk3 = Unk3
        | otherwise = False3

instance Eq3 a => Eq3 (Maybe a) where
    (===) Nothing Nothing = Unk3
    (===) (Just x) (Just y) = x === y
    (===) _ _ = Unk3
