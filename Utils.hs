module Utils where

prepend :: t -> t -> [t]
prepend c x = [c, x]

append :: t -> t -> [t]
append c x = [x, c]

indiceList :: (Enum a, Num a) => [b] -> [(a, b)]
indiceList l =
  zip [1 ..] l

every :: (Enum b, Num b) => (b -> Bool) -> [b1] -> [b1]
every p l =
  map snd (filter (\x -> (p . fst) x) (indiceList l))

mapOddEven :: (b1 -> b) -> (b1 -> b) -> [b1] -> [b]
mapOddEven evenf oddf l =
  map (\x -> let num = fst x
                 elem = snd x
             in if even num
                then evenf elem
                else oddf elem) (indiceList l)

replaceNth :: [a] -> Int -> a -> [a]
replaceNth l n newElement =
  take n l ++ [newElement] ++ drop (n + 1) l
