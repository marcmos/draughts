module Utils where

prepend :: t -> t -> [t]
prepend c x = [c, x]

append :: t -> t -> [t]
append c x = [x, c]

indiceList :: [a] -> [(Int, a)]
indiceList l =
  zip [1 ..] l

every :: (Int -> Bool) -> [b] -> [b]
every p l =
  map snd (filter (\x -> (p . fst) x) (indiceList l))

mapOddEven :: (a -> b) -> (a -> b) -> [a] -> [b]
mapOddEven evenf oddf l =
  map (\x -> let num = fst x
                 element = snd x
             in if even num
                then evenf element
                else oddf element) (indiceList l)

replaceNth :: Int -> [a] -> a -> [a]
replaceNth n l newElement =
  take n l ++ [newElement] ++ drop (n + 1) l
