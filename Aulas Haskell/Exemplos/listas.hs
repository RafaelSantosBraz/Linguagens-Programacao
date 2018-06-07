tamanho::[t]->Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

invert_lista::[t]->[t]
invert_lista [] = []
invert_lista (x:xs) = invert_lista xs ++ [x]

pertence::[Int]->Int->Bool
pertence [] _ = False
pertence (x:xs) z |x==z = True
 |otherwise = pertence xs z

saopares::[Int]->Bool
saopares [] = True
saopares (x:xs) | (mod x 2 /= 0) = False
 |otherwise = saopares xs

numerospares::[Int]->[Int]
numerospares [] = []
numerospares (x:xs) | (saopares [x] == True) = [x] ++ numerospares xs
 |otherwise = numerospares xs