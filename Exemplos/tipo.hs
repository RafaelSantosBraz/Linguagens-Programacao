type Pessoa = (String, Int, Char)

pessoa :: Int -> Pessoa
pessoa 1 = ("Aluno", 20, 'm')
pessoa 2 = ("Aluno2", 19, 'f')
pessoa 3 = ("Aluno3", 21, 'f')

nome :: (Pessoa) -> String
nome (a,_,_) = a

idade :: (Pessoa) -> Int
idade (_,b,_) = b

sexo :: (Pessoa) -> Char
sexo (_,_,c) = c

lista_nomes :: Int -> String
lista_nomes x
 | x == 0 = "nenhum nome"
 | x == 1 = nome(pessoa 1)
 | otherwise = nome(pessoa x) ++ " " ++ lista_nomes(x-1)

comparaId :: Pessoa  -> Pessoa -> Pessoa
comparaId x y
 | idade (x) < idade (y) = x
 | otherwise = y

menoridade :: Int -> Pessoa
menoridade x
 | x == 1 = (pessoa 1)
 | otherwise = comparaId (pessoa x) (menoridade(x - 1))