base::Int->(Int, String, String, Char)
base x
 |x==0 = (1793, "Pedro Paulo", "MESTRE", 'M')
 |x==1 = (1797, "Joana Silva Alencar", "MESTRE", 'F')
 |x==2 = (1534, "João de Medeiros", "DOUTOR", 'M')
 |x==3 = (1267, "Cláudio César de Sá", "DOUTOR", 'M')
 |x==4 = (1737, "Paula de Medeiros", "MESTRE", 'F')
 |x==5 = (1888, "Rita de Matos", "MESTRE", 'F')
 |x==6 = (1698, "Tereza Cristina Andrade", "MESTRE", 'F')

returnNome::Int->String
returnNome x = pegaNome(base x)

pegaNome::(Int, String, String, Char)->String
pegaNome(_,n,_,_) = n

returnMatricula::Int->Int
returnMatricula x = pegaMatricula(base x)

pegaMatricula::(Int, String, String, Char)->Int
pegaMatricula(m,_,_,_)=m

returnTitulo::Int->String
returnTitulo x = pegaTitulo(base x)

pegaTitulo::(Int, String, String, Char)->String
pegaTitulo(_,_,t,_) = t

returnSexo::Int->Char
returnSexo x = pegaSexo(base x)

pegaSexo::(Int, String, String, Char)->Char
pegaSexo(_,_,_,s) = s

numDoutor::Int->Int
numDoutor x
 |x == 1 = contaDoutor(returnTitulo(0))
 |otherwise = contaDoutor(returnTitulo(x)) + numDoutor(x-1)
 
contaDoutor::String->Int
contaDoutor x
 |x == "DOUTOR" = 1
 |otherwise=0
 
numMulheres::Int->Int
numMulheres x
 |x == 1 = contaMulheres(returnSexo(0))
 |otherwise = contaMulheres(returnSexo(x)) + numMulheres(x-1)
 
contaMulheres::Char->Int
contaMulheres x
 |x == 'F' = 1
 |otherwise = 0

mestreMasc::Int->Int
mestreMasc x
 |x == 1 = contaMestreMasc(0)
 |otherwise = contaMestreMasc(x) + mestreMasc(x-1)

contaMestreMasc::Int->Int
contaMestreMasc x
 |returnSexo(x) == 'M' && returnTitulo(x)== "MESTRE" = 1
 |otherwise = 0

maisAntigo::Int->(Int, String, String, Char)
maisAntigo x
 |x==1= base 0
 |otherwise = comparaMat(base x)(maisAntigo(x-1))

comparaMat::(Int, String, String, Char)->(Int, String, String, Char)->(Int, String, String, Char)
comparaMat x y
 |x < y = x
 |otherwise = y

professorAntigo::Int->String
professorAntigo x = pegaNome(maisAntigo(x))