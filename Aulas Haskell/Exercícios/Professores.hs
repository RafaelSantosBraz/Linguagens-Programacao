base::Int->(Int, String, String, Char)
base x
 |x==1 = (1793, "Pedro Paulo", "MESTRE", 'M')
 |x==2 = (1797, "Joana Silva Alencar", "MESTRE", 'F')
 |x==3 = (1534, "João de Medeiros", "DOUTOR", 'M')
 |x==4 = (1267, "Cláudio César de Sá", "DOUTOR", 'M')
 |x==5 = (1737, "Paula de Medeiros", "MESTRE", 'F')
 |x==6 = (1888, "Rita de Matos", "MESTRE", 'F')
 |x==7 = (1698, "Tereza Cristina Andrade", "MESTRE", 'F')

returnNome::Int->String
returnNome x = pegaNome(base x)

pegaNome::(Int, String, String, Char)->String
pegaNome(_,b,_,_) = b

returnMatricula::Int->Int
returnMatricula x = pegaMatricula(base x)

pegaMatricula::(Int, String, String, Char)->Int
pegaMatricula(a,_,_,_) = a

returnTitulo::Int->String
returnTitulo x = pegaTitulo(base x)

pegaTitulo::(Int, String, String, Char)->String
pegaTitulo(_,_,c,_) = c

returnSexo::Int->Char
returnSexo x = pegaSexo(base x)

pegaSexo::(Int, String, String, Char)->Char
pegaSexo(_,_,_,d) = d

numDoutor::Int->Int
numDoutor x
 |x == 1 = contaDoutor(returnTitulo(1))
 |otherwise = contaDoutor(returnTitulo(x)) + numDoutor(x-1)

contaDoutor::String->Int
contaDoutor x
 |x == "DOUTOR" = 1
 |otherwise = 0

numMulheres::Int->Int
numMulheres x
 |x == 1 = contaMulheres(returnSexo(1))
 |otherwise = contaMulheres(returnSexo(x)) + numMulheres(x-1)

contaMulheres::Char->Int
contaMulheres x
 |x == 'F' = 1
 |otherwise = 0

numMestresMasculino::Int->Int
numMestresMasculino x
 |x == 1 = contaMestresMasculino(base 1)
 |otherwise = contaMestresMasculino(base x) + numMestresMasculino(x-1)

contaMestresMasculino::(Int, String, String, Char)->Int
contaMestresMasculino(_,_,c,d)
 |c == "MESTRE" && d == 'M' = 1
 |otherwise = 0

returnNomeProfMaisAnt::Int->String
returnNomeProfMaisAnt x
 |x == 1 = returnNome(1)
 |otherwise compMat((base x)(base (x-1)))

compMat::(Int, String, String, Char)->(Int, String, String, Char)->(Int, String, String, Char)
