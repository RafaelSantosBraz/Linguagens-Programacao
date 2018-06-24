module Cadastro where
-- Para função toUpper
import Data.Char
import System.Environment
-- Biblioteca para Classe IO
import System.IO
import System.IO.Error
import Control.Exception

-- exibe o menu de opções
menu :: IO()
menu = do
        putStrLn " "
        putStrLn "--------------------------------------------------"
        putStrLn "|                                                |"
        putStrLn "|              CADASTRO DE CACHORROS             |"
        putStrLn "|                                                |"
        putStrLn "|        a - Inserir                             |"
        putStrLn "|        b - Excluir por raça                    |"
        putStrLn "|        c - Listar todos                        |"
        putStrLn "|        d - Raça melhor colocada no ranking     |"
        putStrLn "|        e - Exibir menor preço mínimo           |"
        putStrLn "|        f - Exibir média dos preços mínimos     |"
        putStrLn "|        g - Exibir raças por origem             |"
        putStrLn "|        h - Exibir raças entre posições         |"
        putStrLn "|        i - Exibir por função e peso            |"
        putStrLn "|        j - Exibir raças pelo tamanho           |"
        putStrLn "|        k - Exibir quantia por tamanho          |"
        putStrLn "|        l - Sair do Sistema                     |"
        putStrLn "--------------------------------------------------"
        putStrLn " "
        putStrLn "Digite uma opção"
        le_opcao

-- faz a leitura da opção digitada no menu
le_opcao :: IO()
le_opcao = do
            opcao <- getChar
            putStrLn " "
            f_menu (toUpper(opcao))

-- redireciona para a função correta baseando-se na opção digitada
f_menu :: Char -> IO()
f_menu i = 
            case i of
              'A' ->  insere_cadastro
              'B' ->  excluir_um_cadastro   
              'C' ->  imprime_cadastros   
              'D' ->  imprime_ranking
              'E' ->  imprime_preco_minimo
              'F' ->  imprime_media_preco
              'G' ->  imprime_por_origem
              'H' ->  imprime_entre_posicoes
              'I' ->  imprime_por_funcao_peso
              'J' ->  imprime_por_peso
              otherwise -> sair i

-- sair do programa
sair :: Char -> IO()
sair i
      |i == 'I' = putStrLn "Saindo do sistema..."
      |otherwise= putStrLn "Opcao invalida..."

-- cadastra um novo cachorro
insere_cadastro :: IO()
insere_cadastro = do
                    -- apenas ignora o enter da função anterior
                    lixo <- getLine
                    -- necessário para fazer um getChar
                    putStrLn " "
                    putStrLn "Raça: "
                    raca <- getLine
                    putStrLn "Preço Mínimo: ";
                    preco <- getLine;
                    putStrLn "Origem: ";
                    origem <- getLine;
                    putStrLn "Tamanho Médio Macho: "    
                    tamanho <- getLine
                    putStrLn "Peso Médio Macho: "    
                    peso <- getLine
                    putStrLn "Cor: "   
                    cor <- getLine
                    putStrLn "Função Original: "   
                    funcao <- getLine
                    putStrLn "Ranking: "   
                    posicao <- getLine
                    -- cria uma variável local String para juntar todas as partes do "registro"
                    let cadastro = raca ++ "," ++ preco ++ "," ++ origem ++ "," ++ tamanho ++ "," ++ peso ++ "," ++ cor ++ "," ++ funcao ++ "," ++ posicao
                    -- escreve mais uma linha no arquivo, com um fim de linha
                    appendFile "dados.csv" (cadastro ++ "\n")

-- faz a exclusão de um cachorro por raça
excluir_um_cadastro :: IO()
excluir_um_cadastro = do
                        -- ignora o enter da função anterior
                        lixo <- getLine
                        putStrLn "O cadastro será apagado pela raça"
                        putStrLn "Digite a raça desejada:"
                        raca <- getLine
                        arquivo <- abreArquivo "dados.csv" ReadMode
                        conteudo <- (hGetContents arquivo)
                        cadastro <- (converteConteudo (conteudo))
                        -- atualiza o conteúdo do arqui em uma variável local, sem o registro selecionado
                        let novo_conteudo = apaga_pelo_nome cadastro raca
                        arquivobkp <- abreArquivo "auxiliar.txt" WriteMode
                        hPutStr arquivobkp novo_conteudo
                        fechaArquivo arquivobkp
                        fechaArquivo arquivo
                        copiar "auxiliar.txt" "dados.csv"

-- necessária para tratar a possibilidade de o arquivo estar indisponível
abreArquivo :: String -> IOMode -> IO Handle
abreArquivo arquivo modo = do
                            {catch (abrir) trata_erro}
                            where
                            abrir = do
                                      {
                                      conteudo <- openFile arquivo modo;
                                      return conteudo;
                                      }
                            trata_erro erro = if isDoesNotExistError erro 
                            then do
                                  {
                                  putStr "Arquivo não existe";
                                  putStrLn "Será criado automaticamente um arquivo em branco";
                                  conteudo <- openFile "dados.csv" WriteMode;
                                  fechaArquivo conteudo;
                                  abreArquivo "dados.csv" ReadMode
                                  }
                            else ioError erro

-- converte o conteúdo do arquivo em uma estrutura de String, sem "\n" e sem ","
converteConteudo :: String -> IO [[String]]
converteConteudo conteudo = return (map (explodir ',') (explodir '\n' conteudo))

-- faz a separação como um "split" por um Char
explodir :: Eq a => a -> [a] -> [[a]]
explodir a [] = []
explodir a(x:xs)
                |(takeWhile (/=a) (x:xs)) == [] = explodir a xs
                |x == a = (takeWhile (/=a) xs) : explodir a (dropWhile (/= a) xs)
                |otherwise = (takeWhile (/= a)(x:xs)) : explodir a (dropWhile (/=a) (x:xs))

-- retira da estrutura de String a "linha" que possui a raça indicada
apaga_pelo_nome :: [[String]] -> String -> String
apaga_pelo_nome [] nm = "\n"
apaga_pelo_nome (x:xs) nm
                        |nm == (raca x) = (apaga_pelo_nome xs nm)
                        |otherwise = (foldl1 (\a b->a++","++b) x) ++ "\n" ++ (apaga_pelo_nome xs nm)

-- retorna uma string dentro da lista
raca, preco, origem, tamanho, peso, cor, funcao, ranking :: [String] -> String
raca(a:b:c:d:e:f:g:h:[]) = a
preco(a:b:c:d:e:f:g:h:[]) = b
origem(a:b:c:d:e:f:g:h:[]) = c
tamanho(a:b:c:d:e:f:g:h:[]) = d
peso(a:b:c:d:e:f:g:h:[]) = e
cor(a:b:c:d:e:f:g:h:[]) = f
funcao(a:b:c:d:e:f:g:h:[]) = g
ranking(a:b:c:d:e:f:g:h:[]) = h

-- copia o conteúdo de um arquivo para outro
copiar origem destino = do
                          arquivoO <- abreArquivo origem ReadMode
                          conteudo <- (hGetContents arquivoO)
                          arquivoD <- abreArquivo destino WriteMode
                          hPutStr arquivoD conteudo
                          fechaArquivo arquivoD
                          fechaArquivo arquivoO

-- fecha o arquivo (encerra o uso)
fechaArquivo :: Handle -> IO()
fechaArquivo handle_arq = hClose handle_arq

-- faz a impressão de todos os cachorros cadastrados
imprime_cadastros :: IO()
imprime_cadastros = do
                      putStrLn " "
                      putStrLn "---------------------------------------------------"
                      arq <- (openFile "dados.csv" ReadMode)
                      conteudo <- (hGetContents arq)
                      cadastro <- (converteConteudo (conteudo))
                      imprime cadastro
                      fechaArquivo arq
                      putStrLn "---------------------------------------------------"

-- recebe todo o conteúdo do arquivo e o exibe
imprime :: [[String]] -> IO()
imprime [] = putStrLn " "
imprime (x:xs) = do
                  putStrLn (foldl1 (\a b->a++ " " ++b) x)
                  imprime xs

-- verifica qual é a raça com melhor Ranking
imprime_ranking :: IO()
imprime_ranking = do    
                    arq <- (openFile "dados.csv" ReadMode)
                    conteudo <- (hGetContents arq)
                    cadastro <- (converteConteudo (conteudo))
                    imprime_ranking_aux cadastro (menor_ranking cadastro)
                    fechaArquivo arq

-- exibe o cachorro do menor ranking
imprime_ranking_aux :: [[String]] -> String -> IO()
imprime_ranking_aux [] menor = putStrLn " "
imprime_ranking_aux (a:b) menor
                              |(read(ranking a)::Int) <= (read(menor)::Int) = do
                                                                                {
                                                                                putStrLn(foldl1 (\a b->a++" "++b) a);
                                                                                imprime_ranking_aux b menor;
                                                                                }
                              |otherwise = imprime_ranking_aux b menor

-- retorna qual o menor ranking no conteúdo passado
menor_ranking :: [[String]] -> String
menor_ranking [] = "1000"
menor_ranking (a:b) = do
                        -- tive que criar uma variável auxiliar para cada execução, senão seria exponencial
                        let menor = menor_ranking b
                        if (read(ranking a)::Int) < (read(menor)::Int)
                        then ranking a
                        else menor

-- verifica qual é o cachorro com menor preço mínimo      
imprime_preco_minimo :: IO()
imprime_preco_minimo = do    
                        arq <- (openFile "dados.csv" ReadMode)
                        conteudo <- (hGetContents arq)
                        cadastro <- (converteConteudo (conteudo))
                        imprime_preco_minimo_aux cadastro (menor_preco_minimo cadastro)
                        fechaArquivo arq

-- exibe o cachorro que possui o menor preço mínimo  
imprime_preco_minimo_aux :: [[String]] -> String -> IO()
imprime_preco_minimo_aux [] menor = putStrLn " "
imprime_preco_minimo_aux (a:b) menor
                              |(read(preco a)::Int) <= (read(menor)::Int) = do
                                                                                {
                                                                                putStrLn(foldl1 (\a b->a++" "++b) a);
                                                                                imprime_preco_minimo_aux b menor;
                                                                                }
                              |otherwise = imprime_preco_minimo_aux b menor

-- retorna qual o menor preço mínimo no conteúdo passado
menor_preco_minimo :: [[String]] -> String
menor_preco_minimo [] = "10000"
menor_preco_minimo (a:b) = do
                        -- tive que criar uma variável auxiliar para cada execução, senão seria exponencial
                        let menor = menor_preco_minimo b
                        if (read(preco a)::Int) < (read(menor)::Int)
                        then preco a
                        else menor

-- verifica qual é a média dos preços mínimos
imprime_media_preco :: IO()
imprime_media_preco = do
                        arquivo <- abreArquivo "dados.csv" ReadMode
                        conteudo <- (hGetContents arquivo)
                        cadastro <- (converteConteudo (conteudo))
                        putStrLn(show (media_precos cadastro))
                        fechaArquivo arquivo

-- faz a divisão dos preços cadastrados
media_precos :: [[String]] -> Float
media_precos [] = 0
media_precos x = (soma_precos x) / fromIntegral(length x)

-- soma todos os preços mínimos
soma_precos :: [[String]] -> Float
soma_precos [] = 0
soma_precos (x:xs) = (read(preco x)::Float) + (soma_precos xs)

-- exibe cachorro por origem
imprime_por_origem :: IO()
imprime_por_origem = do
                      -- ignora o enter da função anterior
                      lixo <- getLine
                      putStr ""
                      putStr "Digite a origem do cachorro: "
                      origem <- getLine
                      -- faz uma busca no cadastro a origem informada
                      buscageral filtrar_origem origem

-- faz a chamada da função específica de busca com o valor informado
buscageral :: ([[String]] -> a -> IO b) -> a -> IO()
buscageral funcao filtro = do
                            putStrLn " "
                            putStrLn "-------------------------------------------------------------"
                            arquivo <- abreArquivo "dados.csv" ReadMode
                            conteudo <- (hGetContents arquivo)
                            cadastro <- (converteConteudo (conteudo))
                            -- usa o recurso de passagem de função por parâmetro para executar o correto
                            funcao cadastro filtro
                            fechaArquivo arquivo
                            putStrLn "-------------------------------------------------------------"

-- função-parâmetro para encontrar os cadastros da referida origem
filtrar_origem :: [[String]] -> String -> IO()
filtrar_origem [] nm = putStrLn ""
filtrar_origem (x:xs) nm
                      |(origem x) == nm = do
                                            putStrLn(foldl1 (\a b->a++" "++b) x)
                                            filtrar_origem xs nm
                      |otherwise = filtrar_origem xs nm

-- verifica quais as raças que estão entre as posições do Ranking
imprime_entre_posicoes :: IO()
imprime_entre_posicoes = do    
                          arq <- (openFile "dados.csv" ReadMode)
                          conteudo <- (hGetContents arq)
                          cadastro <- (converteConteudo (conteudo))
                          -- ignora o enter da função anterior
                          lixo <- getLine
                          putStr ""
                          putStr "Digite a primeira posição (mais baixa): "
                          p1 <- getLine
                          putStr "Digite a segunda posição (mais alta): "
                          p2 <- getLine
                          putStrLn " "
                          putStrLn "-------------------------------------------------------------"
                          verifica_posicao cadastro p1 p2
                          putStrLn "-------------------------------------------------------------"
                          fechaArquivo arq

-- compara as posições para imprimir na tela
verifica_posicao :: [[String]] -> String -> String -> IO()
verifica_posicao [] p1 p2 = putStr ""
verifica_posicao (x:xs) p1 p2 = do
                                  if ((read(ranking x)::Int) <= (read(p1)::Int) && (read(ranking x)::Int) >= (read(p2)::Int))
                                  then do
                                        putStrLn (foldl1 (\a b->a++ " " ++b) x)
                                        verifica_posicao xs p1 p2
                                  else verifica_posicao xs p1 p2

-- exibe cachorro por função original e peso médio
imprime_por_funcao_peso :: IO()
imprime_por_funcao_peso = do
                            -- ignora o enter da função anterior
                            lixo <- getLine
                            putStr ""
                            putStr "Digite a função original do cachorro: "
                            funcao <- getLine
                            putStr "Digite o peso médio do cachorro: "
                            peso <- getLine
                            buscageral_2_argumentos filtrar_funcao_peso funcao peso

-- faz a chamada da função específica de busca com os valores informados
buscageral_2_argumentos :: ([[String]] -> a -> b -> IO c) -> a -> b -> IO()
buscageral_2_argumentos funcao filtro1 filtro2 = do
                                                  putStrLn " "
                                                  putStrLn "-------------------------------------------------------------"
                                                  arquivo <- abreArquivo "dados.csv" ReadMode
                                                  conteudo <- (hGetContents arquivo)
                                                  cadastro <- (converteConteudo (conteudo))
                                                  -- usa o recurso de passagem de função por parâmetro para executar o correto
                                                  funcao cadastro filtro1 filtro2
                                                  fechaArquivo arquivo
                                                  putStrLn "-------------------------------------------------------------"

-- função-parâmetro para encontrar os cadastros com a função e o peso
filtrar_funcao_peso :: [[String]] -> String -> String -> IO()
filtrar_funcao_peso [] f p = putStrLn " "
filtrar_funcao_peso (x:xs) f p
                              |((funcao x) == f && (peso x) == p) = do
                                                                      putStrLn(foldl1 (\a b->a++" "++b) x)
                                                                      filtrar_funcao_peso xs f p
                              |otherwise = filtrar_funcao_peso xs f p

-- exibe cachorro por origem
imprime_por_peso :: IO()
imprime_por_peso = do
                    -- ignora o enter da função anterior
                    lixo <- getLine
                    putStr ""
                    putStr "Digite o peso médio do cachorro: "
                    peso <- getLine
                    buscageral filtrar_peso peso

-- função-parâmetro para encontrar os cadastros do referido peso
filtrar_peso :: [[String]] -> String -> IO()
filtrar_peso [] nm = putStrLn " "
filtrar_peso (x:xs) nm
                      |(peso x) == nm = do
                                            putStrLn(foldl1 (\a b->a++" "++b) x)
                                            filtrar_peso xs nm
                      |otherwise = filtrar_peso xs nm