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
              otherwise -> sair i

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

-- exibe a raça do cachorro do menor ranking
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
menor_ranking (a:b) 
                  |(read(ranking a)::Int) < (read(menor_ranking b)::Int) = ranking a
                  |otherwise = menor_ranking b
 
-- sai do programa
sair :: Char -> IO()
sair i
      |i == 'I' = putStrLn "Saindo do sistema..."
      |otherwise= putStrLn "Opcao invalida..."