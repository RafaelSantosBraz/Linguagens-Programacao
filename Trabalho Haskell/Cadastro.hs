module Cadastro where
import Data.Char  -- Para função toUpper
import System.Environment
import System.IO  -- Biblioteca para Classe IO
import System.IO.Error  -- 
import Control.Exception

-- =================Menu de Opções==================================================--
-- =================================================================================
menu :: IO()
menu = 
 do 
   putStrLn " "
   putStrLn "--------------------------------------------------"
   putStrLn "|                                                |"
   putStrLn "|              CADASTRO DE CACHORROS             |"
   putStrLn "|                                                |"
   putStrLn "|        a - Insere cadastro                     |"
   putStrLn "|        b - Imprime cadastro                    |"
   putStrLn "|        c - Busca por nomes                     |"
   putStrLn "|        d - Soma das idades                     |"
   putStrLn "|        e - Média das alturas                   |"
   putStrLn "|        f - Busca por sexo                      |"
   putStrLn "|        g - Excluir um cadastro                 |"
   putStrLn "|        h - Excluir todos cadastro              |"
   putStrLn "|        i - Sair do Sistema                     |"
   putStrLn "--------------------------------------------------"
   putStrLn " "
   putStrLn "Digite uma opção"
   le_opcao

 --Le as opção digitadas no menu --
le_opcao:: IO()
le_opcao =
          do {
              opcao <- getChar;
              putStrLn " "; -- linha necessária para execução do getChar
              f_menu (toUpper(opcao));
             }

--Redireciona para a função --
f_menu::Char -> IO()
f_menu i = 
          case i of
                   'A' ->  insere_cadastro
                   'B' ->  imprime_cadastros
                   'C' ->  busca_por_nomes
                   'D' ->  soma_idades
                   'E' ->  media_alturas
                   'F' ->  busca_por_sexo
                   'G' ->  excluir_um_cadastro
                   'H' ->  excluir_tudo
                   otherwise -> sair i

 -- Funcao sair ----------
sair :: Char -> IO()
sair i |i == 'I' = putStrLn "Saindo do sistema..."
 |otherwise= putStrLn "Opcao invalida..."
 
 
 
--Inserir Cadastro --
-- ================ --
insere_cadastro::IO()
insere_cadastro = 
                 do
                   lixo <- getLine -- Variavel lixo para pegar o enter q vem da função anterior 
                   putStrLn " "
                   putStrLn "Nome: "
                   nm <- getLine
                   putStrLn "Idade: ";
                   id <- getLine;
                   putStrLn "Altura: ";
                   alt <- getLine;
                   putStrLn "Sexo: M-Masculino | F-Feminino "   
                   sex <- getChar
                   putStrLn " " -- linha necessária para execução do getChar
                   let cadastro = nm ++"#"++id++"#"++alt++"#'"++[(toUpper sex)]++"'"
                   appendFile "dados.txt" (cadastro ++ "\n") --insere dados no arquivo
 
 
 
---IMPRIMIR TODOS OS CADASTROS ----
-- ===========================
imprime_cadastros :: IO()
imprime_cadastros =
                   do
                     putStrLn " "
                     putStrLn "---------------------------------------------------"
                     arq <- (openFile "dados.txt" ReadMode) -- Abre o arquivo
                     conteudo <- (hGetContents arq)
                     cadastro <- (converteConteudo (conteudo))
                     imprime cadastro
                     hClose arq -- Fecha arquivo
                     putStrLn "---------------------------------------------------"
 
 
--- Busca por Nome 
--  ==============
busca_por_nomes :: IO()
busca_por_nomes =
                 do
                   lixo <- getLine -- Variavel lixo para pegar o enter q vem da função anterior 
                   putStr ""
                   putStr "Digite o nome desejado: "
                   nome <- getLine
                   buscageral achanome nome


-- Soma Idades
-- ===========
soma_idades :: IO()
soma_idades =
 do
 arquivo <- abreArquivo "dados.txt" ReadMode
 conteudo <- (hGetContents arquivo)
 cadastro <- (converteConteudo (conteudo))
 putStrLn(show (somaidades cadastro))
 fechaArquivo arquivo

--media_alturas
media_alturas :: IO()
media_alturas =
 do
 arquivo <- abreArquivo "dados.txt" ReadMode
 conteudo <- (hGetContents arquivo)
 cadastro <- (converteConteudo (conteudo))
 putStrLn(show (mediaAlturas cadastro))
 fechaArquivo arquivo 
 
 -- Busca por Sexo
busca_por_sexo :: IO()
busca_por_sexo =
                 do
                   lixo <- getLine -- Variavel lixo para pegar o enter q vem da função anterior 
                   putStr ""
                   putStr "Digite o sexo para pesquisa: "
                   sexo <- getChar
                   buscageral achasexo sexo

 
--Excluir CADASTRO
excluir_um_cadastro :: IO()
excluir_um_cadastro =
 do
 lixo <- getLine -- Variavel lixo para pegar o enter q vem da função anterior 
 putStrLn "O cadastro será apagado pelo nome"
 putStrLn "Digite o nome desejado:"
 nome <- getLine
 arquivo <- abreArquivo "dados.txt" ReadMode
 conteudo <- (hGetContents arquivo)
 cadastro <- (converteConteudo (conteudo))
 let novo_conteudo = apaga_pelo_nome cadastro nome
 arquivobkp <- abreArquivo "auxiliar.txt" WriteMode
 hPutStr arquivobkp novo_conteudo
 fechaArquivo arquivobkp
 fechaArquivo arquivo
 copiar "auxiliar.txt" "dados.txt"

--Excluir todo o CADASTRO
excluir_tudo::IO()
excluir_tudo =
 do
   lixo <- getLine -- Variavel lixo para pegar o enter q vem da função anterior 
   putStrLn "Tem certeza que deseja apagar todos os dados do sistema?(s/n)"
   resp <- getChar
   if ((toUpper resp) == 'N')
   then putStr ""
   else do
         {
          arquivo <- abreArquivo "dados.txt" WriteMode;
          fechaArquivo arquivo;
          putStrLn "Dados Apagados";
         }

-- ============================================FUNCAO Auxiliar EXCLUSÃO ==============
apaga_pelo_nome :: [[String]]->String->String
apaga_pelo_nome [] nm = "\n"
apaga_pelo_nome (x:xs) nm
 |nm == (nome x) = (apaga_pelo_nome xs nm)
 |otherwise = (foldl1 (\a b->a++"#"++b) x) ++ "\n" ++ (apaga_pelo_nome xs nm)

-- ============================== Copiar Origem e Destino =============================
copiar origem destino =
 do
 arquivoO <- abreArquivo origem ReadMode
 conteudo <- (hGetContents arquivoO)
 arquivoD <-  abreArquivo destino WriteMode
 hPutStr arquivoD conteudo
 fechaArquivo arquivoD
 fechaArquivo arquivoO
 
-- =============================================================================================== -  
-- FUNCOES AUXILIARES DE BUSCA-------------
-- ===============================================================================================
buscageral :: ([[String]] -> a -> IO b) -> a -> IO ()
buscageral funcao filtro =
                          do
                            putStrLn " "
                            putStrLn "--------------------------------------------"
                            arquivo <- abreArquivo "dados.txt" ReadMode
                            conteudo <- (hGetContents arquivo)
                            cadastro <- (converteConteudo (conteudo))
                            funcao cadastro filtro --Chama a "funcao" passada no parametro
                            fechaArquivo arquivo
                            putStrLn "-------------------------------------------------------------"

 ---Funcao q procura o nome -------------
achanome::[[String]]->String -> IO()
achanome []nm = putStrLn ""
achanome (x:xs) nm
                 |(nome x) == nm = do
                                    putStrLn(foldl1 (\a b->a++" "++b) x)
                                    achanome xs nm
                 |otherwise = achanome xs nm

 ---Funcao q procura o sexo -------------
achasexo::[[String]]->Char ->IO()
achasexo []sx = putStrLn ""
achasexo (x:xs) sx
                  |(sexo x) == ("'"++[(toUpper sx)]++"'") = do
                                                putStrLn(foldl1 (\a b->a++" "++b) x)
                                                achasexo xs sx
                  |otherwise = achasexo xs sx
 
--Funcao Auxiliar Consulta----------
converteConteudo :: String -> IO [[String]]
converteConteudo conteudo = return (map (explodir '#') (explodir '\n' conteudo))

--Funcao Auxiliar
explodir :: Eq a=> a -> [a] -> [[a]]
explodir a [] = []
explodir a(x:xs)
                |(takeWhile (/=a) (x:xs)) == [] = explodir a xs
                |x == a = (takeWhile (/=a) xs) : explodir a (dropWhile (/= a) xs)
                |otherwise = (takeWhile (/= a)(x:xs)) : explodir a (dropWhile (/=a) (x:xs))


--Retorna a posicao 
nome, idade, altura, sexo :: [String] -> String
nome (a:b:c:d:[]) = a
idade(a:b:c:d:[]) = b
altura(a:b:c:d:[]) = c
sexo(a:b:c:d:[]) = d
 
-- ========================================================================================= --
--Funcao Auxiliar de impressao
-- =========================================================================================
imprime :: [[String]] ->IO()
imprime [] = putStrLn ""
imprime (x:xs) = do
                    putStrLn (foldl1 (\a b->a++ " " ++b) x)
                    imprime xs

 
 
-- ==============================================================================
-- FUNÇÕES COM NUMEROS
-- ==============================================================================
--Soma idades
somaidades:: [[String]] -> Integer
somaidades [] = 0
somaidades (x:xs) = (read (idade x) :: Integer) + (somaidades xs)

--somaAlturas
somaAlturas::[[String]] -> Float
somaAlturas [] = 0
somaAlturas (x:xs) = (read (altura x) :: Float) + (somaAlturas xs)

--MediaAlturas
mediaAlturas::[[String]] -> Float
mediaAlturas [] = 0
mediaAlturas x = (somaAlturas x) / fromIntegral(length x)


----------------------------------------------------------------------------------
---- FUNCAO AUXILIARES DE ARQUIVO
----------------------------------------------------------------------------------
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
 conteudo <- openFile "dados.txt" WriteMode;
 fechaArquivo conteudo;
 abreArquivo "dados.txt" ReadMode
 }
 else ioError erro
 

fechaArquivo :: Handle -> IO()
fechaArquivo handle_arq = hClose handle_arq
 
 
 
 
 
 

