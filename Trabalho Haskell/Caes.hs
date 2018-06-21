module Cadastro where
import Data.Char  -- Para função toUpper
import System.Environment
import System.IO  -- Biblioteca para Classe IO
import System.IO.Error  -- 
import Control.Exception

-- =================Menu de Opções==================================================--
-- =================================================================================--
menu :: IO()
menu = 
 do 
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

--Le a opção digitada no menu --
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
  'B' ->  excluir_um_cadastro   
  'C' ->  imprime_cadastros   
  'D' ->  imprime_ranking
  otherwise -> sair i

--Inserir Cadastro --
-- ================ --
insere_cadastro::IO()
insere_cadastro = 
 do
  lixo <- getLine -- Variavel lixo para pegar o enter q vem da função anterior 
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
  putStrLn " " -- linha necessária para execução do getChar
  let cadastro = raca ++ "," ++ preco ++ "," ++ origem ++ "," ++ tamanho ++ "," ++ peso ++ "," ++ cor ++ "," ++ funcao ++ "," ++ posicao
  appendFile "dados.csv" (cadastro ++ "\n") --insere dados no arquivo

--Excluir CADASTRO
excluir_um_cadastro :: IO()
excluir_um_cadastro =
 do
  lixo <- getLine -- Variavel lixo para pegar o enter q vem da função anterior 
  putStrLn "O cadastro será apagado pela raça"
  putStrLn "Digite a raça desejada:"
  raca <- getLine
  arquivo <- abreArquivo "dados.csv" ReadMode
  conteudo <- (hGetContents arquivo)
  cadastro <- (converteConteudo (conteudo))
  let novo_conteudo = apaga_pelo_nome cadastro raca
  arquivobkp <- abreArquivo "auxiliar.txt" WriteMode
  hPutStr arquivobkp novo_conteudo
  fechaArquivo arquivobkp
  fechaArquivo arquivo
  copiar "auxiliar.txt" "dados.csv"

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

 -- ============================================FUNCAO Auxiliar EXCLUSÃO ==============

  apaga_pelo_nome :: [[String]]->String->String
apaga_pelo_nome [] nm = "\n"
apaga_pelo_nome (x:xs) nm
 |nm == (raca x) = (apaga_pelo_nome xs nm)
 |otherwise = (foldl1 (\a b->a++","++b) x) ++ "\n" ++ (apaga_pelo_nome xs nm)

--Retorna a posicao 
raca, preco, origem, tamanho, peso, cor, funcao, ranking :: [String] -> String
raca(a:b:c:d:e:f:g:h:[]) = a
preco(a:b:c:d:e:f:g:h:[]) = b
origem(a:b:c:d:e:f:g:h:[]) = c
tamanho(a:b:c:d:e:f:g:h:[]) = d
peso(a:b:c:d:e:f:g:h:[]) = e
cor(a:b:c:d:e:f:g:h:[]) = f
funcao(a:b:c:d:e:f:g:h:[]) = g
ranking(a:b:c:d:e:f:g:h:[]) = h

-- ============================== Copiar Origem e Destino =============================
copiar origem destino =
    do
     arquivoO <- abreArquivo origem ReadMode
     conteudo <- (hGetContents arquivoO)
     arquivoD <- abreArquivo destino WriteMode
     hPutStr arquivoD conteudo
     fechaArquivo arquivoD
     fechaArquivo arquivoO

fechaArquivo :: Handle -> IO()
fechaArquivo handle_arq = hClose handle_arq

---IMPRIMIR TODOS OS CADASTROS ----
-- ===========================
imprime_cadastros :: IO()
imprime_cadastros =
 do
    putStrLn " "
    putStrLn "---------------------------------------------------"
    arq <- (openFile "dados.csv" ReadMode) -- Abre o arquivo
    conteudo <- (hGetContents arq)
    cadastro <- (converteConteudo (conteudo))
    imprime cadastro
    hClose arq -- Fecha arquivo
    putStrLn "---------------------------------------------------"

-- ========================================================================================= --
--Funcao Auxiliar de impressao
-- =========================================================================================
imprime :: [[String]] ->IO()
imprime [] = putStrLn ""
imprime (x:xs) = do
 putStrLn (foldl1 (\a b->a++ " " ++b) x)
 imprime xs

--Funcao Auxiliar Consulta----------
converteConteudo :: String -> IO [[String]]
converteConteudo conteudo = return (map (explodir ',') (explodir '\n' conteudo))

--Funcao auxiliar de ranking--
imprime_ranking :: IO()
imprime_ranking =
 do    
    arq <- (openFile "dados.csv" ReadMode) -- Abre o arquivo
    conteudo <- (hGetContents arq)
    cadastro <- (converteConteudo (conteudo))
    putStrLn conteudo
    hClose arq -- Fecha arquivo
	
imprime_ranking_aux :: [[String]]->String->String
imprime_ranking_aux [] nm = "\n"
imprime_ranking_aux (x:xs) nm
 |nm == (raca x) = (imprime_ranking_aux xs nm)
 |nm == (ranking x) = (imprime_ranking_aux xs nm)
 |otherwise = (foldl1 (\a b->a++","++b) x) ++ "\n" ++ (imprime_ranking_aux xs nm)

--Funcao Auxiliar
explodir :: Eq a=> a -> [a] -> [[a]]
explodir a [] = []
explodir a(x:xs)
                |(takeWhile (/=a) (x:xs)) == [] = explodir a xs
                |x == a = (takeWhile (/=a) xs) : explodir a (dropWhile (/= a) xs)
                |otherwise = (takeWhile (/= a)(x:xs)) : explodir a (dropWhile (/=a) (x:xs))
 
 -- Funcao sair ----------
sair :: Char -> IO()
sair i |i == 'I' = putStrLn "Saindo do sistema..."
 |otherwise= putStrLn "Opcao invalida..."

