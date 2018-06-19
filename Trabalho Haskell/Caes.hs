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
  'D' ->  imprime_melhor_posicao               
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
  tamanho <- getChar
  putStrLn "Cor: "   
  cor <- getChar
  putStrLn "Função Original: "   
  funcao <- getChar
  putStrLn "Ranking: "   
  posicao <- getChar
  putStrLn " " -- linha necessária para execução do getChar
  let cadastro = raca ++ "," preco ++ "," origem ++ "," tamanho ++ "," peso ++ "," cor ++ "," funcao ++ "," posicao
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
 |nm == (nome x) = (apaga_pelo_nome xs nm)
 |otherwise = (foldl1 (\a b->a++","++b) x) ++ "\n" ++ (apaga_pelo_nome xs nm)

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

imprime_melhor_posicao:: Pessoa->Pessoa->Pessoa
imprime_melhor_posicao x y
 |idade(x) < idade(y) = x
 |otherwise = y
 
 -- Funcao sair ----------
 sair :: Char -> IO()
 sair i |i == 'I' = putStrLn "Saindo do sistema..."
  |otherwise= putStrLn "Opcao invalida..."

