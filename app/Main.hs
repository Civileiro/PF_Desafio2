module Main where

import JogoMoedas
import System.IO
import System.Console.ANSI
import Text.Read
import Control.Monad


main :: IO ()
main = do
    clearScreen >> setCursorPosition 0 0
    opcao <- readOpcao
    case opcao of
        Exemplo n -> iniciarComInput $ exemploEntradas !! n
        Manual -> do
            input <- prompt "Digite a entrada: "
            print input
            iniciarComInput input
        Testes -> testarExemplos
    showCursor
    -- putStrLn "Execute o programa diretamente para não precisar re-compilar:"
    -- putStrLn "./result/bin/PFDesafio2"

testarExemplos :: IO ()
testarExemplos = do
    return ()

iniciarComInput :: String -> IO ()
iniciarComInput input = do
    hideCursor
    let paises = parseInput input
    case parseInput input of
        Just paises -> setupJogo paises
        Nothing -> putStr "Input não conseguiu ser lido!\n"

setupJogo :: [Pais] -> IO ()
setupJogo xs = do
    let (e, jogo) = iniciarJogo xs
    case e of
        Overlap -> imprimirJogoErrado jogo Overlap
        Ilha -> imprimirJogoErrado jogo Ilha
        Vitoria -> finalizar jogo
        otherwise -> iniciarLoop jogo

iniciarLoop :: Jogo -> IO ()
iniciarLoop j = do
    imprimirJogo j
    loop j

loop :: Jogo -> IO ()
loop old = do
    -- threadDelay 10000
    let new = tickJogo old
    when (dia new `rem` 100 == 0) $ do
        imprimirJogo new
        hFlush stdout
    case estadoJogo new of
        Vitoria -> finalizar new
        otherwise -> loop new
    
estadoJogo :: Jogo -> EstadoJogo
estadoJogo j
    | checarVitoria j = Vitoria
    | otherwise = Continuar

finalizar :: Jogo -> IO ()
finalizar ultimoEstado = do
    imprimirJogo ultimoEstado
    setCursorPosition ((+) (alturaMapa + 2) $ length $ paises ultimoEstado) 0
    putStr "Todas os países estão completos\n"

imprimirJogoErrado :: Jogo -> EstadoJogo -> IO ()
imprimirJogoErrado j e = do
    imprimirJogo j
    setCursorPosition (alturaMapa + 1) 0
    case e of
        Overlap -> putStrLn "Existem cidades que se sobrepõem"
        Ilha -> putStrLn "Paises não podem estar em ilhas diferentes"
    

getOpcao :: IO String
getOpcao = do
    return ""


exemploEntradas :: [String]
exemploEntradas = [
    "[[3][France 1 4 4 6][Spain 3 1 6 3][Portugal 1 1 2 2]]",
    "[[6][France 3 4 7 7][Spain 3 1 6 3][Portugal 1 1 2 2][Italy 8 1 9 4][Germany 8 5 10 8][Irlanda 2 8 3 10]]",
    "[[10][France 3 4 7 7][Spain 3 1 6 3][Portugal 1 1 2 2][Italy 8 1 9 4][Germany 8 5 10 8][Irlanda 2 8 3 10][Atlantis 1 3 2 7][UK 4 8 7 10][Slovenia 10 4 10 4][Monaco 7 3 7 3]]",
    "[[20][France 3 4 7 7][Spain 3 1 6 3][Portugal 1 1 2 2][Italy 8 1 9 4][Germany 8 5 10 8][Irlanda 2 8 3 10][Atlantis 1 3 2 7][UK 4 8 7 10][Slovenia 10 4 10 4][Monaco 7 3 7 3][A 1 9 1 9][B 1 10 1 10][C 7 1 7 1][D 7 2 7 2][E 10 1 10 1][F 10 2 10 2][G 10 3 10 3][H 8 10 8 10][I 9 10 9 10][J 10 10 10 10]]",
    "[[4][A 1 5 5 10][B 6 5 10 10][C 1 1 5 3][D 6 1 10 3]]",
    "[[8][A 1 1 1 1][B 2 1 2 1][C 3 1 3 1][D 4 1 4 1][E 4 2 4 2][F 4 3 4 3][G 4 4 4 4][H 10 10 10 10]]",
    "[[1][Pangeia 5 5 5 5]]"
    ]

formatarExemplos :: Int -> String
formatarExemplos n = unlines $ map (\(n, xs) -> "\t" ++ show n ++ " - " ++ xs) (zip [n..] exemploEntradas)

apresentarOpcoes :: IO ()
apresentarOpcoes = do
    putStr "\
    \Selecione uma opção de entrada:\n\
    \\t0 - Digitar entrada manualmente\n"
    putStr $ formatarExemplos 1

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
    
data Opcao = Manual | Testes | Exemplo Int

readOpcao :: IO Opcao
readOpcao = do
    apresentarOpcoes
    line <- getLine :: IO String
    let mi = readMaybe line :: Maybe Int
    case mi of
        Nothing -> do
            clearScreen >> setCursorPosition 0 0
            putStrLn "Digite um numero!"
            readOpcao
        Just i -> case i of
                    0 -> return Manual
                    n -> if (n - 1) >= length exemploEntradas || (n - 1) < 0
                            then do
                                clearScreen >> setCursorPosition 0 0
                                putStrLn "Essa opção não é válida!"
                                readOpcao
                            else return $ Exemplo (n - 1)