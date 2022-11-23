module JogoMoedas where

import Data.Maybe
import Text.Read
import Data.List.Split
import Data.List
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Array.Unboxed -- (UArray)
-- import qualified Data.Array.Unboxed as UArray
import Data.Array.MArray (MArray)
import qualified Data.Array.MArray as MArray
import Data.Array.ST
import Data.STRef
import System.Console.ANSI
import System.Console.ANSI.Codes
import Control.Monad.ST
import Control.Monad

larguraMapa, alturaMapa :: Int
larguraMapa = 10
alturaMapa = 10

data Jogo = Jogo {
    paises :: [Pais],
    cidades :: MatrizCidade,
    moedas :: MatrizMoedas,
    dia :: Int
}

instance Show Jogo where
    show Jogo{paises=p, moedas=m} = show p ++ show m

instance Eq Jogo where
    (==) j1 j2 = (moedas j1) == (moedas j2) 

data EstadoJogo = Vitoria | Ilha | Overlap | Continuar

data Cidade = Cidade {
    pais :: Pais,
    overlap :: Bool,
    completo :: Bool
} deriving (Show)


data Pais = Pais {
    indice :: Int,
    nome :: String,
    cor :: String,
    diaCompleto :: Maybe Int,
    llCoord :: Coordenada, -- Coordenada do canto inferior esquerdo
    urCoord :: Coordenada  -- Coordenada do canto superior direito
}

instance Show Pais where
    show p = (show $ indice p) ++ "-" ++ nome p

instance Eq Pais where
    (==) p1 p2 = (nome p1) == (nome p2) 

instance Ord Pais where
    (<=) p1 p2 = (nome p1) <= (nome p2)

type ID = Int
type Moeda = Int
type Coordenada = (Int, Int) -- Coordenada X Y
type Coordenada3D = (Int, Int, Int) -- Coordenada X Y Z
type MatrizCidade = Array Coordenada (Maybe Cidade)
type MatrizMoedas = UArray Coordenada3D Moeda


jogoVazio :: Jogo
jogoVazio = Jogo {
    paises = [],
    cidades = matrizVazia,
    moedas = listArray ((1, 1, 1), (1, 1, 1)) [],
    dia = 0
}

matrizVazia :: MatrizCidade
matrizVazia = array ((1, 1), (larguraMapa, alturaMapa)) [(c, Nothing) | c <- todasCoords]

todasCoords :: [Coordenada]
todasCoords = range ((1, 1), (alturaMapa, larguraMapa))

cidadePadrao :: MatrizCidade -> Pais -> Coordenada -> Cidade
cidadePadrao m p c = Cidade {
    pais = p,
    overlap = isJust $ m!c,
    completo = False
}

assocsMapa :: MatrizCidade -> [(Coordenada, Cidade)]
assocsMapa = mapMaybe assocmToMassoc . assocs
    where
        assocmToMassoc (coord, mc) = do
            c <- mc
            return (coord, c)


coordenadasPais :: Pais -> [Coordenada]
coordenadasPais Pais{llCoord = ll, urCoord = ur} = range (ll, ur)

moedasCidade :: MatrizMoedas -> Coordenada -> [Moeda]
moedasCidade ms (x,y) = 
    let ((_,_,z1),(_,_,z2)) = bounds ms
    in [ ms!(x,y,z) | z <- [z1..z2] ]

matrizMoedasPadrao :: [Pais] -> MatrizMoedas
matrizMoedasPadrao ps = runST $ do
    let bound = ((1, 1, 1), (larguraMapa, alturaMapa, length ps))
    ms <- newArray bound 0 :: ST s (STUArray s Coordenada3D Int)
    forM_ [(a,b,c) 
        | p <- ps
        , (a,b) <- coordenadasPais p
        , let c = indice p] $ \coord -> do
            writeArray ms coord 1000000

    freeze ms

matrizCidadesPadrao :: [Pais] -> MatrizCidade
matrizCidadesPadrao ps = runST $ do
    let bound = ((1, 1), (larguraMapa, alturaMapa))
    cs <- newArray bound Nothing :: ST s (STArray s Coordenada (Maybe Cidade))
    
    forM_ [(p, coord)
        | p <- ps
        , coord <- coordenadasPais p
        ] $ \(p, coord) -> do
            oldC <- readArray cs coord
            writeArray cs coord $ return Cidade {
                pais = p,
                overlap = isJust oldC,
                completo = False
            }
    
    freeze cs


iniciarJogo :: [Pais] -> (EstadoJogo, Jogo)
iniciarJogo ps = (e, j)
    where
        e
            | possuiOverlap j = Overlap
            | possuiIlha j = Ilha
            | checarVitoria j = Vitoria
            | otherwise = Continuar
        j = Jogo {
            paises = ps,
            cidades = matrizCidadesPadrao ps,
            moedas = matrizMoedasPadrao ps,
            dia = 0
        } 

tickJogo :: Jogo -> Jogo
tickJogo old = Jogo {
    paises = map (checarPaisCompleto novoCidades novoDia) (paises old) ,
    cidades = novoCidades,
    moedas = novoMoedas,
    dia = novoDia
} where
    novoCidades = checarCompletos novoMoedas (cidades old)
    novoMoedas = distribuir (cidades old) (moedas old)
    novoDia = dia old + 1

checarPaisCompleto :: MatrizCidade -> Int -> Pais -> Pais
checarPaisCompleto m d p = 
    if isJust $ diaCompleto p
        then p
        else Pais {
            indice = indice p,
            nome = nome p,
            cor = cor p,
            llCoord = llCoord p,
            urCoord = urCoord p,
            diaCompleto = dia
        } 
    where
        dia = if and [completo c | coord <- coordenadasPais p, Just c <- [m!coord]]
            then Just d
            else Nothing

checarCompletos :: MatrizMoedas -> MatrizCidade -> MatrizCidade
checarCompletos ms cs = array (bounds cs) $ fmap checarCidadeCompleto (range $ bounds cs)
    where
        checarCidadeCompleto :: Coordenada -> (Coordenada, Maybe Cidade)
        checarCidadeCompleto co = (co, mc)
            where mc = do
                    c <- cs!co
                    if completo c
                        then return c
                        else return Cidade {
                            pais = pais c,
                            overlap = False,
                            completo = comTodasAsMoedas (moedasCidade ms co)
                        }
        comTodasAsMoedas :: [Moeda] -> Bool
        comTodasAsMoedas m = and $ fmap (>0) m

checarVitoria :: Jogo -> Bool
checarVitoria Jogo{paises=ps} = and $ map (isJust.diaCompleto) ps

distribuir :: MatrizCidade -> MatrizMoedas -> MatrizMoedas
distribuir cs ms = result
    where
        result = runST $ do
            dist <- distribuido
            forM_ (range $ bounds ms) $ \(coord) -> do
                oldV <- readArray dist coord
                writeArray dist coord (oldV + ms!coord)
            freeze dist

        distribuido :: ST s (STUArray s Coordenada3D Int)
        distribuido = do
            moedas <- newArray_ (bounds ms) :: ST s (STUArray s Coordenada3D Int)

            forM_ [ms | (coord, c) <- assocsMapa cs
                , ms <- pegarMoedasVizinhas cs ms coord
                ] $ \(coordM, m) -> do
                    oldM <- readArray  moedas coordM
                    writeArray moedas coordM (oldM + m)
                
            return moedas


pegarMoedasVizinhas :: MatrizCidade -> MatrizMoedas -> Coordenada -> [(Coordenada3D, Moeda)]
pegarMoedasVizinhas cs ms (x, y) = valoresPegos
    where
        ((x1,y1,z1),(x2,y2,z2)) = bounds ms
        coordVizinhos = (vizinhos cs (x,y))
        coordsMoedasVizinhos = [(a,b,c) | (a,b) <- coordVizinhos, c <- range (z1,z2)]
        valoresPegos = [
            coordV
            | coord@(_,_,c) <- coordsMoedasVizinhos
            , let v = pegarMoedas $ ms!coord
            , coordV <- [((x, y, c), v), (coord, -v)] ]
    

vizinhos :: MatrizCidade -> Coordenada -> [Coordenada]
vizinhos m (x, y) = 
    [c
    | c <- [(x, y+1), (x, y-1), (x-1, y), (x+1, y)]
    , inRange (bounds m) c
    , isJust $ m!c
    ]

pegarMoedas :: Moeda -> Moeda
pegarMoedas = \x -> x `div` 1000
            
possuiOverlap :: Jogo -> Bool
possuiOverlap = or . map overlap . catMaybes . elems . cidades

possuiIlha :: Jogo -> Bool
possuiIlha Jogo{paises=ps, cidades=cs} = (length ps) /= (length $ paisesUnicos paisesConectados)
    where
        paisesUnicos:: [Pais] -> [Pais]
        paisesUnicos = map head . group . sortBy (\p1 p2 -> compare (indice p1) (indice p2))
        paisesConectados :: [Pais]
        paisesConectados = map (pais . fromJust . (cs !)) $ cidadesConectadas primCidade
        primCidade :: Coordenada
        primCidade = fst $ head $ assocsMapa cs
        cidadesConectadas :: Coordenada -> [Coordenada]
        cidadesConectadas prim = runST $ do
            arr <- newArray (bounds cs) False
            writeArray arr prim True
            cd arr [prim]
        cd :: STUArray s Coordenada Bool -> [Coordenada] -> ST s [Coordenada]
        cd explorado [] = do
            a <- MArray.getAssocs explorado
            return [c | (c,b) <- a, b]
        cd explorado (prox:es) = do
            proximos <- proxExplorar explorado prox
            cd explorado (es ++ proximos)

        proxExplorar :: STUArray a Coordenada Bool -> Coordenada -> ST a [Coordenada]
        proxExplorar exp co = do
            v <- forM [c | c <- vizinhos cs co
                    ] $ \c -> do
                        explorado <- explorarCoord c exp
                        return (c, explorado)

            return [c | (c,e) <- v, not e]


        explorarCoord :: Coordenada -> STUArray a Coordenada Bool -> ST a (Bool)
        explorarCoord c arr = do
            explorado <- readArray arr c
            writeArray arr c True
            return explorado

removerColchete :: String -> String
removerColchete = filter (\c -> c /= '[' && c /= ']')

sgrCores :: [String]
sgrCores = cycle [setSGRCode [SetColor Foreground intensity color]
    |   intensity <- [Dull, Vivid],
        color <- [Red .. White]]

parseInput :: String -> Maybe [Pais]
parseInput xs = do
    let (n:paises) = (map removerColchete) $ (splitOn "][") xs
    total <- readMaybe n :: Maybe Int
    if total > 20 || total < 0 || total /= length paises
        then Nothing
        else traverse parsePais [i | i <- zip [1..length paises] $ zip sgrCores paises]

-- Aceita String's no formato "Nome xl yl xh yh"
parsePais :: (Int, (String, String)) -> Maybe Pais
parsePais (i, (cor, xs)) = do
    let (nome:t) = words xs
    [xl, yl, xh, yh] <- traverse readMaybe t
    return Pais {
        indice = i,
        nome = nome,
        cor = cor,
        diaCompleto = Nothing,
        llCoord = (xl, yl),
        urCoord = (xh, yh)
    }

quantMoedasCidade :: MatrizMoedas -> Coordenada -> Int
quantMoedasCidade ms c = sum $ fmap (\n -> if n > 0 then 1 else 0) $ moedasCidade ms c

charCidade :: MatrizMoedas -> Coordenada -> Char
charCidade ms c = last $ show $ quantMoedasCidade ms c

imprimirCidade :: MatrizCidade -> MatrizMoedas -> Coordenada -> IO ()
imprimirCidade cs ms (x,y) = do
    setCursorPosition (alturaMapa - y + 1) x
    setSGR [Reset]
    let mc = cs!(x,y)
    case mc of
        Nothing -> return ()
        Just c -> do
            putStr $ cor $ pais c
            if completo c
                then putChar '#'
                else if overlap c
                    then imprimirCidadeErro "#"
                    else putChar $ charCidade ms (x,y)

imprimirCidadeErro :: String -> IO ()
imprimirCidadeErro xs = do
    setSGR [
        SetColor Background Dull Black,
        SetColor Foreground Vivid Red
        ]
    putStr xs
    setSGR [Reset]
    

imprimirMapa :: Jogo -> IO ()
imprimirMapa Jogo{paises = ps, cidades = cs, moedas=ms} = do
    mapM_ (imprimirCidade cs ms) $ (range $ bounds cs)
    setSGR [Reset]

imprimirJogo :: Jogo -> IO ()
imprimirJogo j@Jogo{paises = ps, dia=d} = do
    clearScreen
    imprimirMapa j
    setCursorPosition 0 0
    putStr $ "Dia " ++ show d
    setCursorPosition (alturaMapa + 1) 0
    putStr $ foldl (++) "" ["\n" ++ nomeCor ++ " demorou " ++ show d ++ " dias para ficar completo"
        | pais <- ps, Just d <- [diaCompleto pais], let nomeCor = nomeColorido pais]

nomeColorido :: Pais -> String
nomeColorido p = cor p ++ nome p ++ setSGRCode [Reset]