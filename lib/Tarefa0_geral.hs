{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
{-|
Module      : Tarefa0_geral
Description : Funções auxiliares gerais.

Módulo que define funções genéricas sobre listas e matrizes.
-}

module Tarefa0_geral where

-- * Tipos de dados

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/matriz.png>>
type Matriz a = [[a]]
-- | Uma posição numa matriz é dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/posicaomatriz.png>>
type Posicao = (Int,Int)

-- | A dimensão de uma matrix dada como um par (/número de linhas/,/número de colunhas/).
type Dimensao = (Int,Int)

-- | Uma direção é dada pela rosa dos ventos. Ou seja, os 4 pontos cardeais e os 4 pontos colaterais.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/rosadosventos.jpg>>
data Direcao = Norte | Nordeste | Este | Sudeste | Sul | Sudoeste | Oeste | Noroeste
    deriving (Eq,Ord,Show,Read,Enum)

-- * Funções não-recursivas.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido _ [] = False
eIndiceListaValido i lst = i >= 0 && i < length lst 


-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz m = (length m, if null m then 0 else length (head m) )

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida _ [] = False
ePosicaoMatrizValida (x,y) m = x >= 0 && x < length m && y >= 0 && y < length (head m)

-- | Move uma posição uma unidade no sentido de uma direção.
movePosicao :: Direcao -> Posicao -> Posicao
movePosicao dir (x,y) =
    case dir of 
        Norte -> (x-1, y)
        Sul -> (x+1, y)
        Oeste -> (x, y-1)
        Este -> (x, y+1)
        Nordeste -> (x-1, y+1)
        Noroeste -> (x-1, y-1)
        Sudeste -> (x+1, y+1)
        Sudoeste -> (x+1, y-1)

-- | Versão da função 'movePosicao' que garante que o movimento não se desloca para fora de uma janela.
--
-- __NB:__ Considere uma janela retangular com origem no canto superior esquerdo definida como uma matriz. A função recebe a dimensao da janela.
movePosicaoJanela :: Dimensao -> Direcao -> Posicao -> Posicao
movePosicaoJanela (dimX, dimY) dir pos =
    let (x,y) = movePosicao dir pos
    in if x >= 0 && x < dimX && y >= 0 && y < dimY 
        then (x,y)
        else pos 

-- | Converte uma posição no referencial em que a origem é no canto superior esquerdo da janela numa posição em que a origem passa a estar no centro da janela.
--
-- __NB:__ Considere posições válidas. Efetue arredondamentos como achar necessário.
origemAoCentro :: Dimensao -> Posicao -> Posicao
origemAoCentro (dimX, dimY) (x,y) = ((dimX `div` 2) - x, y - (dimY `div` 2))

-- | Roda um par (posição,direção) 45% para a direita.
--
-- __NB:__ Vendo um par (posição,direção) como um vector, cria um novo vetor do desto com a próxima direção da rosa dos ventos rodando para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/rodaposicaodirecao.png>>
rodaDirecao :: Direcao -> Direcao
rodaDirecao dir = 
    case dir of 
        Norte -> Nordeste
        Sul ->Sudoeste
        Oeste -> Noroeste
        Este -> Sudeste
        Nordeste -> Este
        Noroeste -> Norte
        Sudeste -> Sul
        Sudoeste -> Oeste

rodaPosicaoDirecao :: (Posicao, Direcao) -> (Posicao, Direcao)
rodaPosicaoDirecao (pos, dir) = (movePosicao dir pos, rodaDirecao dir)

-- * Funções recursivas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __NB:__ Retorna @Nothing@ se o índice não existir.
encontraIndiceLista :: Int -> [a] -> Maybe a
encontraIndiceLista i a
    | i >= 0 && i < length a = Just (a !! i)
    | otherwise = Nothing

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista 0 e (_:xs) = e : xs
atualizaIndiceLista i e (x:xs)
    | i < 0     = x:xs
    | otherwise = x : atualizaIndiceLista (i-1) e xs

-- | Devolve o elemento numa dada posição de uma matriz.
--
-- __NB:__ Retorna @Nothing@ se a posição não existir.
encontraPosicaoMatriz :: Posicao -> Matriz a -> Maybe a
encontraPosicaoMatriz (x, y) m
    | ePosicaoMatrizValida (x,y) m = Just ((m !! x) !! y)
    | otherwise = Nothing

-- | Modifica um elemento numa dada posição de uma matriz.
--
-- __NB:__ Devolve a própria matriz se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz _ _ [] = []
atualizaPosicaoMatriz (x,y) e m
    | not (ePosicaoMatrizValida (x,y) m) = m
    | otherwise =
        take x m ++
        [atualizaIndiceLista y e (m !! x)] ++
        drop (x+1) m
-- | Aplica uma sequência de movimentações a uma posição, pela ordem em que ocorrem na lista.
moveDirecoesPosicao :: [Direcao] -> Posicao -> Posicao
moveDirecoesPosicao [] p = p
moveDirecoesPosicao (x:xs) p = moveDirecoesPosicao xs (movePosicao x p)
    
-- | Aplica a mesma movimentação a uma lista de posições.
moveDirecaoPosicoes :: Direcao -> [Posicao] -> [Posicao]
moveDirecaoPosicoes _ [] = []
moveDirecaoPosicoes d (x:xs) = movePosicao d x : moveDirecaoPosicoes d xs

-- | Verifica se uma matriz é válida, no sentido em que modela um rectângulo.
--
-- __NB:__ Todas as linhas devem ter o mesmo número de colunas. 
eMatrizValida :: Matriz a -> Bool
eMatrizValida [] = True
eMatrizValida (x:xs) = all ((== length x) . length) xs