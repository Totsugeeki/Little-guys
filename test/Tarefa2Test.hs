module Main where

import Labs2025
import Tarefa2
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 2
testesTarefa2 :: [(NumMinhoca,Jogada,Estado)]
testesTarefa2 = 
    [ (0, Move Este, estadoMoveEste)
    , (0, Move Sul, estadoMoveSul)
    , (0, Dispara Bazuca Este, estadoDisparaBazuca)
    , (0, Dispara Dinamite Sul, estadoDisparaDinamite)
    , (0, Move Oeste, estadoMoveOeste)
    ]

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = testesTarefa2
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2


-- Movimento para Este
estadoMoveEste :: Estado
estadoMoveEste = Estado mapa [] [minhoca]
  where
    mapa = replicate 3 (replicate 3 Ar)
    minhoca = Minhoca (Just (1,1)) (Viva 100) 0 0 0 0 0

-- Movimento para Sul
estadoMoveSul :: Estado
estadoMoveSul = Estado mapa [] [minhoca]
  where
    mapa = replicate 3 (replicate 3 Ar)
    minhoca = Minhoca (Just (1,1)) (Viva 100) 0 0 0 0 0


-- Disparo de Bazuca
estadoDisparaBazuca :: Estado
estadoDisparaBazuca = Estado mapa [] [minhoca]
  where
    mapa = replicate 3 (replicate 3 Ar)
    minhoca = Minhoca (Just (1,1)) (Viva 100) 0 0 1 0 0

-- Disparo de Dinamite
estadoDisparaDinamite :: Estado
estadoDisparaDinamite = Estado mapa [] [minhoca]
  where
    mapa = replicate 3 (replicate 3 Ar)
    minhoca = Minhoca (Just (1,1)) (Viva 100) 0 0 0 0 1
estadoMoveOeste :: Estado 
estadoMoveOeste = Estado mapa [] [minhoca]
  where
    mapa = [[Ar,Ar], [Terra,Terra]]
    minhoca = Minhoca (Just (0,1)) (Viva 100) 0 0 0 0 1