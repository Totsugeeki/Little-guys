module Main where

import Labs2025
import Tarefa3
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 3
testesTarefa3 :: [Estado]
testesTarefa3 =
    [ estadoGravidade
    , estadoAguaMorte
    , estadoExplosao
    ]

dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3


-- Minhoca deve cair um nível (gravidade)
estadoGravidade :: Estado
estadoGravidade = Estado mapa [] [Minhoca (Just (0,0)) (Viva 100) 0 0 0 0 0]
  where
    mapa = [ [Ar, Ar, Ar]
           , [Ar, Ar, Ar]
           , [Terra, Terra, Terra] ]

-- Minhoca cai em Água e morre
estadoAguaMorte :: Estado
estadoAguaMorte = Estado mapa [] [Minhoca (Just (1,0)) (Viva 100) 0 0 0 0 0]
  where
    mapa = [ [Ar, Ar, Ar]
           , [Agua, Agua, Agua]
           , [Terra, Terra, Terra] ]

-- Disparo com tempo 0 deve explodir
estadoExplosao :: Estado
estadoExplosao = Estado mapa [Disparo (1,1) Este Bazuca (Just 0) 0] []
  where
    mapa = replicate 3 (replicate 3 Ar)