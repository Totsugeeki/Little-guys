module Main where

import Labs2025
import Tarefa1
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 =   
    [ estadoBasico
    , estadoComPedra
    , estadoComAgua
    ]

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1



-- Estado simples com terreno destrutível.
estadoBasico :: Estado
estadoBasico = Estado mapa [] []
  where
    mapa = [ [Terra, Terra, Terra]
           , [Terra, Ar,    Terra]
           , [Terra, Terra, Terra] ]

-- Estado com obstáculo indestrutível (Pedra).
estadoComPedra :: Estado
estadoComPedra = Estado mapa [] []
  where
    mapa = [ [Pedra, Pedra, Pedra]
           , [Pedra, Ar,    Pedra]
           , [Pedra, Pedra, Pedra] ]

-- Estado com terreno de Água (para verificar destruição incorreta).
estadoComAgua :: Estado
estadoComAgua = Estado mapa [] []
  where
    mapa = [ [Agua, Ar, Terra]
           , [Ar,   Ar, Ar]
           , [Terra, Ar, Ar] ]
