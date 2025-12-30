module Main where

import Labs2025
import Tarefa4
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 4
testesTarefa4 :: [Estado]
testesTarefa4 =
  [ testeMapaVazio
  , testeMinhocasProximas
  , testeMuitasMinhocas
  , testeComAgua
  , testeTerrenoDenso
  , testeMuitasMinhocasColadas
  , estadoInicial
  , estadoInicial2
  ]


dataTarefa4 :: IO TaskData
dataTarefa4 = do
    let ins = testesTarefa4
    outs <- mapM (runTest . tatica) ins
    return $ T4 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa4


testeMapaVazio :: Estado
testeMapaVazio = Estado mapa [] [m0]
  where
    mapa = replicate 10 (replicate 10 Ar)

    m0 = Minhoca
      { posicaoMinhoca     = Just (5,5)
      , vidaMinhoca        = Viva 100
      , jetpackMinhoca     = 0
      , escavadoraMinhoca  = 0
      , bazucaMinhoca      = 10
      , minaMinhoca        = 0
      , dinamiteMinhoca    = 5
      }

testeMinhocasProximas :: Estado
testeMinhocasProximas = Estado mapa [] [m0,m1]
  where
    mapa = replicate 10 (replicate 10 Terra)

    m0 = minhoca (4,4)
    m1 = minhoca (4,5)

    minhoca p = Minhoca
      { posicaoMinhoca     = Just p
      , vidaMinhoca        = Viva 100
      , jetpackMinhoca     = 0
      , escavadoraMinhoca  = 0
      , bazucaMinhoca      = 10
      , minaMinhoca        = 3
      , dinamiteMinhoca    = 3
      }


testeMuitasMinhocas :: Estado
testeMuitasMinhocas = Estado mapa [] minhocas
  where
    mapa = replicate 15 (replicate 15 Terra)

    minhocas =
      [ minhoca (2,2)
      , minhoca (7,7)
      , minhoca (12,12)
      ]

    minhoca p = Minhoca
      { posicaoMinhoca     = Just p
      , vidaMinhoca        = Viva 100
      , jetpackMinhoca     = 0
      , escavadoraMinhoca  = 0
      , bazucaMinhoca      = 10
      , minaMinhoca        = 1
      , dinamiteMinhoca    = 2
      }


testeComAgua :: Estado
testeComAgua = Estado mapa [] [m0]
  where
    mapa =
      replicate 6 (replicate 10 Ar) ++
      replicate 2 (replicate 10 Agua)

    m0 = Minhoca
      { posicaoMinhoca     = Just (4,5)
      , vidaMinhoca        = Viva 100
      , jetpackMinhoca     = 0
      , escavadoraMinhoca  = 0
      , bazucaMinhoca      = 10
      , minaMinhoca        = 0
      , dinamiteMinhoca    = 2
      }


testeTerrenoDenso :: Estado
testeTerrenoDenso = Estado mapa [] [m0]
  where
    mapa = replicate 12 (replicate 12 Terra)

    m0 = Minhoca
      { posicaoMinhoca     = Just (6,6)
      , vidaMinhoca        = Viva 100
      , jetpackMinhoca     = 0
      , escavadoraMinhoca  = 0
      , bazucaMinhoca      = 10
      , minaMinhoca        = 0
      , dinamiteMinhoca    = 5
      }

testeMuitasMinhocasColadas :: Estado
testeMuitasMinhocasColadas = Estado mapa [] minhocas
  where
    mapa = replicate 15 (replicate 15 Terra)

    minhocas =
      [ minhoca (2,2)
      , minhoca (3,2)
      , minhoca (2,3)
      ]

    minhoca p = Minhoca
      { posicaoMinhoca     = Just p
      , vidaMinhoca        = Viva 100
      , jetpackMinhoca     = 0
      , escavadoraMinhoca  = 0
      , bazucaMinhoca      = 10
      , minaMinhoca        = 1
      , dinamiteMinhoca    = 2
      }


estadoInicial :: Estado
estadoInicial = Estado mapa16 [] [m0, m1]
  where
    m0 = Minhoca
      { posicaoMinhoca     = Just (3,3)
      , vidaMinhoca        = Viva 100
      , jetpackMinhoca     = 0
      , escavadoraMinhoca  = 0
      , bazucaMinhoca      = 10
      , minaMinhoca        = 0
      , dinamiteMinhoca    = 5
      }

    m1 = Minhoca
      { posicaoMinhoca     = Just (4,9)
      , vidaMinhoca        = Viva 100
      , jetpackMinhoca     = 0
      , escavadoraMinhoca  = 0
      , bazucaMinhoca      = 10
      , minaMinhoca        = 0
      , dinamiteMinhoca    = 5
      }

-- MAPA 16x16 NAO MUDAR TAMANHO DOS TERRENOS EM DESENHAR
mapa16 :: Mapa
mapa16 =
  [ [ terreno x y | x <- [0..15] ]
  | y <- [0..15]
  ]


terreno :: Int -> Int -> Terreno
terreno x y
  | x == 0 || x == 15 || y == 0 || y == 15 = Pedra
  | y < alturaSolo x                       = Ar
  | caverna x y                            = Agua
  | y > 8                                  = Pedra
  | otherwise                              = Terra


alturaSolo :: Int -> Int
alturaSolo x =
  8
  + (x `mod` 3)
  - (x `mod` 2)


caverna :: Int -> Int -> Bool
caverna x y =
     x > 4  && x < 7  && y > 9  && y < 12
  || x > 9  && x < 12 && y > 8  && y < 11


estadoInicial2 :: Estado
estadoInicial2 = Estado mapa [] [m0, m1]
  where
    m0 = Minhoca
      { posicaoMinhoca     = Just (1,1)
      , vidaMinhoca        = Viva 100
      , jetpackMinhoca     = 0
      , escavadoraMinhoca  = 0
      , bazucaMinhoca      = 10
      , minaMinhoca        = 0
      , dinamiteMinhoca    = 5
      }

    m1 = Minhoca
      { posicaoMinhoca     = Just (5,9)
      , vidaMinhoca        = Viva 100
      , jetpackMinhoca     = 0
      , escavadoraMinhoca  = 0
      , bazucaMinhoca      = 10
      , minaMinhoca        = 0
      , dinamiteMinhoca    = 5
      }
    mapa = 
      [[Ar, Ar, Ar, Ar,Ar, Ar, Ar, Ar,Ar, Ar, Ar, Ar],
       [Terra,Ar,Ar,Ar,Terra,Ar,Ar,Ar,Terra,Ar,Ar,Ar],
       [Terra,Terra, Ar,Ar,Terra,Terra, Ar,Ar,Terra,Terra, Ar,Ar],
       [Terra, Terra, Terra, Terra,Terra, Terra, Terra, Agua,Terra, Terra, Terra, Agua],
       [Ar, Ar, Ar, Ar,Ar, Ar, Ar, Ar,Ar, Ar, Ar, Ar],
       [Terra,Ar,Ar,Ar,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar],
       [Terra,Terra, Ar,Ar,Terra,Terra, Ar,Ar,Terra,Terra, Ar,Ar],
       [Terra, Terra, Terra, Agua,Terra, Terra, Terra, Agua,Terra, Terra, Terra, Agua],
       [Ar, Ar, Ar, Ar,Ar, Ar, Ar, Ar,Ar, Ar, Ar, Ar],
       [Terra,Ar,Ar,Ar,Terra,Ar,Ar,Ar,Terra,Ar,Ar,Ar],
       [Terra,Terra, Ar,Ar,Terra,Terra, Ar,Ar,Terra,Terra, Ar,Ar],
       [Terra, Terra, Terra, Agua,Terra, Terra, Terra, Agua,Terra, Terra, Terra, Agua]]