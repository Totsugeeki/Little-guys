module Mapa where

import Labs2025

-- ============================================================================
-- MAPA 1: ILHAS FLUTUANTES (64x64)
-- ============================================================================

mapa1 :: Mapa
mapa1 = [[terreno x y | x <- [0..63]] | y <- [0..63]]

terreno :: Int -> Int -> Terreno
terreno x y
  | x == 0 || x == 63 || y == 0 || y == 63 = Pedra
  | y < 12 = Ar
  | plataformaFlutuanteEsq x y = Terra
  | plataformaFlutuanteDir x y = Terra
  | plataformaFlutuanteCentro x y = Terra
  | lagoEsquerdo x y = Agua
  | lagoDireito x y = Agua
  | cavernaEsquerda x y = Ar
  | cavernaDireita x y = Ar
  | cavernaCentral x y = Ar
  | lagoSubterraneo x y = Agua
  | y >= 58 = Pedra
  | y >= alturaSolo x = Terra
  | otherwise = Ar

alturaSolo :: Int -> Int
alturaSolo x
  | x < 8 || x > 55 = 50
  | otherwise = baseAltura - ondulacao x
  where
    baseAltura = 45
    ondulacao px
      | px >= 20 && px <= 25 = 8
      | px >= 28 && px <= 36 = 12
      | px >= 38 && px <= 44 = 8
      | otherwise = smoothTransition px
    smoothTransition px
      | px >= 15 && px < 20 = (px - 15) * 2
      | px > 25 && px < 28 = 8 - (px - 25) * 2
      | px > 36 && px < 38 = 12 - (px - 36) * 2
      | px > 44 && px < 48 = max 0 (8 - (px - 44) * 2)
      | otherwise = 0

plataformaFlutuanteEsq :: Int -> Int -> Bool
plataformaFlutuanteEsq x y = x >= 8 && x <= 14 && y >= 15 && y <= 17

plataformaFlutuanteDir :: Int -> Int -> Bool
plataformaFlutuanteDir x y = x >= 49 && x <= 55 && y >= 15 && y <= 17

plataformaFlutuanteCentro :: Int -> Int -> Bool
plataformaFlutuanteCentro x y = x >= 28 && x <= 36 && y >= 22 && y <= 24

lagoEsquerdo :: Int -> Int -> Bool
lagoEsquerdo x y = x >= 1 && x <= 7 && y >= 45 && y < 50

lagoDireito :: Int -> Int -> Bool
lagoDireito x y = x >= 56 && x <= 62 && y >= 45 && y < 50

lagoSubterraneo :: Int -> Int -> Bool
lagoSubterraneo x y = x >= 28 && x <= 36 && y >= 50 && y <= 54

cavernaEsquerda :: Int -> Int -> Bool
cavernaEsquerda x y =
  x >= 10 && x <= 16 && y >= 48 && y <= 52 && dentroDaElipse (13, 50) (3, 2) (x, y)

cavernaDireita :: Int -> Int -> Bool
cavernaDireita x y =
  x >= 47 && x <= 53 && y >= 48 && y <= 52 && dentroDaElipse (50, 50) (3, 2) (x, y)

cavernaCentral :: Int -> Int -> Bool
cavernaCentral x y =
  x >= 28 && x <= 36 && y >= 40 && y <= 48 && dentroDaElipse (32, 44) (4, 4) (x, y)

-- ============================================================================
-- MAPA 2: MONTANHAS E VALES (64x64)
-- ============================================================================

mapa2 :: Mapa
mapa2 = [[terreno2 x y | x <- [0..63]] | y <- [0..63]]

terreno2 :: Int -> Int -> Terreno
terreno2 x y
  | x == 0 || x == 63 || y == 0 || y == 63 = Pedra
  | y < 8 = Ar
  | plataformaTopoEsquerda x y = Terra
  | plataformaTopoDireita x y = Terra
  | ponteAerea x y = Terra
  | cavernaEsquerda2 x y = Ar
  | cavernaDireita2 x y = Ar
  | lagoValeEsquerdo x y = Agua
  | lagoValeDireito x y = Agua
  | rioSubterraneo x y = Agua
  | y >= 58 = Pedra
  | y >= alturaMontanha x = Terra
  | otherwise = Ar

alturaMontanha :: Int -> Int
alturaMontanha x
  | x < 5 || x > 58 = 50
  | x >= 8 && x <= 15 = 25  -- Montanha esquerda
  | x >= 48 && x <= 55 = 25  -- Montanha direita
  | x >= 28 && x <= 35 = 38  -- Vale central mais fundo
  | x >= 16 && x <= 27 = transicaoEsquerda x
  | x >= 36 && x <= 47 = transicaoDireita x
  | otherwise = 45
  where
    transicaoEsquerda px = 25 + ((px - 15) * 13) `div` 12
    transicaoDireita px = 38 - ((px - 36) * 13) `div` 12

plataformaTopoEsquerda :: Int -> Int -> Bool
plataformaTopoEsquerda x y = 
  x >= 10 && x <= 13 && y >= 15 && y <= 17

plataformaTopoDireita :: Int -> Int -> Bool
plataformaTopoDireita x y = 
  x >= 50 && x <= 53 && y >= 15 && y <= 17

ponteAerea :: Int -> Int -> Bool
ponteAerea x y = 
  x >= 26 && x <= 37 && y >= 30 && y <= 31

cavernaEsquerda2 :: Int -> Int -> Bool
cavernaEsquerda2 x y =
  x >= 8 && x <= 15 && y >= 35 && y <= 42 && dentroDaElipse (11, 38) (3, 3) (x, y)

cavernaDireita2 :: Int -> Int -> Bool
cavernaDireita2 x y =
  x >= 48 && x <= 55 && y >= 35 && y <= 42 && dentroDaElipse (51, 38) (3, 3) (x, y)

lagoValeEsquerdo :: Int -> Int -> Bool
lagoValeEsquerdo x y = 
  x >= 18 && x <= 25 && y >= 45 && y < 50

lagoValeDireito :: Int -> Int -> Bool
lagoValeDireito x y = 
  x >= 38 && x <= 45 && y >= 45 && y < 50

rioSubterraneo :: Int -> Int -> Bool
rioSubterraneo x y = 
  x >= 28 && x <= 35 && y >= 52 && y <= 55

-- ============================================================================
-- MAPA 3: LABIRINTO VERTICAL (64x64)
-- ============================================================================

mapa3 :: Mapa
mapa3 = [[terreno3 x y | x <- [0..63]] | y <- [0..63]]

terreno3 :: Int -> Int -> Terreno
terreno3 x y
  | x == 0 || x == 63 || y == 0 || y == 63 = Pedra
  | y < 6 = Ar
  | plataformaNivel1 x y = Terra
  | plataformaNivel2 x y = Terra
  | plataformaNivel3 x y = Terra
  | plataformaNivel4 x y = Terra
  | paredeVerticalEsquerda x y = Pedra
  | paredeVerticalDireita x y = Pedra
  | paredeVerticalCentro x y = Pedra
  | cameraSecretaEsquerda x y = Ar
  | cameraSecretaDireita x y = Ar
  | y >= 57 = Agua  
  | otherwise = Ar

-- Nível 1 (mais alto)
plataformaNivel1 :: Int -> Int -> Bool
plataformaNivel1 x y = 
  (x >= 5 && x <= 12 && y >= 15 && y <= 17) ||
  (x >= 20 && x <= 28 && y >= 15 && y <= 17) ||
  (x >= 35 && x <= 43 && y >= 15 && y <= 17) ||
  (x >= 51 && x <= 58 && y >= 15 && y <= 17)

-- Nível 2
plataformaNivel2 :: Int -> Int -> Bool
plataformaNivel2 x y = 
  (x >= 8 && x <= 16 && y >= 25 && y <= 27) ||
  (x >= 24 && x <= 39 && y >= 25 && y <= 27) ||
  (x >= 47 && x <= 55 && y >= 25 && y <= 27)

-- Nível 3
plataformaNivel3 :: Int -> Int -> Bool
plataformaNivel3 x y = 
  (x >= 5 && x <= 13 && y >= 35 && y <= 37) ||
  (x >= 20 && x <= 28 && y >= 35 && y <= 37) ||
  (x >= 35 && x <= 43 && y >= 35 && y <= 37) ||
  (x >= 50 && x <= 58 && y >= 35 && y <= 37)

-- Nível 4 (mais baixo)
plataformaNivel4 :: Int -> Int -> Bool
plataformaNivel4 x y = 
  (x >= 8 && x <= 20 && y >= 45 && y <= 47) ||
  (x >= 28 && x <= 35 && y >= 45 && y <= 47) ||
  (x >= 43 && x <= 55 && y >= 45 && y <= 47)

paredeVerticalEsquerda :: Int -> Int -> Bool
paredeVerticalEsquerda x y = 
  x == 17 && y >= 18 && y <= 34

paredeVerticalDireita :: Int -> Int -> Bool
paredeVerticalDireita x y = 
  x == 46 && y >= 18 && y <= 34

paredeVerticalCentro :: Int -> Int -> Bool
paredeVerticalCentro x y = 
  x == 31 && y >= 28 && y <= 44

cameraSecretaEsquerda :: Int -> Int -> Bool
cameraSecretaEsquerda x y =
  x >= 3 && x <= 6 && y >= 48 && y <= 54 && dentroDaElipse (4, 51) (2, 3) (x, y)

cameraSecretaDireita :: Int -> Int -> Bool
cameraSecretaDireita x y =
  x >= 57 && x <= 60 && y >= 48 && y <= 54 && dentroDaElipse (58, 51) (2, 3) (x, y)

-- ============================================================================
-- FUNÇÕES AUXILIARES
-- ============================================================================

dentroDaElipse :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
dentroDaElipse (cx, cy) (rx, ry) (x, y) =
  let dx = fromIntegral (x - cx) :: Float
      dy = fromIntegral (y - cy) :: Float
      rxf = fromIntegral rx :: Float
      ryf = fromIntegral ry :: Float
  in (dx * dx) / (rxf * rxf) + (dy * dy) / (ryf * ryf) <= 1.0


