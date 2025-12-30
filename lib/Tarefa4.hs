{-|
Module      : Tarefa4
Description : Implementar uma tática de jogo.

Módulo para a realização da Tarefa 4 de LI1\/LP1 em 2025\/26.
-}
module Tarefa4 where

import Data.Either

import Labs2025
import Tarefa2
import Tarefa3
import Data.List (minimumBy)

-- | Função principal da Tarefa 4. Dado um estado retorna uma lista de jogadas, com exatamente 100 jogadas.
tatica :: Estado -> [(NumMinhoca,Jogada)]
tatica e = reverse $ snd $ foldl avancaTatica (e,[]) [0..99]

-- | Aplica uma sequência de jogadas a um estado, avançando o tempo entre jogadas.
avancaTatica :: (Estado,[(NumMinhoca,Jogada)]) -> Ticks -> (Estado,[(NumMinhoca,Jogada)])
avancaTatica (e,js) tick = (avancaJogada j e,j:js)
    where j = jogadaTatica tick e

-- | Aplica uma jogada de uma minhoca a um estado, e avança o tempo.
avancaJogada :: (NumMinhoca,Jogada) -> Estado -> Estado
avancaJogada (i,j) e@(Estado _ objetos minhocas) = foldr aplicaDanos e'' danoss''
    where
    e'@(Estado mapa' objetos' minhocas') = efetuaJogada i j e
    minhocas'' = map (avancaMinhocaJogada e') (zip3 [0..] minhocas minhocas')
    (objetos'',danoss'') = partitionEithers $ map (avancaObjetoJogada (e' { minhocasEstado = minhocas''}) objetos) (zip [0..] objetos')
    e'' = Estado mapa' objetos'' minhocas''

-- | Avança o tempo para o estado de uma minhoca, se não efetuou a última jogada.
avancaMinhocaJogada :: Estado -> (NumMinhoca,Minhoca,Minhoca) -> Minhoca
avancaMinhocaJogada e (i,minhoca,minhoca') = if posicaoMinhoca minhoca == posicaoMinhoca minhoca'
    then avancaMinhoca e i minhoca'
    else minhoca'

-- | Avança o tempo para o estado de um objeto, se não foi criado pela última jogada.
avancaObjetoJogada :: Estado -> [Objeto] -> (NumObjeto,Objeto) -> Either Objeto Danos
avancaObjetoJogada e objetos (i,objeto') = if elem objeto' objetos
    then avancaObjeto e i objeto'
    else Left objeto'

-- | Para um número de ticks desde o início da tática, dado um estado, determina a próxima jogada.
jogadaTatica :: Ticks -> Estado -> (NumMinhoca, Jogada)
jogadaTatica t (Estado _ _ minhocas)
  | null vivos = (0, Move Este)
  | otherwise  = (i, jogada)
  where
    -- minhocas vivas e posicionadas
    vivos =
      [ (i,m)
      | (i,m) <- zip [0..] minhocas
      , vidaMinhoca m /= Morta
      , posicaoMinhoca m /= Nothing
      ]

    (i,m) = vivos !! (t `mod` length vivos)
    Just (x,y) = posicaoMinhoca m

    -- inimigos vivos
    inimigos =
      [ p
      | m' <- minhocas
      , m' /= m
      , vidaMinhoca m' /= Morta
      , Just p <- [posicaoMinhoca m']
      ]

    -- distância Manhattan
    dist (ix,iy) = abs (ix-x) + abs (iy-y)

    inimigoMaisProximo =
      case inimigos of
        [] -> Nothing
        _  -> Just (minimumBy (\a b -> compare (dist a) (dist b)) inimigos)

    -- direção até um ponto
    dirPara (ix,iy)
      | abs (ix-x) > abs (iy-y) =
          if ix > x then Sul else Norte
      | otherwise =
          if iy > y then Este else Oeste

    dirFuga (ix,iy)
      | ix > x    = Norte
      | ix < x    = Sul
      | iy > y    = Oeste
      | otherwise = Este

    jogada =
      case inimigoMaisProximo of
        Nothing ->
          Move Este

        Just p ->
          let d = dist p
              dirAtaque = dirPara p
              dirEscape = dirFuga p
          in
            if d <= 2 then
              Move dirEscape                     -- foge se estiver muito perto
            else if d <= 4 then
              Dispara Dinamite dirAtaque         -- curto alcance
            else if d <= 7 then
              Dispara Bazuca dirAtaque           -- médio alcance
            else
              Move dirAtaque                     -- aproxima
