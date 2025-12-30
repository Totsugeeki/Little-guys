{-|
Module      : Tarefa3
Description : Avançar o tempo do jogo.

Implementa a **Tarefa 3** do projeto LI1 2025/26.

Esta tarefa é responsável por simular a passagem do tempo no jogo,
atualizando a posição das minhocas (gravidade), movimentando objetos
como disparos e aplicando danos resultantes de explosões.
-}

module Tarefa3 where

import Data.Either

import Labs2025
import Tarefa0_2025


type Dano = Int
type Danos = [(Posicao,Dano)]

-- | Função principal da Tarefa 3.
--
-- Avança o estado do jogo em um tick:
--
-- * move as minhocas afetadas pela gravidade;
-- * atualiza os objetos (disparos, barris, etc.);
-- * aplica os danos resultantes de explosões.
avancaEstado :: Estado -> Estado
avancaEstado e@(Estado mapa objetos minhocas) = foldr aplicaDanos e' danoss
    where
    minhocas' = map (uncurry $ avancaMinhoca e) (zip [0..] minhocas)
    (objetos',danoss) = partitionEithers $ map (uncurry $ avancaObjeto $ e { minhocasEstado = minhocas' }) (zip [0..] objetos)
    e' = Estado mapa objetos' minhocas'

------------------------------------------------------------
-- MINHOCAS
------------------------------------------------------------
-- | Atualiza a posição e estado de uma minhoca.
--
-- A minhoca cai se houver ar por baixo e morre se cair em água.
avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca (Estado _ _ _) _ m@(Minhoca Nothing _ _ _ _ _ _) = m
avancaMinhoca (Estado mapa _ _) _ m@(Minhoca (Just (x,y)) vida _ _ _ _ _)
  | vida == Morta = m
  | not (ePosicaoMatrizValida (x+1, y) mapa) =
      m { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
  | terrenoAbaixo == Ar =
      m { posicaoMinhoca = Just (x+1, y) }
  | terrenoAbaixo == Agua =
      m { posicaoMinhoca = Just (x+1, y), vidaMinhoca = Morta }
  | terrenoAtual == Agua =
      m { vidaMinhoca = Morta }
  | otherwise = m
  where
    terrenoAbaixo
      | x+1 >= length mapa = Ar 
      | otherwise = (mapa !! (x+1)) !! y
    terrenoAtual = (mapa !! x) !! y


posicaoTemMinhoca :: Posicao -> NumMinhoca -> [Minhoca] -> Bool
posicaoTemMinhoca pos donoDisparo minhocas =
  any (\(i, m) -> i /= donoDisparo && posicaoMinhoca m == Just pos && vidaMinhoca m /= Morta)
      (zip [0..] minhocas)


-- CORREÇÃO: Adicionar verificação de barril
posicaoTemBarril :: Posicao -> [Objeto] -> Bool
posicaoTemBarril pos objetos =
  any (\obj -> case obj of
                 Barril p _ -> p == pos
                 _          -> False) objetos


temColisao :: Posicao -> NumMinhoca -> Mapa -> [Minhoca] -> [Objeto] -> Bool
temColisao pos dono mapa minhocas objetos =
  not (ePosicaoMatrizValida pos mapa) ||
  not (ePosicaoMapaLivre pos mapa) ||
  posicaoTemMinhoca pos dono minhocas ||
  posicaoTemBarril pos objetos  -- CORREÇÃO: Verificar barris

------------------------------------------------------------
-- OBJETOS
------------------------------------------------------------

-- | Atualiza o estado de um objeto num tick.
avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos

-- Bazuca
avancaObjeto (Estado mapa objetos minhocas) _ d@Disparo
  { posicaoDisparo = pos
  , direcaoDisparo = dir
  , tipoDisparo    = Bazuca
  , donoDisparo    = dono
  } =
  let novaPos = movePosicao dir pos
  in if temColisao novaPos dono mapa minhocas objetos  -- CORREÇÃO: Passar objetos
     then Right (geraDanos pos 5 mapa)
     else Left d { posicaoDisparo = novaPos }

-- Dinamite - CORREÇÃO: Aumentar distância de queda
avancaObjeto (Estado mapa _ _) _ d@Disparo
  { posicaoDisparo = pos
  , direcaoDisparo = dir
  , tipoDisparo    = Dinamite
  , tempoDisparo   = t
  } =
  case t of
    Just 0 -> Right (geraDanos pos 7 mapa)
    Just n ->
      let novaDir = Sul
          -- CORREÇÃO: Tentar cair até 3 blocos se possível
          possivelPos = tentarCair pos mapa 3
      in Left d { posicaoDisparo = possivelPos, direcaoDisparo = novaDir, tempoDisparo = Just (n-1) }
    Nothing -> Left d

-- Mina 
avancaObjeto (Estado mapa _ minhocas) _ d@Disparo
  { posicaoDisparo = (x, y)
  , tipoDisparo    = Mina
  , tempoDisparo   = t
  , donoDisparo    = dono
  } =
    case t of
      -- Explode
      Just 0 ->
        Right (geraDanos (x, y) 3 mapa)

      -- Countdown
      Just n ->
        Left d { tempoDisparo = Just (n - 1) }

      -- Mina ainda não ativada
      Nothing ->
        if existeMinhocaNaoDono (x, y) dono minhocas
          then Left d { tempoDisparo = Just 2 }
          else
            -- Movimento da mina
            if terrenoAbaixo == Ar || terrenoAbaixo == Agua
              then Left d { posicaoDisparo = (x + 1, y) }
              else Left d
  where
    terrenoAbaixo
      | x + 1 >= length mapa = Ar
      | y < 0 || y >= length (head mapa) = Ar
      | otherwise = (mapa !! (x + 1)) !! y

-- Barril
avancaObjeto (Estado mapa _ _) _ b@(Barril pos explodir)
  | explodir  = Right (geraDanos pos 5 mapa)
  | otherwise = Left b { explodeBarril = estaNoArOuAgua pos mapa }

-- Outros
avancaObjeto _ _ obj = Left obj


existeMinhocaNaoDono :: Posicao -> NumMinhoca -> [Minhoca] -> Bool
existeMinhocaNaoDono pos dono minhocas =
  any (\(i,m) -> posicaoMinhoca m == Just pos && i /= dono && vidaMinhoca m /= Morta)
      (zip [0..] minhocas)


estaNoArOuAgua :: Posicao -> Mapa -> Bool
estaNoArOuAgua (x,y) mapa
  | not (ePosicaoMatrizValida (x+1,y) mapa) = False
  | otherwise =
      let terreno = (mapa !! (x+1)) !! y
      in terreno == Ar || terreno == Agua

------------------------------------------------------------
-- EXPLOSÕES E DANOS
------------------------------------------------------------


geraDanos :: Posicao -> Int -> Mapa -> Danos
geraDanos (x, y) diam mapa =
  [ ((x + dx, y + dy), calculaDano dist)
  | dx <- [-r..r]
  , dy <- [-r..r]
  , let dist = distancia dx dy
  , dist <= fromIntegral r  
  , ePosicaoMatrizValida (x + dx, y + dy) mapa
  ]
  where
    r = diam `div` 2
    
    distancia :: Int -> Int -> Float
    distancia dx dy = sqrt (fromIntegral (dx*dx + dy*dy))
 
    calculaDano :: Float -> Int
    calculaDano dist
      | dist <= 0.5 = diam * 10       
      | dist <= fromIntegral r / 2 = (diam - 1) * 10 
      | otherwise = (diam - 2) * 10   


-- CORREÇÃO: Fazer barris explodirem quando recebem dano
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos danos (Estado mapa objetos minhocas) =
  Estado mapa' objetos' minhocas'
  where
    -- Destruir terreno
    mapa' = foldr destroi mapa (map fst danos)

    destroi pos m =
      case encontraPosicaoMatriz pos m of
        Just Terra -> atualizaPosicaoMatriz pos Ar m
        _          -> m

    -- CORREÇÃO: Ativar barris que recebem dano
    objetos' = map ativarBarril objetos
    
    ativarBarril obj =
      case obj of
        Barril pos explodir ->
          if any (\(danoPos, _) -> danoPos == pos) danos
            then Barril pos True  -- Ativar explosão
            else obj
        _ -> obj

    -- Aplicar dano às minhocas
    minhocas' = map aplica minhocas

    aplica m =
      case posicaoMinhoca m of
        Nothing -> m
        Just p  ->
          let total = sum [ d | (pos,d) <- danos, pos == p ]
          in if total == 0
             then m  
             else case vidaMinhoca m of
                   Viva v -> if total >= v
                               then m { vidaMinhoca = Morta }
                               else m { vidaMinhoca = Viva (v - total) }
                   Morta -> m

-- Função auxiliar para fazer a dinamite cair mais longe
tentarCair :: Posicao -> Mapa -> Int -> Posicao
tentarCair pos@(x,y) mapa maxDist
  | maxDist <= 0 = pos
  | not (ePosicaoMatrizValida (x+1,y) mapa) = pos
  | not (ePosicaoMapaLivre (x+1,y) mapa) = pos
  | otherwise = tentarCair (x+1,y) mapa (maxDist-1)
