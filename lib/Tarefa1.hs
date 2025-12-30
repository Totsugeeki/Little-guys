{-|
Module      : Tarefa1
Description : Modificação do mapa de jogo (destruição de terreno)

Este módulo implementa as funções correspondentes à **Tarefa 1** do projeto LI1 2025/26.

O objetivo desta tarefa é permitir a modificação do mapa, nomeadamente a destruição
de terreno em posições específicas, simulando o efeito de explosões ou escavações.
As funções aqui definidas lidam com a atualização do `Mapa`, assegurando que apenas
terrenos destrutíveis (como `Terra`) são removidos, substituindo-os por `Ar`.
-}
module Tarefa1 where

import Labs2025
import Tarefa0_2025
import Data.List (nub)

-- | Função principal da Tarefa 1. Recebe um estado e retorna se este é válido ou não.
validaEstado :: Estado -> Bool
validaEstado (Estado mapa objs worms) =
    validaMapa mapa &&
    validaMinhocas mapa worms &&
    validaObjetos (Estado mapa objs worms) && 
    semColisoesMinhocas worms &&
    semMinhocaEmBarril worms objs

-- Validação do mapa
validaMapa :: Mapa -> Bool
validaMapa [] = False
validaMapa linhas =
    not (null (head linhas)) &&
    allMesmaLargura linhas && 
    allTerrenosValidos linhas 

 -- Verifica se todas as linhas tem a mesma largura
allMesmaLargura :: [[a]] -> Bool
allMesmaLargura [] = True
allMesmaLargura (x:xs) = all ((== length x) . length) xs

-- Verifica se o mapa tem terreno validos
allTerrenosValidos :: Mapa -> Bool
allTerrenosValidos mapa = all linhaValida mapa
  where
    linhaValida :: [Terreno] -> Bool
    linhaValida = all terrenoValido

    terrenoValido :: Terreno -> Bool
    terrenoValido Ar    = True
    terrenoValido Agua  = True
    terrenoValido Terra = True
    terrenoValido Pedra = True 


-- Validação das minhocas
validaMinhocas :: Mapa -> [Minhoca] -> Bool
validaMinhocas mapa = all (validaMinhoca mapa)

-- valida o estado e posicao de uma minhoca individualmente
validaMinhoca :: Mapa -> Minhoca -> Bool
validaMinhoca mapa (Minhoca pos vida j e b m d) =
    all (>= 0) [j, e, b, m, d] &&
    case vida of
        Morta   -> True
        Viva v  -> v > 0 &&  v <= 100 && case pos of
            Nothing -> False
            Just p  -> posicaoDentroMapa mapa p && posicaoAr mapa p && ePosicaoMapaLivre p mapa

  
posicaoDentroMapa :: Mapa -> Posicao -> Bool
posicaoDentroMapa mapa (x, y) =
    let numLinhas = length mapa
        numColunas = if null mapa then 0 else length (head mapa)
    in x >= 0 && x < numLinhas && y >= 0 && y < numColunas

posicaoAr :: Mapa -> Posicao -> Bool
posicaoAr mapa (x,y) = case (mapa !! x) !! y of
  Ar -> True
  _ -> False



semColisoesMinhocas :: [Minhoca] -> Bool
semColisoesMinhocas ms =
  let posicoes = [ p | Minhoca (Just p) (Viva _) _ _ _ _ _ <- ms ]
  in length posicoes == length (nub posicoes)

semMinhocaEmBarril :: [Minhoca] -> [Objeto] -> Bool
semMinhocaEmBarril ms objs =
  all (\m -> case posicaoMinhoca m of
               Nothing -> True
               Just p  -> not (existeBarril p objs)
      ) ms

existeBarril :: Posicao -> [Objeto] -> Bool
existeBarril p = any (\o -> case o of
                              Barril p' _ -> p == p'
                              _           -> False)


validaObjetos :: Estado -> Bool
validaObjetos (Estado mapa objs ms) =
  all (validaObjeto mapa objs ms) objs &&
  disparosValidos objs ms mapa

-- | Valida um objeto individual com todas as regras
validaObjeto :: Mapa -> [Objeto] -> [Minhoca] -> Objeto -> Bool
validaObjeto mapa objs ms obj =
  posicaoDentroMapa mapa (posicaoObjeto obj) &&
  ePosicaoMapaLivre (posicaoObjeto obj) mapa &&
  regrasColisao
  where
    regrasColisao =
      case obj of
        Barril p _ ->
          not (qualquerOutroBarril p obj objs) &&
          not (minhocaNaPos p ms)

        Disparo _ _ Mina _ _ ->
          not (qualquerBarrilNaPos  obj objs)

        Disparo _ _ Dinamite _ _ ->
          not (qualquerBarrilNaPos obj objs)

        _ -> True

posicaoObjeto :: Objeto -> Posicao
posicaoObjeto obj = case obj of
    Disparo {posicaoDisparo = p} -> p
    Barril  {posicaoBarril  = p} -> p


-- | Checa se algum outro barril ocupa a posição
qualquerOutroBarril :: Posicao -> Objeto -> [Objeto] -> Bool
qualquerOutroBarril pos self objs =
    any (\o -> case o of
                 Barril p _ -> p == pos && o /= self
                 _          -> False) objs

-- | Checa se algum barril ocupa a posição
qualquerBarrilNaPos :: Objeto -> [Objeto] -> Bool
qualquerBarrilNaPos obj objs =
    let pos = case obj of
                Disparo p _ _ _ _ -> p
                Barril p _        -> p
                _                 -> (-1,-1) -- não usado
    in any (\o -> case o of
                    Barril p _ -> p == pos
                    _          -> False) objs

-- | Checa se alguma minhoca viva ocupa a posição
minhocaNaPos :: Posicao -> [Minhoca] -> Bool
minhocaNaPos pos = any (\m -> case posicaoMinhoca m of
                                Just p  -> p == pos && vidaMinhoca m /= Morta
                                Nothing -> False)


disparosValidos :: [Objeto] -> [Minhoca] -> Mapa -> Bool
disparosValidos objs ms mapa =
  all (validaDisparo ms mapa objs) disparos &&
  donosUnicos disparos
  where
    disparos = [ d | d@(Disparo _ _ _ _ _) <- objs ]

validaDisparo :: [Minhoca] -> Mapa -> [Objeto] -> Objeto -> Bool
validaDisparo ms mapa objs (Disparo pos dir arma tempo dono) =
  donoValido &&
  armaValida &&
  tempoValido &&
  posicaoDisparoValida
  where
    donoValido =
      dono >= 0 && dono < length ms

    armaValida =
      arma /= Jetpack && arma /= Escavadora

    tempoValido =
      case arma of
        Bazuca   -> tempo == Nothing
        Mina     -> tempo == Nothing || tempo `elem` map Just [0..2]
        Dinamite -> tempo `elem` map Just [0..4]
        _        -> True

    posicaoDisparoValida =
      case arma of
        Bazuca -> bazucaPodePerfura
        _      -> case encontraPosicaoMatriz pos mapa of
                    Just t  -> not (eTerrenoOpaco t)
                    Nothing -> False  -- posição inválida não é considerada válida

    bazucaPodePerfura =
      let pAnterior = movePosicao (opposite dir) pos
      in case encontraPosicaoMatriz pAnterior mapa of
          Just t  -> not (eTerrenoOpaco t)
          Nothing -> False

opposite :: Direcao -> Direcao
opposite dir = case dir of
    Norte     -> Sul
    Sul       -> Norte
    Este      -> Oeste
    Oeste     -> Este
    Nordeste  -> Sudoeste
    Sudoeste  -> Nordeste
    Noroeste  -> Sudeste
    Sudeste   -> Noroeste

donosUnicos :: [Objeto] -> Bool
donosUnicos objs = all temApenasUmDisparo [(d, a) | d <- donos, a <- armas]
  where
    -- Lista de todos os donos únicos de disparos
    donos = nub [d | Disparo {donoDisparo = d} <- objs]

    -- Tipos de arma que queremos verificar
    armas = [Bazuca, Mina, Dinamite]

    -- Função que conta quantos disparos de um certo dono com uma certa arma existem
    countDisparos :: NumMinhoca -> TipoArma -> Int
    countDisparos d a = length $ filter isDisparoDaArma objs
      where
        isDisparoDaArma (Disparo {donoDisparo = d', tipoDisparo = t}) = d' == d && t == a
        isDisparoDaArma _ = False

    -- Retorna True se há exatamente um disparo desse dono com essa arma
    temApenasUmDisparo (d, a) = countDisparos d a == 1
