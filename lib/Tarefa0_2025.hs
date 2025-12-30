{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-|
Module      : Tarefa0_2025
Description : Funções auxiliares.

Módulo que define funções auxiliares que serão úteis na resolução do trabalho prático de LI1\/LP1 em 2025\/26.
-}

-- | Este módulo 
module Tarefa0_2025 where

import Labs2025


-- | Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.
encontraQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int
encontraQuantidadeArmaMinhoca Jetpack    = jetpackMinhoca
encontraQuantidadeArmaMinhoca Escavadora = escavadoraMinhoca
encontraQuantidadeArmaMinhoca Bazuca     = bazucaMinhoca
encontraQuantidadeArmaMinhoca Mina       = minaMinhoca
encontraQuantidadeArmaMinhoca Dinamite   = dinamiteMinhoca


-- | Atualia a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca Jetpack minhoca qnt = minhoca { jetpackMinhoca = qnt }
atualizaQuantidadeArmaMinhoca Escavadora minhoca qnt = minhoca { escavadoraMinhoca = qnt }
atualizaQuantidadeArmaMinhoca Bazuca minhoca qnt = minhoca { bazucaMinhoca = qnt }
atualizaQuantidadeArmaMinhoca Mina minhoca qnt = minhoca { minaMinhoca = qnt }
atualizaQuantidadeArmaMinhoca Dinamite minhoca qnt = minhoca { dinamiteMinhoca = qnt}

-- | Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
--
-- __NB:__ Apenas @Terra@ é destrutível.
eTerrenoDestrutivel :: Terreno -> Bool
eTerrenoDestrutivel t = t == Terra



-- | Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
--
-- __NB:__ Apenas @Terra@ ou @Pedra@ são opacos.
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco terreno =
  case terreno of
    Terra -> True
    Pedra -> True
    _     -> False

-- | Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se não contiver um terreno opaco.
ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre p m =
    case encontraPosicaoMatriz p m of
        Just terreno -> terreno /= Terra && terreno /= Pedra
        Nothing      -> False

-- | Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se o mapa estiver livre e se não estiver já uma minhoca ou um barril nessa posição.
ePosicaoEstadoLivre :: Posicao -> Estado -> Bool
ePosicaoEstadoLivre pos estado =
  ePosicaoMapaLivre pos (mapaEstado estado)
  && not (posicaoOcupadaPorObjeto pos (objetosEstado estado))
  && not (posicaoOcupadaPorMinhoca pos (minhocasEstado estado))
  where
    -- Verifica se algum objeto ocupa a posição
    posicaoOcupadaPorObjeto :: Posicao -> [Objeto] -> Bool
    posicaoOcupadaPorObjeto p = any (\obj ->
      case obj of
        Disparo {posicaoDisparo = pd} -> pd == p
        Barril {posicaoBarril = pb}   -> pb == p)

    -- Verifica se alguma minhoca viva está na posição
    posicaoOcupadaPorMinhoca :: Posicao -> [Minhoca] -> Bool
    posicaoOcupadaPorMinhoca p = any (\m ->
      case posicaoMinhoca m of
        Just pm -> pm == p && estaViva (vidaMinhoca m)
        Nothing -> False)

    -- Minhoca viva se o campo vidaMinhoca for do tipo Viva
    estaViva :: VidaMinhoca -> Bool
    estaViva (Viva _) = True
    estaViva Morta    = False

-- | Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
minhocaTemDisparo arma numMinhoca =
  any ehDisparoDaMinhoca
  where
    ehDisparoDaMinhoca (Disparo _ _ tipo _ dono) =
      tipo == arma && dono == numMinhoca
    ehDisparoDaMinhoca _ = False

-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
--
-- __NB__: Só terrenos @Terra@ pode ser destruídos.
destroiPosicao :: Posicao -> Mapa -> Mapa
destroiPosicao p mapa =
  case encontraPosicaoMatriz p mapa of
    Just Terra -> atualizaPosicaoMatriz p Ar mapa
    _          -> mapa


-- Adiciona um novo objeto a um estado.
--
-- __NB__: A posição onde é inserido não é relevante.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj estado =
  estado { objetosEstado = obj : objetosEstado estado }



