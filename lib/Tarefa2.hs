{-|
Module      : Tarefa2
Description : Efetuar jogadas no estado do jogo.

Este módulo implementa as funções da **Tarefa 2** do projeto LI1 2025/26.

A Tarefa 2 é responsável por aplicar uma jogada a uma minhoca específica dentro do estado do jogo.
As jogadas podem ser movimentos (`Move`) ou disparos de armas (`Dispara`).
A função principal `efetuaJogada` é o ponto de entrada para atualizar o `Estado`
consoante a ação escolhida.
-}
module Tarefa2 where

import Labs2025
import Tarefa0_2025
import Data.Bool (Bool)

-- | Função principal da Tarefa 2.
--
-- Recebe:
--
-- * o índice da minhoca (`NumMinhoca`) na lista de minhocas;
-- * uma jogada (`Jogada`);
-- * e o estado atual (`Estado`).
--
-- Retorna um novo estado em que a jogada foi executada.
--
-- Casos principais:
--
-- * `Move dir` — move a minhoca viva e dentro do mapa;
-- * `Dispara arma dir` — cria um novo disparo e reduz a munição.

efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n jogada estado@(Estado _ _ minhocas) =
  case minhocas !! n of
    m@(Minhoca { posicaoMinhoca = Just pos, vidaMinhoca = Viva _ }) ->
      case jogada of
        -- ============================================================
        --  MOVIMENTAÇÃO
        -- ============================================================
        Move dir ->
          let novaPos = movePosicao dir pos
          in movimentaMinhoca n m pos novaPos estado

        -- ============================================================
        --  DISPARO
        -- ============================================================
        Dispara arma dir ->
          disparaArma n m pos arma dir estado

    _ -> estado -- Minhoca morta ou fora do mapa → nada acontece


-- ============================================================
-- AUXILIAR: MOVIMENTAR MINHOCA
-- ============================================================
movimentaMinhoca :: NumMinhoca -> Minhoca -> Posicao -> Posicao -> Estado -> Estado
movimentaMinhoca n m (x,y) novaPos estado@(Estado mapa objetos minhocas)
  | not (ePosicaoMatrizValida novaPos mapa) =
      let nova = m { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
          novas = atualizaLista n nova minhocas
      in estado { minhocasEstado = novas }

  | otherwise =
      -- só move se a posição estiver livre (não há objetos nem barris)
      if not (posicaoLivre novaPos mapa objetos)
         then estado
         else
           case encontraPosicaoMatriz novaPos mapa of
             Just Agua ->
               let nova = m { posicaoMinhoca = Just novaPos, vidaMinhoca = Morta }
                   novas = atualizaLista n nova minhocas
               in estado { minhocasEstado = novas }

             Just Ar ->
               if estaNoAr (x,y) mapa
                  then estado -- não se pode mover no ar
                else
                  let nova = m { posicaoMinhoca = Just novaPos }
                      novas = atualizaLista n nova minhocas
                  in estado { minhocasEstado = novas }
     

-- Função auxiliar para detectar se o movimento é para cima
movParaCima :: Posicao -> Posicao -> Bool
movParaCima (x1,y1) (x2,y2) = x2 < x1

-- Verifica se a posição está livre de objetos (barril ou disparos)
posicaoLivreObjetos :: Posicao -> [Objeto] -> Bool
posicaoLivreObjetos pos objs = not $ any (posicaoOcupada pos) objs

-- Função que verifica se um objeto ocupa a posição dada
posicaoOcupada :: Posicao -> Objeto -> Bool
posicaoOcupada pos obj =
  case obj of
    Barril posB _          -> pos == posB
    Disparo posD _ _ _ _   -> pos == posD
    -- Não existem construtores Mina ou Dinamite em Objeto, então não os colocamos aqui
    _                      -> False

-- Função para verificar se existe uma mina numa posição (objeto Disparo com tipo Mina)
posicaoTemMina :: Posicao -> [Objeto] -> Bool
posicaoTemMina pos objs =
  any isMina objs
  where
    isMina (Disparo posD _ tipo _ _) = posD == pos && tipo == Mina
    isMina _ = False

-- Função principal que verifica se uma posição está livre no mapa e sem objetos (barril ou disparos)
posicaoLivre :: Posicao -> Mapa -> [Objeto] -> Bool
posicaoLivre pos mapa objs =
  ePosicaoMapaLivre pos mapa && posicaoLivreObjetos pos objs && not (posicaoTemMina pos objs) 


-- ============================================================
-- AUXILIAR: DISPARAR ARMAS
-- ============================================================
disparaArma :: NumMinhoca -> Minhoca -> Posicao -> TipoArma -> Direcao -> Estado -> Estado
disparaArma n m pos arma dir estado@(Estado mapa objetos minhocas)
  | not (temMunicao arma m) = estado
  | not (podeDisparar arma n objetos) = estado  
  | otherwise =
      case arma of
        Jetpack ->
          let novaPos = movePosicao dir pos
          in if ePosicaoMatrizValida novaPos mapa
                && ePosicaoEstadoLivre novaPos estado
                then
                  let nova  = gastaMunicao arma (m { posicaoMinhoca = Just novaPos })
                      novas = atualizaLista n nova minhocas
                  in estado { minhocasEstado = novas }
                else estado
        Escavadora ->
          let novaPos = movePosicao dir pos
          in if not (ePosicaoMatrizValida novaPos mapa)
                then estado
                else case encontraPosicaoMatriz novaPos mapa of
                  Just Terra ->
                    if posicaoLivreObjetos novaPos objetos && not (posicaoTemMina novaPos objetos)
                      then
                        let mapa' = atualizaPosicaoMatriz novaPos Ar mapa
                            nova  = gastaMunicao arma m { posicaoMinhoca = Just novaPos }
                            novas = atualizaLista n nova minhocas
                        in estado { mapaEstado = mapa', minhocasEstado = novas }
                      else estado 
                  Just Ar ->
                    if posicaoLivreObjetos novaPos objetos && not (posicaoTemMina novaPos objetos)
                      then
                        let nova  = gastaMunicao arma m { posicaoMinhoca = Just novaPos }
                            novas = atualizaLista n nova minhocas
                        in estado { minhocasEstado = novas }
                      else estado
                  Just Pedra -> estado  -- Ne peut pas détruire la pierre
                  Just Agua -> 
                    let nova = gastaMunicao arma m { posicaoMinhoca = Just novaPos, vidaMinhoca = Morta }
                        novas = atualizaLista n nova minhocas
                    in estado { minhocasEstado = novas }
                  Nothing -> estado


        Bazuca ->
          let posDisparo = movePosicao dir pos
              disp = Disparo posDisparo dir arma Nothing n
              nova = gastaMunicao arma m
              novas = atualizaLista n nova minhocas
          in if ePosicaoMatrizValida posDisparo mapa
                then estado { objetosEstado = disp : objetos, minhocasEstado = novas }
                else estado -- fora do mapa, ignora

        Mina ->
          let posDisparo = movePosicao dir pos
              finalPos = if posicaoLivre posDisparo mapa objetos then posDisparo else pos
              disp = Disparo finalPos dir arma Nothing n
              nova = gastaMunicao arma m
              novas = atualizaLista n nova minhocas
          in estado { objetosEstado = disp : objetos, minhocasEstado = novas }


        Dinamite ->
          let posDisparo = movePosicao dir pos
              finalPos = if posicaoLivre posDisparo mapa objetos then posDisparo else pos
              disp = Disparo finalPos dir arma (Just 4) n
              nova = gastaMunicao arma m
              novas = atualizaLista n nova minhocas
          in estado { objetosEstado = disp : objetos, minhocasEstado = novas }
          
podeDisparar :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
podeDisparar arma dono objs =
  not $ any (\o -> case o of
                     Disparo { tipoDisparo = a, donoDisparo = d } -> a == arma && d == dono
                     _ -> False) objs


-- ============================================================
-- AUXILIAR: DETEÇÃO DE "NO AR"
-- ============================================================
estaNoAr :: Posicao -> Mapa -> Bool
estaNoAr (x,y) mapa = x+1 < length mapa && encontraPosicaoMatriz (x+1,y) mapa == Just Ar
-- | Substitui o elemento da lista no índice indicado por um novo valor.
atualizaLista :: Int -> a -> [a] -> [a]
atualizaLista _ _ [] = []
atualizaLista 0 novo (_:xs) = novo : xs
atualizaLista n novo (x:xs) = x : atualizaLista (n - 1) novo xs


-- | Verifica se a minhoca tem munição disponível para a arma indicada.
temMunicao :: TipoArma -> Minhoca -> Bool
temMunicao arma m =
  case arma of
    Jetpack   -> jetpackMinhoca m   > 0
    Escavadora -> escavadoraMinhoca m > 0
    Bazuca    -> bazucaMinhoca m    > 0
    Mina      -> minaMinhoca m      > 0
    Dinamite  -> dinamiteMinhoca m  > 0


-- | Define o tempo padrão (em ticks) para o disparo de cada arma.
tempoPadrao :: TipoArma -> Maybe Ticks
tempoPadrao Jetpack   = Nothing     -- Jetpack não é disparo explosivo
tempoPadrao Escavadora = Nothing    -- Escavadora também não
tempoPadrao Bazuca    = Just 5
tempoPadrao Mina      = Just 10
tempoPadrao Dinamite  = Just 8


-- | Diminui a munição da arma indicada em uma unidade.
gastaMunicao :: TipoArma -> Minhoca -> Minhoca
gastaMunicao arma m =
  case arma of
    Jetpack   -> m { jetpackMinhoca = max 0 (jetpackMinhoca m - 1) }
    Escavadora -> m { escavadoraMinhoca = max 0 (escavadoraMinhoca m - 1) }
    Bazuca    -> m { bazucaMinhoca = max 0 (bazucaMinhoca m - 1) }
    Mina      -> m { minaMinhoca = max 0 (minaMinhoca m - 1) }
    Dinamite  -> m { dinamiteMinhoca = max 0 (dinamiteMinhoca m - 1) }
