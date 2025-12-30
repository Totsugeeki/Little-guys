module Tempo where

import Labs2025
import Worms
import Tarefa2
import Tarefa3
import Types 
import Sprites 
import Camera
import qualified Data.Set as Set

type Segundos = Float

-- Função antiga (para compatibilidade)
reageTempo :: Float -> Worms -> Worms
reageTempo dt w = reageTempoComAnimacoes undefined dt w

-- NOVA FUNÇÂO COM ANIMAÇÃES E EQUIPAS
reageTempoComAnimacoes :: Sprites -> Float -> Worms -> Worms
reageTempoComAnimacoes _ _ Menu = Menu
reageTempoComAnimacoes _ _ (TeamSetupMenu config) = TeamSetupMenu config
reageTempoComAnimacoes _ _ (WinScreen winner) = WinScreen winner
reageTempoComAnimacoes _ _ (SettingsMenu game) = SettingsMenu game
reageTempoComAnimacoes _ dt jogo@(Jogo e num weapon t counter anims animSelecao _ keys cam config)
  -- Verificar condição de vitória
  | Just winner <- checkWinCondition e config = WinScreen winner
  
  -- Tempo acabou - mudar para próxima equipa
  | t <= 0 = changeWormController jogo
  
  -- Atualizar animações e câmera
  | counter + dt < 0.1 = 
      let 
          mapa = mapaEstado e
          altura = length mapa
          largura = length (head mapa)
          minhocaAtual = minhocasEstado e !! num
          
          camAtualizada = updateCamera dt cam minhocaAtual largura altura
          
      in jogo { updateCounter = counter + dt
              , roundTimeLeft = t - dt
              , animacoes = map (atualizarAnimacao dt) anims 
              , animacaoSelecao = atualizarAnimacaoSelecao dt animSelecao
              , camera = camAtualizada  
              }
  
  -- Atualizar física do jogo
  | otherwise =
      let 
          direction = getDirection keys weapon
          
          -- Aplicar movimento/disparo baseado na direção
          e' = case direction of
                Just d -> 
                  case weapon of
                    Jetpack -> efetuaJogada num (Dispara Jetpack d) e
                    Escavadora -> efetuaJogada num (Dispara Escavadora d) e
                    _ -> efetuaJogada num (Move d) e 
                Nothing -> e
          
          -- Aplicar gravidade
          eDepoisGravidade = avancaEstado e'
          
          -- Restaurar posição da minhoca ao usar Jetpack
          novoEstado = case (weapon, num, direction) of
            (Jetpack, n, Just _) -> restauraMinhoca n (minhocasEstado e') eDepoisGravidade
            _ -> eDepoisGravidade
          
          -- Atualizar animações de todas as minhocas
          animsAtualizadas = zipWith atualizarAnim (minhocasEstado novoEstado) anims
          
          atualizarAnim :: Minhoca -> AnimacaoMinhoca -> AnimacaoMinhoca
          atualizarAnim m anim =
            let novoTipo = determinarTipoAnimacao m (mapaEstado novoEstado)
                animComTipo = mudarAnimacao novoTipo anim
            in atualizarAnimacao dt animComTipo

          novaAnimSelecao = atualizarAnimacaoSelecao dt animSelecao

          mapa = mapaEstado novoEstado
          altura = length mapa
          largura = length (head mapa)
          minhocaAtual = minhocasEstado novoEstado !! num
          camAtualizada = updateCamera dt cam minhocaAtual largura altura
      
      in jogo { estadoJogo = novoEstado
              , roundTimeLeft = t - dt
              , updateCounter = 0
              , animacoes = animsAtualizadas
              , animacaoSelecao = novaAnimSelecao
              , camera = camAtualizada
              }

restauraMinhoca :: Int -> [Minhoca] -> Estado -> Estado
restauraMinhoca n minhocasAntes (Estado m o minhocas) =
  let minhocas' = take n minhocas ++ [minhocasAntes !! n] ++ drop (n+1) minhocas
  in Estado m o minhocas'

-- Determinar que animação mostrar
determinarTipoAnimacao :: Minhoca -> Mapa -> TipoAnimacao
determinarTipoAnimacao (Minhoca pos vida _ _ _ _ _) mapa
  | vida == Morta = Morrer
  | otherwise = case pos of
      Nothing -> Morrer
      Just (lin, col) ->
        if estaNoAr (lin, col) mapa
        then Saltar
        else Parado

getDirection :: Set.Set CustomKey -> TipoArma -> Maybe Direcao
getDirection keys weapon
  | weapon == Jetpack || weapon == Escavadora =
      let w = Set.member CKeyW keys
          a = Set.member CKeyA keys
          s = Set.member CKeyS keys
          d = Set.member CKeyD keys
      in case (w, a, s, d) of
           (True, True, False, False) -> Just Noroeste
           (True, False, False, True) -> Just Nordeste
           (False, True, True, False) -> Just Sudoeste
           (False, False, True, True) -> Just Sudeste
           (True, False, False, False) -> Just Norte
           (False, True, False, False) -> Just Oeste
           (False, False, True, False) -> Just Sul
           (False, False, False, True) -> Just Este
           _ -> Nothing
  
  | otherwise =
      let up = Set.member CKeyUp keys
          down = Set.member CKeyDown keys
          left = Set.member CKeyLeft keys
          right = Set.member CKeyRight keys
      in case (up, down, left, right) of
           (True, False, True, False) -> Just Noroeste
           (True, False, False, True) -> Just Nordeste
           (False, True, True, False) -> Just Sudoeste
           (False, True, False, True) -> Just Sudeste
           (True, False, False, False) -> Just Norte
           (False, False, True, False) -> Just Oeste
           (False, True, False, False) -> Just Sul
           (False, False, False, True) -> Just Este
           _ -> Nothing