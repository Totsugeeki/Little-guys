module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as Set
import Worms
import Labs2025
import Tarefa2
import Types 
import Sprites
import Camera 

-- | Função que altera o estado do jogo no Gloss.
reageEventos :: Event -> Worms -> Worms

-- ============================================================================
-- MENU PRINCIPAL
-- ============================================================================

reageEventos (EventKey (SpecialKey KeySpace) Down _ _) Menu =
  TeamSetupMenu initialTeamConfigExtended
  

-- ============================================================================
-- MENU DE CONFIGURAÇÂO DE EQUIPAS
-- ============================================================================

reageEventos (EventKey (MouseButton LeftButton) Down _ pos) (TeamSetupMenu extended) =
  case handleTeamSetupClick extended pos of
    Left newExtended -> TeamSetupMenu newExtended  -- CContinua no menu
    Right gameState -> gameState -- Start

reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (TeamSetupMenu _) =
  Menu

-- ============================================================================
-- JOGO - SETTINGS
-- ============================================================================

reageEventos (EventKey (SpecialKey KeyTab) Down _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam config) = 
  SettingsMenu (Jogo e num weapon t updt anims animSelecao dir keys cam config)

reageEventos (EventKey (SpecialKey KeyTab) Down _ _) (SettingsMenu game) =
  game

reageEventos _ (SettingsMenu game) =
  SettingsMenu game

-- ============================================================================
-- JOGO - CONTROLE DE CAM
-- ============================================================================

reageEventos (EventKey (MouseButton WheelUp) Down _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam@(Camera _ _ zoomAtual _ _ _) config) =
  Jogo e num weapon t updt anims animSelecao dir keys (setCameraZoom (zoomAtual + 0.5) cam) config
  
reageEventos (EventKey (MouseButton WheelDown) Down _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam@(Camera _ _ zoomAtual _ _ _) config) =
  Jogo e num weapon t updt anims animSelecao dir keys (setCameraZoom (zoomAtual - 0.5) cam) config

reageEventos (EventKey (Char 'r') Down _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam config) =
  Jogo e num weapon t updt anims animSelecao dir keys (resetCameraZoom cam) config

-- ============================================================================
-- JOGO - MOVIMENTOS
-- ============================================================================

-- OESTE
reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (Jogo e num weapon t updt anims animSelecao _ keys cam config) =
  let novoEstado = efetuaJogada num (Move Oeste) e
      novasAnims = atualizarAnimIdx num Andar Esquerda anims
  in Jogo novoEstado num weapon t updt novasAnims animSelecao Oeste (Set.insert CKeyLeft keys) cam config

reageEventos (EventKey (SpecialKey KeyLeft) Up _ _) (Jogo e num weapon t updt anims animSelecao _ keys cam config) =
  let novasAnims = atualizarAnimIdx num Parado Esquerda anims
  in Jogo e num weapon t updt novasAnims animSelecao Oeste (Set.delete CKeyLeft keys) cam config

-- ESTE
reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (Jogo e num weapon t updt anims animSelecao _ keys cam config) =
  let novoEstado = efetuaJogada num (Move Este) e
      novasAnims = atualizarAnimIdx num Andar Direita anims
  in Jogo novoEstado num weapon t updt novasAnims animSelecao Este (Set.insert CKeyRight keys) cam config

reageEventos (EventKey (SpecialKey KeyRight) Up _ _) (Jogo e num weapon t updt anims animSelecao _ keys cam config) =
  let novasAnims = atualizarAnimIdx num Parado Direita anims
  in Jogo e num weapon t updt novasAnims animSelecao Este (Set.delete CKeyRight keys) cam config

-- NORTE
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (Jogo e num weapon t updt anims animSelecao _ keys cam config) =
  let novoEstado = efetuaJogada num (Move Norte) e
      novasAnims = atualizarAnimIdx num Saltar Direita anims
  in Jogo novoEstado num weapon t updt novasAnims animSelecao Norte (Set.insert CKeyUp keys) cam config

reageEventos (EventKey (SpecialKey KeyUp) Up _ _) (Jogo e num weapon t updt anims animSelecao _ keys cam config) =
  let novasAnims = atualizarAnimIdx num Saltar Direita anims
  in Jogo e num weapon t updt novasAnims animSelecao Norte (Set.delete CKeyUp keys) cam config

-- SUL
reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (Jogo e num weapon t updt anims animSelecao _ keys cam config) =
  let novoEstado = efetuaJogada num (Move Sul) e
      novasAnims = atualizarAnimIdx num Andar Direita anims
  in Jogo novoEstado num weapon t updt novasAnims animSelecao Sul (Set.insert CKeyDown keys) cam config

reageEventos (EventKey (SpecialKey KeyDown) Up _ _) (Jogo e num weapon t updt anims animSelecao _ keys cam config) =
  let novasAnims = atualizarAnimIdx num Parado Direita anims
  in Jogo e num weapon t updt novasAnims animSelecao Sul (Set.delete CKeyDown keys) cam config

-- ============================================================================
-- JOGO - MUDAR MINHOCA (MUDA PARA PRÓXIMA EQUIPA)
-- ============================================================================

reageEventos (EventKey (SpecialKey KeySpace) Down _ _) (Jogo e num weapon _ updt anims animSelecao dir keys cam config) =
  changeWormController (Jogo e num weapon roundTimer updt anims animSelecao dir keys cam config)

-- ============================================================================
-- JOGO - ESCOLHER ARMAS
-- ============================================================================

reageEventos (EventKey (Char '1') Down _ _) (Jogo e num _ t updt anims animSelecao dir keys cam config) = 
  Jogo e num Jetpack t updt anims animSelecao dir keys cam config

reageEventos (EventKey (Char '2') Down _ _) (Jogo e num _ t updt anims animSelecao dir keys cam config) = 
  Jogo e num Escavadora t updt anims animSelecao dir keys cam config

reageEventos (EventKey (Char '3') Down _ _) (Jogo e num _ t updt anims animSelecao dir keys cam config) = 
  Jogo e num Bazuca t updt anims animSelecao dir keys cam config

reageEventos (EventKey (Char '4') Down _ _) (Jogo e num _ t updt anims animSelecao dir keys cam config) = 
  Jogo e num Mina t updt anims animSelecao dir keys cam config

reageEventos (EventKey (Char '5') Down _ _) (Jogo e num _ t updt anims animSelecao dir keys cam config) = 
  Jogo e num Dinamite t updt anims animSelecao dir keys cam config

-- ============================================================================
-- JOGO - DISPARAR
-- ============================================================================

reageEventos (EventKey (MouseButton LeftButton) Down _ posRato) (Jogo e num weapon t updt anims animSelecao dir keys cam config) =
  case weapon of
    Jetpack -> Jogo e num weapon t updt anims animSelecao dir keys cam config
    Escavadora -> Jogo e num weapon t updt anims animSelecao dir keys cam config
    _ ->
      case obtainWormPosition e num of
        Just posMinhoca ->
          let mapa = mapaEstado e
              altura = length mapa
              largura = if altura > 0 then length (head mapa) else 0
              posGame = screenToGame posRato largura altura  
              direction = calcDirection posMinhoca posGame
              newState = efetuaJogada num (Dispara weapon direction) e
              novasAnims = atualizarAnimIdx num Atirar (determinarDirecaoSprite dir) anims 
          in changeWormController (Jogo newState num weapon roundTimer updt novasAnims animSelecao dir keys cam config)
        Nothing -> Jogo e num weapon t updt anims animSelecao dir keys cam config

-- ============================================================================
-- JOGO - DISPARAR JETPACK/ESCAVADORA (WASD)
-- ============================================================================

-- W (Norte)
reageEventos (EventKey (Char 'w') Down _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam config)
  | weapon == Jetpack || weapon == Escavadora =
    let novasAnims = atualizarAnimIdx num Saltar Direita anims
    in Jogo e num weapon t updt novasAnims animSelecao Norte (Set.insert CKeyW keys) cam config
  | otherwise = Jogo e num weapon t updt anims animSelecao dir keys cam config

reageEventos (EventKey (Char 'w') Up _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam config) =
  Jogo e num weapon t updt anims animSelecao dir (Set.delete CKeyW keys) cam config

-- A (Oeste)
reageEventos (EventKey (Char 'a') Down _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam config)
  | weapon == Jetpack || weapon == Escavadora =
    let novasAnims = atualizarAnimIdx num Saltar Esquerda anims
    in Jogo e num weapon t updt novasAnims animSelecao Oeste (Set.insert CKeyA keys) cam config
  | otherwise = Jogo e num weapon t updt anims animSelecao dir keys cam config

reageEventos (EventKey (Char 'a') Up _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam config) =
  Jogo e num weapon t updt anims animSelecao dir (Set.delete CKeyA keys) cam config

-- S (Sul)
reageEventos (EventKey (Char 's') Down _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam config)
  | weapon == Jetpack || weapon == Escavadora =
    let novasAnims = atualizarAnimIdx num Saltar Direita anims
    in Jogo e num weapon t updt novasAnims animSelecao Sul (Set.insert CKeyS keys) cam config
  | otherwise = Jogo e num weapon t updt anims animSelecao dir keys cam config

reageEventos (EventKey (Char 's') Up _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam config) =
  Jogo e num weapon t updt anims animSelecao dir (Set.delete CKeyS keys) cam config

-- D (Este)
reageEventos (EventKey (Char 'd') Down _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam config)
  | weapon == Jetpack || weapon == Escavadora =
    let novasAnims = atualizarAnimIdx num Saltar Direita anims
    in Jogo e num weapon t updt novasAnims animSelecao Este (Set.insert CKeyD keys) cam config
  | otherwise = Jogo e num weapon t updt anims animSelecao dir keys cam config

reageEventos (EventKey (Char 'd') Up _ _) (Jogo e num weapon t updt anims animSelecao dir keys cam config) =
  Jogo e num weapon t updt anims animSelecao dir (Set.delete CKeyD keys) cam config

-- ============================================================================
-- DEFAULT
-- ============================================================================

reageEventos _ w = w

-- ============================================================================
-- FUNÇÃES AUXILIARES
-- ============================================================================

atualizarAnimIdx :: Int -> TipoAnimacao -> DirecaoSprite -> [AnimacaoMinhoca] -> [AnimacaoMinhoca]
atualizarAnimIdx idx tipo dir anims =
  take idx anims ++ 
  [virarSprite dir $ mudarAnimacao tipo (anims !! idx)] ++ 
  drop (idx + 1) anims