module Types where

import Graphics.Gloss

-- ============================================================================
-- TYPES FOR TEAMS
-- ============================================================================

data TeamColor = PinkTeam | BlueTeam | WhiteTeam
  deriving (Show, Eq)

-- ============================================================================
-- TYPES FOR ANIMATIONS
-- ============================================================================

data AnimacaoMinhoca = AnimacaoMinhoca
  { tipoAnimacao :: TipoAnimacao
  , frameAtual :: Int
  , tempoFrame :: Float
  , direcaoSprite :: DirecaoSprite
  , animacaoMorteConcluida :: Bool
  }
  deriving (Show, Eq)

data AnimacaoSelecao = AnimacaoSelecao
  { frameSelecao :: Int
  , tempoSelecao :: Float
  }
  deriving (Show, Eq)

data TipoAnimacao
  = Parado
  | Andar
  | Saltar
  | Morrer
  | Atirar
  deriving (Show, Eq)

data DirecaoSprite = Esquerda | Direita
  deriving (Show, Eq)

-- ============================================================================
-- TYPES FOR SPRITES
-- ============================================================================

data SpritesEquipa = SpritesEquipa
  { spritesParado :: [Picture]
  , spritesAndando :: [Picture]
  , spritesSaltando :: [Picture]
  , spritesMorrendo :: [Picture]
  , spritesAtirando :: [Picture]
  }


data Sprites = Sprites
  { spritesPinkTeam :: SpritesEquipa
  , spritesBlueTeam :: SpritesEquipa
  , spritesWhiteTeam :: SpritesEquipa
  , spritesSelector :: [Picture]
  , spriteDinamite :: Picture
  , spriteMina :: Picture
  , spriteMissil :: Picture
  , spriteBarrel :: Picture
  }


novaAnimacao :: AnimacaoMinhoca
novaAnimacao = AnimacaoMinhoca
  { tipoAnimacao = Parado
  , frameAtual = 0
  , tempoFrame = 0
  , direcaoSprite = Direita
  , animacaoMorteConcluida = False
  }

novaAnimacaoSelecao :: AnimacaoSelecao
novaAnimacaoSelecao = AnimacaoSelecao
  { frameSelecao = 0
  , tempoSelecao = 0
  }
