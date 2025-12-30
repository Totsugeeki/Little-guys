module Sprites where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Labs2025
import Control.Exception (catch, IOException)
import Types

-- ============================================================================
-- CONFIGURAÇÕES
-- ============================================================================

velocidadeAnimacao :: TipoAnimacao -> Float
velocidadeAnimacao Parado = 0.3
velocidadeAnimacao Andar = 0.1
velocidadeAnimacao Saltar = 0.15
velocidadeAnimacao Morrer = 0.2
velocidadeAnimacao Atirar = 0.1

velocidadeSelecao :: Float
velocidadeSelecao = 0.5

numFrames :: TipoAnimacao -> Int
numFrames Parado = 4
numFrames Andar = 6
numFrames Saltar = 4
numFrames Morrer = 8
numFrames Atirar = 4

-- ============================================================================
-- FUNÇÕES DE ANIMAÇÃO
-- ============================================================================

atualizarAnimacao :: Float -> AnimacaoMinhoca -> AnimacaoMinhoca
atualizarAnimacao dt anim
  -- Se a animação de morte já foi concluída, não atualizar mais
  | tipoAnimacao anim == Morrer && animacaoMorteConcluida anim = anim
  | otherwise =
      let novoTempo = tempoFrame anim + dt
          velocidade = velocidadeAnimacao (tipoAnimacao anim)
          totalFrames = numFrames (tipoAnimacao anim)
      in if novoTempo >= velocidade
         then 
           let proximoFrame = frameAtual anim + 1
           in if tipoAnimacao anim == Morrer && proximoFrame >= totalFrames
              -- Morte concluída - parar no último frame
              then anim 
                { frameAtual = totalFrames - 1
                , tempoFrame = novoTempo - velocidade
                , animacaoMorteConcluida = True
                }
              -- Outras animações fazem loop normal
              else anim 
                { frameAtual = proximoFrame `mod` totalFrames
                , tempoFrame = novoTempo - velocidade
                }
         else anim { tempoFrame = novoTempo }

atualizarAnimacaoSelecao :: Float -> AnimacaoSelecao -> AnimacaoSelecao
atualizarAnimacaoSelecao dt anim =
  let novoTempo = tempoSelecao anim + dt
  in if novoTempo >= velocidadeSelecao
     then anim 
       { frameSelecao = (frameSelecao anim + 1) `mod` 2  -- Alterna entre 0 e 1
       , tempoSelecao = novoTempo - velocidadeSelecao
       }
     else anim { tempoSelecao = novoTempo }

mudarAnimacao :: TipoAnimacao -> AnimacaoMinhoca -> AnimacaoMinhoca
mudarAnimacao novoTipo anim
  | tipoAnimacao anim == novoTipo = anim
  | tipoAnimacao anim == Morrer && animacaoMorteConcluida anim = anim 
  | otherwise = anim 
      { tipoAnimacao = novoTipo
      , frameAtual = 0
      , tempoFrame = 0
      , animacaoMorteConcluida = False
      }

virarSprite :: DirecaoSprite -> AnimacaoMinhoca -> AnimacaoMinhoca
virarSprite dir anim = anim { direcaoSprite = dir }

obterSpriteAtualPorEquipa :: SpritesEquipa -> AnimacaoMinhoca -> Picture
obterSpriteAtualPorEquipa spritesEq anim =
  let frames = case tipoAnimacao anim of
        Parado   -> spritesParado spritesEq
        Andar    -> spritesAndando spritesEq
        Saltar   -> spritesSaltando spritesEq
        Morrer   -> spritesMorrendo spritesEq
        Atirar   -> spritesAtirando spritesEq
      
      frame = frames !! min (frameAtual anim) (length frames - 1)
      
      frameVirado = case direcaoSprite anim of
        Direita -> frame
        Esquerda -> Scale (-1) 1 frame
  in frameVirado

-- Função auxiliar para obter os sprites da equipa correta
obterSpritesEquipa :: Sprites -> TeamColor -> SpritesEquipa
obterSpritesEquipa sprites PinkTeam = spritesPinkTeam sprites
obterSpritesEquipa sprites BlueTeam = spritesBlueTeam sprites
obterSpritesEquipa sprites WhiteTeam = spritesWhiteTeam sprites

obterSpriteSelecao :: Sprites -> AnimacaoSelecao -> Picture
obterSpriteSelecao sprites anim =
  let frames = spritesSelector sprites
  in if null frames
     then desenharSelecaoPlaceholder
     else frames !! (frameSelecao anim `mod` length frames)


determinarDirecaoSprite :: Direcao -> DirecaoSprite
determinarDirecaoSprite dir = case dir of
  Oeste -> Esquerda
  Noroeste -> Esquerda
  Sudoeste -> Esquerda
  _ -> Direita

-- ============================================================================
-- CARREGAR SPRITES PNG
-- ============================================================================

carregarPNG :: FilePath -> IO Picture
carregarPNG path = do
  resultado <- loadJuicyPNG path
  case resultado of
    Nothing -> do
      putStrLn $ "❌ Erro: " ++ path
      return Blank
    Just pic -> do
      putStrLn $ "✅ " ++ path
      return pic

carregarSpritesEquipe :: String -> IO SpritesEquipa
carregarSpritesEquipe personagem = do
  let base = "assets/sprites/" ++ personagem
  
  -- Idle
  parado1 <- carregarPNG $ base ++ "Idle/Idle1.png"
  parado2 <- carregarPNG $ base ++ "Idle/Idle2.png"
  parado3 <- carregarPNG $ base ++ "Idle/Idle3.png"
  parado4 <- carregarPNG $ base ++ "Idle/Idle4.png"
  
  -- Walking
  andar1 <- carregarPNG $ base ++ "Walking/Walk1.png"
  andar2 <- carregarPNG $ base ++ "Walking/Walk2.png"
  andar3 <- carregarPNG $ base ++ "Walking/Walk3.png"
  andar4 <- carregarPNG $ base ++ "Walking/Walk4.png"
  andar5 <- carregarPNG $ base ++ "Walking/Walk5.png"
  andar6 <- carregarPNG $ base ++ "Walking/Walk6.png"
  
  -- Jumping
  salto1 <- carregarPNG $ base ++ "Jumping/Jump1.png"
  salto2 <- carregarPNG $ base ++ "Jumping/Jump2.png"
  salto3 <- carregarPNG $ base ++ "Jumping/Jump3.png"
  salto4 <- carregarPNG $ base ++ "Jumping/Jump4.png"
  
  -- Dying
  morte1 <- carregarPNG $ base ++ "Death/Death1.png"
  morte2 <- carregarPNG $ base ++ "Death/Death2.png"
  morte3 <- carregarPNG $ base ++ "Death/Death3.png"
  morte4 <- carregarPNG $ base ++ "Death/Death4.png"
  morte5 <- carregarPNG $ base ++ "Death/Death5.png"
  morte6 <- carregarPNG $ base ++ "Death/Death6.png"
  morte7 <- carregarPNG $ base ++ "Death/Death7.png"
  morte8 <- carregarPNG $ base ++ "Death/Death8.png"

  -- Attacking
  tiro1 <- carregarPNG $ base ++ "Attack/Attack1.png"
  tiro2 <- carregarPNG $ base ++ "Attack/Attack2.png"
  tiro3 <- carregarPNG $ base ++ "Attack/Attack3.png"
  tiro4 <- carregarPNG $ base ++ "Attack/Attack4.png"
  
  return SpritesEquipa
    { spritesParado = [parado1, parado2, parado3, parado4]
    , spritesAndando = [andar1, andar2, andar3, andar4, andar5, andar6]
    , spritesSaltando = [salto1, salto2, salto3, salto4]
    , spritesMorrendo = [morte1, morte2, morte3, morte4, morte5, morte6, morte7, morte8]
    , spritesAtirando = [tiro1, tiro2, tiro3, tiro4]
    }

carregarSprites :: IO Sprites
carregarSprites = do
  putStrLn "🎨 Carregando sprites PNG para todas as equipas..."
  
  -- Carregar sprites para cada equipa
  pinkSprites <- carregarSpritesEquipe "PinkMonster/"
  blueSprites <- carregarSpritesEquipe "DudeMonster/"  
  whiteSprites <- carregarSpritesEquipe "OwlMonster/" 
  
  -- Worm Select
  select1 <- carregarPNG "assets/selector/selector0.png"
  select2 <- carregarPNG "assets/selector/selector1.png"
  
  dinamiteImg <- carregarPNG "assets/objects/dynamite.png"
  minaImg <- carregarPNG "assets/objects/mine.png"
  missilImg <- carregarPNG "assets/objects/missile.png"
  barrelImg <- carregarPNG "assets/objects/barril.png"
  
  putStrLn "✅ Sprites carregados para todas as equipas!"
  
  return Sprites
    { spritesPinkTeam = pinkSprites
    , spritesBlueTeam = blueSprites
    , spritesWhiteTeam = whiteSprites
    , spritesSelector = [select1, select2]
    , spriteDinamite = dinamiteImg
    , spriteMina = minaImg
    , spriteMissil = missilImg
    , spriteBarrel = barrelImg
    }

carregarSpritesComFallback :: IO Sprites
carregarSpritesComFallback = do
  putStrLn "🎮 Tentando carregar sprites..."
  carregarSprites `catch` \(e :: IOException) -> do
    putStrLn $ "⚠️  Erro: " ++ show e
    putStrLn "🎨 Usando placeholders..."
    return criarSpritesPlaceholder

criarSpritesPlaceholder :: Sprites
criarSpritesPlaceholder = 
  let pinkEq = SpritesEquipa
        { spritesParado = replicate 4 (Color (makeColor 1 0.2 0.2 1) $ circleSolid 20)
        , spritesAndando = replicate 6 (Color (makeColor 1 0.2 0.2 1) $ circleSolid 20)
        , spritesSaltando = replicate 4 (Color (makeColor 1 0.2 0.2 1) $ circleSolid 20)
        , spritesMorrendo = replicate 8 (Color (makeColor 1 0.2 0.2 1) $ circleSolid 20)
        , spritesAtirando = replicate 4 (Color (makeColor 1 0.2 0.2 1) $ circleSolid 20)
        }
      blueEq = SpritesEquipa
        { spritesParado = replicate 4 (Color (makeColor 0.2 0.4 1 1) $ circleSolid 20)
        , spritesAndando = replicate 6 (Color (makeColor 0.2 0.4 1 1) $ circleSolid 20)
        , spritesSaltando = replicate 4 (Color (makeColor 0.2 0.4 1 1) $ circleSolid 20)
        , spritesMorrendo = replicate 8 (Color (makeColor 0.2 0.4 1 1) $ circleSolid 20)
        , spritesAtirando = replicate 4 (Color (makeColor 0.2 0.4 1 1) $ circleSolid 20)
        }
      whiteEq = SpritesEquipa
        { spritesParado = replicate 4 (Color (makeColor 0.2 1 0.2 1) $ circleSolid 20)
        , spritesAndando = replicate 6 (Color (makeColor 0.2 1 0.2 1) $ circleSolid 20)
        , spritesSaltando = replicate 4 (Color (makeColor 0.2 1 0.2 1) $ circleSolid 20)
        , spritesMorrendo = replicate 8 (Color (makeColor 0.2 1 0.2 1) $ circleSolid 20)
        , spritesAtirando = replicate 4 (Color (makeColor 0.2 1 0.2 1) $ circleSolid 20)
        }
  in Sprites
      { spritesPinkTeam = pinkEq
      , spritesBlueTeam = blueEq
      , spritesWhiteTeam = whiteEq
      , spritesSelector = [desenharSelecaoPlaceholder, desenharSelecaoPlaceholder2]
      , spriteDinamite = desenharDinamitePlaceholder
      , spriteMina = desenharMinaPlaceholder
      , spriteMissil = desenharMissilPlaceholder
      , spriteBarrel = desenharBarrelPlaceholder
      }
      

desenharSelecaoPlaceholder :: Picture
desenharSelecaoPlaceholder = 
  Pictures
    [ Color yellow $ ThickCircle 18 3
    , Translate 0 25 $ Color yellow $ Polygon [(0, 10), (-5, 0), (5, 0)]
    ]

desenharSelecaoPlaceholder2 :: Picture
desenharSelecaoPlaceholder2 = 
  Pictures
    [ Color orange $ ThickCircle 15 3  
    , Translate 0 25 $ Color orange $ Polygon [(0, 10), (-5, 0), (5, 0)]
    ]

desenharDinamitePlaceholder :: Picture
desenharDinamitePlaceholder =
  Pictures
    [ Color red $ rectangleSolid 6 15
    , Translate 0 8 $ Color (makeColor 0.4 0.2 0.1 1) $ rectangleSolid 2 6
    , Translate 0 11 $ Color orange $ circleSolid 3
    ]

desenharMinaPlaceholder :: Picture
desenharMinaPlaceholder =
  Pictures
    [ Color black $ circleSolid 8
    , Color red $ circleSolid 4
    , Color yellow $ thickCircle 6 2
    ]

desenharMissilPlaceholder :: Picture
desenharMissilPlaceholder =
  Pictures
    [ Color red $ circleSolid 5
    , Color orange $ Polygon [(0, 0), (-8, 3), (-8, -3)]
    , Color yellow $ circleSolid 2
    ]

desenharBarrelPlaceholder :: Picture
desenharBarrelPlaceholder =
  Pictures
    [ Color (greyN 0.5) $ rectangleSolid 20 10   
    , Color black $ circleSolid 3         
    , Color (greyN 0.3) $ Translate (-7) 0 $ rectangleSolid 5 5 
    ]