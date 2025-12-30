module Desenhar where

import Graphics.Gloss
import Worms
import Labs2025
import Data.Int ()
import Types 
import Sprites
import Camera 

-- ============================================================================
-- CONFIGURAÇÂO
-- ============================================================================
windowWidth, windowHeight :: Float
windowWidth = 1920
windowHeight = 1080

hudMargin :: Float
hudMargin = 10

-- ============================================================================
-- TEXTURAS
-- ============================================================================

data Texturas = Texturas
  { fundoTex :: Picture
  , aguaTex :: Picture
  , terraTex :: Picture
  , pedraTex :: Picture
  , selecaoTex :: Picture  
  }
data UI = UI
  { menuUI :: Picture
  , settingsUI :: Picture
  , now1 :: Picture
  , now2 :: Picture
  , now3 :: Picture
  , now4 :: Picture
  , now5 :: Picture
  , map1 :: Picture
  , map2 :: Picture
  , map3 :: Picture
  , players2map1 :: Picture
  , players2map2 :: Picture
  , players2map3 :: Picture
  , players3map1 :: Picture
  , players3map2 :: Picture
  , players3map3 :: Picture
  , pinkwinner :: Picture
  , bluewinner :: Picture
  , whitewinner :: Picture
  }

bg :: Color
bg = black

matrizParaPixel :: Int -> Int -> Int -> Int -> (Float, Float)
matrizParaPixel col lin largura altura = (xPixel, yPixel)
  where
    xPixel = fromIntegral col * tamanhoBlocos
             - fromIntegral largura * tamanhoBlocos / 2
             + tamanhoBlocos / 2
    yPixel = fromIntegral altura * tamanhoBlocos / 2
             - fromIntegral lin * tamanhoBlocos
             - tamanhoBlocos / 2

-- ============================================================================
-- MENUS
-- ============================================================================

desenharMenu :: UI -> Picture
desenharMenu  ui = menuUI ui

desenharTeamSetupVisual :: UI -> TeamConfigExtended -> Picture
desenharTeamSetupVisual ui extended =
  let config = baseConfig extended
      wormsNum = wormsPerTeam config
      playersPic = numberOfPlayersUI ui extended
      wormsPic = drawNowButton ui wormsNum
  in Pictures [playersPic, wormsPic]

numberOfPlayersUI :: UI -> TeamConfigExtended -> Picture
numberOfPlayersUI ui extended = 
  let config = baseConfig extended
      playersNum = numTeams config 
      mapSelect = selectedMap extended
  in  imgBgChoosing ui mapSelect playersNum
   
imgBgChoosing :: UI -> Int -> Int -> Picture
imgBgChoosing ui map players = case players of
  2 -> case map of 
    1 -> players2map1 ui
    2 -> players2map2 ui
    3 -> players2map3 ui
    _ -> players2map1 ui
  3 -> case map of 
    1 -> players3map1 ui
    2 -> players3map2 ui
    3 -> players3map3 ui
    _ -> players3map1 ui
  _ -> players2map1 ui
  
drawNowButton :: UI -> Int -> Picture
drawNowButton ui numWorm =
  let nowBarX = 500
      nowBarY = -190
      nowBarScale = 1.5
  in Translate nowBarX nowBarY $ Scale nowBarScale nowBarScale $
       case numWorm of
         1 -> now1 ui
         2 -> now2 ui
         3 -> now3 ui
         4 -> now4 ui
         5 -> now5 ui
         _ -> now1 ui


desenharPreviewEquipas :: TeamConfig -> [Picture]
desenharPreviewEquipas config =
  let teamsList = teams config
      startY = -100
      spacing = 50
  in zipWith (desenharTeamPreview startY spacing) [0..] teamsList

desenharTeamPreview :: Float -> Float -> Int -> Team -> Picture
desenharTeamPreview startY spacing idx team =
  let y = startY - fromIntegral idx * spacing
      (r, g, b) = getTeamColor team
      cor = makeColor r g b 1.0
      numWorms = length (teamWormIndices team)
  in Pictures
      [ Translate (-200) y $ Color cor $ rectangleSolid 30 30
      , Translate (-150) (y - 10) $ Scale 0.2 0.2 $ Color white $ 
          Text (teamName team)
      , Translate (100) (y - 10) $ Scale 0.15 0.15 $ Color (greyN 0.8) $ 
          Text (show numWorms ++ " worms")
      ]

desenharSettings :: UI -> Picture
desenharSettings ui = settingsUI ui
-- ============================================================================
-- TELA DE VITORIA
-- ============================================================================

desenharWinScreen :: UI -> Team -> Picture
desenharWinScreen ui winner =
  case teamColor winner of 
    PinkTeam -> pinkwinner ui
    BlueTeam -> bluewinner ui
    WhiteTeam -> whitewinner ui
    _ -> pinkwinner ui

-- ============================================================================
-- DESENHO PRINCIPAL
-- ============================================================================

desenhaComTexturas :: Texturas -> UI -> Sprites -> Worms -> Picture
desenhaComTexturas _ ui _ Menu = desenharMenu ui
desenhaComTexturas _ ui _ (TeamSetupMenu extended) = desenharTeamSetupVisual ui extended
desenhaComTexturas _ ui _ (SettingsMenu _) = desenharSettings ui
desenhaComTexturas _ ui _ (WinScreen winner) = desenharWinScreen ui winner
desenhaComTexturas tex _ spr (Jogo e num arma t _ anims animSelecao _ _ cam config) = 
    Pictures
      [ applyCameraTransform cam $ desenharMundoComTexturasESprites tex spr e anims animSelecao num config
      , desenharHUDCompleto e num arma t config
      ]

desenharMundoComTexturasESprites :: Texturas -> Sprites -> Estado -> [AnimacaoMinhoca] -> AnimacaoSelecao -> NumMinhoca -> TeamConfig -> Picture
desenharMundoComTexturasESprites tex spr e@(Estado mapa objetos _) anims animSelecao num config =
    Pictures
      [ desenharFundo tex
      , desenharMapaComTexturas tex (mapaEstado e)
      , Pictures $ drawObjects spr objetos altura largura
      , desenharMinhocasComSprites altura largura spr e anims config 
      , desenharSelecaoMinhocaComSprite altura largura spr animSelecao minhocaAtual
      ]
    where
      altura = length mapa
      largura = length (head mapa)
      minhocasAtivas = minhocasEstado e
      minhocaAtual = minhocasAtivas !! num

-- ============================================================================
-- HUD COMPLETO COM EQUIPAS E MUNIÇÃES
-- ============================================================================

desenharHUDCompleto :: Estado -> NumMinhoca -> TipoArma -> Float -> TeamConfig -> Picture
desenharHUDCompleto estado num arma timer config =
  Pictures
    [ -- Barra superior escura
      Translate 0 (windowHeight/2 - 70) $
        Color (makeColor 0 0 0 0.7) $
          rectangleSolid windowWidth 200
    
    -- Elementos do HUD com posições fixas e espaçamento adequado
    , desenharTempoHUD timer
    , desenharArmaHUDCompleta arma (minhocasEstado estado !! num)
    , desenharMinhocaHUDComEquipa estado num config
    , desenharTeamsStatus estado config
    ]

-- Timer no canto superior esquerdo
desenharTempoHUD :: Float -> Picture
desenharTempoHUD timer =
  let x = -windowWidth/2 + 100
      y = windowHeight/2 - 65
      timeStr = show (floor timer :: Int) ++ "s"
      timeColor = if timer > 10 then white else red
  in Pictures
      [ Translate x y $ Scale 0.2 0.2 $ Color (greyN 0.7) $ Text "TIME"
      , Translate (x - 10) (y - 35) $ Scale 0.35 0.35 $ Color timeColor $ Text timeStr
      ]

-- Arma no centro-esquerda da barra superior
desenharArmaHUDCompleta :: TipoArma -> Minhoca -> Picture
desenharArmaHUDCompleta arma minhoca =
  let x = -300
      y = windowHeight/2 - 65
      (armaStr, municoes) = case arma of
        Bazuca -> ("BAZOOKA", bazucaMinhoca minhoca)
        Jetpack -> ("JETPACK", jetpackMinhoca minhoca)
        Escavadora -> ("DRILL", escavadoraMinhoca minhoca)
        Mina -> ("MINE", minaMinhoca minhoca)
        Dinamite -> ("DYNAMITE", dinamiteMinhoca minhoca)
        _ -> (show arma, 0)
      
      municoesStr = if municoes > 999 
                    then "Infinite" 
                    else show municoes
      
      corMunicoes = if municoes == 0 then red
                    else if municoes < 3 then yellow
                    else green
  in Pictures
      [ Translate x y $ Scale 0.2 0.2 $ Color (greyN 0.7) $ Text "WEAPON"
      , Translate (x - 20) (y - 30) $ Scale 0.25 0.25 $ Color orange $ Text armaStr
      , Translate (x + 150) y $ Scale 0.15 0.15 $ Color (greyN 0.7) $ Text "AMMO:"
      , Translate (x + 210) (y - 5) $ Scale 0.3 0.3 $ Color corMunicoes $ Text municoesStr
      ]

-- Info da minhoca no canto superior direito
desenharMinhocaHUDComEquipa :: Estado -> NumMinhoca -> TeamConfig -> Picture
desenharMinhocaHUDComEquipa (Estado _ _ minhocas) num config =
  let x = windowWidth/2 - 250
      y = windowHeight/2 - 65
      minhoca = minhocas !! num
      
      vida = case vidaMinhoca minhoca of
        Viva v -> v
        Morta -> 0
      vidaCor = if vida > 70 then green
                else if vida > 30 then yellow
                else red
      
      teamInfo = case getWormTeam config num of
        Just team -> 
          let (r, g, b) = getTeamColor team
              teamCor = makeColor r g b 1.0
          in (teamName team, teamCor)
        Nothing -> ("No Team", white)
      
      (teamNome, teamCor) = teamInfo

  in Pictures
      [ -- Team info
        Translate x (y + 15) $ Scale 0.15 0.15 $ Color (greyN 0.7) $ Text "TEAM"
        , Translate (x - 10) (y - 10) $ Scale 0.22 0.22 $ Color teamCor $ Text teamNome
      
        -- HP info
        , Translate x (y - 45) $ Scale 0.15 0.15 $ Color (greyN 0.7) $ Text "HP"
        , Translate (x + 35) (y - 50) $ Scale 0.25 0.25 $ Color vidaCor $ Text (show vida)
        , desenharBarraVida (x + 75) (y - 70) vida
      ]

desenharBarraVida :: Float -> Float -> Int -> Picture
desenharBarraVida x y vida =
  let larguraBarra = 100
      alturaBarra = 18
      larguraVida = larguraBarra * (fromIntegral vida / 100)
      corVida = if vida > 70 then green
                else if vida > 30 then yellow
                else red
  in Pictures
      [ Translate x y $ Color (greyN 0.3) $ rectangleSolid larguraBarra alturaBarra
      , Translate (x - larguraBarra/2 + larguraVida/2) y $ 
          Color corVida $ rectangleSolid larguraVida alturaBarra
      , Translate x y $ Color white $ rectangleWire larguraBarra alturaBarra
      ]

-- Status das equipas no canto inferior esquerdo
desenharTeamsStatus :: Estado -> TeamConfig -> Picture
desenharTeamsStatus estado config =
  let x = -windowWidth/2 + 30
      y = -windowHeight/2 + 180
      allTeams = teams config
  in Pictures $ zipWith (desenharTeamStatus estado x y) [0..] allTeams
  where
    desenharTeamStatus :: Estado -> Float -> Float -> Int -> Team -> Picture
    desenharTeamStatus e baseX baseY idx team =
      let yPos = baseY - fromIntegral idx * 45
          (r, g, b) = getTeamColor team
          cor = makeColor r g b 1.0
          wormIndices = teamWormIndices team
          aliveCount = length $ filter (\i -> not (isDead (minhocasEstado e !! i))) wormIndices
          totalCount = length wormIndices
          statusStr = show aliveCount ++ "/" ++ show totalCount
      in Pictures
          [ -- Quadrado colorido da equipa
            Translate baseX yPos $ Color cor $ rectangleSolid 25 25
          , Translate baseX yPos $ Color white $ rectangleWire 25 25
          
          -- Nome da equipa
          , Translate (baseX + 20) (yPos - 10) $ Scale 0.15 0.15 $ Color white $ 
              Text (teamName team)
          
          -- Contador de worms vivos/total
          , Translate (baseX + 130) (yPos - 10) $ Scale 0.2 0.2 $ 
              Color (if aliveCount > 0 then green else red) $ Text statusStr
          ]
-- ============================================================================
-- SELEÇÂO DE MINHOCA COM PNG
-- ============================================================================

desenharSelecaoMinhocaComSprite :: Int -> Int -> Sprites -> AnimacaoSelecao -> Minhoca -> Picture
desenharSelecaoMinhocaComSprite altura largura sprites anim (Minhoca pos _ _ _ _ _ _) =
  case pos of
    Nothing -> Blank
    Just (lin, col) ->
      let (px, py) = matrizParaPixel col lin largura altura
          indicador = obterSpriteSelecao sprites anim
          escala = 1.5
          alturaSprite = 32 
      in Translate px (py + (alturaSprite * escala / 2) - tamanhoBlocos/2) (Scale escala escala indicador)

-- ============================================================================
-- DESENHAR MAPA E TERRENO
-- ============================================================================

desenharFundo :: Texturas -> Picture
desenharFundo tex =
    Scale escalaX escalaY (fundoTex tex)
  where
    escalaX = 1.5
    escalaY = 1.5

desenharMapaComTexturas :: Texturas -> Mapa -> Picture
desenharMapaComTexturas tex mapa =
  Pictures
    [ Translate x y (desenharTerrenoComTextura tex terreno)
    | (lin, linha) <- zip [0..] mapa
    , (col, terreno) <- zip [0..] linha
    , let (x,y) = matrizParaPixel col lin largura altura
    ]
  where
    altura = length mapa
    largura = length (head mapa)

desenharTerrenoComTextura :: Texturas -> Terreno -> Picture
desenharTerrenoComTextura _ Ar = Blank
desenharTerrenoComTextura tex Agua = Scale escala escala (aguaTex tex)
  where escala = tamanhoBlocos / 128  
desenharTerrenoComTextura tex Terra = Scale escala escala (terraTex tex)
  where escala = tamanhoBlocos / 128  
desenharTerrenoComTextura tex Pedra = Scale escala escala (pedraTex tex)
  where escala = tamanhoBlocos / 128  

-- ============================================================================
-- DESENHAR MINHOCAS COM SPRITES
-- ============================================================================

desenharMinhocasComSprites :: Int -> Int -> Sprites -> Estado -> [AnimacaoMinhoca] -> TeamConfig -> Picture
desenharMinhocasComSprites altura largura sprites estado anims config =
  Pictures (zipWith3 (desenharMinhocaComSprite altura largura sprites config) 
                     (minhocasEstado estado) 
                     anims 
                     [0..])

desenharMinhocaComSprite :: Int -> Int -> Sprites -> TeamConfig -> Minhoca -> AnimacaoMinhoca -> Int -> Picture
desenharMinhocaComSprite altura largura sprites config (Minhoca pos status _ _ _ _ _) anim wormIdx
  -- Se está morta mas a animação ainda não terminou, mostra a animação
  | status == Morta && not (animacaoMorteConcluida anim) = 
      case pos of
        Nothing -> Blank
        Just (lin,col) ->
          let (px,py) = matrizParaPixel col lin largura altura
              spritesEq = case getWormTeam config wormIdx of
                Just team -> obterSpritesEquipa sprites (teamColor team)
                Nothing -> spritesPinkTeam sprites
              sprite = obterSpriteAtualPorEquipa spritesEq anim
              alturaSprite = 32 
              escala = 1.5
          in Translate px (py + (alturaSprite * escala / 2) - tamanhoBlocos/2) $ 
             Scale escala escala sprite
  
  -- Se a animação de morte já terminou, não desenha
  | status == Morta && animacaoMorteConcluida anim = Blank
  
  -- Minhoca viva - desenha normalmente
  | otherwise = case pos of
      Nothing -> Blank
      Just (lin,col) ->
        let (px,py) = matrizParaPixel col lin largura altura
            spritesEq = case getWormTeam config wormIdx of
              Just team -> obterSpritesEquipa sprites (teamColor team)
              Nothing -> spritesPinkTeam sprites
            sprite = obterSpriteAtualPorEquipa spritesEq anim
            alturaSprite = 32 
            escala = 1.5
        in Translate px (py + (alturaSprite * escala / 2) - tamanhoBlocos/2) $ 
           Scale escala escala sprite
-- ============================================================================
-- DESENHAR OBJETOS
-- ============================================================================

drawObjects :: Sprites -> [Objeto] -> Int -> Int -> [Picture]
drawObjects sprites objs altura largura = map (drawObject sprites altura largura) objs

drawObject :: Sprites -> Int -> Int -> Objeto -> Picture
drawObject sprites altura largura (Disparo pos dir tipo _ _) = 
  drawShot sprites pos dir tipo altura largura
drawObject sprites altura largura (Barril pos explode) = 
  drawBarrelWithSprite sprites pos explode altura largura

drawShot :: Sprites -> Posicao -> Direcao -> TipoArma -> Int -> Int -> Picture
drawShot sprites (lin, col) _ Mina altura largura =
  let (posX, posY) = matrizParaPixel col lin largura altura
      escala = 0.1
  in Translate posX posY $ Scale escala escala $ drawProjectile sprites Mina

drawShot sprites (lin, col) dir tipo altura largura =
  let (posX, posY) = matrizParaPixel col lin largura altura
      rotation = directionToAngle dir
      escala = 0.05  
  in Translate posX posY $ Rotate rotation $ Scale escala escala $ drawProjectile sprites tipo


drawProjectile :: Sprites -> TipoArma -> Picture
drawProjectile sprites Bazuca = spriteMissil sprites

drawProjectile sprites Mina = spriteMina sprites

drawProjectile sprites Dinamite = spriteDinamite sprites

drawProjectile _ Jetpack = 
  Pictures
    [ Color blue $ rectangleSolid 10 6
    , Color cyan $ circleSolid 3
    , Translate 0 (-5) $ Color cyan $ circleSolid 3
    ]

drawProjectile _ Escavadora = 
  Pictures
    [ Color brown $ circleSolid 8
    , Color black $ Polygon [(8, 0), (12, 3), (12, -3)]
    , Color darkGray $ circleSolid 4
    ]
  where
    darkGray = makeColor 0.3 0.3 0.3 1
    brown = makeColor 0.6 0.4 0.2 1


drawBarrelWithSprite :: Sprites -> Posicao -> Bool -> Int -> Int -> Picture
drawBarrelWithSprite sprites (lin, col) explode altura largura =
  let (posX, posY) = matrizParaPixel col lin largura altura
      escala = 0.1
  in if explode
     then drawBarrelExplosion posX posY
     else Translate posX posY $ Scale escala escala $ spriteBarrel sprites
  where
    drawBarrelExplosion x y =
      Pictures
        [ -- Explosão principal
          Translate x y $ Color orange $ circleSolid 40
        , Translate x y $ Color red $ circleSolid 30
        , Translate x y $ Color yellow $ circleSolid 20
        , Translate x y $ Color white $ circleSolid 10
        
        -- Partículas da explosão
        , Translate (x + 20) (y + 20) $ Color orange $ circleSolid 12
        , Translate (x - 20) (y + 20) $ Color red $ circleSolid 12
        , Translate (x + 20) (y - 20) $ Color yellow $ circleSolid 12
        , Translate (x - 20) (y - 20) $ Color orange $ circleSolid 12
        
        , Translate (x + 30) y $ Color red $ circleSolid 8
        , Translate (x - 30) y $ Color orange $ circleSolid 8
        , Translate x (y + 30) $ Color yellow $ circleSolid 8
        , Translate x (y - 30) $ Color red $ circleSolid 8
        ]
      
directionToAngle :: Direcao -> Float
directionToAngle dir = case dir of
  Norte     -> 0
  Sul       -> 180
  Oeste     -> -90
  Este      -> 90
  Nordeste  -> 45
  Noroeste  -> -45
  Sudeste   -> 135
  Sudoeste  -> -135
