module Main where

import Graphics.Gloss
import Desenhar
import Eventos
import Worms
import Tempo
import Sprites

janela :: Display
janela = InWindow "Worms" (1920, 1080) (100, 100)

fundo :: Color
fundo = black

fr :: Int
fr = 60

main :: IO ()
main = do
  -- Carregar as texturas
  
  fundoImg <- loadBMP "assets/fundo.bmp"
  aguaImg <- loadBMP "assets/agua.bmp"
  terraImg <- loadBMP "assets/terra.bmp"
  pedraImg <- loadBMP "assets/pedra.bmp"
  menuImg <- loadBMP "assets/menu.bmp"
  settingsImg <- loadBMP "assets/settings.bmp"
  now1Img <- loadBMP "assets/now1.bmp"
  now2Img <- loadBMP "assets/now2.bmp"
  now3Img <- loadBMP "assets/now3.bmp"
  now4Img <- loadBMP "assets/now4.bmp"
  now5Img <- loadBMP "assets/now5.bmp"
  players2map1Img <- loadBMP "assets/sprites/selectMenu/players2map1.bmp"
  players2map2Img <- loadBMP "assets/sprites/selectMenu/players2map2.bmp"
  players2map3Img <- loadBMP "assets/sprites/selectMenu/players2map3.bmp"
  players3map1Img <- loadBMP "assets/sprites/selectMenu/players3map1.bmp"
  players3map2Img <- loadBMP "assets/sprites/selectMenu/players3map2.bmp"
  players3map3Img <- loadBMP "assets/sprites/selectMenu/players3map3.bmp"
  pinkWinnerImg <- loadBMP "assets/pinkwinner.bmp"
  blueWinnerImg <- loadBMP "assets/bluewinner.bmp"
  whiteWinnerImg <- loadBMP "assets/whitewinner.bmp"

  let texturas = Texturas
        { fundoTex = fundoImg
        , aguaTex = aguaImg
        , terraTex = terraImg
        , pedraTex = pedraImg
        }
  let ui       = UI 
        { menuUI = menuImg
        , settingsUI = settingsImg
        , now1 = now1Img
        , now2 = now2Img
        , now3 = now3Img
        , now4 = now4Img
        , now5 = now5Img
        , players2map1 = players2map1Img
        , players2map2 = players2map2Img
        , players2map3 = players2map3Img
        , players3map1 = players3map1Img
        , players3map2 = players3map2Img
        , players3map3 = players3map3Img
        , pinkwinner = pinkWinnerImg
        , bluewinner = blueWinnerImg
        , whitewinner = whiteWinnerImg
        }
        
  sprites <- carregarSpritesComFallback
  
  -- Iniciar o jogo COM texturas
  play
    janela
    fundo
    fr
    Menu
    (desenhaComTexturas texturas ui sprites)  
    reageEventos
    (reageTempoComAnimacoes sprites)

