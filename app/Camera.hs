module Camera where

import Graphics.Gloss
import Labs2025


data Camera = Camera
  { camPosX :: Float     
  , camPosY :: Float   
  , camZoom :: Float     
  , camTargetX :: Float  
  , camTargetY :: Float   
  , camTargetZoom :: Float
  } deriving (Show)

tamanhoBlocos:: Float
tamanhoBlocos = 24


initialCamera :: Camera
initialCamera = Camera
  { camPosX = 0
  , camPosY = 0
  , camZoom = 1
  , camTargetX = 0
  , camTargetY = 0
  , camTargetZoom = 1
  }


cameraLerpSpeed :: Float
cameraLerpSpeed = 0.08 

zoomLerpSpeed :: Float
zoomLerpSpeed = 0.05

defaultZoom :: Float
defaultZoom = 2

maxZoom :: Float
maxZoom = 3

minZoom :: Float 
minZoom = 0.05


updateCamera :: Float -> Camera -> Minhoca -> Int -> Int -> Camera
updateCamera dt cam minhoca largura altura =
  case posicaoMinhoca minhoca of
    Nothing -> cam  
    Just (lin, col) ->
      let
          targetX = fromIntegral col * tamanhoBlocos 
                    - fromIntegral largura * tamanhoBlocos / 2 
                    + tamanhoBlocos / 2
          targetY = fromIntegral altura * tamanhoBlocos / 2 
                    - fromIntegral lin * tamanhoBlocos 
                    - tamanhoBlocos / 2
          
        
          newX = lerp (camPosX cam) targetX cameraLerpSpeed
          newY = lerp (camPosY cam) targetY cameraLerpSpeed
          newZoom = lerp (camZoom cam) (camTargetZoom cam) zoomLerpSpeed
          
      in cam { camPosX = newX
             , camPosY = newY
             , camZoom = newZoom
             , camTargetX = targetX
             , camTargetY = targetY
             }

-- Zomm Cam
setCameraZoom :: Float -> Camera -> Camera
setCameraZoom zoom cam = cam { camTargetZoom = clamp minZoom maxZoom zoom }

clamp :: Float -> Float -> Float -> Float
clamp minVal maxVal val
  | val <= minVal = minVal
  | val >= maxVal = maxVal
  | otherwise = val

-- Reset Cam
resetCameraZoom :: Camera -> Camera
resetCameraZoom = setCameraZoom defaultZoom



applyCameraTransform :: Camera -> Picture -> Picture
applyCameraTransform cam pic =
  Scale (camZoom cam) (camZoom cam) $
    Translate (-camPosX cam) (-camPosY cam) pic

-- ============================================================================
-- Extra funcs
-- ============================================================================


lerp :: Float -> Float -> Float -> Float
lerp current target speed = current + (target - current) * speed

-- Center Cam
snapCameraTo :: Posicao -> Int -> Int -> Camera -> Camera
snapCameraTo (lin, col) largura altura cam@(Camera _ _ zoom _ _ _) =
  let targetX = fromIntegral col * tamanhoBlocos 
                - fromIntegral largura * tamanhoBlocos / 2 
                + tamanhoBlocos / 2
      targetY = fromIntegral altura * tamanhoBlocos / 2 
                - fromIntegral lin * tamanhoBlocos 
                - tamanhoBlocos / 2
  in cam { camPosX = targetX
         , camPosY = targetY
         , camTargetX = targetX
         , camTargetY = targetY
         , camZoom = zoom 
         , camTargetZoom = zoom
         }

-- camLimits
getCameraBounds :: Camera -> (Float, Float, Float, Float)
getCameraBounds cam =
  let halfWidth = 400 / camZoom cam   -- 800/2 = 400
      halfHeight = 300 / camZoom cam  -- 600/2 = 300
  in ( camPosX cam - halfWidth   -- left
     , camPosX cam + halfWidth   -- right
     , camPosY cam - halfHeight  -- bottom
     , camPosY cam + halfHeight  -- top
     )
