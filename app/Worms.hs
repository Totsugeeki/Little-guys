module Worms where

import Labs2025
import Types 
import Camera
import Mapa
import Tarefa1 
import qualified Data.Set as Set
import System.Random (RandomGen, randomR, mkStdGen)

-- | État du jeu dans Gloss
data Worms
  = Menu
  | TeamSetupMenu TeamConfigExtended
  | Jogo 
      { estadoJogo :: Estado
      , minhocaAtiva :: NumMinhoca
      , armaAtual :: TipoArma
      , roundTimeLeft :: Float
      , updateCounter :: Float  
      , animacoes :: [AnimacaoMinhoca]
      , animacaoSelecao :: AnimacaoSelecao  
      , ultimaDirecao :: Direcao    
      , keysPressed :: Set.Set CustomKey 
      , camera :: Camera
      , teamConfig :: TeamConfig
      }
  | WinScreen Team
  | SettingsMenu Worms 
  deriving Show

data TeamConfig = TeamConfig
  { numTeams :: Int           -- Numero de equipas (max 3)
  , wormsPerTeam :: Int       -- Minhocas por equipa
  , teams :: [Team]
  } deriving Show

data Team = Team
  { teamId :: Int
  , teamName :: String
  , teamColor :: TeamColor
  , teamWormIndices :: [Int]
  } deriving (Show, Eq)

data CustomKey = CKeyW | CKeyA | CKeyS | CKeyD 
               | CKeyUp | CKeyDown | CKeyLeft | CKeyRight
               deriving (Eq, Ord, Show)

data TeamConfigExtended = TeamConfigExtended
  { baseConfig :: TeamConfig
  , selectedMap :: Int      -- 1, 2, ou 3
  , randomSeed :: Int       -- Seed para aleatorio
  } deriving Show

initialTeamConfigExtended :: TeamConfigExtended
initialTeamConfigExtended = TeamConfigExtended
  { baseConfig = defaultTeamConfig
  , selectedMap = 2
  , randomSeed = 42
  }
               
roundTimer :: Float
roundTimer = 45

-- ============================================================================
-- TEAM CONFIG
-- ============================================================================

defaultTeamConfig :: TeamConfig
defaultTeamConfig = TeamConfig
  { numTeams = 2
  , wormsPerTeam = 2
  , teams = 
      [ Team 0 "Pink Team" PinkTeam [0, 2]  -- Worms 0 e 2
      , Team 1 "Blue Team" BlueTeam [1, 3]  -- Worms 1 e 3
      ]
  }

-- Gera os indices das minhocas por ordem de rotação
-- Exemplo com 2 equipas et 2 worms/Tean: [0,1,2,3] -> Tean 0: [0,2], Tean 1: [1,3]
-- Exemplo com 3 equipas e 2 worms/team: [0,1,2,3,4,5] -> Tean 0: [0,3], Tean 1: [1,4], Tean 2: [2,5]
teamWormIndicesGenerator :: Int -> Int -> [[Int]]
teamWormIndicesGenerator wormsPerTeam numTeams =
    [ [team + worm * numTeams | worm <- [0 .. wormsPerTeam - 1]]
    | team <- [0 .. numTeams - 1]
    ]


createTeamConfig :: Int -> Int -> TeamConfig
createTeamConfig nTeams wPerTeam =
  let -- 3 equipas maximo
      limitedTeams = min 3 (max 1 nTeams)
      colors = [PinkTeam, BlueTeam, WhiteTeam]
      names = ["Pink Team", "Blue Team", "White Team"]
      
      indices = teamWormIndicesGenerator wPerTeam limitedTeams
      
      teamsList = 
        [ Team i (names !! i) (colors !! i) (indices !! i)
        | i <- [0 .. limitedTeams - 1]
        ]
  in TeamConfig limitedTeams wPerTeam teamsList

-- ============================================================================
-- TEAM FUNCS
-- ============================================================================

getWormTeam :: TeamConfig -> Int -> Maybe Team
getWormTeam config wormIdx = 
  let allTeams = teams config
  in findTeam allTeams wormIdx
  where
    findTeam [] _ = Nothing
    findTeam (t:ts) idx
      | idx `elem` teamWormIndices t = Just t
      | otherwise = findTeam ts idx

getTeamColor :: Team -> (Float, Float, Float)
getTeamColor team = case teamColor team of
  PinkTeam  -> (1.0, 0.4, 0.7)
  BlueTeam -> (0.2, 0.4, 1.0)
  WhiteTeam -> (1.0, 1.0, 1.0)

getAliveTeams :: Estado -> TeamConfig -> [Team]
getAliveTeams estado config =
  filter (teamHasAliveWorms estado) (teams config)
  where
    teamHasAliveWorms :: Estado -> Team -> Bool
    teamHasAliveWorms e team =
      any (\idx -> not (isDead (minhocasEstado e !! idx))) (teamWormIndices team)

checkWinCondition :: Estado -> TeamConfig -> Maybe Team
checkWinCondition estado config =
  case getAliveTeams estado config of
    [winner] -> Just winner
    _ -> Nothing

-- ============================================================================
-- WORM CHANGE
-- ============================================================================

changeWormController :: Worms -> Worms
changeWormController (Jogo e@(Estado _ _ minhocas) numMinhocaAtiva arma t updt anims selectanim dir keys cam config) = 
  case checkWinCondition e config of
    Just winner -> WinScreen winner
    Nothing ->
      let novoNum = nextWormInRotation numMinhocaAtiva minhocas config
          
          mapa = mapaEstado e
          altura = length mapa
          largura = if altura > 0 then length (head mapa) else 0
          novaMinhoca = minhocas !! novoNum
          
          novaCam = case posicaoMinhoca novaMinhoca of
                      Just pos -> snapCameraTo pos largura altura cam
                      Nothing -> cam
      in Jogo e novoNum arma roundTimer updt anims selectanim dir keys novaCam config

-- Procura o proximo worm na rotação
--Ordem natural: 0 -> 1 -> 2 -> 3 -> ...
nextWormInRotation :: NumMinhoca -> [Minhoca] -> TeamConfig -> NumMinhoca
nextWormInRotation currentIdx minhocas config =
  let totalWorms = length minhocas
      
      findNext idx attempts
        | attempts >= totalWorms = currentIdx  -- Fallback se todos mortos
        | isDead (minhocas !! idx) = findNext ((idx + 1) `mod` totalWorms) (attempts + 1)
        | otherwise = idx
  in findNext ((currentIdx + 1) `mod` totalWorms) 0

nextWorm :: NumMinhoca -> [Minhoca] -> NumMinhoca
nextWorm n list = go ((n + 1) `mod` length list) 0
  where
    go idx attempts
      | attempts >= length list = n  
      | isDead (list !! idx) = go ((idx + 1) `mod` length list) (attempts + 1)
      | otherwise = idx

isDead :: Minhoca -> Bool
isDead (Minhoca _ status _ _ _ _ _) = status == Morta

-- ============================================================================
-- USEFUL FUNCS
-- ============================================================================

obtainWormPosition :: Estado -> NumMinhoca -> Maybe Posicao
obtainWormPosition estado num =
  if num < length (minhocasEstado estado)
    then posicaoMinhoca (minhocasEstado estado !! num)
    else Nothing 

calcDirection :: Posicao -> Posicao -> Direcao
calcDirection (linMinhoca, colMinhoca) (linRato, colRato) =
  let dx = colRato - colMinhoca      
      dy = -(linRato - linMinhoca)   
      angle = atan2 (fromIntegral dy) (fromIntegral dx)
      angleDeg = angle * 180 / pi
  in angleToDirection angleDeg

screenToGame :: (Float, Float) -> Int -> Int -> Posicao
screenToGame (x, y) largura altura = 
  let centerX = fromIntegral largura * tamanhoBlocos / 2
      centerY = fromIntegral altura * tamanhoBlocos / 2
      relX = x + centerX
      relY = centerY - y 
      col = round (relX / tamanhoBlocos - 0.5)
      lin = round (relY / tamanhoBlocos - 0.5)
  in (lin, col)

angleToDirection :: Float -> Direcao
angleToDirection angle
  | angle >= -22.5 && angle < 22.5   = Este       
  | angle >= 22.5 && angle < 67.5    = Nordeste   
  | angle >= 67.5 && angle < 112.5   = Norte      
  | angle >= 112.5 && angle < 157.5  = Noroeste   
  | angle >= 157.5 || angle < -157.5 = Oeste      
  | angle >= -157.5 && angle < -112.5 = Sudoeste  
  | angle >= -112.5 && angle < -67.5  = Sul       
  | otherwise                         = Sudeste

-- ============================================================================
-- GERAÇÃO ALEATÓRIA
-- ============================================================================

-- Gera N positions aléatoires valides (seulement sur Ar com Terra embaixo)
gerarPosicoesAleatorias :: RandomGen g => Int -> Int -> Int -> Mapa -> g -> [Posicao] -> ([Posicao], g)
gerarPosicoesAleatorias 0 _ _ _ gen _ = ([], gen)
gerarPosicoesAleatorias n altura largura mapa gen posOcupadas =
  let (lin, gen1) = randomR (0, altura - 1) gen
      (col, gen2) = randomR (0, largura - 1) gen1
      pos = (lin, col)
  in if pos `elem` posOcupadas || not (posicaoValida mapa pos)
     then gerarPosicoesAleatorias n altura largura mapa gen2 posOcupadas  -- retry
     else let (resto, genFinal) = gerarPosicoesAleatorias (n - 1) altura largura mapa gen2 (pos : posOcupadas)
          in (pos : resto, genFinal)

-- Verifica se a posição abaixo tem Terra
temTerraEmbaixo :: Mapa -> Posicao -> Bool
temTerraEmbaixo mapa (lin, col) =
  let altura = length mapa
      linhaBaixo = lin + 1
  in if linhaBaixo < altura
     then case (mapa !! linhaBaixo) !! col of
            Terra -> True
            _     -> False
     else False  -- Se está na última linha, não tem nada embaixo

-- Vérifica se posicao é valida 
posicaoValida :: Mapa -> Posicao -> Bool
posicaoValida mapa pos@(lin, col) =
  let altura = length mapa
      largura = if altura > 0 then length (head mapa) else 0
      
      dentroLimites = lin >= 0 && lin < altura && col >= 0 && col < largura
      
      estaEmAr = dentroLimites && case (mapa !! lin) !! col of
                   Ar -> True
                   _  -> False
      
      -- Verifica se tem Terra diretamente embaixo
      terraEmbaixo = temTerraEmbaixo mapa pos
      
  in estaEmAr && terraEmbaixo


criarMinhocasAleatorias :: RandomGen g => [Posicao] -> g -> ([Minhoca], g)
criarMinhocasAleatorias [] gen = ([], gen)
criarMinhocasAleatorias (pos:rest) gen =
  let 
      worm = Minhoca
        { posicaoMinhoca = Just pos
        , vidaMinhoca = Viva 100
        , jetpackMinhoca = 999
        , escavadoraMinhoca = 999
        , bazucaMinhoca = 999
        , minaMinhoca = 999
        , dinamiteMinhoca = 999
        }
      
      (restWorms, genFinal) = criarMinhocasAleatorias rest gen
  in (worm : restWorms, genFinal)

-- ============================================================================
-- ZONAS CLICK PARA TEAM CONFIG
-- ============================================================================

setaEsquerdaMap :: (Float, Float) -> Bool
setaEsquerdaMap (x, y) = 
  x >= -925 && x <= -825 && y >= -50 && y <= 50

setaDireitaMap :: (Float, Float) -> Bool
setaDireitaMap (x, y) = 
  x >= -300 && x <= -200 && y >= -50 && y <= 50

now :: Int -> (Float, Float) -> Bool
now numWorm (x, y) =
  let baseX = 100 + fromIntegral (numWorm - 1) * 130  
      baseY = -190                                    
      width  = 120 * 1.5                               
      height = 120 * 1.5                               
  in x >= baseX && x <= (baseX + width) && y >= baseY && y <= (baseY + height)

minusPlayersZone :: (Float, Float) -> Bool
minusPlayersZone (x, y) = 
  x >= 340 && x <= 400 && y >= 10 && y <= 60

plusPlayersZone :: (Float, Float) -> Bool
plusPlayersZone (x, y) = 
  x >= 570 && x <= 620 && y >= 10 && y <= 60

playButtonZone :: (Float, Float) -> Bool
playButtonZone (x, y) = 
  x >= -170 && x <= 150 && y >= -450 && y <= -380

updateWormsPerTeam :: TeamConfig -> Int -> TeamConfig
updateWormsPerTeam config newWormsPerTeam =
  createTeamConfig (numTeams config) newWormsPerTeam

-- ============================================================================
-- START GAME - VERSION PURE SANS IO
-- ============================================================================
startGameFromTeamSetup :: TeamConfigExtended -> Worms
startGameFromTeamSetup extended =
  let config = baseConfig extended
      mapa = case selectedMap extended of
               1 -> mapa1
               2 -> mapa2
               _ -> mapa3
      
      altura = length mapa
      largura = if altura > 0 then length (head mapa) else 0
      
      gen = mkStdGen (randomSeed extended)
      
      totalWorms = numTeams config * wormsPerTeam config
      
      -- Passer le mapa pour vérifier que c'est Ar com Terra embaixo
      (posWorms, gen1) = gerarPosicoesAleatorias totalWorms altura largura mapa gen []
      
      (numBarris, gen2) = randomR (3, 8) gen1
      
      -- Passer le mapa para os barris também (precisam de Terra embaixo)
      (posBarris, gen3) = gerarPosicoesAleatorias numBarris altura largura mapa gen2 posWorms
      
      (minhocas, _) = criarMinhocasAleatorias posWorms gen3
      
      barris = map (\pos -> Barril pos False) posBarris
      
      estado = Estado mapa barris minhocas
      
      armaInicial = Bazuca
  
  in Jogo estado 0 armaInicial roundTimer 0.1
          (replicate totalWorms novaAnimacao)
          novaAnimacaoSelecao Este Set.empty initialCamera config

handleTeamSetupClick :: TeamConfigExtended -> (Float, Float) -> Either TeamConfigExtended Worms
handleTeamSetupClick extended pos

  | setaEsquerdaMap pos = 
      let newMap = if selectedMap extended <= 1 then 3 else selectedMap extended - 1
          -- Changer le seed quand on change de map
          newSeed = randomSeed extended + 1000
      in Left $ extended { selectedMap = newMap, randomSeed = newSeed }
  
  | setaDireitaMap pos = 
      let newMap = if selectedMap extended >= 3 then 1 else selectedMap extended + 1
          newSeed = randomSeed extended + 1000
      in Left $ extended { selectedMap = newMap, randomSeed = newSeed }
  
  | now 1 pos = 
      let newSeed = randomSeed extended + 1
      in Left $ extended { baseConfig = updateWormsPerTeam (baseConfig extended) 1, randomSeed = newSeed }
  | now 2 pos = 
      let newSeed = randomSeed extended + 2
      in Left $ extended { baseConfig = updateWormsPerTeam (baseConfig extended) 2, randomSeed = newSeed }
  | now 3 pos = 
      let newSeed = randomSeed extended + 3
      in Left $ extended { baseConfig = updateWormsPerTeam (baseConfig extended) 3, randomSeed = newSeed }
  | now 4 pos = 
      let newSeed = randomSeed extended + 4
      in Left $ extended { baseConfig = updateWormsPerTeam (baseConfig extended) 4, randomSeed = newSeed }
  | now 5 pos = 
      let newSeed = randomSeed extended + 5
      in Left $ extended { baseConfig = updateWormsPerTeam (baseConfig extended) 5, randomSeed = newSeed }

  | minusPlayersZone pos = 
      let config = baseConfig extended
          newNum = if numTeams config <= 2 then 3 else numTeams config - 1
          newConfig = createTeamConfig newNum (wormsPerTeam config)
          newSeed = randomSeed extended + 10
      in Left $ extended { baseConfig = newConfig, randomSeed = newSeed }
  
  | plusPlayersZone pos = 
      let config = baseConfig extended
          newNum = if numTeams config >= 3 then 2 else numTeams config + 1
          newConfig = createTeamConfig newNum (wormsPerTeam config)
          newSeed = randomSeed extended + 10
      in Left $ extended { baseConfig = newConfig, randomSeed = newSeed }
  
  | playButtonZone pos = Right $ startGameFromTeamSetup extended
  
  | otherwise = Left extended