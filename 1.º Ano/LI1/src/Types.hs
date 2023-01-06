{- | Neste módulo estão presentes todos os types e datas utilizados ao longo da realização das tarefas, bem como algumas funções fundamentais na construção das mesmas. -}
--

module Types where

import System.Random
import Data.List

data State = State 
    {
        maze :: Maze
    ,   playersState :: [Player]
    ,   level :: Int
    }

type Maze = [Corridor]
type Corridor = [Piece]
data Piece =  Food FoodType | PacPlayer Player| Empty | Wall deriving (Eq)
data Player =  Pacman PacState | Ghost GhoState deriving (Eq)

data Orientation = L | R | U | D | Null deriving (Eq,Show)
data PacState= PacState 
    {   
        pacState :: PlayerState
    ,   timeMega :: Double
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode
    
    } deriving Eq

data GhoState= GhoState 
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    } deriving Eq

type Coords = (Int,Int)
type PlayerState = (Int, Coords, Double , Orientation, Int, Int) -- ^ (ID, (x,y), velocidade, orientação, pontos, vidas) 
--                 (ID,  (x,y), velocity, orientation, points, lives) 
data Mouth = Open | Closed deriving (Eq,Show)
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
data GhostMode = Dead  | Alive deriving (Eq,Show)
data FoodType = Big | Little deriving (Eq)
data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq 

data Play = Move Int Orientation deriving (Eq,Show)

type Instructions = [Instruction]

data Instruction = Instruct [(Int, Piece)] | Repeat Int deriving (Show, Eq)


instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

instance Show GhoState where
   show (GhoState x Dead ) =  "?"
   show (GhoState x Alive ) =  "M"

instance Show FoodType where
   show ( Big ) =  "o"
   show ( Little ) =  "."

instance Show Piece where
   show (  Wall ) = coloredString "#" None
   show (  Empty ) = coloredString " " None
   show (  Food z ) = coloredString (show z )   Green
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Normal)  ) Yellow
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Mega)  ) Blue
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Dying)  ) Red
   show ( PacPlayer (Ghost z) ) = coloredString (show z)  Purple

-- | Atribui cores a uma string
--
coloredString :: String -> Color -> String
coloredString x y = x
{- coloredString :: String -> Color -> String
coloredString x y
    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise =  "\x1b[0m" ++ x -}


-- | Um exemplo de um labirinto simples sem quaisquer restrições associadas:
--
sampleMaze :: Maze
sampleMaze = [
                [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
                [Empty, Food Little, Food Little, Food Big, Food Little, Food Big, Food Little, Empty],
                [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
             ]

-- | Gera um número aleatório entre 0 e 99
--
genRandom :: Int -> Int -> [Int]
genRandom n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (0,99) gen -- takes the first n elements from an infinite series of random numbers between 0-9


-- | Dada uma certa seed devolve um número inteiro aleatório:
--
nrRandom :: Int -> Int
nrRandom seed = head $ genRandom 1 seed


-- | Converte uma lista numa lista de listas de tamanho n
-- 
subList :: Int -> [a] -> [[a]]
subList _ [] = []
subList n l = take n l: subList n (drop n l)


{- | Converte um número inteiro numa peça (Piece)
3 <=> Food Big
0 <= n < 70 <=> Food Little
70 < n <= 99 <=> Wall -}
--
convertPiece :: Int -> Piece
convertPiece x
    | x == 3 = Food Big
    | x >= 0 && x < 70 = Food Little
    | otherwise = Wall


-- | Converte um corredor numa string
--
printCorridor :: Corridor -> String
printCorridor [] = "\n"
printCorridor (x:xs) = show x ++ printCorridor xs


-- | Converte uma lista de inteiros num corredor
--
convertCorridor :: [Int] -> Corridor
convertCorridor [] = []
convertCorridor (x:xs) = convertPiece x : convertCorridor xs


-- | Converte uma lista de uma lista de inteiros num labirinto  
--
convertMaze :: [[Int]] -> Maze
convertMaze [] = []
convertMaze (x:xs) = convertCorridor x : convertMaze xs


-- | Converte um labirinto em forma de string
--
printMaze :: Maze -> String
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs ) 


-- | Coloca o Maze (convertido em string)
--
placeMaze :: Maze -> IO()
placeMaze x = do putStrLn (printMaze (x))

-- | Coloca uma lista de jogadores num labirinto
--
placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )


-- | Converte as estatísticas de um jogador numa string
--
printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"


-- | Devolve o estado de jogo (PlayerState) de um jogador 
--
getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a


-- | Devolve o ID de um jogador
--
getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x


-- | Devolve as coordenadas de um jogador
--
getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y


-- | Devolve a orientação de um jogador
--
getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation (Ghost (GhoState (x,y,z,t,h,l) q )) = t

-- |
--
getPlayerVelocity:: Player -> Double
getPlayerVelocity (Pacman (PacState (x,y,z,t,h,l) q c d )) = z
getPlayerVelocity (Ghost (GhoState (x,y,z,t,h,l) q )) = z

-- |
--
getPlayerMouth:: Player -> Mouth
getPlayerMouth (Pacman (PacState (x,y,z,t,h,l) q c d )) = c


-- | Devolve os pontos de um jogador
--
getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h


-- | Devolve o número de vidas de um jogador
--
getPlayerLives :: Player -> Int
getPlayerLives (Pacman (PacState (x,y,z,t,h,l) q c d )) = l
getPlayerLives (Ghost (GhoState (x,y,z,t,h,l) q )) = l


-- | Devolve o modo do jogador pacman 
--
getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d

-- |
--
getTimeMega :: Player -> Double
getTimeMega (Pacman (PacState a b c d)) = b
getTimeMega x = 0 

-- | Devolve o modo do jogador fantasma
--
getGhostMode :: Player -> GhostMode
getGhostMode (Ghost (GhoState a d)) = d

-- | Coloca as coordenadas de um player
--
setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )


-- | Devolve a orientação de uma peça 
--
getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null


-- | Subsitui uma peça que está numas certas coordenadas do labirinto por outra peça, devolvendo o labirinto atualizado
--
replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs

getPacman :: [Player] -> Player
getPacman (Pacman ps : xs) = Pacman ps
getPacman (x:xs) = getPacman xs

-- | Recebe um índice, um elemento de uma lista e uma lista. Devolve a lista atualizada cujo elemento no índice indicado foi substituído pelo elemento dado
--
replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = [] 
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

checkPacman :: Player -> Bool
checkPacman (Pacman (PacState (id, (x,y), v, ort, p, l) t o m)) = True
checkPacman (Ghost (GhoState (id, (x,y), v, ort, p, l) m)) = False

checkGhostDead :: Player -> Bool
checkGhostDead (Ghost (GhoState (id, (x,y), v, ort, p, l) Dead)) = True
checkGhostDead (Ghost (GhoState (id, (x,y), v, ort, p, l) Alive)) = False
checkGhostDead (Pacman (PacState (id, (x,y), v, ort, p, l) t o m)) = False

