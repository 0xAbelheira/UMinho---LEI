module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6


data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    } 


loadManager :: Manager
loadManager = ( Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime )

rotatePlayerWithID :: Key -> [Player] -> Int -> [Player]
rotatePlayerWithID KeyUpArrow (j:js) id = if getPlayerID j == id then (changePlayerOrientation U j):js else j:(rotatePlayerWithID KeyUpArrow js id)
rotatePlayerWithID KeyDownArrow (j:js) id = if getPlayerID j == id then (changePlayerOrientation D j):js else j:(rotatePlayerWithID KeyDownArrow js id)
rotatePlayerWithID KeyLeftArrow (j:js) id = if getPlayerID j == id then (changePlayerOrientation L j):js else j:(rotatePlayerWithID KeyLeftArrow js id)
rotatePlayerWithID KeyRightArrow (j:js) id = if getPlayerID j == id then (changePlayerOrientation R j):js else j:(rotatePlayerWithID KeyRightArrow js id)

updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer KeyUpArrow (Manager (State m (j:js) lvl) pid step bf delt del) = Manager (State m (rotatePlayerWithID KeyUpArrow (j:js) pid) lvl) pid step bf delt del
updateControlledPlayer KeyDownArrow (Manager (State m (j:js) lvl) pid step bf delt del) = Manager (State m (rotatePlayerWithID KeyDownArrow (j:js) pid) lvl) pid step bf delt del
updateControlledPlayer KeyLeftArrow (Manager (State m (j:js) lvl) pid step bf delt del) = Manager (State m (rotatePlayerWithID KeyLeftArrow (j:js) pid) lvl) pid step bf delt del
updateControlledPlayer KeyRightArrow (Manager (State m (j:js) lvl) pid step bf delt del) = Manager (State m (rotatePlayerWithID KeyRightArrow (j:js) pid) lvl) pid step bf delt del

updateTimeMega :: [Player] -> [Player]
updateTimeMega [] = []
updateTimeMega ((Pacman (PacState (id, (x,y), v, ort, p, l) t o m)):xs) = (Pacman (PacState (id, (x,y), v, ort, p, l) (t-250) o m)):xs
updateTimeMega (x:xs) = x:updateTimeMega xs

updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

updateTime :: Integer -> Manager -> Manager 
updateTime now man = man {delta = (now - before man)}

resetTimer :: Integer -> Manager -> Manager 
resetTimer now man = man {delta = 0, before = now}

nextFrame :: Integer -> Manager -> Manager 
nextFrame now man@(Manager (State maze pls lvl) pid s b d e) = Manager (passTime s (State maze (updateTimeMega pls) lvl)) pid (s+1) now 0 e

loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorWhite ColorDefault  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager

