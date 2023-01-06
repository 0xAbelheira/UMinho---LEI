{- | 

= Introdução

De acordo com o jogo do Pac-Man, os jogadores (fantasmas e Pac-Man) tomam uma determinada orientação e, a partir daí, andam sozinhos com a passagem do tempo até mudarem de orientação novamente e repetirem o mesmo processo.
É nesta tarefa 4 que definimos as funções que permitem aos jogadores do nosso projeto movimentarem-se com a passagem do tempo, tal como no jogo original.

= Desenvolvimento

Tal como foi referido na introdução, o principal objetivo desta tarefa é implementar o efeito da passagem do tempo no estado de jogo.

Para isto foram tomadas as seguintes medidas: considerou-se que o tempo que passa entre cada iteração é 250 milisegundos. Isto significa que a cada segundo, se o jogador estiver com uma velocidade normal, move-se quatro vezes; foram implementadas funções adicionais na tarefa 2 que permitem ao pacman abrir e fechar a sua boca a cada jogada (dando uma animação mais similar à do jogo clássico) e que fazem com que o pacman perca tempo em modo mega, obedecendo às condições impostas previamente; foi feita uma função passTime (com a ajuda de uma função auxilixar) que, de acordo com o estado de jogo atual, devolve o estado de jogo uma iteração (ou iterações) depois; mais tarde, os argumentos usados na função auxiliar da passTime foram alterados de modo a que as jogadas dos fantasmas e do pacman correspondessem, respetivamente, às jogadas definidas na tarefa 5 e tarefa 6.

= Conclusão

Concluímos, com gosto, esta tarefa, visto que conseguimos cumprir os requisitos indicados pelos docentes e ficamos muito satisfeitos com o resultado final e o seu funcionamento.
 -}

module Tarefa4 where 

import Types
import Tarefa2
import Tarefa5
import Tarefa6

-- | Tempo regular de cada iteração
--
defaultDelayTime = 250 -- 250 ms

-- | Permite atualizações no estado de jogo com a passagem do tempo
--
passTime :: Int -> State -> State
passTime n s@(State maze (x:xs) lvl) = passTimeAux (x:xs) n s

-- | Função auxiliar à passTime
--
passTimeAux :: [Player] -> Int -> State -> State
passTimeAux [] n s = s
passTimeAux (x:xs) n s 
    | (checkPacman x) && (getPacmanMode x == Mega) = passTimeAux xs n (play (getBotPlay (bot (getPlayerID x) s)) s)
    | (checkPacman x) && ((getTimeMega x) <= 0) = passTimeAux xs n (play (getBotPlay (bot (getPlayerID x) s)) s)
    | not (checkGhostDead x) = passTimeAux xs n (play (getPlay (ghostPlay s) (getPlayerID x)) s)
    | (even n) && (checkGhostDead x) = passTimeAux xs n (play (getPlay (ghostPlay s) (getPlayerID x)) s)
    | (odd n) && (checkGhostDead x) = passTimeAux xs n s
    | otherwise = passTimeAux xs n s  
  
-- | Vai buscar a jogada relativa a um ID dado a uma lista de jogadas
--  
getPlay :: [Play] -> Int -> Play
getPlay ((Move id ort):xs) id'
    | id == id' = Move id ort
    | otherwise = getPlay xs id'

-- | Transforma uma Maybe Play numa jogada
--
getBotPlay :: Maybe Play -> Play
getBotPlay (Just (Move id ort)) = Move id ort

{- | Função para jogar sem bot

passTimeAux :: [Player] -> Int -> State -> State
passTimeAux [] n s = s
passTimeAux (x:xs) n s 
    | (checkPacman x) && (getPacmanMode x == Mega) = passTimeAux xs n (play (Move (getPlayerID x) (getPlayerOrientation x)) s)
    | (checkPacman x) && ((getTimeMega x) <= 0) = passTimeAux xs n (play (Move (getPlayerID x) (getPlayerOrientation x)) s)
    | not (checkGhostDead x) = passTimeAux xs n (play (getPlay (ghostPlay s) (getPlayerID x)) s)
    | (even n) && (checkGhostDead x) = passTimeAux xs n (play (getPlay (ghostPlay s) (getPlayerID x)) s)
    | (odd n) && (checkGhostDead x) = passTimeAux xs n s
    | otherwise = passTimeAux xs n s
-}

    

