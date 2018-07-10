{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Tetris where

import System.Random
import Graphics.Gloss.Interface.Pure.Game

-- quantos quadros por segundo sao renderizados

glob_fps::Int
glob_fps = 60

-- principal funçao
run :: IO ()
run = do
 g <- newStdGen
 play display bgColor fps (genUniverse g ) drawTetris handleTetris updateTetris
   where
    display = InWindow "Tetris" (screenWidthreal, screenHeightreal) (0, 0)
    bgColor = black   -- cor de fundo
    fps     = glob_fps   -- numero de quadros por segundo


-- =========================================
-- Tipos
-- =========================================


-- largura da tela
screenWidthreal :: Int
screenWidthreal = 800

-- altura da tela
screenHeightreal :: Int
screenHeightreal = 800

-- coordenadas do tipo botton x1,x2 ,y1,y2
tetrTypbutton::(Float,Float,Float,Float)
tetrTypbutton = (34,100,250,290)


-- bloco de dillin em tetris circulares
angle :: Float
angle = 36

-- tamanho do bloco em Float.
blockSizeFloat :: Float
blockSizeFloat = 30

-- O angulo que voce tem que desenhar separadamente, porque o recurso
-- ThickArc desenha do outro lado
specangel :: Float
specangel = 324

-- | Deslocamento
offset2 :: Float
offset2 = 100

-- | Escala para o mundo
myscale :: Float
myscale = 2.3

-- | tamanho adequado
sizefit ::Float
sizefit = 5

-- | tamanho adequado em Int.
sizefitInt ::Int
sizefitInt = 5

-- | Outro offset
offsedge ::Int
offsedge = 15

-- | A largura do circulo da Tetris 2
thicknessOne::Float
thicknessOne = 6

-- | A largura do circulo de Tetris 2
thicknessTwo::Float
thicknessTwo = 2

-- | O primeiro passo do Tetris escalocado
inintTactStepped::Float
inintTactStepped = 0.7

-- Primeiro passo do Tetris 
inintTactSmooth::Float
inintTactSmooth = 0.01

-- | As coordenadas dos botoes de movimento x1,x2 ,y1,y2
tetrMoveButton ::(Float,Float,Float,Float)
tetrMoveButton = (101,148,250,290)

-- | O tamanho do bloco na forma
blockSize :: Int
blockSize = 30

-- | velocidade inicial da figura em queda
init_tact::Time
init_tact = 0.7

-- | Para o tetris liso
conSmooth:: Float
conSmooth = 225

-- | Para o fundo de um tetris circular
conCircleB:: Float
conCircleB = 205

-- | Para o tetris circular
conCircle:: Float
conCircle = 123

-- | Para o menu de passos de Tetris
conRecMenu:: Float
conRecMenu =113

-- | Escala da conta
scaleSc::Float
scaleSc = 0.01

-- | dividindo a largura da tela
transl::Float
transl = 2

-- | aumentando o quadro atras
scaleBackGr::Float
scaleBackGr = 1.3

-- | escala do texto
scaleT::Float
scaleT = 0.008

-- | texto da escala em tetris normal
scaleTSmooth::Float
scaleTSmooth = 13

-- | escala de quadros atras
transtBackGround::Float
transtBackGround = -100

-- | escala de texto
translateT::Float
translateT = -1.5

-- | transparencia
alpha::Float
alpha = 0.7

-- | a celula esta cheia?
data Block = Free  -- vazio
            | Full  -- preenchido
         deriving(Eq, Show)

-- | Linhas
type Row = [Block]

-- | Campo
type Board = [Coord]

-- | Pontuacao
type Score = Int

-- as coordenadas da Figura
-- a rotacao é determinada exclusivamente pela sua consistencia
-- coordenadas x, y
type Coord1 = (Int, Int,Int)

-- | coordenadas do bloco x, y e sua cor
data Coord = Coord 
  { x   :: Int  -- coordenada x.
  , y   :: Int  -- coordenada y.
  , clr :: Int  -- cor do bloco
  }

-- | Время.
type Time = Float


-- | O tipo de jogo de tetris é retangular ou circular
data TetrisType = TetrisRect   -- ^ retangular
                | TetrisRound  -- ^ circular
    deriving(Eq, Show)

-- tipo de tetris - piso ou liso
data TetrisMove = TetrisStepped -- ^ Piso.
                 | TetrisSmooth -- ^ Suave.
   deriving(Eq, Show)
-- O estado do jogo atual (dividiu o tabuleiro e a figura)
-- para que quando o voo da figura nao pisque toda a prancha
-- tambem que seja mais otimizado)
-- [Figure] - lita infinita de figuras no estado atual, 
-- pegamos o primeiro elemento da lista.
-- contagem de tempo de velocidade da placa de 
-- forma circular (1) ou retangular (0) lisa (1) ou 
-- densa (0) init_tack.] 
----------------------------------------------------------------------------------------------------------------------------------------------------------
type Gamestate = (Board,  [Figure], (Speed, Time), Score,TetrisType,TetrisMove,Time)

-- Estado do jogo
data GameState = GameState
 {  board   :: Board            -- ^ prancha do jogo
  , figure :: [Figure]            -- ^ lista de figuras
  , speedandtime   :: (Speed, Time) -- ^ tempo e velocidade
  , score   :: Score            -- ^ Conta
  ,typerepres::TetrisType          -- ^ Tipo de apresentacao
  ,typemoving :: TetrisMove       -- ^ Тipo de movimento
  ,tactgamestate :: Time           -- ^ Tempo
  } 

-- transferir para o estado antigo (para depuracao)
fromGS :: GameState -> Gamestate
fromGS GameState{..} = (board, figure, speedandtime, score, typerepres,typemoving,tactgamestate)

-- traducao para um novo estado (para depuracao)
toGS :: Gamestate -> GameState
toGS (board, figure, (speed, time), score, typerepres,typemoving,tactgamestate) 
        = GameState board figure (speed, time) score  typerepres typemoving tactgamestate

-- traducao em coordenadas antigas

fromCoord :: Coord -> Coord1
fromCoord Coord{..} = (x,y,clr)

-- traducao para novas coordenadas
toCoord :: Coord1 -> Coord
toCoord (x,y,clr) = Coord x y clr




-- Velocidade
type Speed = Float

-- Para cada figura, o seu tipo, entao voce pode definitivamente
-- defini-lo e o tipo de operacoes sobre eles, por exemplo, 
-- a figura I pode ser girada
-- apenas a uma distancia de mais de 4 celulas da borda,
-- uma figura 0 a uma distancia de mais de 2 celulas da borda

-- | Tipos de figuras
data FigureType = O 
                 | I
                 | T
                 | J
                 | L
                 | S
                 | Z
                      deriving(Eq, Show)

-- | Direcao da figura
data Direction = DUp -- Para cima .
               | DDown  -- ^ Para baixo.
               | DLeft -- ^ Esquerda.
               | DRight  -- ^ Direita.
                      deriving(Eq, Show)

-- | Tipo de figura para o jogo
data Figure = Figure FigureType Direction Coord 
                      
-- | Tipo de forma de blocos
type BlockedFigure = (Coord, Coord, Coord, Coord)

-- | Largura da tela
screenWidth :: Int
screenWidth = 300

-- | A altura da Tela
screenHeight :: Int
screenHeight = 600





-- =========================================
-- Generando
-- =========================================


-- A entrada é um numero aleatorio de 0 a 6, que define a forma
genFigure :: Int -> Figure
genFigure a
  | a == 0    = Figure O DUp startpos
  | a == 1    = Figure I DUp startpos
  | a == 2    = Figure T DUp startpos
  | a == 3    = Figure J DUp startpos
  | a == 4    = Figure L DUp startpos
  | a == 5    = Figure S DUp startpos
  | otherwise = Figure Z DUp startpos
  where
    startpos = Coord {x = div screenWidth 2, y = blockSize * 2, clr = a}


-- | Inicializacao infinita aleatorio
-- uma lista de numero de 0 a 6 que correspondem as figuras
initFigures :: StdGen -> [Figure]
initFigures g = map genFigure
  (randomRs getrange g)

-- Faixa de geracao de numeros aleatorios
getrange :: (Int, Int)
getrange = (0, 6)
  


-- | Tabuleiro vazio
genEmptyBoard :: Board
genEmptyBoard = [ ]
  
-- | Preeenchendo as linhas
genRows::Int->Int->[Row]
genRows _ 0 = []
genRows w h = (genRows w (h-1)) ++ [genRow w]

-- | Preenchendo as linhas
genRow::Int->Row
genRow 0 = []
genRow w = (genRow (w-1)) ++ [Free]

-- | Gerando o universo
genUniverse::StdGen -> GameState
genUniverse g = GameState{ board   = genEmptyBoard  
                          , figure  = initFigures g
                          , speedandtime   = (init_tact, 0)
                          , score    = 0
                          ,typerepres =    TetrisRect
                          ,typemoving =  TetrisStepped
                          ,tactgamestate     = 0.7
                          }





-- =========================================
-- Virando
-- =========================================


-- | Girando a forma
turn::GameState -> GameState
turn u   |(typerepres u) == TetrisRound =  turnRound u
         |(typemoving u)== TetrisStepped = turnStepped u 
        |otherwise = turnSmooth u
-- | Girando a forma em circular
turnRound :: GameState -> GameState
turnRound u= u{figure = cons  (chRotation (getf(figure u)))   (rest  (figure u))}
-- | Rodando a figura no passo Tetris
turnStepped::GameState -> GameState
turnStepped u = u{figure = cons  (chRotation (getf(figure u)))   (rest  (figure u))}
       

-- | Alterando o estado da rotacao
chRotation:: Figure->Figure
chRotation  (Figure t DUp c) = (Figure t DRight c)
chRotation  (Figure t DRight c) = (Figure t DDown c)
chRotation (Figure t DDown c) = (Figure t DLeft c)
chRotation (Figure t DLeft c) = (Figure t DUp c)

-- Girando a figura no Tetris
turnSmooth::GameState -> GameState
turnSmooth u = u{figure = cons  (chRotation (getf(figure u))) (rest  (figure u))}
      


-- =========================================
-- Preparando para desenhar
-- =========================================


-- | Preparando as figuras para desenho
figureToDraw :: Figure -> BlockedFigure
figureToDraw (Figure O d c) = figureToDrawO (Figure O d c)
figureToDraw (Figure I d c) = figureToDrawI (Figure I d c)
figureToDraw (Figure T d c) = figureToDrawT (Figure T d c)
figureToDraw (Figure J d c) = figureToDrawJ (Figure J d c)
figureToDraw (Figure L d c) = figureToDrawL (Figure L d c)
figureToDraw (Figure S d c) = figureToDrawS (Figure S d c)
figureToDraw (Figure Z d c) = figureToDrawZ (Figure Z d c)


-- Preparando o quadro para renderizacao
-- retornando as coordenadas de 4 blocos
figureToDrawO :: Figure -> BlockedFigure
figureToDrawO (Figure _ _ c) 
  = (c, c {x = x c + bs}, c {y = y c - bs}, c {x = x c + bs, y = y c - bs})
  where
    bs = blockSize

-- Preparando para desenhar
-- retornando as coordenadas de 4 blocos
figureToDrawI :: Figure -> BlockedFigure
figureToDrawI (Figure _ d c) 
  | (d == DUp) || (d == DDown) = (c {y = y c + bs}, c, c {y = y c - bs}, c {y = y c - 2*bs})
  | otherwise                  = (c {x = x c - bs}, c, c {x = x c + bs}, c {x = x c + 2*bs})
  where
    bs = blockSize

-- preparando o ziguezague esquerdo para renderizacao
figureToDrawZ :: Figure -> BlockedFigure
figureToDrawZ (Figure _ d c) 
  | (d == DUp) || (d == DDown) = (c {x = x c - bs, y = y c - bs},c {x = x c - bs}, c,c {y = y c + bs})
  | otherwise                  = (c {x = x c - bs},c,c {y = y c - bs}, c {x = x c + bs, y = y c - bs})
  where
    bs = blockSize

-- Preparando o ziguezague para a direita para a renderizacao
figureToDrawS :: Figure -> BlockedFigure
figureToDrawS (Figure _ d c) 
  | (d == DUp) || (d == DDown) = (c {x = x c - bs, y = y c + bs}, c {x = x c - bs}, c, c {y = y c - bs})
  | otherwise                  = (c {x = x c - bs},c, c {y = y c + bs}, c {x = x c + bs, y = y c + bs})
  where
    bs = blockSize

-- Preparando a figura em forma de J para renderizacao
figureToDrawJ :: Figure -> BlockedFigure
figureToDrawJ (Figure _ d c) 
  | d == DDown  = (c {x = x c - bs, y = y c - bs}, c {y = y c - bs}, c, c {y = y c + bs})
  | d == DUp    = (c {y = y c - bs},c,c {y = y c + bs}, c {x = x c + bs, y = y c + bs})
  | d == DRight = (c {x = x c - bs},c,c {x = x c + bs}, c {x = x c + bs, y = y c - bs})
  | otherwise   = (c {x = x c - bs, y = y c + bs}, c {x = x c - bs}, c,      c {x = x c + bs})
  where
    bs = blockSize

-- Preparando a figura em forma de L para renderizacao
figureToDrawL :: Figure -> BlockedFigure
figureToDrawL (Figure _ d c) 
  | d == DDown  = (c {y = y c + bs},c,c {y = y c - bs}, c {x = x c + bs, y = y c - bs})
  | d == DUp    = (c {y = y c - bs},c,c {y = y c + bs}, c {x = x c - bs, y = y c + bs})
  | d == DRight = (c {x = x c - bs},c,c {x = x c + bs}, c {x = x c + bs, y = y c + bs})
  | otherwise   = (c {x = x c - bs, y = y c - bs}, c {x = x c - bs}, c,      c {x = x c + bs})
  where
    bs = blockSize

-- Preparando a figura em forma de T para renderizacao
figureToDrawT :: Figure -> BlockedFigure
figureToDrawT (Figure _ d c) 
  | d == DDown  = (c {x = x c - bs}, c, c {x = x c + bs}, c {y = y c - bs})
  | d == DUp    = (c {x = x c - bs}, c, c {x = x c + bs}, c {y = y c + bs})
  | d == DRight = (c {y = y c + bs}, c, c {y = y c - bs}, c {x = x c + bs})
  | otherwise   = (c {y = y c + bs}, c, c {y = y c - bs}, c {x = x c - bs})
  where
    bs = blockSize






-- =========================================
-- Movendo
-- =========================================

-- movendo a figura para a esquerda em um passo
moveLeft::GameState -> GameState
moveLeft u |((typemoving u)==TetrisStepped && (typerepres u)== TetrisRect) = moveLeftSteppedRect u 
           |((typemoving u)==TetrisStepped && (typerepres u)== TetrisRound) = moveLeftSteppedRound u
           | ((typemoving u)==TetrisSmooth && (typerepres u)== TetrisRect) = moveLeftSmoothRect u

           |otherwise = moveLeftSmoothRound u


-- movendo a figura para a esquerda de maneira escalonada 
-- em uma forma retangular
moveLeftSteppedRect ::GameState -> GameState
moveLeftSteppedRect u   | collidewall = u{   figure =cons (mul8or9 (getf(figure u))) (rest  (figure u))}
                    | collide = u
                    |otherwise = u{ figure = cons  (minbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSides (figureToDraw (minbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallLeft (figureToDraw (minbl (getf(figure u)))) (board u)


-- movendo a figura para a esquerda em um passo em um circulo
moveLeftSteppedRound ::GameState -> GameState
moveLeftSteppedRound u    = u{ figure = cons  (minbl (getf(figure u))) (rest  (figure u))}


-- dependendo do tipo de figura, é preciso decidir quanto mover 
-- antes do comeco, se o usuario cruzou a fronteira
mul8or9::Figure->Figure
mul8or9  (Figure s t u)   |( s==O )|| (s==Z && (t /= DUp && t/=DDown))
                                               ||(s==S && (t/= DUp && t/=DDown))
                                               ||(s==J && t/= DDown)
                                               ||(s==L && t /=DUp)||(s==T && t/=DLeft) 
                                                = (Figure s t u{x = 8*blockSize})
                          |(s==I && (t /=DDown && t/=DUp)) = (Figure s t u{x = 7*blockSize})
                          |otherwise = (Figure s t u{x = 9*blockSize})            

-- pegando o bloco da coordenada
minbl::Figure->Figure
minbl  (Figure s t u)   = (Figure s t u{x = (x u) - blockSize})  


-- retornando o final da lista
rest ::[Figure]->[Figure]
rest (_:fs) = fs
rest [] = []


-- anexando a figura a lista de figuras
cons ::Figure->[Figure]->[Figure]
cons  a f = a:f

-- virando a esquerda para o suave em um retangular
moveLeftSmoothRect ::GameState -> GameState
moveLeftSmoothRect u   | collidewall = u{   figure =cons (mul8or9 (getf(figure u))) (rest  (figure u))}
                   | collide = u
                   |otherwise = u{ figure = cons  (minbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSidesSmooth (figureToDraw (minbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallLeft (figureToDraw (minbl (getf(figure u)))) (board u)      


-- virando a esquerda no suave em um circular
moveLeftSmoothRound ::GameState -> GameState
moveLeftSmoothRound u   = u{ figure = cons  (minbl (getf(figure u))) (rest  (figure u))}

                   



-- virando a direita
moveRight::GameState -> GameState
moveRight u |((typemoving u)==TetrisStepped && ((typerepres u) == TetrisRect)) = moveRightSteppedRect u 
            |((typemoving u)==TetrisStepped && ((typerepres u) == TetrisRound)) = moveRightSteppedRound u
            |((typemoving u)==TetrisSmooth && ((typerepres u) == TetrisRect)) = moveRightSmoothRect u
            |otherwise = moveRightSmoothRound u



-- virando a direita no modo retangular
moveRightSteppedRect ::GameState -> GameState
moveRightSteppedRect u   | collidewall = u{   figure =cons (bl (getf(figure u))) (rest  (figure u))}
                     | collide = u
                     |otherwise = u{ figure = cons  (plbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSides (figureToDraw (plbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallRight (figureToDraw (plbl (getf(figure u)))) (board u)


-- movendo a direita no modo circular
moveRightSteppedRound ::GameState -> GameState
moveRightSteppedRound u    = u{ figure = cons  (plbl (getf(figure u))) (rest  (figure u))}

                    


-- dependendo do tipo de figura, decidimos o quanto 
-- comecamos a mover a forma
bl::Figure->Figure
bl  (Figure s t u)  | (s==O &&t == DDown)||(s== Z)||(s==S)||(s==J && t/=DUp)
                                         || (s==L && t/=DDown)||(s==T && t == DUp)
                                         ||(s==T && t /= DRight)||(s==I && t /= DUp && t/=DDown ) 
                                         = (Figure s t u{x = blockSize})
                    | otherwise =    (Figure s t u{x = 0})           
-- para a coordenada da figura, adicionamos um bloco
plbl::Figure->Figure
plbl  (Figure s t u)   = (Figure s t u{x = (x u) + blockSize})  

-- virando a direira para a circular em um retangular
moveRightSmoothRect ::GameState -> GameState
moveRightSmoothRect u   | collidewall = u{   figure =cons (bl (getf(figure u))) (rest  (figure u))}
                    | collide = u
                    |otherwise = u{ figure = cons  (plbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSidesSmooth (figureToDraw (plbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallRight (figureToDraw (plbl (getf(figure u)))) (board u)   


-- virando a direita para o suave em um retangular
moveRightSmoothRound ::GameState -> GameState
moveRightSmoothRound u    = u{ figure = cons  (plbl (getf(figure u))) (rest  (figure u))}

                    

-- verificando se o bloco da borda da janela do jogo se cruza
collidesBlock::Coord -> Bool
collidesBlock Coord{..} | (x < 0) || (x  + blockSize > screenWidth) 
                              || (y < 0) 
                              || (y + blockSize > screenHeight) = True
                       |otherwise = False

-- verificando se o cloco cruza as bordas laterais da janela
-- ou se a placa esta em um trampolim escalonado
collidesBlockSides::Coord -> Board -> Bool
collidesBlockSides block [] = (x block < 0) || (x block  + blockSize > screenWidth)
collidesBlockSides block (c:[]) = (x block < 0)   || (x block + blockSize > screenWidth) 
                                                  || (x block == x c) && (y block == y c)

collidesBlockSides block (c:brds) | (x block < 0) || (x block + blockSize > screenWidth) 
                                                  || (x block == x c) && (y block == y c)  = True
                                  | otherwise = collidesBlockSides block brds

-- verificando os limites do campo a esquerda
collidesBlockSidesWhallLeft::Coord -> Board -> Bool
collidesBlockSidesWhallLeft u _= (x u) <0

-- verificando os limites do campo a direita
collidesBlockSidesWhallRight::Coord -> Board -> Bool
collidesBlockSidesWhallRight u _= ((x u) + blockSize) > screenWidth

-- verificando se o bloco cruza laterais da janela ou
-- ou se a placa esta em um trampolim escalonado
collidesBlockSidesSmooth::Coord -> Board -> Bool
collidesBlockSidesSmooth u [] = ((x u) < 0) || ((x u)  + blockSize > screenWidth)
collidesBlockSidesSmooth u (u1:[]) = ((x u) < 0) || ((x u)  + blockSize > screenWidth) 
                                  || ((x u) ==(x u1)) && ((y u)==(y u1))
                                  ||(((x u)==(x u1)) &&((y u)>((y u1) - blockSize) && (y u)<((y u1) + blockSize)))

collidesBlockSidesSmooth u (u1:brds) | ((x u) < 0) || ((x u)  + blockSize > screenWidth) 
                                       || ((x u) ==(x u1)) && ((y u)==(y u1))
                                       ||(((x u)==(x u1)) &&((y u)>((y u1) - blockSize) && (y u)<((y u1) + blockSize))) = True
                                     | otherwise = collidesBlockSides u brds

-- verificando se o bloco cruza o piso ou a placa em um degrau
collidesBlockDown::Coord -> Board-> Bool
collidesBlockDown block []  =   (y block + blockSize > screenHeight)
collidesBlockDown block (c:[])  =   ((y block + blockSize > screenHeight) || ((x block == x c) && (y block == y c)) 
                                                  || (((x block) + 300) == x c && (y block == y c)) 
                                                  || (((x block) - 300)== x c && (y block == y c)))

collidesBlockDown block (c:brds)  | ((y block + blockSize > screenHeight) || (x block == x c) && (y block == y c)
                                                  || (((x block) + 300) == x c && (y block == y c))
                                                  || (((x block) - 300)== x c && (y block == y c)))  = True
                                  |  otherwise = collidesBlockDown block brds
                                
-- verificando se o bloco cruza o chao ou a tabua de forma suave
collidesBlockDownSmooth::Coord -> Board-> Bool
collidesBlockDownSmooth u []  =   ((y u)  > screenHeight)        || ((y u) < 0)

collidesBlockDownSmooth u (u1:[])  = ((((y u)  > screenHeight)   || ((x u) ==(x u1)) && (((y u) )==(y u1)))
                                                      || ((y u) < 0) 
                                                      || (((x u) + 300) == x u1 && (y u == y u1)) 
                                                      || (((x u) - 300)== x u1 && (y u == y u1)))

collidesBlockDownSmooth u (u1:brds)  |  ((((y u)  > screenHeight)|| ((x u) ==(x u1)) && (((y u) )==(y u1)))
                                                      || ((y u) < 0)|| (((x u) + 300) == x u1 && (y u == y u1)) 
                                                      || (((x u) - 300)== x u1 && (y u == y u1))) =True
                                     |  otherwise = collidesBlockDownSmooth u brds
                           
-- verifica se o bloco cruza o teto ou o tabuleiro
collidesBlockUp :: Coord -> Board-> Bool
collidesBlockUp c []                    =  y c < 0
collidesBlockUp c (c1 : [])   = (y c < 0) && (y c == y c1)
collidesBlockUp c (c1 : brds)  
  | y c < 0 && (y c == y c1)  = True
  | otherwise                 = collidesBlockUp c brds

-- a figura cruza o tabuleiro ou a borda de um modo escalonado?
collidesFigure::BlockedFigure -> Board -> Bool
collidesFigure (a,b,c,d) board = (collidesFigureSides (a,b,c,d) board) || (collidesFigureDown (a,b,c,d) board)

-- a figura cruza o tabuleiro ou faz bordas sem problemas?
collidesFigureSmooth :: BlockedFigure -> Board -> Bool
collidesFigureSmooth (a, b, c, d) brd = or
  [ collidesFigureSidesSmooth (a, b, c, d) brd
  , collidesFigureDownSmooth  (a, b, c, d) brd ]

-- verificando se a figura cruza as bordas laterais da janela ou a plca
collidesFigureSides::BlockedFigure -> Board -> Bool
collidesFigureSides (a,b,c,d) board | (collidesBlockSides a board) 
                                        || (collidesBlockSides b board) 
                                        || (collidesBlockSides c board) 
                                        || (collidesBlockSides d board) = True
                                    |otherwise = False

-- a fronteira cruza a esquerda em um degrau ( para que o 
-- usuario possa zanzar) ?
collidesFigureSidesWallLeft    ::BlockedFigure -> Board -> Bool
collidesFigureSidesWallLeft  (a,b,c,d) board | (collidesBlockSidesWhallLeft a board)
                                              || (collidesBlockSidesWhallLeft b board) 
                                              || (collidesBlockSidesWhallLeft c board) 
                                              || (collidesBlockSidesWhallLeft d board) = True
                                             |otherwise = False

-- A fronteira cruza a direita em etapas ( para que o usuario
-- possa zanzar)?
collidesFigureSidesWallRight::BlockedFigure -> Board -> Bool
collidesFigureSidesWallRight (a,b,c,d) board | (collidesBlockSidesWhallRight a board) 
                                               || (collidesBlockSidesWhallRight b board) 
                                               || (collidesBlockSidesWhallRight c board) 
                                               || (collidesBlockSidesWhallRight d board) = True
                                             |otherwise = False

-- A figura cruza suavemente
collidesFigureSidesSmooth::BlockedFigure -> Board -> Bool
collidesFigureSidesSmooth (a,b,c,d) board | (collidesBlockSidesSmooth a board)
                                            || (collidesBlockSidesSmooth b board) 
                                            || (collidesBlockSidesSmooth c board)
                                            || (collidesBlockSidesSmooth d board) = True
                                          |otherwise = False        

-- Verifique se a figura toca a parte inferior da placa ou o 
-- campo em uma etapa.
collidesFigureDown::BlockedFigure -> Board -> Bool
collidesFigureDown (a,b,c,d) board | (collidesBlockDown a board) 
                                     || (collidesBlockDown b board) 
                                     || (collidesBlockDown c board) 
                                     || (collidesBlockDown d board) = True
                                   |otherwise = False

-- verifique se a figura toca a parte inferior da placa ou o campo
-- de forma suave
collidesFigureDownSmooth::BlockedFigure -> Board -> Bool
collidesFigureDownSmooth (a,b,c,d) board | (collidesBlockDownSmooth a board) 
                                           || (collidesBlockDownSmooth b board) 
                                           || (collidesBlockDownSmooth c board)
                                           || (collidesBlockDownSmooth d board) = True
                                        |otherwise = False



-- Fim do jogo
isGameOver::GameState -> Bool
isGameOver u |((typemoving u) == TetrisStepped) = collidesFigureDown (figureToDraw 
                                                  (getf(rest  (figure u))) ) (board u)  
             |otherwise = collidesFigureDownSmooth (figureToDraw (getf(figure u))) (board u)



-- Cassificando linhas
sortRows :: Board -> Board
sortRows []     = []
sortRows (c : brds) = sortRows (filter (\c1 -> y c1 > y c) brds) ++ [c] ++ sortRows 
                (filter (\c1 -> y c1 <= y c) brds)

-- Cadeia de preeenchimento descartadas
deleteRows :: Board -> Board
deleteRows [] = []
deleteRows (c : brds)
  | isFullRow (row brd (y c)) = deleteRows . boardMoveDown $ (upperRows brd (y c)) ++ (lowerRows brd (y c))
  | otherwise = (row brd (y c)) ++ (deleteRows (upperRows brd (y c)))
  where 
    brd = (c : brds)

-- as linhas sao maiores que a string especificada
upperRows :: Board -> Int -> Board
upperRows brd scope = (filter (\c1 -> y c1 < scope) brd)

-- as linhas estao abaixo da string dada
lowerRows :: Board -> Int -> Board
lowerRows brd scope = (filter (\c1 -> y c1 > scope) brd)

-- deslocando as linhas do tabuleiro para baixo
boardMoveDown :: Board -> Board
boardMoveDown [] = []
boardMoveDown (c : brd) = c {y = (y c) + blockSize} : boardMoveDown brd

-- n-esima linha do tabuleiro
row :: Board -> Int -> [Coord]
row b n = (filter (\b1 -> n == y b1) b)

--o tabuleiro esta cheio
isFullRow :: [Coord] -> Bool
isFullRow r = (length r) >= 10



-- Pressionando a tecla 'para baixo, a forma cai
dropit::GameState -> Int -> GameState
dropit u ptr |(typemoving u)  == TetrisStepped = dropitStepped u ptr
             |otherwise = dropitSmooth u ptr
-- Cai em um passo
dropitStepped ::GameState -> Int -> GameState
dropitStepped u pts | collide = u{score = ((score u) + (div pts blockSize))}   
                    |otherwise = dropitStepped u{figure = cons  (plbly (getf(figure u)))   
                     (rest  (figure u))} pts
                    where                                           
                collide = collidesFigureDown (figureToDraw (plbly (getf (figure u)))) (board u)
-- adicionado o tamanho do bloco a ordenada
plbly::Figure->Figure
plbly  (Figure s t u)   = (Figure s t u{y = (y u) + blockSize}) 

-- cai em um passo 
dropitSmooth ::GameState -> Int -> GameState
dropitSmooth u pts | collide = u{score = (score u) + (div pts blockSize)}   
                    |otherwise = dropitSmooth u{figure = cons  (ploy (getf (figure u)))   (rest  (figure u))} pts
                    where                                           
                    collide = collidesFigureDownSmooth (figureToDraw (plbly (getf (figure u)))) (board u)

-- adicione tamanho 1 a ordenada
ploy::Figure->Figure
ploy  (Figure s t u)   = (Figure s t u{y  = (y u) + 1}) 


-- =========================================
-- Desenhando
-- =========================================

-- Desenhe uma tabua de forma retangular
drawBoard::Board  -> Picture
drawBoard s = pictures (map drawBlock s)

-- Desenho um quadro em uma circular
drawBoardCircle :: Board -> Picture
drawBoardCircle s = pictures (map drawBlockCircleHelp s)

-- auxiliar para desenhar uma circular
drawBlockCircleHelp :: Coord -> Picture
drawBlockCircleHelp u = drawBlockCircle (fromCoord u)


-- desenhando o bloco em tetris circular
drawBlockCircle :: Coord1-> Picture
drawBlockCircle  (b,c,d) 
                    |(b==270|| b==570|| b==870||b== -30|| b== -330|| b== -630) = 
  pictures [ translate (-w) (h - offset2) (scale  myscale myscale  ( pictures[ 
    (rotate (-specangel) (color (numtocolor d)   (thickArc (0) (angle) 
                      (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) 
                      ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -thicknessTwo ) (fromIntegral 1) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) 
                      (fromIntegral (c + sizefitInt-offsedge) / sizefit -thicknessTwo ) (fromIntegral 1) )) ),
   (rotate (-specangel) (color magenta   (thickArc (0) (1) 
                       (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) ))),
    (rotate (-specangel) (color magenta   (thickArc (angle - 1) (angle) 
                       (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) ))) ]))
    ]                 
    
        
     
                   |otherwise = pictures [ translate (-w) (h - offset2) (scale  myscale myscale  (pictures
 [ 
  color (numtocolor d)   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) 
                          (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) ) ,            
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) 
                          (fromIntegral (c + sizefitInt+offsedge) / sizefit  - thicknessTwo) (fromIntegral 1) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) 
                          (fromIntegral (c + sizefitInt-offsedge) / sizefit  - thicknessTwo) (fromIntegral 1) ) ,

   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle ) (((fromIntegral b)/blockSizeFloat)*angle + 1) 
                         (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle +35) (((fromIntegral b)/blockSizeFloat)*angle + angle)
                       (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) )
   ]))
    ]
  where
  w = fromIntegral 0
  h = fromIntegral 0

-- | Quadro  para  blocos
magframe :: Int -> Int -> [Picture]
magframe b c = 
  [ color magenta  (polygon [ (fromIntegral b,        fromIntegral (-c))
                            , (fromIntegral b,        fromIntegral (-c - 2))
                            , (fromIntegral (b + blockSize), fromIntegral (-c - 2))
                            , (fromIntegral (b + blockSize), fromIntegral (-c)) 
                            ])
  , color magenta  (polygon [ (fromIntegral b,        fromIntegral (-c))
                            , (fromIntegral b,        fromIntegral (-c - blockSize))
                            , (fromIntegral (b + 2),  fromIntegral (-c - blockSize))
                            , (fromIntegral (b + 2),  fromIntegral (-c))
                            ])
  , color magenta  (polygon [ (fromIntegral b,        fromIntegral (-c - blockSize - 2))
                            , (fromIntegral b,        fromIntegral (-c - blockSize))
                            , (fromIntegral (b + blockSize), fromIntegral (-c - blockSize))
                            , (fromIntegral (b + blockSize), fromIntegral (-c - blockSize - 2))
                            ])
  , color magenta  (polygon [ (fromIntegral (b + blockSize - 2), fromIntegral (-c))
                            , (fromIntegral (b + blockSize - 2), fromIntegral (-c - blockSize))
                            , (fromIntegral (b + blockSize), fromIntegral (-c - blockSize))
                            , (fromIntegral (b + blockSize), fromIntegral (-c)) 
                            ])
  ]

-- correspondendo os numeros das cores
numtocolor :: Int -> Color
numtocolor 0 = azure
numtocolor 1 = blue
numtocolor 2 = yellow
numtocolor 3 = red
numtocolor 4 = green
numtocolor 5 = orange
numtocolor _ = white

-- desenhando um bloco em tetris retangular
drawBlock :: Coord-> Picture
drawBlock  crd 
  =  pictures [ translate (-w) h (scale  1 1 (pictures (
                  [ color (numtocolor clr1) (polygon [ (fromIntegral b,         fromIntegral (-c))
                                                    , (fromIntegral b,         fromIntegral (-c - blockSize))
                                                    , (fromIntegral  (b + blockSize), fromIntegral (-c - blockSize))
                                                    , (fromIntegral  (b + blockSize), fromIntegral (- c))
                                                    ])
                  ] ++ (magframe b c)))) ]
  where
    w = fromIntegral screenWidth  / transl
    h = fromIntegral screenHeight / transl
    b = x crd
    c = y crd
    clr1 = clr crd

-- desenhando uma figura de forma retangular
drawFigure::GameState  ->  Picture
drawFigure u = drawBlockedFigure  (figureToDraw (getf (figure u) ))


-- desenhando uma figura na circular
drawFigureCircle::GameState  ->  Picture
drawFigureCircle u = drawBlockedFigureCircle(figureToDraw (getf (figure u) ))

-- obtem o primeiro item da lista
getf ::[Figure]->Figure
getf (f:_) = f
getf [] = Figure T DUp Coord {x = div screenWidth 2, y = blockSize * 2, clr = 0}
  
-- desenhando uma forma de blocos de forma circular
drawBlockedFigureCircle :: BlockedFigure -> Picture
drawBlockedFigureCircle ((a, b, c, d)) =         pictures  [drawBlockCircle  (fromCoord a),
                                                            drawBlockCircle   (fromCoord b ),
                                                            drawBlockCircle   (fromCoord  c),
                                                            drawBlockCircle    (fromCoord d )]

-- Desenhando uma forma de blocos de forma retangular
drawBlockedFigure::BlockedFigure -> Picture
drawBlockedFigure ((a, b, c, d)) =         pictures  [drawBlock   a,
                                                      drawBlock   b,
                                                      drawBlock   c,
                                                      drawBlock   d ]



-- Desenhando quadrado
rect :: Point -> Point -> Picture
rect (l, b) (r, t) = polygon [ (l, b), (l, t), (r, t), (r, b) ]

-- retangulo com bordas arredondadas e um limite de uma determinada
-- espessura
roundedRect
  :: Color    -- ^ Preencendo com cor
  -> Color    -- ^ cor da borda
  -> Float    -- ^ largura do retangulo
  -> Float    -- ^ altura do retangulo
  -> Float    -- ^ raio da curvatura
  -> Float    -- ^ espessura da borda
  -> Picture
roundedRect innerColor borderColor w h r d = pictures
  [ color innerColor inner
  , color borderColor border
  ]
  where
    border = pictures
      [ rect (-w/2 - d/2, -h/2 + r) (-w/2 + d/2, h/2 - r)
      , rect ( w/2 - d/2, -h/2 + r) ( w/2 + d/2, h/2 - r)
      , rect ( w/2 - r, -h/2 + d/2) (-w/2 + r, -h/2 - d/2)
      , rect ( w/2 - r,  h/2 + d/2) (-w/2 + r,  h/2 - d/2)
      , translate (-w/2 + r) ( h/2 - r) (rotate 270 cornerBorder)
      , translate (-w/2 + r) (-h/2 + r) (rotate 180 cornerBorder)
      , translate ( w/2 - r) (-h/2 + r) (rotate 90 cornerBorder)
      , translate ( w/2 - r) ( h/2 - r) cornerBorder
      ]

    inner = pictures
      [ rect (-w/2, -h/2 + r) (-w/2 + r,  h/2 - r)
      , rect ( w/2, -h/2 + r) ( w/2 - r,  h/2 - r)
      , rect (-w/2 + r, -h/2) ( w/2 - r, -h/2 + r)
      , rect (-w/2 + r,  h/2) ( w/2 - r,  h/2 - r)
      , rect (-w/2 + r, -h/2 + r) (w/2 - r, h/2 - r)
      , translate (-w/2 + r) ( h/2 - r) (rotate 270 corner)
      , translate (-w/2 + r) (-h/2 + r) (rotate 180 corner)
      , translate ( w/2 - r) (-h/2 + r) (rotate 90 corner)
      , translate ( w/2 - r) ( h/2 - r) corner
      ]

    corner = thickArc 0 90 (r/2) r
    cornerBorder = thickArc 0 90 r d


-- | Altura do campo
fieldHeight :: Float
fieldHeight = 40

-- | Largura do campo
fieldWidth :: Float
fieldWidth = 150

-- | Desenhando o Tetris
drawTetris ::GameState-> Picture
drawTetris u | ((typerepres u)==TetrisRound) =  pictures
  [ drawFigureCircle  u,
    drawBoardCircle (board u),
    drawScore (score u),
    drawCircleBackGr,
    drawRectangleMenu,
    drawmenuCircle tetrTypbutton,
    drawmenuSmooth tetrMoveButton,
    drawtextCircle,
    drawtextSmooth 
  
  
  ] 
    |otherwise = pictures
  [drawFigure  u,
   drawBoard (board u) ,
   drawScore (score u),
   drawRectangleMenu,
   drawmenuCircle tetrTypbutton,
   drawmenuSmooth tetrMoveButton,
   drawtextCircle ,
   drawtextSmooth  ,
   drawRectanglBackGr
  ] 

-- desenhando um quadrado de fundo
drawRectanglBackGr::Picture
drawRectanglBackGr = pictures [ translate ((-(fromIntegral screenWidth  / transl)))
                      (fromIntegral screenHeight / transl) (scale  scaleBackGrTwo scaleBackGrTwo   
                    (color cyan (line [(0,0) , (0,-screenHeightFloat),
                      (screenWidthFloat,-screenHeightFloat),(screenWidthFloat,0),(0,0)])))]


scaleBackGrTwo::Float
scaleBackGrTwo = 1

screenHeightFloat::Float
screenHeightFloat = 600

screenWidthFloat ::Float
screenWidthFloat = 300

scalCirclB ::Float
scalCirclB = 1.3

drawTextConSmooth::Float
drawTextConSmooth = -18

drawTextConCircle::Float
drawTextConCircle = 4
translMenu::Float
translMenu = 10

translMenu2::Float
translMenu2= 15
transCircleBack::Float
transCircleBack = 0
consCircBack::Float
consCircBack = 43
consCircBackTwo::Float
consCircBackTwo = 53
tranRectCon::Float
tranRectCon = -0.3
tranRectConTwo::Float
tranRectConTwo = 0.02

tranRectConThree::Float
tranRectConThree = 0.1

-- desenhando um circulo de fundo
drawCircleBackGr::Picture
drawCircleBackGr = translate (transCircleBack) ((transCircleBack)  + transtBackGround) 
              (scale scalCirclB scalCirclB (color cyan (circle  ( conCircleB))))

-- desenhando o menu
drawRectangleMenu ::Picture
drawRectangleMenu = translate (tranRectCon * fieldWidth/transl + conRecMenu) 
               (fieldHeight/transl + (fromIntegral screenHeight /transl) - consCircBackTwo)
               (roundedRect (withAlpha alpha white) (greyN alpha) ( fieldWidth/transl + consCircBack) 
               ((fromIntegral screenHeight / translMenu) + fieldHeight / translMenu2) 
                                   (tranRectConThree * fieldWidth) (tranRectConTwo * fieldWidth))

-- desenhando um botao para selecionar um modo suave
drawmenuSmooth:: (Float,Float,Float,Float)->Picture
drawmenuSmooth (a,b,c,d) = (color orange (polygon [ (a, d), (a, c), (b, c), (b, d) ]))

-- Desenhando um botao para selecionar o modo circular
drawmenuCircle :: (Float,Float,Float,Float)->Picture
drawmenuCircle (a,b,c,d) =  (color yellow (polygon [ (a, d), (a, c), (b, c), (b, d) ]))

-- desenhando texto no botao com o modo suave
drawtextSmooth :: Picture
drawtextSmooth = translate (-(fromIntegral screenWidth  / transl) + conSmooth) 
                     (fromIntegral screenHeight / transl  + drawTextConSmooth) 
                      (scale scaleTSmooth scaleTSmooth
                    (pictures [translate (transl) (translateT) (scale scaleT scaleT (color red (text  "Smooth")))]))

-- Desenhando texto no botao com o modo circular
drawtextCircle :: Picture
drawtextCircle = translate (-(fromIntegral screenWidth  / transl) + conCircle) 
                  (fromIntegral screenHeight / transl + drawTextConCircle) 
                      (scale blockSizeFloat blockSizeFloat
                   (pictures [translate (transl) (translateT) (scale scaleT scaleT (color red (text  "Type")))]))


-- desenhando o local de scores
drawScore :: Score -> Picture
drawScore score = translate (-w) h (scale blockSizeFloat blockSizeFloat (pictures
  [ translate transl (translateT) (scale scaleSc scaleSc (color green (text (show score)))) 
  ]))
  where
    w = fromIntegral screenWidth  / transl
    h = fromIntegral screenHeight / transl





-- =========================================
-- Atualizando
-- =========================================

-- uma tupla que esta na lista
vectolist :: (Coord, Coord, Coord, Coord) -> [Coord]
vectolist (a,b,c,d) = [a,b,c,d]

-- atualizando quadro
updateBoard::Figure -> Board ->Board
updateBoard (Figure sha dir u) a = a ++ vectolist (figureToDraw (Figure sha dir u))




-- Atualizando o Tetris
-- cada quadro move a figura para baixo e, enquanto aqui,
-- verifica se a figura do limite inferior  ja foi alcancada
updateTetris :: Float -> GameState -> GameState
updateTetris dt u |(typemoving u) == TetrisStepped = updateTetrisStepped dt u 
             |otherwise = updateTetrisSmooth dt u

-- atualizando pisado
updateTetrisStepped :: Float -> GameState -> GameState 
updateTetrisStepped dt u |gameover = genNewUniverse u (inintTactStepped)  
                    |otherwise = newLevel (newTact u dt (extrSpeed (speedandtime u)) ) 
                               where
                              gameover = isGameOver u   

-- atualizando suave
updateTetrisSmooth :: Float -> GameState -> GameState 
updateTetrisSmooth dt u |gameover = genNewUniverse u (inintTactSmooth)  
                    |otherwise = newLevel (newTact u dt (extrSpeed (speedandtime u)) )                                
                       where
                       gameover = isGameOver u   


genNewUniverse :: GameState ->Float -> GameState
genNewUniverse u fl = u{ board   = genEmptyBoard  
                          , figure  = (rest  (figure u))
                          , speedandtime   = (fl, 0)
                          , score    = 0                                    
                          ,tactgamestate     = fl
                          }  




-- ===========================================
-- Cronometragem
-- ===========================================


-- Nova tatica
newTact::GameState -> Float -> Float -> GameState
newTact u  dt tact |(typemoving u) == TetrisStepped = newTactStepped u dt tact
          |otherwise = newTactSmooth u dt tact

-- definindo o tempo para 0
chZ :: (Speed,Time)    ->(Speed,Time)  
chZ (sp,_) = (sp,0) 

-- tirando tempo
extrTime:: (Speed,Time) -> Time
extrTime (_,ti) = ti

-- extraindo velocidade
extrSpeed:: (Speed,Time) -> Speed
extrSpeed (sp,_) = sp

-- alterando o tempo e a velocidade
chSpeedAndTime::(Speed,Time) ->Float->Float ->(Speed,Time)
chSpeedAndTime (sp,ti) tact dt = (sp, ti + dt + tact * 0.3)

-- alterando o tempo e a velocidade
pldtSpeedAndTime::(Speed,Time) ->Float->(Speed,Time)
pldtSpeedAndTime (sp, ti ) dt= (sp, ti + dt)

-- nova tatica no piso
newTactStepped :: GameState -> Float -> Float -> GameState
newTactStepped u dt tact  | paused =u  
  | new && collides = u{ board = (deleteRows (sortRows (updateBoard (getf(figure u)) (board u)))), 
                        figure = (rest  (figure u)), 
                        speedandtime = (speedandtime u),
                        score =  (score u) + 1
                        }
  | new = newTact u{figure = cons  (plbly (getf(figure u)))   (rest  (figure u)) ,
                      speedandtime = chZ (speedandtime u)} (dt + (extrTime (speedandtime u)) - tact) tact
  | collides = u{speedandtime = (chSpeedAndTime (speedandtime u) tact dt) }
  | otherwise = u{speedandtime = pldtSpeedAndTime (speedandtime u) dt}
                    where
                     new = (extrTime (speedandtime u)) + dt >= tact
                     collides =  collidesFigureDown (figureToDraw (plbly (getf(figure u)))) (board u)
                     paused = (extrSpeed(speedandtime u)) < 0

-- nova tatica no suave
newTactSmooth :: GameState -> Float -> Float -> GameState
newTactSmooth u dt tact  | paused =u  
  | new && collides = u{ board = (deleteRows (sortRows (updateBoard (getf(figure u)) (board u)))), 
                        figure = (rest  (figure u)), 
                        speedandtime = (speedandtime u),
                        score =  (score u) + 1
                        }
  | new = newTact u{figure = cons  (ploy (getf(figure u)))   (rest  (figure u)) ,
                      speedandtime = chZ (speedandtime u)} (dt + (extrTime (speedandtime u)) - tact) tact
  | collides = u{speedandtime = (chSpeedAndTime (speedandtime u) tact dt) }
  | otherwise = u{speedandtime = pldtSpeedAndTime (speedandtime u) dt}
                where
                new = (extrTime (speedandtime u)) + dt >= tact
                collides =  collidesFigureDownSmooth (figureToDraw (plbly (getf(figure u)))) (board u)
                paused = (extrSpeed(speedandtime u)) < 0




-- novo nivel
newLevel::GameState->GameState
newLevel u | l5 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.1 ,(extrTime (speedandtime u)))  }
  | l4 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.15 ,(extrTime (speedandtime u)))  }
  | l3 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.2 ,(extrTime (speedandtime u)))  }
  | l2 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.25 ,(extrTime (speedandtime u)))  }
  | l2 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.3 ,(extrTime (speedandtime u)))  }
  | l1 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.4 ,(extrTime (speedandtime u)))  }
  | otherwise = u
        where 
          l5 = (score u) >= 5000
          l4 = (score u) >= 3000 && (score u) <= 5000
          l3 = (score u) >= 2000 && (score u) <= 3000
          l2 = (score u) >= 1500 && (score u) <= 2000
          l1 = (score u) >= 1000 && (score u) <= 1500








-- =====================
-- Manipulacao das pecas
-- =====================

-- | Controle do Tetris
handleTetris :: Event -> GameState -> GameState

handleTetris (EventKey (Char 'l') Down _ _) u = moveRight u
handleTetris (EventKey (Char 'l') Up _ _) t = t

handleTetris (EventKey (Char 'j') Down _ _)  u  = moveLeft  u
handleTetris (EventKey (Char 'j') Up _ _)  t  = t

handleTetris(EventKey (SpecialKey KeySpace) Down _ _ ) u  = dropit u (screenHeight -  (getc (getf (figure u))))
handleTetris(EventKey (SpecialKey KeySpace) Up _ _ ) t = t

handleTetris (EventKey (Char 'k') Down _ _ ) u = turn  u
handleTetris (EventKey (Char 'k') Up _ _ ) t = t

handleTetris (EventKey (Char 'p') Down _ _ ) u = tetrispause  u
handleTetris (EventKey (Char 'p') Up _ _ ) t = t

handleTetris (EventKey (MouseButton LeftButton) Up _ mouse) u =  (mouseToCell mouse   u )
handleTetris  _ t = t  


-- Pausa
tetrispause :: GameState->GameState
tetrispause u = u { speedandtime = pause (speedandtime u )}

-- Pausa auxiliar
pause :: (Speed,Time) -> (Speed,Time)
pause (sp, ti) = (-sp,ti)

 
-- Puxando Y para fora
getc :: Figure-> Int
getc  (Figure _ _ u) = (y u)


-- Trabalhando clicando nos botoes
mouseToCell :: Point->GameState -> GameState
mouseToCell (x, y) u  
         | onTypeButton (x,y) tetrTypbutton =
                                        u{typerepres = switchTetrisType (typerepres u)}                                                               
         | onMoveButton (x,y) tetrMoveButton = chMoving u 
         |otherwise =  u
onTypeButton::Point -> (Float,Float,Float,Float) -> Bool
onTypeButton (x,y)  (x1,x2,y1,y2)=(x> x1 && x<x2 && y > y1 && y < y2 ) 

onMoveButton ::Point -> (Float,Float,Float,Float) -> Bool
onMoveButton (x,y)  (x1,x2,y1,y2)=(x> x1 && x<x2 && y > y1 && y < y2 )                               
-- Alterando o tipo de tetrus (retangular ou circular)
switchTetrisType :: TetrisType -> TetrisType
switchTetrisType TetrisRect = TetrisRound
switchTetrisType TetrisRound = TetrisRect



-- Gerando um novo tetris, dependendo do tipo de tetris
chMoving:: GameState->GameState
chMoving u |(typemoving u)==TetrisStepped = genTetris u inintTactSmooth 
           |otherwise =  genNewUniverse u inintTactStepped 

genTetris::GameState-> Float->GameState
genTetris u df= u{ board   = genEmptyBoard  
                          , figure  = (rest  (figure u))
                          , speedandtime   = (df, 0)
                          , score    = 0
                          
                          ,typemoving =  switchTetrisMove (typemoving u)
                          ,tactgamestate     = df
                          }
-- Alterando o tipo de teris (plano ou etapa)
switchTetrisMove :: TetrisMove -> TetrisMove
switchTetrisMove TetrisStepped = TetrisSmooth
switchTetrisMove TetrisSmooth = TetrisStepped