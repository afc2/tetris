module TetrisPieces where

    import Data.Array as DA
    import Data.List as DL
    import System.Glib.MainLoop ( HandlerId(..))
    import Graphics.UI.Gtk (DrawingArea(..), Label(..))
    
    -- *** Tipos dos dados ***
    
    type Coord = (Int,Int)
    type TransCoord = (Int,Int)
    
    type TetrisLine = DA.Array Int TetrisBlock
    type TetrisBoard = DA.Array Int TetrisLine
    
    data TetrisPiece = TPiece
          { segCoords :: [Coord]      -- coordenadas das caixas
          , rotTrans  :: [TransCoord] -- translacao para aplicar antes de girar
          , block     :: TetrisBlock  -- cor dos blocos
          } deriving (Eq, Show)
    
    data TetrisBlock = Nil | Red | Green | Blue | Yellow | Cyan | Purple | Orange | Gray
      deriving (Eq, Show)
    
    data TetrisRotation = TRot
          { kx :: Int                 -- fator X quando rotaciona 
          , ky :: Int                 -- fator Y quando rotaciona 
          , sw :: Bool                -- Trocando X e Y
          } deriving (Eq, Show)
    
    data TetrisBlockState = TBState
          { cx :: Int                 -- posicao x
          , cy :: Int                 -- posicao y 
          , rot :: Int                -- rotacao
          , tp :: TetrisPiece         -- q peca
          } deriving (Eq, Show)
    
    instance Show DrawingArea where
        show _ = "<Gtk drawing area>"
    
    instance Show Label where
        show _ = "<Gtk label>"
    
    data TetrisGameState = NaS | TGState
          { blstate :: TetrisBlockState
          , bdstate :: TetrisBoard
          , gtime :: Int
          , pnext :: TetrisPiece
          , hID :: HandlerId
          , can :: DrawingArea
          , pcan :: DrawingArea
          , lLNum :: Label
          , lScore :: Label
          } deriving (Eq, Show)
    
    -- *** Pecas do Tetris ***
    
    pLine   = TPiece [(0,0),(0,-1),(0,-2),(0,-3)]   [(0,0),(-2,0),(0,-3),(1,0)]   Cyan    -- I
    pLRight = TPiece [(0,0),(-1,0),(0,-1),(0,-2)]   [(0,0),(-1,0),(-1,-2),(1,-1)] Blue    -- J
    pLLeft  = TPiece [(0,0),(1,0),(0,-1),(0,-2)]    [(0,0),(-1,-1),(0,-2),(1,0)]  Orange  -- L
    pSquare = TPiece [(0,0),(1,0),(1,-1),(0,-1)]    [(0,0),(0,-1),(1,-1),(1,0)]   Yellow  -- O
    pZLeft  = TPiece [(0,0),(0,-1),(-1,-1),(-1,-2)] [(0,0),(-1,0),(-1,-2),(1,-1)] Green   -- S
    pTee    = TPiece [(0,0),(0,-1),(0,-2),(1,-1)]   [(0,0),(-1,-1),(0,-2),(1,0)]  Purple  -- T
    pZRight = TPiece [(0,0),(0,-1),(1,-1),(1,-2)]   [(0,0),(-1,-1),(1,-2),(1,0)]  Red     -- Z
    
    tPieces = [pSquare,pLLeft,pLRight,pZLeft,pZRight,pLine,pTee]
    
    -- *** Definindo Cores *** 
    
    tBlockToRGBd :: TetrisBlock -> (Double,Double,Double)
    tBlockToRGBd t = case t of
                         Nil -> (0,0,0)
                         Red -> (1,0,0)
                         Green -> (0,1,0)
                         Blue -> (0,0,1)
                         Yellow -> (1,1,0)
                         Cyan -> (0,1,1)
                         Purple -> (0.75,0,0.75)
                         Orange -> (1,0.5,0)
                         Gray -> (0.3,0.3,0.3)
    
    -- *** Rotacao ***
    
    tRotations :: [TetrisRotation]
    tRotations = [ TRot ( 1) ( 1) False
                 , TRot ( 1) (-1) True
                 , TRot (-1) (-1) False
                 , TRot (-1) ( 1) True
                 ]
    
    -- *** ESTADO STUFF ***
    -- Quebrei a abstracao em alguns lugares do  Main
    -- porque substituir o material um por um ficaria muito pesado

    newState :: TetrisPiece -> TetrisBlockState
    newState tp = TBState 4 0 0 tp
    
    stateReplaceTP :: TetrisPiece -> TetrisBlockState -> TetrisBlockState
    stateReplaceTP tp (TBState cx cy r _) = TBState cx cy r tp
    
    gStateReplaceTP :: TetrisPiece -> TetrisGameState -> TetrisGameState
    gStateReplaceTP = gameStateLift.stateReplaceTP
    
    gStateReplaceGtm :: Int -> TetrisGameState -> TetrisGameState
    gStateReplaceGtm gtm tgs = tgs { gtime = gtm }
    
    gStateReplaceNxt :: TetrisPiece -> TetrisGameState -> TetrisGameState
    gStateReplaceNxt tpc tgs = tgs { pnext = tpc }
    
    gStateReplaceHID :: HandlerId -> TetrisGameState -> TetrisGameState
    gStateReplaceHID hid tgs = tgs { hID = hid }
    
    newGameState :: TetrisPiece -> TetrisPiece -> DrawingArea -> DrawingArea -> Label -> Label -> TetrisGameState
    newGameState tp1 tp2 can pcan lLn lSc = TGState (newState tp1) emptyBoard 1000 tp2 0 can pcan lLn lSc
    
    -- levantando uma operacao BlockState  em um GameState
    gameStateLift :: (TetrisBlockState -> TetrisBlockState) -> TetrisGameState -> TetrisGameState
    gameStateLift fn tgst = tgst { blstate = fn $ blstate tgst }
    
    stateModify :: Int -> Int -> Int -> TetrisBlockState -> TetrisBlockState
    stateModify dx dy dr (TBState cx cy rot tp) = TBState (cx+dx) (cy+dy) (mod (rot+dr) 4) tp
    
    -- movimentos nao verificados
    stateMoveRight_ = stateModify ( 1) ( 0) ( 0)
    stateMoveLeft_  = stateModify (-1) ( 0) ( 0)
    stateMoveDown_  = stateModify ( 0) ( 1) ( 0)
    stateMoveUp_    = stateModify ( 0) (-1) ( 0)
    stateRotateCW_  = stateModify ( 0) ( 0) ( 1)
    stateRotateCCW_ = stateModify ( 0) ( 0) (-1)
    
    -- movimento generalizado mas tem que verificar se ha limites
    bCheckedMove :: (TetrisBlockState -> TetrisBlockState) -> TetrisBlockState -> TetrisBlockState
    bCheckedMove fn oldB = if stateOffGrid newB then oldB else newB
      where newB = fn oldB
    
    -- movimentos verificados (SOMENTE bordas MAIS colisões)
    stateMoveRight  = bCheckedMove stateMoveRight_
    stateMoveLeft   = bCheckedMove stateMoveLeft_
    stateMoveDown   = bCheckedMove stateMoveDown_
    stateMoveUp     = bCheckedMove stateMoveUp_
    stateRotateCW   = bCheckedMove stateRotateCW_
    stateRotateCCW  = bCheckedMove stateRotateCCW_
    
    --- solta o bloco o máximo possível, derrubando-o repetidamente até colidir
    checkedMove :: (TetrisBlockState -> TetrisBlockState) -> TetrisGameState -> TetrisGameState
    checkedMove fn oldS = if collided then oldS else newS
      where newB = fn $ blstate oldS
            newS = oldS { blstate = newB }
            collided = stateIsCollisionOrOffGrid (bdstate oldS) newB
    
    -- movimentos verificados  - apenas bordas laterais e inferiores
    gStateMoveRight  = checkedMove stateMoveRight_
    gStateMoveLeft   = checkedMove stateMoveLeft_
    gStateMoveDown   = checkedMove stateMoveDown_
    gStateMoveUp     = checkedMove stateMoveUp_
    gStateRotateCW   = checkedMove stateRotateCW_
    gStateRotateCCW  = checkedMove stateRotateCCW_
    
    -- verifica se ha limites e colisao
    -- isso requer o estado do jogo inteiro
    gStateMoveBottom :: TetrisGameState -> TetrisGameState
    gStateMoveBottom tgS = snd.head $ dropWhile fst $ 
                           DL.iterate (gApplyCompare gStateMoveDown) (True,tgS)
      where gApplyCompare fn (_, tgS) = (tgS /= tgS', tgS')
              where tgS' = fn tgS
    
    -- *** FUNCOES PARA MANIPULAR BLOCOS e ESTADOS ***
    
    applyRotation :: TetrisRotation -> TransCoord -> Coord -> Coord
    applyRotation (TRot kx ky sw) (dx,dy) (x,y) = if sw then (y'+dx,x'+dy) else (x'+dx,y'+dy)
      where x' = kx*x
            y' = ky*y
    
    shiftRotateTetrisPiece :: Coord -> Int -> TetrisPiece -> TetrisPiece
    shiftRotateTetrisPiece (dx,dy) rot (TPiece sC rT b) = TPiece sC' [] b
      where (dtx,dty) = rT !! rot
            rotTetr = tRotations !! rot
            sC' = map (applyRotation rotTetr (dx+dtx,dy+dty)) sC
    
    -- levar um estado e aplique o deslocamento e rotacao da peca
    tetrisBlockStateToPiece :: TetrisBlockState -> TetrisPiece
    tetrisBlockStateToPiece (TBState cx cy rot tp) =
            shiftRotateTetrisPiece (cx,cy) rot tp
    
    -- ************************************
    -- *** FUNÇÕES PARA MODIFICAR BOARD ***
    -- ************************************
    -- estas funções modificam o TetrisBoard,
    -- removendo linhas ou adicionando
    -- uma peça ou um estado para uma placa existente     
    -- peças colocadas são representadas por uma matriz bidimensional de blocos

    
    emptyLine :: TetrisLine
    emptyLine = DA.listArray (0,9) $ take 10 (repeat Nil)
    
    emptyBoard :: TetrisBoard
    emptyBoard = DA.listArray (0,19) $ take 20 (repeat emptyLine)
    
    lineIsComplete :: TetrisLine -> Bool
    lineIsComplete ln = all (/= Nil) $ DA.elems ln
    
    completeLines :: TetrisBoard -> [Int]
    completeLines tboard = map fst $ filter (lineIsComplete.snd) $ DA.assocs tboard
    
    removeLine :: Int -> TetrisBoard -> TetrisBoard
    removeLine lnum = (// [(0,emptyLine)]).(ixmap (0,19) nline)
      where nline i = case (i<=lnum,i) of
                       (_,   0) -> 0
                       (True,_) -> i-1
                       (_   ,_) -> i
    
    -- removeLinesSorted requer uma lista de linhas na ordem ordenada reversa!
    removeLinesSorted :: [Int] -> TetrisBoard -> TetrisBoard
    removeLinesSorted lnums tboard = foldr removeLine tboard lnums
    
    removeLines :: [Int] -> TetrisBoard -> TetrisBoard
    removeLines lnums tboard = removeLinesSorted (reverse $ sort lnums) tboard
    
    removeCompleteLines :: TetrisBoard -> (Int,TetrisBoard)
    removeCompleteLines tboard = (nlines,tboard')
      where rlines = reverse $ completeLines tboard
            nlines = length rlines
            tboard' = removeLinesSorted rlines tboard
    
    insertBlock :: TetrisBlock -> Coord -> TetrisBoard -> TetrisBoard
    insertBlock tb (x,y) tboard = tboard // [(y,newX)]
      where oldX = tboard ! y
            newX = oldX // [(x,tb)]
    
    addPieceToBoard :: TetrisBoard -> TetrisPiece -> TetrisBoard
    addPieceToBoard tboard (TPiece sC _ b) = foldr (insertBlock b) tboard $ 
                                                   filter (not.outOfBounds_) sC
    
    addStateToBoard :: TetrisBoard -> TetrisBlockState -> TetrisBoard
    addStateToBoard tbd tbs = addPieceToBoard tbd $ tetrisBlockStateToPiece tbs
    
    -- *** DETECCANDO COLISÃO ***
    -- esses predicados são usados ​​para detectar
    -- colisões com alguma combinação de fundo,
    -- tábua e lados

    pieceBoundCheck :: (Coord -> Bool) -> TetrisPiece -> Bool
    pieceBoundCheck fn (TPiece sC _ _) = any fn sC
    
    pieceOffBottom = pieceBoundCheck outOfBottom
    pieceOffGrid = pieceBoundCheck outOfBounds
    pieceAboveBounds = pieceBoundCheck aboveBounds
    
    stateBoundCheck :: (TetrisPiece -> Bool) -> TetrisBlockState -> Bool
    stateBoundCheck fn = fn.tetrisBlockStateToPiece
    
    stateOffBottom = stateBoundCheck pieceOffBottom
    stateOffGrid = stateBoundCheck pieceOffGrid
    stateAboveBounds = stateBoundCheck pieceAboveBounds
    
    -- é a peça abaixo do fundo ou fora de qualquer lado?
    outOfBounds :: Coord -> Bool
    outOfBounds (x,y) = x < 0 || x > 9 || y > 19
    
    -- outOfBounds, incluindo fora do topo do tabuleiro
    outOfBounds_ :: Coord -> Bool
    outOfBounds_ (x,y) = x < 0 || x > 9 || y < 0 || y > 19
    
    -- acima dos limites - fora do topo do tabuleiro
    aboveBounds :: Coord -> Bool
    aboveBounds (_,y) = y < 0
    
    -- fora do fundo do tabuleiro
    outOfBottom :: Coord -> Bool
    outOfBottom (x,y) = y > 19
    
    lookupCoord :: TetrisBoard -> Coord -> TetrisBlock
    lookupCoord tboard (x,y) = (tboard ! y) ! x
    
    isCollision :: TetrisBoard -> TetrisPiece -> Bool
    isCollision tboard (TPiece sC _ _) = 
                any ((/=Nil).(lookupCoord tboard)) $ filter (not.outOfBounds_) sC
    
    isCollisionOrBottom :: TetrisBoard -> TetrisPiece -> Bool
    isCollisionOrBottom tboard tpiece = isCollision tboard tpiece || pieceOffBottom tpiece
    
    isCollisionOrOffGrid :: TetrisBoard -> TetrisPiece -> Bool
    isCollisionOrOffGrid tboard tpiece = isCollision tboard tpiece || pieceOffGrid tpiece
    
    liftStatePred :: (TetrisBoard -> TetrisPiece -> Bool) -> TetrisBoard -> TetrisBlockState -> Bool
    liftStatePred pred tbd tbs = pred tbd $ tetrisBlockStateToPiece tbs
    
    stateIsCollision = liftStatePred isCollision
    stateIsCollisionOrBottom = liftStatePred isCollisionOrBottom
    stateIsCollisionOrOffGrid = liftStatePred isCollisionOrOffGrid
    
    -- esta é a função que usamos quando o temporizador avança um clique
    stateDropPiece :: TetrisGameState -> (Bool,TetrisGameState)
    stateDropPiece tgs = (collided,tgs')
      where tbs = blstate tgs
            tbd = bdstate tgs
            tbs' = stateMoveDown_ tbs
            collided = stateIsCollisionOrBottom tbd tbs'
            tbd' = if collided then addStateToBoard tbd tbs else tbd
            tbs'' = if collided then newState $ pnext tgs else tbs'
            tgs' = tgs { blstate = tbs'' , bdstate = tbd' }
    
    -- *** TEST ***
    -- coisas para testar o manuseio dos tabuleiros
    
    redLine :: TetrisLine
    redLine = DA.listArray (0,9) $ take 10 (repeat Red)
    
    blueLine :: TetrisLine
    blueLine = DA.listArray (0,9) $ take 10 (repeat Blue)
    
    testBoard :: TetrisBoard
    testBoard = DA.listArray (0,19) $ take 18 (repeat emptyLine) ++ [redLine,blueLine]