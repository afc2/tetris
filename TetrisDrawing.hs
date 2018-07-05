module TetrisDrawing where

    import Graphics.Rendering.Cairo
    import TetrisPieces
    import Data.Array as DA
    
    -- *** FUNÇÕES GERAIS DE DESENHO ***
    -- estas funções são para desenhar as várias peças de tetris no Cairo

    drawTetrisBoard :: Double -> Double -> TetrisBoard -> Render ()
    drawTetrisBoard dunitx dunity tb = 
                mapM (drawTetrisLine dunitx dunity) (DA.assocs tb) >> return ()
    
    drawTetrisLine :: Double -> Double -> (Int,TetrisLine) -> Render ()
    drawTetrisLine dunitx dunity (y,lx) = mapM drawFromLine (DA.assocs lx) >> return ()
      where drawFromLine (x,tbl) = drawTetrisBlock tbl dunitx dunity x y
    
    drawTetrisBlock :: TetrisBlock -> Double -> Double -> Int -> Int -> Render ()
    drawTetrisBlock tb dunitx dunity cx cy =
      if tb == Nil then return () else do
      setSourceTBlock tb
      rectangle (dunitx / 20 + dunitx * fromIntegral cx) 
                (dunity / 20 + dunity * fromIntegral cy)
                (dunitx - dunitx / 10)
                (dunity - dunity / 10)
      fill
    
    drawTetrisPiece :: TetrisPiece -> Double -> Double -> Int -> Int -> Int -> Render ()
    drawTetrisPiece tp dunitx dunity cx cy rot = 
      mapM (\(x,y) -> drawTetrisBlock tb dunitx dunity x y) sC >> return ()
        where (TPiece sC _ tb) = shiftRotateTetrisPiece (cx,cy) rot tp
    
    -- define cor com base no tipo TetrisBlock
    setSourceTBlock :: TetrisBlock -> Render ()
    setSourceTBlock t = setSourceRGB tr tg tb
      where (tr,tg,tb) = tBlockToRGBd t
    
    -- redesenha a janela principal
    reDraw :: Bool -> (Int,Int) -> TetrisGameState -> Render ()
    reDraw doShad (x,y) tgS = do
      let (TBState cx cy rot tp) = blstate tgS
      let bstate = bdstate tgS
      let ddx = fromIntegral x / 10
      let ddy = fromIntegral y / 20
      let (TBState cx' cy' rot' tp') = blstate (gStateMoveBottom tgS)
      drawTetrisBoard ddx ddy bstate
      if doShad then drawTetrisPiece (tp' { block = Gray }) ddx ddy cx' cy' rot' else return ()
      drawTetrisPiece tp ddx ddy cx cy rot
    
    --- redesenha a janela de pré-visualização
    --- isso não usa o Bool, basta colocar lá para
    --- manter a assinatura do tipo igual à acima
    preDraw :: Bool -> (Int,Int) -> TetrisGameState -> Render ()
    preDraw _ (x,y) tgS = do
      let np = pnext tgS
      let ddx = fromIntegral x / 3
      let ddy = fromIntegral y / 4
      drawTetrisPiece np ddx ddy 1 3 0
    
    
    