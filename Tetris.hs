module Main where

    import Data.Array ((!))
    import Graphics.UI.Gtk hiding (eventModifier, eventKeyName, eventSent)
    import Graphics.Rendering.Cairo (Render(..))
    import Graphics.UI.Gtk.Gdk.Events
    import Control.Concurrent.MVar
    import Control.Monad (liftM,mapM)
    import System.Random
    import TetrisPieces
    import TetrisDrawing
    
    -- *** FUNÇÕES DO MANIPULADOR ***
    -- estas funções são os manipuladores do GTK e seus vários ajudantes
    handleButtonPress :: Window -> [MVar TetrisGameState] -> [DrawingArea] ->
                         [IO Bool] -> MVar Bool -> Event -> IO Bool
    handleButtonPress win (mS1:mS2:[]) (dA1:pA1:dAs) (tH1:tHs) doShad ev = do
          g1State <- readMVar $ mS1
          g2State <- readMVar $ mS2
          let is2p = g2State /= NaS
          let tH2 = head tHs
          let dA2 = head dAs
          let pA2 = head $ tail dAs
          let mqd1 = modifyThenQueueDraw mS1 dA1
          let mqp1 = modifyThenQueueDraw mS1 pA1
          let mqd2 = modifyThenQueueDraw mS2 dA2
          let mqp2 = modifyThenQueueDraw mS2 pA2
          let hID1 = hID g1State
          let hID2 = if is2p then hID g2State else 0
          hBx <- ioify head =<< containerGetChildren win
          case (eventKeyChar ev,eventModifier ev,eventKeyName ev,hID1==0,hID2==0) of
               -- CONTROLE DO JOGO
               (Just 'p',[Control],_,_,_) -> togglePauseAllGames [(mS1,tH1),(mS2,tH2)] >> return True
               (Just '1',[Control],_,_,_) -> widgetDestroy hBx >>
                                             setupPlayers win hID1 hID2 False >> return True
               (Just '2',[Control],_,_,_) -> widgetDestroy hBx >>
                                             setupPlayers win hID1 hID2 True >> return True
               (Just 'q',[Control],_,_,_) -> widgetDestroy win >> return True
               (Just 's',[Control],_,_,_) -> modifyMVar_ doShad (ioify not) >> 
                                             widgetQueueDraw win >> return True
               
                -- CONTROLE PLAYER 1
               (Just 'e',[],_,False,_) -> mqd1 gStateRotateCW
               (Just 'w',[],_,False,_) -> mqd1 gStateRotateCCW
               (Just 'a',[],_,False,_) -> mqd1 gStateMoveLeft
               (Just 'd',[],_,False,_) -> mqd1 gStateMoveRight
               (Just 's',[],_,False,_) -> tH1 >> return True
               (Just 'q',[],_,False,_) -> mqd1 gStateMoveBottom >> tH1 >> return True
               (Just 'f',[],_,False,False) -> genRandomPiece >>= \npiece ->
                                              mqp2 $ gStateReplaceNxt npiece
               -- CONTROLE PLAYER 2
               (Just '=', [],_,_,False)  -> mqd2 gStateRotateCW
               (Just '0', [],_,_,False)  -> mqd2 gStateRotateCCW
               (Just 'p', [],_,_,False)  -> mqd2 gStateMoveLeft
               (Just ']', [],_,_,False)  -> mqd2 gStateMoveRight
               (Just '[', [],_,_,False)  -> tH2 >> return True
               (Just '-', [],_,_,False)  -> mqd2 gStateMoveBottom >> tH2 >> return True
               (Just 'o', [],_,False,False) -> genRandomPiece >>= \npiece ->
                                               mqp1 $ gStateReplaceNxt npiece
               -- CASO CONTRARIO, NAO DA PARA JOGAR
               (_,_,_,_,_) -> return False
    
    -- PAUSA UM OU 2 JOGOS
    togglePauseAllGames :: [(MVar TetrisGameState,IO Bool)] -> IO [Bool]
    togglePauseAllGames = mapM toggleIfNotNaS
      where toggleIfNotNaS (mS,tH) = readMVar mS >>= \x -> 
                                     if x /= NaS
                                      then togglePauseGame mS tH
                                      else return False
    
    -- PAUSA O JOGO
    togglePauseGame :: MVar TetrisGameState -> IO Bool -> IO Bool
    togglePauseGame tgS tHnd = do
               gst <- readMVar tgS
               if hID gst == 0
                then do nHID <- timeoutAdd tHnd $ gtime gst
                        modifyMVar_ tgS (ioify $ \_ -> gst { hID = nHID })
                        return True
                else do safeTimeoutRemove $ hID gst
                        modifyMVar_ tgS (ioify $ \_ -> gst { hID = 0 })
                        return True
    
    -- COMECA NOVO JOGO
    startNewGame :: MVar TetrisGameState -> Label -> Label -> Window -> IO Bool -> IO Bool
    startNewGame tgS lLn lSn w tHnd = do
               gst <- readMVar tgS
               safeTimeoutRemove $ hID gst           -- Remove o tempo limite antigo
               nGS <- genNewState gst                -- NOVO JOGO
               nHID <- timeoutAdd tHnd 1000          -- adiciona novo tempo limite
               modifyMVar_ tgS (ioify $ \_ -> nGS { hID = nHID }) -- salva estado
               labelSetText lLn "0"                  -- reset contagem de linhas
               labelSetText lSn "0"                  -- reset pontugacao
               widgetQueueDraw w                     -- tela de redesenho
               return True
    
    -- Aqui ta muito feio e imperativo
    -- essa parte é muito suja
    timerHandler :: MVar TetrisGameState -> MVar TetrisGameState -> IO Bool
    timerHandler mtgS mtgS2 = do
               tgs <- readMVar mtgS                     -- le o estado do jogo
               tgs2 <- readMVar mtgS2
               let daMain = can tgs
               let daPrev = pcan tgs
               let lLn = lLNum tgs
               let lSc = lScore tgs
               let (collided,tgs') = stateDropPiece tgs -- dropa peca, detecta colisoes
               if collided  -- se tiver uma colisao, entao tem alguns junçoes para fazer
                then do 
                 newpiece <- genRandomPiece                            -- primeira peca, entao vai pra proxima
                 let (nlines,tbd) = removeCompleteLines $ bdstate tgs' -- alguma linha completa
                 (tgs'',tgs2',retval) <-                               
                  if stateAboveBounds $ blstate tgs -- se a peca que colidiu esta fora do topo
                   then do                  -- game over!
                     safeTimeoutRemove $ hID tgs'           -- cancela interrupcao
                     return $ ( tgs' { bdstate = tbd        -- atualiza estado do jogo
                                     , gtime = 1000
                                     , pnext = newpiece
                                     , hID = 0 -- pausa
                                     , blstate = (TBState 4 (-1) 0 pSquare) }
                              , tgs2                        -- nenhuma atualizacao para o estado 2p
                              , False )                     -- reinicia o tempo antigo
                   else
                     if (nlines > 0)  -- desta vez, fazemos novas linhas
                      then do oldNLines <- liftM read $ labelGetText lLn  -- oculta algum estado
                              oldScore  <- liftM read $ labelGetText lSc  -- nos rotulos
                              let newNLines = oldNLines + nlines          
                              let newScore = oldScore + ((2 * nlines) ^ 3)
                              labelSetText lLn $ show newNLines
                              labelSetText lSc $ show newScore
                              -- aumenta a velocidade a cada 10 linhas
                              -- se for 2 jogadores, suas linhas aumentam a velocidade do oponente
                              case ((newNLines `div` 10) /= (oldNLines `div` 10),tgs2) of
                                (True,NaS) -> do    -- jogo do 1 player 
                                       let newTime = (gtime tgs') `div` 6 * 5 -- acelera
                                       nHID <- timeoutAdd                 -- adiciona novo timeout para outro jogador
                                               (timerHandler mtgS mtgS2)
                                               newTime
                                       safeTimeoutRemove $ hID tgs'       -- remove o tempo limite antigo
                                       return $ ( tgs' { bdstate = tbd    -- atualiza estado
                                                       , gtime = newTime
                                                       , pnext = newpiece
                                                       , hID = nHID }
                                                , tgs2                    -- nenhuma atualizacao para o estado 2p
                                                , False ) -- nao reinicia o tempo limite antigo
                                (True, _ ) -> do    -- jogo de 2 players 
                                       let newTime = (gtime tgs2) `div` 6 * 5 -- acelera outro jogador
                                       nHID <- timeoutAdd                     -- adiciona novo tempo limite para outro jogador
                                               (timerHandler mtgS2 mtgS)
                                               newTime
                                       safeTimeoutRemove $ hID tgs2           -- remove tempo limite antigo
                                       return $ ( tgs' { bdstate = tbd
                                                       , pnext = newpiece }
                                                , tgs2 { gtime = newTime      -- atualiza o estado do tempo de outro jogador
                                                       , hID = nHID }
                                                , True )                      -- reinicia o temporizador
                                (False, _) -> return $ ( tgs' { pnext = newpiece  -- senao apenas atualiza 
                                                              , bdstate = tbd }   -- o estado
                                                       , tgs2                     -- nenhuma atualizacao para o estado 2p
                                                       , True )                   -- e reinicia o tempo limite
                      else return ( tgs' { pnext = newpiece } , tgs2, True ) -- sem novas linhas
                 modifyMVar_ mtgS (ioify $ (\_ -> tgs''))        -- troca no novo estado do jogo
                 modifyMVar_ mtgS2 (ioify $ (\_ -> tgs2'))
                 widgetQueueDraw daMain                          -- redesenha a janela principal
                 widgetQueueDraw daPrev                          -- redesenha a janela principal
                 return retval                                   -- feito
                else do 
                 modifyMVar_ mtgS (ioify $ (\_ -> tgs')) -- apenas atualiza o estado do jogo
                 widgetQueueDraw daMain                  -- redesenha a janela principal
                 return True                             -- reagenda o manipulador
    
    genRandomPiece :: IO TetrisPiece
    genRandomPiece = (\x -> return $ tPieces !! x) =<< randomRIO (0,6)
    
    genNewState :: TetrisGameState -> IO TetrisGameState
    genNewState tgs = do
               tp1 <- genRandomPiece
               tp2 <- genRandomPiece
               return $ tgs { blstate = (newState tp1)
                            , pnext = tp2 }
    
    canvasHandler :: DrawingArea -> (Bool -> (Int,Int) -> TetrisGameState -> Render ())
                     -> MVar TetrisGameState -> MVar Bool -> Event -> IO Bool
    canvasHandler cname fn st shSt ev = do
               dwin <- widgetGetDrawWindow cname
               csize <- widgetGetSize cname
               cblock <- readMVar st
               doShad <- readMVar shSt
               renderWithDrawable dwin (fn doShad csize cblock)
               return $ eventSent ev
    
    safeTimeoutRemove :: HandlerId -> IO ()
    safeTimeoutRemove hndId = if hndId /= 0 then timeoutRemove hndId else return ()
    
    -- *** FUNCOES DE UTILIDADE COM MVAR ***
    
    ioify :: (a -> b) -> (a -> IO b)
    ioify y = \x -> return $ y x
    
    modifyThenQueueDraw :: MVar TetrisGameState -> DrawingArea ->
                           (TetrisGameState -> TetrisGameState) -> IO Bool
    modifyThenQueueDraw b d fn = modifyMVar_ b (ioify fn) >> widgetQueueDraw d >> return True
    
    -- *** FUNCOES DE CONFIGURACAO DA JANELA ***
    
    -- esta é a configuracao da janela do jogador 1 
    setupPlayers :: Window -> HandlerId -> HandlerId -> Bool -> IO ()
    setupPlayers window t1 t2 is2p = do
    -- um monte de coisas do GTK
               safeTimeoutRemove t1
               safeTimeoutRemove t2
    
               button <- buttonNewWithMnemonic "  _Quit  "
               hbx    <- hBoxNew False 0
               vbx    <- vBoxNew False 0
               aframe <- aspectFrameNew 0.5 0.5 (Just 0.5)
               afram2 <- aspectFrameNew 0.5 0.5 (Just 0.75)
               canvas <- drawingAreaNew
               preCan <- drawingAreaNew
               lScore <- labelNew $ Just "p1 Score:"
               lLines <- labelNew $ Just "p1 Lines:"
               lLnNum <- labelNew $ Just "0"
               lScNum <- labelNew $ Just "0"
    
    -- faça o 2 round, se necessario
               aframe2 <- if is2p then aspectFrameNew 0.5 0.5 (Just 0.5) else return aframe
               afram22 <- if is2p then aspectFrameNew 0.5 0.5 (Just 0.75) else return afram2
               canvas2 <- if is2p then drawingAreaNew else return canvas
               preCan2 <- if is2p then drawingAreaNew else return preCan
               lScore2 <- if is2p then labelNew $ Just "p2 Score:" else return lScore
               lLines2 <- if is2p then labelNew $ Just "p2 Lines:" else return lLines
               lLnNum2 <- if is2p then labelNew $ Just "0" else return lLnNum
               lScNum2 <- if is2p then labelNew $ Just "0" else return lScNum
    
    -- configura  com seu conteudo
               set lLnNum [widgetCanFocus := True]
               set window [containerChild := hbx, containerBorderWidth := 10, windowTitle := "Tetris",
                           windowDefaultHeight := 600, windowDefaultWidth := 480]
               set aframe [containerChild := canvas]
               set afram2 [containerChild := preCan]
               let aspectRatio = if is2p then 1.4 else 0.75
               windowSetGeometryHints window (Just aframe) (Just (300,300)) (Just (1000,1000))
                                             Nothing Nothing (Just (aspectRatio,aspectRatio))
               if is2p then do set aframe2 [containerChild := canvas2]
                               set afram22 [containerChild := preCan2]
                       else return ()
    
    -- hbox principal
               boxPackStart hbx aframe PackGrow 0
               boxPackStart hbx vbx PackNatural 0
               if is2p then boxPackStart hbx aframe2 PackGrow 0 else return ()
    
    -- vbox a direita (ou no meio se for jogo de 2 jogadores)
               boxPackStart vbx afram2 PackGrow 0
               boxPackStart vbx lLines PackNatural 0
               boxPackStart vbx lLnNum PackNatural 0
               boxPackStart vbx lScore PackNatural 0
               boxPackStart vbx lScNum PackNatural 0
               if is2p
                then do boxPackStart vbx afram22 PackGrow 0
                        boxPackStart vbx lLines2 PackNatural 0
                        boxPackStart vbx lLnNum2 PackNatural 0
                        boxPackStart vbx lScore2 PackNatural 0
                        boxPackStart vbx lScNum2 PackNatural 0
                else return ()
               boxPackStart vbx button PackNatural 0
    
    -- define cor de fundo
               let canvases = if is2p then [canvas,preCan,canvas2,preCan2] else [canvas,preCan]
               mapM (\x -> widgetModifyBg x StateNormal (Color 0 0 0)) canvases
    
    -- certifica-se de que a barra de espaco nao faça o jogo parar
               widgetGrabFocus lLnNum  
               widgetShowAll window
    
    -- coisas do estado do jogo
               mg1State <- newMVar $ newGameState pSquare pSquare canvas preCan lLnNum lScNum
               mg2State <- if is2p 
                            then newMVar $ newGameState pSquare pSquare canvas2 preCan2 lLnNum2 lScNum2
                            else newMVar NaS
               shadState <- newMVar False
    
    
    -- manipuladores para as janelas principal e de pre-visualizacao
               let exposeStuff = zip3 canvases 
                                      [mg1State,mg1State,mg2State,mg2State] $
                                      cycle [reDraw,preDraw]
               mapM (\(c,s,d) -> onExpose c $ canvasHandler c d s shadState) exposeStuff
    
    -- botao sair
               onClicked button (widgetDestroy window)
               onDestroy window mainQuit
    
    -- manipulador do temporizador
               let tH1 = timerHandler mg1State mg2State
               let tH2 = if is2p then timerHandler mg2State mg1State
                                 else return False
               startNewGame mg1State lLnNum lScNum window tH1
               if is2p then startNewGame mg2State lLnNum2 lScNum2 window tH2 else return False
    
    -- manipulador do teclado
               let handlers = if is2p then [tH1,tH2] else [tH1]
               kHandler <- onKeyPress window $
                           handleButtonPress window [mg1State,mg2State] canvases handlers shadState
               onDestroy hbx $ signalDisconnect kHandler
               return ()
    -- *****************
    -- ***** MAIN ******
    -- *****************
    -- inicial o jogo para um jogador (1) 
    main :: IO ()
    main = do
               initGUI
               window <- windowNew
               setupPlayers window 0 0 False
               mainGUI
    