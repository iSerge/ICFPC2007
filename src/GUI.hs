{-# LANGUAGE BangPatterns #-}

module GUI (setupGUI, Ui (..)) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State.Strict as St
import DNAProcessor
import DNASeq
import DNAops
import Data.Sequence as S
import Graphics.UI.Gtk
import Paths_DNAExplorer (getDataFileName)
import RNA
import RNAprocessor

data Ui = Ui
  { mainWindow :: Window,
    rnaView :: Image,
    processDnaBtn :: Button,
    processRnaBtn :: Button,
    saveImageBtn :: Button,
    saveRnaBtn :: Button,
    statusBar :: Statusbar,
    interactiveRenderCB :: CheckButton,
    prefixField :: Entry,
    prefixProgram :: TextView
  }

stateTLens :: (Monad m) => (a -> b) -> (b -> a -> a) -> StateT b m d -> StateT a m d
stateTLens accessor modifier action = do
  st <- St.get
  (!result, !newSt) <- lift $ runStateT action (accessor st)
  modify $ modifier newSt
  return result

overDna :: (Monad m) => StateT DnaProcSt m d -> StateT (DnaProcSt, RnaProcSt) m d
overDna = stateTLens fst (\b (_, a) -> (b, a))

overRna :: (Monad m) => StateT RnaProcSt m d -> StateT (DnaProcSt, RnaProcSt) m d
overRna = stateTLens snd (\b (a, _) -> (a, b))

chooseFile :: Window -> IO String
chooseFile parentWindow = do
  dialog <-
    fileChooserDialogNew
      ( Just $
          "Demo of the standard dialog to select "
            ++ "an existing file" -- dialog title
      )
      (Just parentWindow) -- the parent window
      FileChooserActionOpen -- the kind of dialog we want
      [ ( "gtk-cancel", -- The buttons to display
          ResponseCancel
        ),
        ( "gtk-open",
          ResponseAccept
        )
      ]

  widgetShow dialog
  dlgResponse <- dialogRun dialog
  widgetHide dialog
  case dlgResponse of
    ResponseAccept -> do
      Just fileName <- fileChooserGetFilename dialog
      return fileName
    _ -> return ""

createControls :: IO Ui
createControls = do
  builderMainWinFile <- getDataFileName "mainWindow.ui"
  builder <- builderNew
  builderAddFromFile builder builderMainWinFile
  mainWnd <- builderGetObject builder castToWindow "mainWindow"
  rnaV <- builderGetObject builder castToImage "rnaView"
  statusbar <- builderGetObject builder castToStatusbar "statusBar"
  interactiveCB <- builderGetObject builder castToCheckButton "interactiveCBtn"
  [pRna, pDna, sImg, sRna] <-
    mapM
      (builderGetObject builder castToButton)
      ["processRNABtn", "processDNABtn", "saveImgBtn", "saveRNABtn"]
  prefixV <- builderGetObject builder castToEntry "prefixView"
  prog <- builderGetObject builder castToTextView "controlTerm"
  return
    Ui
      { mainWindow = mainWnd,
        rnaView = rnaV,
        processDnaBtn = pDna,
        processRnaBtn = pRna,
        saveImageBtn = sImg,
        saveRnaBtn = sRna,
        statusBar = statusbar,
        interactiveRenderCB = interactiveCB,
        prefixField = prefixV,
        prefixProgram = prog
      }

enableButtons :: Ui -> IO ()
enableButtons u = do
  widgetSetSensitive (processRnaBtn u) True
  widgetSetSensitive (processDnaBtn u) True
  widgetSetSensitive (saveImageBtn u) True
  widgetSetSensitive (saveRnaBtn u) True

disableButtons :: Ui -> IO ()
disableButtons u = do
  widgetSetSensitive (processRnaBtn u) False
  widgetSetSensitive (processDnaBtn u) False
  widgetSetSensitive (saveImageBtn u) False
  widgetSetSensitive (saveRnaBtn u) False

clearImage :: Image -> IO ()
clearImage img = do
  pbuf <- pixbufNew ColorspaceRgb False 8 600 600
  pixbufFill pbuf 0 0 0 255
  imageSetFromPixbuf img pbuf

setupGUI :: IO Ui
setupGUI = do
  u <- createControls
  clearImage (rnaView u)
  statusCtxId <- statusbarGetContextId (statusBar u) "Main status"
  _ <- statusbarPush (statusBar u) statusCtxId "Ready to work!"
  _ <- mainWindow u `on` objectDestroy $ mainQuit
  _ <- processRnaBtn u `on` buttonActivated $ do
    fileName <- chooseFile (mainWindow u)
    interactive <- toggleButtonGetActive $ interactiveRenderCB u
    _ <- forkIO $ unless (Prelude.null fileName) $ do
      let rnaProc S.Empty = return ()
          rnaProc (r S.:<| rs) = do
            rnaIter r
            rnaProc rs
      r <- readRNA fileName
      postGUISync $ disableButtons u
      postGUISync $ clearImage (rnaView u)
      _ <- postGUISync $ statusbarPush (statusBar u) statusCtxId "Processing RNA"
      rnaST <- initRnaProcessor (rnaView u) interactive
      st <- execStateT (rnaProc r) rnaST
      unless interactive $ updateImage (rnaView u) (bitmaps st)
      _ <- postGUISync $ statusbarPop (statusBar u) statusCtxId
      postGUISync $ enableButtons u
    return ()
  _ <- processDnaBtn u `on` buttonActivated $ do
    interactive <- toggleButtonGetActive $ interactiveRenderCB u
    prefixStr <- entryGetText $ prefixField u
    putStrLn ("Prefix: " ++ prefixStr)
    dnaFile <- getDataFileName "endo.dna"
    dnaStr <- readFile dnaFile
    programStrBuf <- textViewGetBuffer $ prefixProgram u
    start <- textBufferGetStartIter programStrBuf
    end <- textBufferGetEndIter programStrBuf
    programStr <- textBufferGetText programStrBuf start end False
    _ <-
      forkIO $ do
        let haveDNA :: StateT (DnaProcSt, RnaProcSt) IO Bool
            haveDNA = do
              dna <- overDna $ gets getDna
              return $ not $ isEmpty dna
            loop :: StateT (DnaProcSt, RnaProcSt) IO ()
            loop = do
              overDna $ modify (\s -> s {getRna = emptyRNA})
              rna <- overDna dnaIter
              iteration <- overDna $ gets dnaIteration
              when (iteration `mod` 1000 == 0) $ do
                _ <- liftIO $ postGUISync $ do
                  statusbarPop (statusBar u) statusCtxId
                  statusbarPush (statusBar u) statusCtxId ("Processing DNA: iteration - " ++ show iteration)
                return ()
              overRna $ mapM_ rnaIter rna
        postGUISync $ do
          _ <- statusbarPush (statusBar u) statusCtxId "Processing DNA"
          disableButtons u
          clearImage (rnaView u)
        programmedPrefix <- calcPrefix programStr
        dnaST <- initDnaProcessor (fromString (prefixStr ++ programmedPrefix ++ dnaStr))
        rnaST <- initRnaProcessor (rnaView u) interactive
        (_, rnaST') <- flip execStateT (dnaST, rnaST) $ whileM_ haveDNA loop
        unless interactive $ updateImage (rnaView u) (bitmaps rnaST')
        postGUISync $ do
          statusbarPop (statusBar u) statusCtxId
          enableButtons u
    return ()
  return u
