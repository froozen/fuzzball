{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Main
Description : The Main module of the tool

The Main module of the fuzzy matching tool.
-}

module Main where

import Fuzzball.Algorithm
import UI.NCurses
import System.Posix.IO
import Control.Applicative (Applicative, (<$>))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State (MonadState, gets, modify)
import Control.Monad.Trans.State hiding (gets, modify)
import Control.DeepSeq
import qualified Data.Foldable as F (forM_)
import Data.List (nub, sort)

-- | The state of a running fuzzball instance
data FuzzballState = FuzzballState {
      input       :: String
    , candidates  :: [String]
    , inputW      :: Window
    , candidatesW :: Window
    , highlight   :: ColorID
    }

-- | The Fuzzball custom monad
newtype Fuzzball a = Fuzzball (StateT FuzzballState Curses a)
    deriving (Functor, Applicative, Monad, MonadState FuzzballState)

-- | Run a Fuzzball
runFuzzball :: FuzzballState -> Fuzzball a -> Curses a
runFuzzball s (Fuzzball m) = evalStateT m s

-- | Lift a Curses into a Fuzzball
liftC :: Curses a -> Fuzzball a
liftC = Fuzzball . lift

-- | The main routine
main :: IO ()
main = do
    -- Read candidates from stdin
    inputCandidates <- nub . lines <$> getContents
    deepseq inputCandidates . return $ ()

    -- Remap tty to stdin
    tty <- openFd "/dev/tty" ReadWrite Nothing defaultFileFlags
    void . dupTo tty $ stdInput

    result <- runCurses $ do
        setEcho True
        setCBreak True
        defaultWindow >>= flip setKeypad True

        -- Create the two windows
        (height, width) <- screenSize
        inputW <- newWindow 1 width 0 0
        candidatesW <- newWindow (height - 1) width 1 1
        setKeypad inputW True
        setKeypad candidatesW True

        -- Decide on a highlight color
        highlight <- newColorID ColorDefault ColorCyan 1

        -- Run the fuzzball instance
        let state = FuzzballState "" inputCandidates inputW candidatesW highlight
        result <- runFuzzball state loop

        -- Close the windows
        closeWindow inputW
        closeWindow candidatesW

        return result

    -- Maybe print a result
    F.forM_ result putStrLn

-- | The main loop of the program
loop :: Fuzzball (Maybe String)
loop = do
    draw
    handleEvent

-- | Draw the two windows
draw :: Fuzzball ()
draw = do
    clearWindows
    drawCandidates
    drawInput
    liftC render

-- | A disgusting hack to clear all the windows
clearWindows :: Fuzzball ()
clearWindows = do
    (height, width) <- liftC screenSize

    inputW <- gets inputW
    liftC . updateWindow inputW $ do
        moveCursor 0 0
        drawString . replicate (fromInteger (width - 1)) $ ' '

    candidatesW <- gets candidatesW
    liftC . updateWindow candidatesW $ do
        moveCursor 0 0
        drawString . replicate (fromInteger ((width - 1) * (height - 1))) $ ' '

-- | Draw the input field
drawInput :: Fuzzball ()
drawInput = do
    input <- gets input
    inputW <- gets inputW
    width <- fromInteger . snd <$> liftC screenSize

    liftC . updateWindow inputW $ do
        moveCursor 0 0
        -- Make sure the input fits the input line
        let dropLength = length input - width + 3
            fixedInput = if dropLength > 0 then drop dropLength input else input
        drawString $ "> " ++ fixedInput

-- | Draw the matching candidates
drawCandidates :: Fuzzball ()
drawCandidates = do
    candidatesW <- gets candidatesW
    matchingCandidates <- matchingCandidates
    height <- fromInteger . (\x -> x - 1) . fst <$> liftC screenSize
    width <- snd <$> liftC screenSize
    highlight <- gets highlight

    liftC . updateWindow candidatesW $ do
        forM_ (zip [0..] . take height $ matchingCandidates) $ \(y, candidate) -> do
            moveCursor y 0
            when (y == 0) $ do
                setColor highlight
                drawLineH (Just . Glyph ' ' $ []) width
            displayMatchResult candidate
            when (y == 0) $ setColor defaultColorID

-- | Returns all the candidates matching the current input
matchingCandidates :: Fuzzball [(MatchRating, String)]
matchingCandidates = liftM2 order (gets input) (gets candidates)
  where order x xs = sort . squash . flip zip xs . map (fuzzyMatch x) $ xs
        squash [] = []
        squash ((Just a, b):xs) = (a, b):squash xs
        squash (_:xs) = squash xs

-- | Display a matched candidate
displayMatchResult :: (MatchRating, String) -> Update ()
displayMatchResult ((MatchRating _ []), s) = drawString s
-- Underline a matched character
displayMatchResult ((MatchRating _ (0:xs)), (c:cs)) = do
    setAttribute AttributeUnderline True
    drawString [c]
    setAttribute AttributeUnderline False

    let nextRating = MatchRating 0 . map (\x -> x - 1) $ xs
    displayMatchResult (nextRating, cs)
displayMatchResult ((MatchRating _ xs), (c:cs)) = do
    drawString [c]

    let nextRating = MatchRating 0 . map (\x -> x - 1) $ xs
    displayMatchResult (nextRating, cs)

-- | Handle the different incoming keyboard events
handleEvent :: Fuzzball (Maybe String)
handleEvent = do
    event <- liftC (defaultWindow >>= flip getEvent Nothing)
    case event of
        Just e -> handle e
        _ -> return Nothing

    where -- Delete a charcter
          handle (EventSpecialKey KeyBackspace) = do
            input <- gets input
            unless (null input) (modify $ \s -> s { input = init input })
            loop

          -- Maybe return the matched result
          handle (EventCharacter '\n') = do
            result <- matchingCandidates
            return $ if null result
                then Nothing
                else Just . snd . head $ result

          -- Append a character
          handle (EventCharacter c) = do
            input <- gets input
            modify $ \s -> s { input = input ++ [c] }
            loop

          -- Resize the windows
          handle EventResized = do
            candidatesW <- gets candidatesW
            liftC . closeWindow $ candidatesW

            (height, width) <- liftC screenSize
            newCandidatesW <- liftC . newWindow (height - 1) width 1 $ 0
            modify $ \s -> s { candidatesW = newCandidatesW }

            loop

          handle _ = loop
