{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (log)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Colog.Journald
import Colog hiding (Message, Severity(..))

processLinesLog :: (WithLog env (Msg Priority) m, MonadIO m) => m ()
processLinesLog = do
    line <- liftIO TextIO.getLine
    case Text.length line of
        0 -> do
            -- here goes logging
            log Warning "Empty input"
            processLinesLog
        n -> do
            log Debug "Correct line"
            log Info $ "Line length: " <> Text.pack (show n)



main :: IO ()
main = do
 usingLoggerT logMessageToJournal processLinesLog
