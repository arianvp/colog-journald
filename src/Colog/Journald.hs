{-# LANGUAGE RecordWildCards #-}
module Colog.Journald 
( logJournalFields
, logMessageToJournal
, upgradeJournalAction
, log
, Message
, Priority(..)
)where
import Prelude hiding (log)
import GHC.Stack
import Control.Monad.IO.Class
import Systemd.Journal
import Colog.Core.Action
import qualified Data.Text  as T
import Data.Text
import Colog.Message (log, Msg(..))

type Message = Msg Priority

-- | Dirctly log journal fields to journald
logJournalFields :: MonadIO m => LogAction m JournalFields
logJournalFields = LogAction (liftIO . sendJournalFields)

-- | Log 
logMessageToJournal :: MonadIO m => LogAction m Message
logMessageToJournal = upgradeJournalAction logJournalFields

upgradeJournalAction :: LogAction m JournalFields -> LogAction m Message
upgradeJournalAction = cmap msgToJournalFields
  where
    msgToJournalFields :: Msg Priority -> JournalFields
    msgToJournalFields (Msg{..}) =
      message msgText <>
      priority msgSeverity <>
      callStackToJournalFields msgStack
    callStackToJournalFields :: CallStack -> JournalFields
    callStackToJournalFields callStack =
      case sourceLocOfCallStack callStack of
        Nothing -> mempty
        Just (callerName, SrcLoc {..})  ->
          codeFile srcLocFile <>
          codeLine srcLocStartLine <>
          codeFunc (T.pack callerName)
    sourceLocOfCallStack :: CallStack -> Maybe (String, SrcLoc)
    sourceLocOfCallStack cs = case getCallStack cs of
      [] ->  Nothing
      [x] -> Just x
      (_, loc) : (callerName, _): _ -> Just (callerName, loc)

