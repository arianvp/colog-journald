{-# LANGUAGE RecordWildCards #-}
module Colog.Journald where
import GHC.Stack
import Control.Monad.IO.Class
import Systemd.Journal
import Colog.Message
import Colog.Core.Action
import qualified Data.Text  as T

type Message = Msg Priority

journalAction :: MonadIO m => LogAction m JournalFields
journalAction = LogAction (liftIO . sendJournalFields)

journalMessageAction :: MonadIO m => LogAction m (Msg Priority)
journalMessageAction = upgradeJournalAction journalAction

sourceLocOfCallStack :: CallStack -> Maybe (String, SrcLoc)
sourceLocOfCallStack cs = case getCallStack cs of
  [] ->  Nothing
  [x] -> Just x
  (_, loc) : (callerName, _): _ -> Just (callerName, loc)

-- Maps coarse-grained severities to syslog priorities.
-- TODO ask upstream to add Functor instance to Msg
-- Useful if you were already using Severity in your code
-- severitytoPriority :: Severity -> Priority
-- severitytoPriority = undefined

callStackToJournalFields :: CallStack -> JournalFields
callStackToJournalFields callStack =
  case sourceLocOfCallStack callStack of
    Nothing -> mempty
    Just (callerName, SrcLoc {..})  ->
      codeFile srcLocFile <>
      codeLine srcLocStartLine <>
      codeFunc (T.pack callerName)

upgradeJournalAction :: LogAction m JournalFields -> LogAction m (Msg Priority)
upgradeJournalAction = cmap msgToJournalFields
  where
    msgToJournalFields :: Msg Priority -> JournalFields
    msgToJournalFields (Msg{..}) =
      message msgText <>
      priority msgSeverity <>
      callStackToJournalFields msgStack
      
