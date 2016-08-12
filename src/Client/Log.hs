{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Client.Log
Description : Logging of messages
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the logic for logging messages to files.

-}

module Client.Log
  (LogMsg(..)
  ,writeLogMessages
  ,writeLogMessage
  ,getLogDirectory
  ,logFileName
  ) where

import           Control.Lens (view)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.IO (hPutStrLn)
import           Data.Time (ZonedTime,defaultTimeLocale,formatTime)
import           System.Directory (createDirectoryIfMissing,getXdgDirectory,XdgDirectory(..))
import           System.FilePath ((</>),takeDirectory)
import           System.IO (withFile,IOMode(..))

import           Irc.Identifier (idText)
import           Irc.Message (IrcMsg(..))
import           Irc.RawIrcMsg (msgPrefix, msgCommand, msgParams)
import           Irc.UserInfo (renderUserInfo)

import           Client.State.Focus (Focus(..))
import           Client.Image.Message (MessageRendererParams(..),renderCapCmd)
import           Client.Message (MessageBody(..))

data LogMsg =
  LogMsg { logFocus :: !Focus
         , logBody :: !MessageBody
         , logTime :: !ZonedTime
         , logRendererParams :: !MessageRendererParams
         }

writeLogMessages :: [LogMsg] -> IO ()
writeLogMessages = mapM_ writeLogMessage

writeLogMessage :: LogMsg -> IO ()
writeLogMessage log =
  do dir <- getLogDirectory
     let logFile = dir </> logFileName (logFocus log) (logTime log)
     -- TODO do this once when the channel/network is created?
     createDirectoryIfMissing True (takeDirectory logFile)
     withFile logFile AppendMode $ \h ->
       hPutStrLn h (renderLogMessage log)

logFileName :: Focus -> ZonedTime -> FilePath
logFileName focus time =
  case focus of
    Unfocused                       -> 
      timeSuffix
    NetworkFocus network            -> 
      Text.unpack network <> "-" <> timeSuffix
    ChannelFocus network identifier -> 
      Text.unpack network </> 
      Text.unpack (idText identifier) <> "-" <> timeSuffix
  where
    timeSuffix = formatTime defaultTimeLocale "%F" time

getLogDirectory :: IO FilePath
getLogDirectory = getXdgDirectory XdgData ("glirc" </> "logs")

renderLogMessage :: LogMsg -> Text
renderLogMessage msg =
  Text.pack (formatTime defaultTimeLocale "%T" (logTime msg)) <> " " <>
  statusMsg (rendStatusMsg (logRendererParams msg)) <>
  renderBody (logBody msg) (logRendererParams msg)

renderBody :: MessageBody -> MessageRendererParams -> Text
renderBody (IrcBody irc) rp  = renderIrc irc rp
renderBody (ErrorBody txt) _ = renderError txt
renderBody (NormalBody txt) _      = "Thread finished"

separator :: Text
separator = "·"

separatedParams :: [Text] -> Text
separatedParams = Text.intercalate separator

renderNormal :: Text -> Text
renderNormal txt = "cient " <> txt

renderIrc :: IrcMsg -> MessageRendererParams -> Text
renderIrc body rp =
  let pal       = rendPalette rp
      sigils    = rendUserSigils rp
      myNicks   = rendMyNicks rp
      nicks     = rendNicks rp
  in
  case body of
    Nick old new ->
      "nick " <>
      Text.pack sigils <>
      renderUserInfo old <>
      " became " <>
      idText new

    Join nick _chan ->
      "join " <>
      renderUserInfo nick

    Part nick _chan mbreason ->
      "part " <>
      renderUserInfo nick <>
      foldMap (\reason -> " (" <> reason <> ")") mbreason

    Quit nick mbreason ->
      "quit "   <>
      renderUserInfo nick   <>
      foldMap (\reason -> " (" <> reason <> ")") mbreason

    Kick kicker _channel kickee reason ->
      "kick " <>
      Text.pack sigils <>
      renderUserInfo kicker <>
      " kicked " <>
      idText kickee <>
      ": " <>
      reason

    Topic src _dst txt ->
      renderUserInfo src <>
      " changed topic to " <>
      txt

    Notice src _dst txt ->
      "note " <>
      Text.pack sigils <>
      renderUserInfo src <>
      ": " <>
      txt

    Privmsg src _dst txt ->
      "chat " <>
      Text.pack sigils <>
      renderUserInfo src <>
      ": " <>
      txt

    Ctcp src _dst "ACTION" txt ->
      "actp * " <>
      Text.pack sigils <>
      renderUserInfo src <>
      " " <>
      txt

    CtcpNotice src _dst "ACTION" txt ->
      "actn * " <>
      Text.pack sigils <>
      renderUserInfo src <>
      " " <>
      txt

    Ctcp src _dst cmd txt ->
      "ctcp ! " <>
      Text.pack sigils <>
      renderUserInfo src <>
      " " <>
      cmd <>
      separator <>
      txt

    CtcpNotice src _dst cmd txt ->
      "ctcp ! " <>
      Text.pack sigils <>
      renderUserInfo src <>
      " " <>
      cmd <>
      separator <>
      txt

    Ping params ->
      "PING " <> separatedParams params

    Pong params ->
      "PONG " <> separatedParams params

    Error reason ->
      "ERROR " <>
      reason

    Reply code params ->
      Text.pack (show code) <>
      " " <>
      separatedParams params

    UnknownMsg irc ->
      maybe "" (\ui -> renderUserInfo ui <> " ")
        (view msgPrefix irc) <>
      view msgCommand irc <>
      " " <>
      separatedParams (view msgParams irc)

    Cap cmd args ->
      (renderCapCmd cmd) <>
      ": " <>
      separatedParams args

    Authenticate{} -> "AUTHENTICATE ***"

    Mode nick _chan params ->
      "mode " <>
      Text.pack sigils <>
      renderUserInfo nick <>
      " set mode: " <>
      separatedParams params

renderError :: Text -> Text
renderError txt = "Error " <> txt

statusMsg :: [Char] {- ^ sigils -} -> Text
statusMsg modes
  | null modes = ""
  | otherwise  = "(" <> Text.pack modes <> ") "
