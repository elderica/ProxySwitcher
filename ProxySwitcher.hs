{-# LANGUAGE ScopedTypeVariables #-}
-- 2013-05-01
module Main (main)
  where

import Foreign
import System.Win32
import Text.Printf
import Control.Monad (when)
import Options.Applicative -- package optparse-applicative

keyStr = "Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings"

tuesProxy = "proxy.kankyo-u.ac.jp:80"

openIEKey = regOpenKey hKEY_CURRENT_USER keyStr

-- |プロキシは有効か？
isProxyEnable :: IO Bool
isProxyEnable = do
  key <- openIEKey
  ptr <- malloc
  regQueryValueEx key "ProxyEnable" ptr (sizeOf (0 :: DWORD))
  b <- peek ptr
  free ptr
  regCloseKey key
  return $ if b == 0
    then False
    else True

-- |現在設定されているプロキシサーバは？
getProxyServer :: IO String
getProxyServer = do
  key <- openIEKey
  serverAddr <- regQueryValue key $ Just "ProxyServer"
  regCloseKey key
  return serverAddr

-- |プロキシ有効フラグの変更
setProxyIs :: Bool -> IO ()
setProxyIs b = do
  key <- openIEKey
  ptr <- malloc
  let b' = case b of
            True -> 1
            False -> 0           
  poke ptr b'
  regSetValueEx key "ProxyEnable" rEG_DWORD ptr (sizeOf (0 :: DWORD))
  free ptr
  regCloseKey key

-- |プロキシサーバを設定
setProxyServer :: String -> IO ()
setProxyServer host = do
  key <- openIEKey
  regSetValue key "ProxyServer" host
  regCloseKey key
  
-- |コマンドライン引数
data CmdOpt = CmdOpt
  { host :: String,
    checkOnly :: Bool
  }

-- |コマンドライン引数パーサ
cmdParser :: Parser CmdOpt
cmdParser = CmdOpt
  <$> strOption
      ( long "proxy" <>
        short 'p' <>
        metavar "PROXY:PORT" <>
        help "Specify proxy server" <>
        value "" )
  <*> switch
      ( long "check-only" <>
        short 'c' <>
        help "Only print settings" )
        
-- |現在の設定を表示
printCurrentSettings :: IO ()
printCurrentSettings = do
  b <- isProxyEnable
  host <- getProxyServer
  printf "Proxy `%s` is %s.\n" host $ b2ed b

-- |Bool値を表示用に文字列に変換するユーティリティ
b2ed b = if b then "enable" else "disable" 
    
-- |メイン
main' :: CmdOpt -> IO ()
main' (CmdOpt "" chk) = do
  printCurrentSettings
  when (not chk) $ do
    b <- isProxyEnable
    setProxyIs $ not b
    printf "Proxy was %sd.\n" $ b2ed (not b)
main' (CmdOpt newHost chk) = do
  printCurrentSettings
  when (not chk) $ do
    setProxyIs True
    setProxyServer newHost
    printf "Proxy was set `%s` and enabled.\n" newHost

main :: IO ()
main = execParser opts >>= main'
  where opts = info (helper <*> cmdParser)
          ( fullDesc <>
            progDesc "Proxy Swiching" <>
            header "ProxySwitcher" )
      
  