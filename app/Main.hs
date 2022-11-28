{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Sample webapp
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Nov 22 16:59:23 2022.
module Main
  ( main,
  )
where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Lucid
import Weather.App
import qualified Web.Scotty as S

blaze :: Html () -> S.ActionM ()
blaze = S.html . renderText

vegaSources :: [T.Text]
vegaSources =
  [ "https://cdn.jsdelivr.net/npm/vega@5",
    "https://cdn.jsdelivr.net/npm/vega-lite@5",
    "https://cdn.jsdelivr.net/npm/vega-embed@6"
  ]

vegaHeader :: Html ()
vegaHeader = mconcat [script_ [src_ s] (mempty :: T.Text) | s <- vegaSources]

css :: Html ()
css = link_ [rel_ "stylesheet", href_ "static/css/style.css"]

header :: Html ()
header = head_ $ vegaHeader <> css

body :: Html () -> Html ()
body x = div_ [class_ "content"] $ body_ x

webapp :: Html () -> Html ()
webapp x = doctypehtml_ $ header <> body x

-- start, end :: LocalTime
-- start = read "2022-09-01 00:00:00"
-- end = read "2022-11-14 00:00:00"

main :: IO ()
main = S.scotty 3000 $ do
  S.get "/" $ do
    x <- liftIO $ weatherApp WAppDefault
    blaze $ webapp x
  S.get "/static/css/style.css" $ do
    S.setHeader "Content-Type" "text/css; charset=utf-8"
    S.file "static/css/style.css"
  S.get "/favicon.ico" $ S.file "static/favicon.ico"
