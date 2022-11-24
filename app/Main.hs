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
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Weather.App
import qualified Web.Scotty as S

blaze :: H.Html -> S.ActionM ()
blaze = S.html . H.renderHtml

vegaSources :: [H.AttributeValue]
vegaSources =
  [ "https://cdn.jsdelivr.net/npm/vega@5",
    "https://cdn.jsdelivr.net/npm/vega-lite@5",
    "https://cdn.jsdelivr.net/npm/vega-embed@6"
  ]

vegaHeader :: H.Html
vegaHeader = mconcat [H.script mempty H.! A.src s | s <- vegaSources]

css :: H.Html
css = H.link H.! A.rel "stylesheet" H.! A.href "static/css/style.css"

header :: H.Html
header = H.head $ vegaHeader <> css

body :: H.Html -> H.Html
body x = H.div H.! A.class_ "content" $ H.body x

webapp :: H.Html -> H.Html
webapp x = H.docTypeHtml $ header <> body x

main :: IO ()
main = S.scotty 3000 $ do
  S.get "/" $ do
    x <- liftIO weatherApp
    blaze $ webapp x
  S.get "/static/css/style.css" $ do
    S.setHeader "Content-Type" "text/css"
    S.file "static/css/style.css"
  S.get "/favicon.ico" $ S.file "static/favicon.ico"
