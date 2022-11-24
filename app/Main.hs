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
import WeatherForecast
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
vegaHeader = H.head $ mconcat [H.script mempty H.! A.src s | s <- vegaSources]

body :: H.Html -> H.Html
body = H.body

webapp :: H.Html -> H.Html
webapp x = H.docTypeHtml $ vegaHeader <> body x

main :: IO ()
main = S.scotty 3000 $ S.get "/" $ do
  x <- liftIO predictWeather
  blaze $ webapp x
