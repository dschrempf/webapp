{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Sample homepage
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

import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Vega
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

vegaBody :: H.Html
vegaBody = H.body $ embed "strip" stripPlot <> embed "parallax" parallaxBreakdown <> embed "gmag" gmagHistogramWithColor'

vega :: H.Html
vega = H.docTypeHtml $ vegaHeader <> vegaBody

main :: IO ()
main = S.scotty 3000 $ S.get "/" $ blaze vega
