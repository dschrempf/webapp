{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Vega
-- Description :  Vega source
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Nov 22 21:04:00 2022.
module Vega
  ( embed,
  )
where

import qualified Data.Aeson.Text as A
import qualified Data.Text.Lazy as T
import Graphics.Vega.VegaLite
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

embed :: T.Text -> VegaLite -> H.Html
embed nm x =
  mconcat
    [ H.div mempty H.! A.id (H.lazyTextValue nm),
      H.script script H.! A.type_ "text/javascript"
    ]
  where
    script =
      H.toHtml $
        T.unlines
          [ "var spec = " <> spec <> ";",
            "vegaEmbed('#" <> nm <> "', spec);"
          ]
    spec = A.encodeToLazyText $ fromVL x
