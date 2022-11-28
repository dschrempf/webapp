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
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Graphics.Vega.VegaLite hiding (toHtml)
import Lucid

embed :: TS.Text -> VegaLite -> Html ()
embed nm x =
  mconcat
    [ div_ [id_ nm] mempty,
      script_ [type_ "text/javascript"] script
    ]
  where
    script =
      toHtmlRaw $
        TS.unlines
          [ "var spec = " <> spec <> ";",
            "vegaEmbed('#" <> nm <> "', spec);"
          ]
    spec = TL.toStrict $ A.encodeToLazyText $ fromVL x
