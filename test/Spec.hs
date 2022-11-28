-- |
-- Module      :  Main
-- Description :  Tests for Spec
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov 28 11:25:31 2022.
module Main
  ( main,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time
import Test.Hspec
import Weather.Zamg

from, to :: LocalTime
from = read "2022-11-01 00:00:00"
to = read "2022-11-14 00:00:00"

main :: IO ()
main = hspec $ describe "zamgDownloadData" $ do
  it "downloads data from the ZAMG data hub" $ do
    d <- zamgDownloadData from to HoheWarte
    r <- BL.readFile "data/test-data.csv"
    d `shouldBe` r
