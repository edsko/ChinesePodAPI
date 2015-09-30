{-# LANGUAGE TemplateHaskell #-}
module Servant.ChinesePod.HSK.HSK2012 (
    hsk1
  , hsk2
  , hsk3
  , hsk4
  , hsk5
  , hsk6
  ) where

import Prelude hiding (Word)

import Servant.ChinesePod.HSK.Aux

$(spliceStickyStudy [
    ("HSK/2012/HSK1.txt", mkName "hsk1")
  , ("HSK/2012/HSK2.txt", mkName "hsk2")
  , ("HSK/2012/HSK3.txt", mkName "hsk3")
  , ("HSK/2012/HSK4.txt", mkName "hsk4")
  , ("HSK/2012/HSK5.txt", mkName "hsk5")
  , ("HSK/2012/HSK6.txt", mkName "hsk6")
  ])
