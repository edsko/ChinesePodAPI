{-# LANGUAGE TemplateHaskell #-}
module Servant.ChinesePod.HSK.Aux (
    readStickyStudy
  , spliceStickyStudy
    -- * Re-exports
  , mkName
  ) where

import Prelude hiding (Word, words)
import Servant.ChinesePod.Vocab
import Language.Haskell.TH
import Language.Haskell.TH.Quote

spliceStickyStudy :: [(FilePath, Name)] -> Q [Dec]
spliceStickyStudy = fmap concat . mapM (uncurry go)
  where
    go :: FilePath -> Name -> Q [Dec]
    go fp nm = do
      words <- runIO $ readStickyStudy fp
      typ <- sigD nm [t| [Word] |]
      def <- valD (varP nm) (normalB (lift words)) []
      return $ [typ, def]

    -- We don't really on the type class instance
    lift :: [Word] -> Q Exp
    lift = dataToExpQ (const Nothing)

readStickyStudy :: FilePath -> IO [Word]
readStickyStudy = fmap parseStickyStudy . readFile

parseStickyStudy :: String -> [Word]
parseStickyStudy = map go . map tabs . lines
  where
    go :: [String] -> Word
    go (simpl:_trad:_pinyinNum:pinyinAcc:en:_) = Word pinyinAcc simpl en
    go line = error $ "Could not translate " ++ show line

-- | Split a string on each tab character
tabs :: String -> [String]
tabs = explode '\t'

-- | Split a string at the specified delimeter
explode :: Char -> String -> [String]
explode needle = go
  where
    go :: String -> [String]
    go haystack = case break (== needle) haystack of
                   (xs, "")                 -> [xs]
                   (xs, _needle':haystack') -> xs : go haystack'
