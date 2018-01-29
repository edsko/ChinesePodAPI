{-# LANGUAGE TemplateHaskell #-}
module Servant.ChinesePod.HSK.Aux (
    readStickyStudy
  , spliceStickyStudy
    -- * Re-exports
  , mkName
  ) where

import Prelude hiding (Word, words)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Servant.ChinesePod.Vocab.Word
import Servant.ChinesePod.Util.String

spliceStickyStudy :: [(FilePath, Name)] -> Q [Dec]
spliceStickyStudy = fmap concat . mapM (uncurry go)
  where
    go :: FilePath -> Name -> Q [Dec]
    go fp nm = do
      words <- runIO $ readStickyStudy fp
      typ   <- sigD nm [t| [Word] |]
      def   <- valD (varP nm) (normalB (lift words)) []
      return $ [typ, def]

    -- We don't rely on a type class instance
    lift :: [Word] -> Q Exp
    lift = dataToExpQ (const Nothing)

readStickyStudy :: FilePath -> IO [Word]
readStickyStudy = fmap (parseStickyStudy . dropBOM) . readFile

parseStickyStudy :: String -> [Word]
parseStickyStudy = map go . map tabs . lines
  where
    go :: [String] -> Word
    go (simpl:_trad:_pinyinNum:pinyinAcc:en:_) = Word pinyinAcc simpl en
    go line = error $ "Could not translate " ++ show line

-- | Split a string on each tab character
--
-- We also trim whitespace from all fields
tabs :: String -> [String]
tabs = map trim . explode '\t'
