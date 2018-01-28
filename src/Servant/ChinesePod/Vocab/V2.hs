{-# LANGUAGE OverloadedStrings #-}
module Servant.ChinesePod.Vocab.V2 (
    Vocab(..)
  , Skipped(..)
  , Lesson(..)
  , Word(..)
  , Level(..)
  , extractVocab
  , loadVocab
    -- * Re-exports
  , V3Id(..)
  ) where

import Prelude hiding (Word)
import Data.Bifunctor (second)
import Data.Binary (Binary, decodeFile)
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.List (partition, isInfixOf)
import Data.Text (Text)
import GHC.Generics
import Text.Show.Pretty (PrettyVal(..))
import qualified Data.Map  as Map
import qualified Data.Text as T

import Servant.ChinesePod.API (V3Id(..))
import Servant.ChinesePod.Vocab.Word
import Servant.ChinesePod.Util.Migrate
import Servant.ChinesePod.Vocab.V1 (Level(..))
import qualified Servant.ChinesePod.API      as API
import qualified Servant.ChinesePod.Vocab.V1 as V1

data Lesson = Lesson {
      title   :: String
    , level   :: Level
    , hosts   :: String
    , key     :: [Word]
    , isVideo :: Bool

      -- | Supplemental vocabulary that appears somewhere in the dialogue
    , supDialog :: [Word]

      -- | Supplemental vocabulary that does not appear in the dialogue
    , supExtra :: [Word]
    }
  deriving (Generic, Data, Show)

data Vocab = Vocab {
      vocab :: Map V3Id Lesson
    }
  deriving (Generic, Data, Show)

instance Binary Lesson
instance Binary Vocab

instance PrettyVal Lesson
instance PrettyVal Vocab

{-------------------------------------------------------------------------------
  Constructing from full lesson content
-------------------------------------------------------------------------------}

data Skipped = Skipped {
      skippedV3Id   :: V3Id
    , skippedTitle  :: String
    , skippedReason :: Text
    }

extractVocab :: [API.LessonContent] -> ([Skipped], Vocab)
extractVocab = second mkVocab . partitionEithers . map go
  where
    mkVocab :: [(V3Id, Lesson)] -> Vocab
    mkVocab = Vocab . Map.fromList

    go :: API.LessonContent -> Either Skipped (V3Id, Lesson)
    go content =
      case extractLesson content of
        Left  err    -> Left $ Skipped {
                            skippedV3Id   = API.lessonContentV3Id  content
                          , skippedTitle  = API.lessonContentTitle content
                          , skippedReason = err
                          }
        Right lesson -> Right $ (API.lessonContentV3Id content, lesson)

extractLesson :: API.LessonContent -> Either Text Lesson
extractLesson API.LessonContent{..} = do
    let title   = lessonContentTitle
        hosts   = lessonContentHosts
        isVideo = lessonContentVideoLesson == Just True
    level      <- extractLevel lessonContentLevel
    vocabulary <- maybeToEither "Lacks lacks vocabulary" lessonContentVocabulary
    let key = map extractWord $ API.vocabularyKeyVocab vocabulary
        sup = map extractWord $ API.vocabularySupVocab vocabulary
        (supDialog, supExtra) = partition wordInDialogue sup
    return Lesson{..}
  where
    wordInDialogue word = or [
        inTranscription word lessonContentTranscription1
      , inTranscription word lessonContentTranscription2
      , maybe False (inDialogue word) lessonContentDialogue
      ]

-- | Try out best to classify the lessons level
--
-- For now we ignore the following levels:
--
-- * Dear Amber
-- * Movie Madness
-- * News and Features
-- * Poems with Pete
--
-- (As of Jan 28, 2018, this comes to a total of 193 skipped lessons.)
extractLevel :: Maybe API.Level -> Either Text Level
extractLevel Nothing                                      = Left "No level specified"
extractLevel (Just API.LevelNewbie)                       = Right Newbie
extractLevel (Just API.LevelElementary)                   = Right Elementary
extractLevel (Just API.LevelIntermediate)                 = Right Intermediate
extractLevel (Just API.LevelUpperIntermediate)            = Right UpperIntermediate
extractLevel (Just API.LevelAdvanced)                     = Right Advanced
extractLevel (Just API.LevelMedia)                        = Right Media
extractLevel (Just (API.LevelOther "Any"))                = Right Newbie
extractLevel (Just (API.LevelOther "Upper-Intermediate")) = Right UpperIntermediate
extractLevel (Just (API.LevelOther "Pre Intermediate"))   = Right Elementary
extractLevel (Just (API.LevelOther l))                    = Left $ T.pack $ "Unknown level " ++ show l

extractWord :: API.Word -> Word
extractWord API.Word{..} = Word {
      pinyin = wordPinyin
    , source = wordSource
    , target = wordTarget
    }

maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither _ (Just a) = Right a
maybeToEither e  Nothing = Left e

{-------------------------------------------------------------------------------
  Splitting supplemental vocabulary
-------------------------------------------------------------------------------}

inTranscription :: Word -> String -> Bool
inTranscription word = (source word `isInfixOf`)

inDialogue :: Word -> [API.Sentence] -> Bool
inDialogue = any . inSentence

inSentence :: Word -> API.Sentence -> Bool
inSentence word API.Sentence{..} = source word `isInfixOf` sentenceSource

{-------------------------------------------------------------------------------
  I/O
-------------------------------------------------------------------------------}

loadVocab :: FilePath -> IO Vocab
loadVocab = decodeFile

{-------------------------------------------------------------------------------
  Migration
-------------------------------------------------------------------------------}

instance Migrate Vocab where
  type MigrateFrom Vocab = V1.Vocab

  migrate V1.Vocab{..} = Vocab (fmap migrate vocab)

instance Migrate Lesson where
  type MigrateFrom Lesson = V1.Lesson

  migrate V1.Lesson{..} = Lesson{
        title     = title
      , level     = level
      , hosts     = hosts
      , key       = key
      , isVideo   = False -- we used to filter all these out
      , supDialog = supDialog
      , supExtra  = supExtra
      }
