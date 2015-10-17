module Servant.ChinesePod.Vocab (
    Vocab(..)
  , Lesson(..)
  , Word(..)
  , Level(..)
  , extractVocab
  , loadVocab
    -- * Re-exports
  , V3Id(..)
  ) where

import Prelude hiding (Word)
import Control.Monad
import Data.Binary (Binary, decodeFile)
import Data.Data (Data)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.List (partition, isInfixOf)
import GHC.Generics
import Text.Show.Pretty (PrettyVal(..))
import qualified Data.Map as Map

import Servant.ChinesePod.API (V3Id(..))
import Servant.ChinesePod.Vocab.Word
import qualified Servant.ChinesePod.API as API

data Lesson = Lesson {
      title :: String
    , level :: Level
    , hosts :: String
    , key   :: [Word]

      -- | Supplemental vocabulary that appears somewhere in the dialogue
    , supDialog :: [Word]

      -- | Supplemental vocabulary that does not appear in the dialogue
    , supExtra :: [Word]
    }
  deriving (Generic, Data, Show)

data Level =
    Newbie
  | Elementary
  | Intermediate
  | UpperIntermediate
  | Advanced
  | Media
  deriving (Generic, Data, Eq, Ord, Show)

data Vocab = Vocab {
      vocab :: Map V3Id Lesson
    }
  deriving (Generic, Data, Show)

instance Binary Lesson
instance Binary Level
instance Binary Vocab

instance PrettyVal Lesson
instance PrettyVal Vocab
instance PrettyVal Level

{-------------------------------------------------------------------------------
  Constructing from full lesson content
-------------------------------------------------------------------------------}

extractVocab :: [API.LessonContent] -> Vocab
extractVocab = Vocab . Map.fromList . catMaybes . map go
  where
    go :: API.LessonContent -> Maybe (V3Id, Lesson)
    go content = do
      lesson <- extractLesson content
      return (API.lessonContentV3Id content, lesson)

extractLesson :: API.LessonContent -> Maybe Lesson
extractLesson API.LessonContent{..} = do
    guard $ lessonContentVideoLesson /= Just True
    let title = lessonContentTitle
        hosts = lessonContentHosts
    level      <- extractLevel lessonContentLevel
    vocabulary <- lessonContentVocabulary
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

extractLevel :: Maybe API.Level -> Maybe Level
extractLevel Nothing                           = Nothing
extractLevel (Just API.LevelNewbie)            = Just Newbie
extractLevel (Just API.LevelElementary)        = Just Elementary
extractLevel (Just API.LevelIntermediate)      = Just Intermediate
extractLevel (Just API.LevelUpperIntermediate) = Just UpperIntermediate
extractLevel (Just API.LevelAdvanced)          = Just Advanced
extractLevel (Just API.LevelMedia)             = Just Media
extractLevel (Just (API.LevelOther _))         = Nothing

extractWord :: API.Word -> Word
extractWord API.Word{..} = Word {
      pinyin = wordPinyin
    , source = wordSource
    , target = wordTarget
    }

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
