module Servant.ChinesePod.Vocab (
    Vocab(..)
  , Lesson(..)
  , Word(..)
  , extractVocab
  , loadVocab
  ) where

import Prelude hiding (Word)
import Control.Monad
import Data.Binary (Binary, decodeFile)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import GHC.Generics
import Text.Show.Pretty (PrettyVal(..), dumpStr)
import qualified Data.Map         as Map
import qualified Text.Show.Pretty as P

import Servant.ChinesePod.API (V3Id(..))
import qualified Servant.ChinesePod.API as API

data Lesson = Lesson {
      lessonTitle    :: String
    , lessonLevel    :: Level
    , lessonHosts    :: String
    , lessonKeyVocab :: [Word]
    , lessonSupVocab :: [Word]
    }
  deriving (Generic)

data Level =
    LevelNewbie
  | LevelElementary
  | LevelIntermediate
  | LevelUpperIntermediate
  | LevelAdvanced
  | LevelMedia
  deriving (Generic)

data Word = Word {
      wordPinyin :: String
    , wordSource :: String
    , wordTarget :: String
    }
  deriving (Generic)

data Vocab = Vocab {
      vocab :: Map V3Id Lesson
    }
  deriving (Generic)

instance Binary Lesson
instance Binary Level
instance Binary Vocab
instance Binary Word

instance PrettyVal Level where
  prettyVal LevelNewbie            = P.Con "Newbie"            []
  prettyVal LevelElementary        = P.Con "Elementary"        []
  prettyVal LevelIntermediate      = P.Con "Intermediate"      []
  prettyVal LevelUpperIntermediate = P.Con "UpperIntermediate" []
  prettyVal LevelAdvanced          = P.Con "Advanced"          []
  prettyVal LevelMedia             = P.Con "Media"             []

instance PrettyVal Word where
  prettyVal Word{..} = P.Con "Word" [
      prettyVal wordPinyin
    , prettyVal wordSource
    , prettyVal wordTarget
    ]

instance PrettyVal Lesson
instance PrettyVal Vocab

instance Show Lesson where show = dumpStr
instance Show Level  where show = dumpStr
instance Show Vocab  where show = dumpStr
instance Show Word   where show = dumpStr

{-------------------------------------------------------------------------------
  Constructing from full lesson content
-------------------------------------------------------------------------------}

-- TODO: filter out video lessons
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
    let lessonTitle = lessonContentTitle
        lessonHosts = lessonContentHosts
    lessonLevel    <- extractLevel lessonContentLevel
    vocabulary     <- lessonContentVocabulary
    let lessonKeyVocab = map extractWord $ API.vocabularyKeyVocab vocabulary
    let lessonSupVocab = map extractWord $ API.vocabularySupVocab vocabulary
    return Lesson{..}

extractLevel :: Maybe API.Level -> Maybe Level
extractLevel Nothing                           = Nothing
extractLevel (Just API.LevelNewbie)            = Just LevelNewbie
extractLevel (Just API.LevelElementary)        = Just LevelElementary
extractLevel (Just API.LevelIntermediate)      = Just LevelIntermediate
extractLevel (Just API.LevelUpperIntermediate) = Just LevelUpperIntermediate
extractLevel (Just API.LevelAdvanced)          = Just LevelAdvanced
extractLevel (Just API.LevelMedia)             = Just LevelMedia
extractLevel (Just (API.LevelOther _))         = Nothing

extractWord :: API.Word -> Word
extractWord API.Word{..} = Word {
      wordPinyin = wordPinyin
    , wordSource = wordSource
    , wordTarget = wordTarget
    }

{-------------------------------------------------------------------------------
  I/O
-------------------------------------------------------------------------------}

loadVocab :: FilePath -> IO Vocab
loadVocab = decodeFile
