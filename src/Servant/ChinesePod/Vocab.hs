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
import GHC.Generics
import Text.Show.Pretty (PrettyVal(..), dumpStr)
import qualified Data.Map         as Map
import qualified Text.Show.Pretty as P

import Servant.ChinesePod.API (V3Id(..))
import qualified Servant.ChinesePod.API as API

data Lesson = Lesson {
      title :: String
    , level :: Level
    , hosts :: String
    , key   :: [Word]
    , sup   :: [Word]
    }
  deriving (Generic, Data)

data Level =
    Newbie
  | Elementary
  | Intermediate
  | UpperIntermediate
  | Advanced
  | Media
  deriving (Generic, Data)

data Word = Word {
      pinyin :: String
    , source :: String
    , target :: String
    }
  deriving (Generic, Data)

data Vocab = Vocab {
      vocab :: Map V3Id Lesson
    }
  deriving (Generic, Data)

instance Binary Lesson
instance Binary Level
instance Binary Vocab
instance Binary Word

instance PrettyVal Word where
  prettyVal Word{..} = P.Con "Word" [
      prettyVal pinyin
    , prettyVal source
    , prettyVal target
    ]

instance PrettyVal Lesson
instance PrettyVal Vocab
instance PrettyVal Level

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
    let title = lessonContentTitle
        hosts = lessonContentHosts
    level      <- extractLevel lessonContentLevel
    vocabulary <- lessonContentVocabulary
    let key = map extractWord $ API.vocabularyKeyVocab vocabulary
    let sup = map extractWord $ API.vocabularySupVocab vocabulary
    return Lesson{..}

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
  I/O
-------------------------------------------------------------------------------}

loadVocab :: FilePath -> IO Vocab
loadVocab = decodeFile
