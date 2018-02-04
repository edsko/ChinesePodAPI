-- | Analyze the vocab lists and try to match them against dialogues
--
-- This module is intended for use in @ghci@.
module Servant.ChinesePod.Analysis (
    -- * Global state
    -- ** Initialization analysis state
    initState
  , resetVocab
    -- ** Saving and loading analysis state
  , save
  , open
  , openV1
    -- ** Saving and loading list of harmless words
  , saveHarmless
  , openHarmless
    -- ** Access global state
  , State(..)
  , getState
    -- * Tools
  , showLessonSummary
  , showWordSummary
  , searchVocab
    -- * Operations on the state
  , filterLessons
  , dropLesson
  , pick
  , skip
  , unskip
    -- * Zooming
  , zoomIn
  , zoomOut
  , setFocus
  , resetFocus
    -- ** Predicates
  , atLevel
  , irrelInHSK
  , maxNumIrrel
  , onlyLessons
  , publishedAfter
  , publishedAfterYear
    -- * HSK tools
  , showHskLevel
  , showHskSplits
  , showSearchHsk
    -- * Automatic ranking
  , Ranked(..)
  , RankedCumulative(..)
  , rank
  , rankAll
  , rankCumulative
  , showRanking
  , rcMaxLevel
  , rcMinNew
  , rcMaxNotInHSK
    -- * Dealing with harmless words
  , showHarmless
  , markHarmless
  , analyzeIrrelevant
    -- * Local optimization
  , Improvement
  , improveLocal
  , showCandidates
    -- * Export
    -- ** Handpicked lessons
  , pickedToPleco
  , pickedToMarkdown
  , downloadPicked
    -- ** Automatically ranked lessons
  , rankedToPleco
  , rankedToMarkdown
  , downloadRanked
    -- * Statistics
  , printStats
  , printStats'
    -- * Summarising
  , showStateSummary
    -- ** Sorting keys
  , countLessonRel
  , countLessonRelIrrel
  , countWordAppearsIn
  , countReallyIrrelevant
    -- * Re-exports
  , module State
  , module Vocab
  , module HSK
  , module Data.IORef
  ) where

import Prelude hiding (Word, words)
import Control.Monad
import Data.Bifunctor
import Data.Binary (encodeFile, decodeFile)
import Data.Fixed
import Data.Function (on)
import Data.Functor.Contravariant
import Data.IORef
import Data.List (intercalate, sortBy, partition, nub, isInfixOf, maximumBy)
import Data.Map (Map)
import Data.Maybe (mapMaybe, fromJust, isNothing)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Time
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))
import System.Process (callProcess)
import Text.Show.Pretty (PrettyVal, dumpStr)
import Text.Printf (printf)
import qualified Data.Map as Map hiding ((!))
import qualified Data.Set as Set

import Servant.ChinesePod.Analysis.State.V2           as State
import Servant.ChinesePod.HSK                         as HSK
import Servant.ChinesePod.Vocab.V2                    as Vocab
import qualified Servant.ChinesePod.API               as API
import qualified Servant.ChinesePod.Analysis.State.V1 as V1
import qualified Servant.ChinesePod.Analysis.State.V2 as V2

{-------------------------------------------------------------------------------
  Global state
-------------------------------------------------------------------------------}

globalAnalysisState :: IORef AnalysisState
{-# NOINLINE globalAnalysisState #-}
globalAnalysisState = unsafePerformIO $ newIORef (error "state not loaded")

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

initState :: FilePath -> [Word] -> IO ()
initState vocabFile words = do
    vocab <- loadVocab vocabFile
    let static  = analysisStatic vocab words
        dynamic = analysisDynamic static
    writeIORef globalAnalysisState (static, dynamic)

-- | Reset the list of available lessons
--
-- Useful when we downloaded new lessons.
-- NOTE: This /only/ updates the static part of the state.
resetVocab :: FilePath -> IO ()
resetVocab vocabFile = do
    vocab <- loadVocab vocabFile
    modifyIORef globalAnalysisState $ \(static, dynamic) ->
      (analysisStatic vocab (analysisAllWords static), dynamic)

analysisStatic :: Vocab -> [Word] -> AnalysisStatic
analysisStatic (Vocab vocab) words = AnalysisStatic {
      analysisAllLessons = vocab
    , analysisAllWords   = words
    , analysisInverse    = computeInverse vocab words'
    }
  where
    words' :: [Simpl]
    words' = map source words

-- | Initial dynamic analysis state
analysisDynamic :: AnalysisStatic -> AnalysisDynamic
analysisDynamic AnalysisStatic{..} = AnalysisDynamic{..}
  where
    analysisTodo      = analysisAllWords
    analysisPicked    = []
    analysisAvailable = initRelevant (simplSet analysisTodo) analysisAllLessons

-- | Initial list of relevant lessons
initRelevant :: Set Simpl
             -> Map V3Id Lesson
             -> Map V3Id RelevantLesson
initRelevant = Map.mapMaybe . mkRelevant (not . null)

mkRelevant :: ([Word] -> Bool)  -- ^ Condition on the set of relevant words
           -> Set Simpl         -- ^ Words of interest
           -> Lesson -> Maybe RelevantLesson
mkRelevant checkRel words Lesson{..} = do
    let (rel, irrel') = partition isRelevantWord (key ++ supDialog)
        irrel         = map withLevel irrel'
    guard $ checkRel rel
    return RelevantLesson{..}
  where
    isRelevantWord :: Word -> Bool
    isRelevantWord Word{..} = source `Set.member` words

simplSet :: [Word] -> Set Simpl
simplSet = Set.fromList . map source

-- | Cull relevant lessons after we've picked a lesson and so covered new words
--
-- We remove these words from the 'relevant' list because they are no longer
-- relevant (and so should no longer count towards how good a choice a lesson
-- is), but we don't add them to the irrelevant words because they should also
-- not count towards how bad a choice a lesson is.
cullRelevant :: Set Simpl
             -> Map V3Id RelevantLesson
             -> Map V3Id RelevantLesson
cullRelevant newCovered = Map.mapMaybe relevantLesson
  where
    relevantLesson :: RelevantLesson -> Maybe RelevantLesson
    relevantLesson RelevantLesson{..} = do
      let rel' = filter isRelevantWord rel
      guard $ not (null rel')
      return RelevantLesson { rel = rel', .. }

    isRelevantWord :: Word -> Bool
    isRelevantWord Word{..} = not (source `Set.member` newCovered)

computeInverse :: Map V3Id Lesson
               -> [Simpl]
               -> Map Simpl [V3Id]
computeInverse vocab = Map.fromList . map go
  where
    vocab' :: [(V3Id, Lesson)]
    vocab' = Map.toList vocab

    go :: Simpl -> (Simpl, [V3Id])
    go word = (word, mapMaybe (contains word) vocab')

    contains :: Simpl -> (V3Id, Lesson) -> Maybe V3Id
    contains simpl (v3id, lesson) = do
      guard $ simpl `elem` map source (key lesson ++ supDialog lesson)
      return v3id

{-------------------------------------------------------------------------------
  Saving/loading
-------------------------------------------------------------------------------}

save :: FilePath -> IO ()
save fp = encodeFile fp =<< readIORef globalAnalysisState

open :: FilePath -> IO ()
open fp = writeIORef globalAnalysisState =<< decodeFile fp

-- | Migration
openV1 :: FilePath -> IO ()
openV1 fp = writeIORef globalAnalysisState =<< (v1_to_v2 <$> decodeFile fp)
  where
    v1_to_v2 :: V1.AnalysisState -> V2.AnalysisState
    v1_to_v2 = migrate

{-------------------------------------------------------------------------------
  Search vocabulary for a word
-------------------------------------------------------------------------------}

-- | Search for a word in the vocabulary of all lessons
--
-- (including this word appearing as a subword)
searchVocab :: Simpl -> IO ()
searchVocab word = do
    st <- getState
    (AnalysisStatic{..}, AnalysisDynamic{..}) <- readIORef globalAnalysisState
    inFocus <- getFocus

    let isMatching :: (V3Id, Lesson) -> Maybe (Summary (V3Id, RelevantLesson), String)
        isMatching (v3id, lesson) = do
          guard $ any (\word' -> word `isInfixOf` source word') (key lesson ++ supDialog lesson)
          let mAlreadyPicked      = lookup v3id analysisPicked
              mAvailable          = Map.lookup v3id analysisAvailable
              (relevant, comment) = case (mAlreadyPicked, mAvailable) of
                (Just alreadyPicked, _) -> (alreadyPicked, "already picked")
                (_, Just available)     -> (available, "available")
                (_, _)                  -> (irrelevantLesson lesson, "other")
              lessonSummary       = summarise st (v3id, relevant)
          guard $ inFocus lessonSummary
          return (lessonSummary, comment)

    let matchingLessons :: [(Summary (V3Id, RelevantLesson), String)]
        matchingLessons = mapMaybe isMatching (Map.toList analysisAllLessons)

    putStrLn $ word ++ " appears in the following in-focus lessons:"
    putStrLn "-----------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map showWithComment
             $ sortByKey (fst `contramap` countLessonRelIrrel)
             $ matchingLessons
  where
    showWithComment :: (Summary (V3Id, RelevantLesson), String) -> String
    showWithComment (summary, comment) = pretty True summary
                                      ++ " (" ++ comment ++ ")"

irrelevantLesson :: Lesson -> RelevantLesson
irrelevantLesson Lesson{..} = RelevantLesson{
    rel   = []
  , irrel = map withLevel (key ++ supDialog)
  }

{-------------------------------------------------------------------------------
  Different kinds of sorting functions
-------------------------------------------------------------------------------}

-- | Number of relevant words in the vocabulary of the lesson
countLessonRel :: SortKey (Summary (V3Id, RelevantLesson))
countLessonRel = SortKey $ \LessonSummary{..} ->
    length lessonRel

-- | Number of relevant and irrelevant words in the vocab of the lesson
--
-- We multiply the number of irrelevant words by -1, so that we sort them
-- in the opposite direction.
countLessonRelIrrel :: SortKey (Summary (V3Id, RelevantLesson))
countLessonRelIrrel = SortKey $ \LessonSummary{..} ->
    (length lessonRel, negate (length lessonHarmful))

-- | Number of lessons that have this word in their key vocabulary
countWordAppearsIn :: SortKey (Summary Simpl)
countWordAppearsIn = SortKey $ \WordSummary{..} ->
    length wordAppearsIn

-- | Number of words in the irrelevant key vocabulary that are not in the
-- specified HSK levels
countReallyIrrelevant :: [HSKLevel] -> SortKey (Summary (V3Id, RelevantLesson))
countReallyIrrelevant ls = SortKey $ \LessonSummary{..} ->
    length $ filter (none (`elem` ls) . snd) lessonHarmful

{-------------------------------------------------------------------------------
  Predicates on lessons
-------------------------------------------------------------------------------}

type LessonPredicate = Summary (V3Id, RelevantLesson) -> Bool

atLevel :: [Level] -> LessonPredicate
atLevel ls LessonSummary{..} = lessonLevel `elem` ls

-- | Only allow irrelevant words from the specified HSK levels
irrelInHSK :: [HSKLevel] -> LessonPredicate
irrelInHSK ls LessonSummary{..} = all (any (`elem` ls) . snd) lessonHarmful

-- | Maximum number of irrelevant words
maxNumIrrel :: Int -> LessonPredicate
maxNumIrrel n LessonSummary{..} = length lessonHarmful <= n

onlyLessons :: [V3Id] -> LessonPredicate
onlyLessons lessonIds LessonSummary{..} = lessonId `elem` lessonIds

publishedAfter :: LocalTime -> LessonPredicate
publishedAfter t LessonSummary{..} = lessonReleased >= t

publishedAfterYear :: Int -> LessonPredicate
publishedAfterYear year = publishedAfter t
  where
    t = parseTimeOrError
          True
          defaultTimeLocale
          "%F %T"
          (show year ++ "-01-01 00:00:00")

{-------------------------------------------------------------------------------
  Operations on the dynamic state
-------------------------------------------------------------------------------}

filterLessons :: LessonPredicate -> IO ()
filterLessons p = updateDynamic_ aux
  where
    aux :: State -> AnalysisDynamic -> AnalysisDynamic
    aux st AnalysisDynamic{..} = AnalysisDynamic{
          analysisAvailable = Map.filterWithKey (curry p') analysisAvailable
        , ..
        }
      where
        p' :: (V3Id, RelevantLesson) -> Bool
        p' = p . summarise st

-- | Remove a lesson from the available set
dropLesson :: V3Id -> IO ()
dropLesson lessonId' = filterLessons notSpecifiedLesson
  where
    notSpecifiedLesson :: LessonPredicate
    notSpecifiedLesson LessonSummary{lessonId} = lessonId /= lessonId'

-- | Pick a lesson
pick :: V3Id -> IO ()
pick lessonId = updateDynamic_ aux
  where
    aux :: State -> AnalysisDynamic -> AnalysisDynamic
    aux State{..} AnalysisDynamic{..} = AnalysisDynamic{
          analysisTodo      = analysisTodo'
        , analysisPicked    = analysisPicked'
        , analysisAvailable = analysisAvailable'
        }
      where
        AnalysisStatic{..} = stateStatic

        analysisTodo'      = filter (not . inLesson) analysisTodo
        analysisPicked'    = analysisPicked ++ [(lessonId, pickedLesson)]
        analysisAvailable' = cullRelevant pickedLessonWords $
                               Map.delete lessonId analysisAvailable

        inLesson :: Word -> Bool
        inLesson Word{..} = source `Set.member` pickedLessonWords

        pickedLesson :: RelevantLesson
        pickedLesson = case Map.lookup lessonId analysisAvailable of
          Just relevant -> relevant
          Nothing       -> irrelevantLesson (analysisAllLessons `mapAt` lessonId)

        pickedLessonWords :: Set Simpl
        pickedLessonWords = simplSet $ rel pickedLesson

-- | Remove a word from TODO without actually covering it
skip :: Simpl -> IO ()
skip simpl = updateDynamic_ aux
  where
    aux :: State -> AnalysisDynamic -> AnalysisDynamic
    aux _ AnalysisDynamic{..} = AnalysisDynamic{
          analysisTodo      = analysisTodo'
        , analysisPicked    = analysisPicked
        , analysisAvailable = analysisAvailable'
        }
      where
        analysisTodo'      = filter ((/= simpl) . source) analysisTodo
        analysisAvailable' = cullRelevant (Set.singleton simpl)
                               analysisAvailable

-- | Add a word back into TODO
--
-- NOTE: This may reintroduce lessons into 'analysisAvailable' that were
-- previously explicitly removed.
unskip :: Simpl -> IO ()
unskip simpl = updateDynamic_ aux
  where
    aux :: State -> AnalysisDynamic -> AnalysisDynamic
    aux State{..} AnalysisDynamic{..} = AnalysisDynamic{
          analysisTodo      = newTodo ++ analysisTodo
        , analysisPicked    = analysisPicked
        , analysisAvailable = Map.union newAvailable analysisAvailable
        }
      where
        AnalysisStatic{..} = stateStatic

        newTodo      = filter ((== simpl) . source) analysisAllWords
        newAvailable = initRelevant (Set.singleton simpl) analysisAllLessons

{-------------------------------------------------------------------------------
  Focusing on a subset
-------------------------------------------------------------------------------}

-- | Add an additional constraint on the focus
zoomIn :: LessonPredicate -> IO ()
zoomIn p = modifyIORef globalFocus $ FocusZoom p

-- | Undo one 'zoomIn'
zoomOut :: IO ()
zoomOut = modifyIORef globalFocus go
  where
    go :: Focus -> Focus
    go FocusAll         = FocusAll
    go (FocusZoom _ ps) = ps

-- | Focus only on lessons satisfying the specified predciate
setFocus :: LessonPredicate -> IO ()
setFocus = writeIORef globalFocus . predicateToFocus
  where
    predicateToFocus :: LessonPredicate -> Focus
    predicateToFocus p = FocusZoom p FocusAll

-- | Get current focus as a predicate
getFocus :: IO LessonPredicate
getFocus = focusPredicate <$> readIORef globalFocus

focusPredicate :: Focus -> LessonPredicate
focusPredicate FocusAll         _ = True
focusPredicate (FocusZoom p ps) l = p l && focusPredicate ps l

-- | Zoom all the way out
resetFocus :: IO ()
resetFocus = writeIORef globalFocus FocusAll

data Focus =
    -- | Show all available lessons
    FocusAll

    -- | Limit the focus
  | FocusZoom LessonPredicate Focus

globalFocus :: IORef Focus
{-# NOINLINE globalFocus #-}
globalFocus = unsafePerformIO $ newIORef FocusAll

{-------------------------------------------------------------------------------
  HSK level for each word
-------------------------------------------------------------------------------}

withLevel :: Word -> (Word, [HSKLevel])
withLevel w = (w, map fst . hskWordLevel . source $ w)

showHskLevel :: Simpl -> IO ()
showHskLevel = putStrLn . dumpStr . hskWordLevel

-- | Try to find split a word into smaller words which are in HSK levels
hskSplits :: Simpl -> [(HSKLevel, Word)]
hskSplits = nub . concatMap hskWordLevel . nub . concat . splits

showHskSplits :: Simpl -> IO ()
showHskSplits w = do
    putStrLn $ "* HSK splits for " ++ w
    putStrLn $ dumpStr (hskSplits w)

searchHsk :: String -> [(HSKLevel, Word)]
searchHsk str = filter matches
              $ concat
              $ Map.elems hskWordIndex
  where
    matches :: (HSKLevel, Word) -> Bool
    matches (_, Word{..}) = or [
        str `isInfixOf` pinyin
      , str `isInfixOf` source
      , str `isInfixOf` target
      ]

showSearchHsk :: String -> IO ()
showSearchHsk = putStrLn . dumpStr . searchHsk

{-------------------------------------------------------------------------------
  Automatic ranking of all dialogues

  When we want fully automatic ranking of dialogues, we cannot really classify
  words as 'harmless' or not. Instead, we look at individual characters only.
-------------------------------------------------------------------------------}

data Ranked = Ranked {
      -- | Score
      rankedScore :: Fixed E2

      -- | All characters in the dialogue
      --
      -- This includes all characters in the key vocabulary, and those
      -- characters in the supplementary vocabulary that also appear in the
      -- dialogue.
    , rankedAllChars :: Set Char

      -- | All characters that appears in the level we're studying
    , rankedInLevel :: Set Char

      -- | All characters that appear in lower levels
    , rankedInLower :: Set Char

      -- | Characters in the dialogue that appear in _higher_ HSK levels
    , rankedInHigher :: Set Char

      -- | Characters in the dialogue that don't appear in the HSK at all
    , rankedNotInHSK :: Set Char

      -- | V3id of the lesson
    , rankedId :: V3Id

      -- | The lesson itself
    , rankedLesson :: Lesson
    }
  deriving (Show)

rank :: HSKLevel -> (V3Id, Lesson) -> Ranked
rank l (rankedId, rankedLesson@Lesson{..}) = Ranked {..}
  where
    rankedScore :: Fixed E2
    rankedScore = if numInLevel == 0
                    then 0
                    else fromIntegral numInLevel
                       / fromIntegral (numAllChars - numInLower)

    -- Classify all characters
    --
    -- We should have:
    --
    -- * rankedAllChars == unions [rankedRelevant, rankedInLower,
    rankedAllChars, rankedInLevel, rankedInLower :: Set Char
    rankedAllChars = Set.fromList $ flatten key ++ flatten supDialog
    rankedInLevel  = Set.filter isThisLevel   rankedAllChars
    rankedInLower  = Set.filter isLowerLevel  rankedAllChars
    rankedInHigher = Set.filter isHigherLevel rankedAllChars
    rankedNotInHSK = Set.filter isNotInHSK    rankedAllChars

    numAllChars, numInLevel, numInLower :: Int
    numAllChars = Set.size rankedAllChars
    numInLevel  = Set.size rankedInLevel
    numInLower  = Set.size rankedInLower

    isThisLevel, isLowerLevel :: Char -> Bool
    isThisLevel   = maybe False (\l' -> l' == l) . hskCharLevel
    isLowerLevel  = maybe False (\l' -> l' <  l) . hskCharLevel
    isHigherLevel = maybe False (\l' -> l' >  l) . hskCharLevel
    isNotInHSK    = isNothing . hskCharLevel

    flatten :: [Word] -> [Char]
    flatten = concatMap source

-- | Rank all dialogues
rankAll :: HSKLevel -> Map V3Id Lesson -> [Ranked]
rankAll l = map (rank l) . Map.toList

-- | Cumulative information about the dialogue ranking
data RankedCumulative = RankedCumulative {
      -- | The actual ranked dialogue
      ranked :: Ranked

      -- | The set of characters that was covered by this dialogue
      -- (that had not been covered previously)
    , rankedNew :: Set Char
    }
  deriving (Show)

rankedSortKey :: SortKey Ranked
rankedSortKey = SortKey $ \Ranked{..} -> (
      level rankedLesson                 -- prefer lower level
    , negate rankedScore                 -- prefer higher score
    , newerFirst $ released rankedLesson -- prefer newer lessons
    )
  where
    newerFirst :: LocalTime -> Rational
    newerFirst = negate . getModJulianDate . localTimeToUT1 0

-- | Construct cumulative ranking
--
-- Also returns the characters that we not covered.
rankCumulative :: HSKLevel                   -- ^ Level we want to study
               -> (RankedCumulative -> Bool) -- ^ Possibility to filter out some lessons
               -> Int                        -- ^ Maximum number of dialogues
               -> Map V3Id Lesson            -- ^ Available lessons
               -> (Set Char, [RankedCumulative])
rankCumulative l p maxNum =
      makeCumulative maxNum (fromJust $ lookup l hskAllChars)
    . sortByKey rankedSortKey
    . rankAll l
  where
    -- We leave the original sorting in place as much as possible
    makeCumulative :: Int -> Set Char -> [Ranked] -> (Set Char, [RankedCumulative])
    makeCumulative 0 todo _  = (todo, []) -- Maximum number of dialogues reached
    makeCumulative _ todo [] = (todo, [])
    makeCumulative n todo (r:rs)
       -- Nothing left to cover
       | Set.null todo =
           (todo, [])
       -- This dialogue does not cover /any/ characters in the chosen HSK level
       -- We don't assume that the list is ordered by score, so we keep looking
       | rankedScore r == 0 =
           makeCumulative n todo rs
       -- Nothing at this level satisfies the predicate
       | Nothing <- candidate =
           makeCumulative n todo rsOther
       -- Even the best candidate with this score covers no new characters
       | Just (best, _) <- candidate, Set.null (rankedNew best) =
           makeCumulative n todo rsOther
       -- Otherwise, use the best candidate
       | Just (best, rest) <- candidate =
           second (best :) $ makeCumulative
                               (n - 1)
                               (todo Set.\\ rankedNew best)
                               (rest ++ rsOther)
      where
        (rsSame, rsOther) = span (sameScore r) rs
        candidates = map (\r' -> (r', cumulative todo r')) (r:rsSame)
        candidate  = pickCandidate candidates

    -- | Pick a candidate with the maximum score, leaving the remaining lessons
    -- in the original order
    pickCandidate :: [(Ranked, RankedCumulative)] -> Maybe (RankedCumulative, [Ranked])
    pickCandidate =
          fmap aux
        . pickBest (p . snd) (comparing $ Set.size . rankedNew . snd)
      where
        aux :: ((Ranked, RankedCumulative), [(Ranked, RankedCumulative)])
            -> (RankedCumulative, [Ranked])
        aux ((_, best), rest) = (best, map fst rest)

    cumulative :: Set Char -> Ranked -> RankedCumulative
    cumulative todo ranked@Ranked{..} = RankedCumulative {..}
      where
        rankedNew = Set.intersection rankedInLevel todo

    -- We can use straight equality here since we use fixed precision
    sameScore :: Ranked -> Ranked -> Bool
    sameScore r1 r2 = rankedScore r1 == rankedScore r2

getRanking :: HSKLevel
           -> (RankedCumulative -> Bool)
           -> Int
           -> IO (Set Char, [RankedCumulative])
getRanking l p n = aux <$> getState
  where
    aux :: State -> (Set Char, [RankedCumulative])
    aux = rankCumulative l p n . analysisAllLessons . stateStatic

showRanking :: HSKLevel -> (RankedCumulative -> Bool) -> Int -> IO ()
showRanking l p n = do
    (notCovered, ranked) <- getRanking l p n
    mapM_ (putStrLn . pretty') ranked
    putStrLn $ concat [
        "("
      , show (length ranked) ++ " dialogues, "
      , show (Set.size notCovered) ++ " not covered: " ++ Set.toList notCovered
      , ")"
      ]

rcMaxLevel :: Level -> RankedCumulative -> Bool
rcMaxLevel l r = level (rankedLesson (ranked r)) <= l

rcMinNew :: Int -> RankedCumulative -> Bool
rcMinNew n r = Set.size (rankedNew r) >= n

rcMaxNotInHSK :: Int -> RankedCumulative -> Bool
rcMaxNotInHSK n r = Set.size (rankedNotInHSK (ranked r)) <= n

{-------------------------------------------------------------------------------
  "Harmless" words are irrelevant words that don't really matter; for instance,
  a lesson might have 十一 in the vocab which isn't listed explicitly in any
  HSK, but shouldn't really count towards the number of irrelevant words in
  that lesson.
-------------------------------------------------------------------------------}

-- | Save the list of harmless words
--
-- We store the list of harmless words separately so we can load it independent
-- of the rest of the state
saveHarmless :: FilePath -> IO ()
saveHarmless fp = encodeFile fp =<< readIORef globalHarmless

-- | Load a list of harmless words
openHarmless :: FilePath -> IO ()
openHarmless fp = writeIORef globalHarmless =<< decodeFile fp

-- | Show current words marked as harmless
showHarmless :: IO ()
showHarmless = putStrLn . format . Set.toList =<< readIORef globalHarmless
  where
    format = intercalate ","

-- | Mark a word as harmless
markHarmless :: Simpl -> IO ()
markHarmless w = modifyIORef globalHarmless $ Set.insert w

globalHarmless :: IORef (Set Simpl)
{-# NOINLINE globalHarmless #-}
globalHarmless = unsafePerformIO $ newIORef Set.empty

-- | Given a lesson, analyze which irrelevant words may be harmless
--
-- This can be applied to relevant still-available lessons as well as lessons
-- we already picked (removing harmless irrelevant words from already picked
-- lessons will improve the statistics).
analyzeIrrelevant :: V3Id -> IO ()
analyzeIrrelevant lessonId = do
    st <- getState
    (AnalysisStatic{..}, AnalysisDynamic{..}) <- readIORef globalAnalysisState
    let relevant = case lookup lessonId analysisPicked of
                     Just rl -> rl
                     Nothing -> analysisAvailable  `mapAt` lessonId
        summary  = summarise st (lessonId, relevant)
    forM_ (lessonHarmful summary) $ \(word, levels) -> do
      putStrLn $ "* " ++ source word ++ " " ++ show levels
      let split     = hskSplits (source word)
          covered   = concatMap (source . snd) split
          uncovered = filter (`notElem` covered) (source word)
      putStrLn $ "Splits: " ++ showMatches split
      unless (null uncovered) $ do
        putStrLn $ "Uncovered: "
        forM_ uncovered $ \char -> do
          let searched = searchHsk [char]
          putStrLn $ [char] ++ ": " ++ showMatches searched
      putStrLn ""
  where
    showMatches :: [(HSKLevel, Word)] -> String
    showMatches = intercalate "," . map compact . sortBy (comparing fst)

    compact :: (HSKLevel, Word) -> String
    compact (level, word) = source word ++ "(" ++ show level ++ ")"

{-------------------------------------------------------------------------------
  Local optimization

  Consider each already picked lesson and see if it can be replaced by another,
  without replacing anything else. This is useful for instance when updating a
  list when new dialogues have been published.
-------------------------------------------------------------------------------}

type Improvement = State                                -- ^ Currnet state
                -> (V3Id, RelevantLesson)               -- ^ Current choice
                -> [(V3Id, RelevantLesson)]             -- ^ Available choices
                -> Either String (V3Id, RelevantLesson) -- ^ New choice, if any

improveLocal :: Handle -> Improvement -> IO ()
improveLocal h f = updateDynamic aux >>= mapM_ (hPutStr h)
  where
    aux :: State -> AnalysisDynamic -> (AnalysisDynamic, [String])
    aux st@State{..} AnalysisDynamic{..} = ( AnalysisDynamic{
            analysisPicked    = newPicked
          , analysisAvailable = cullRelevant newCovered analysisAvailable
          , analysisTodo      = filter
                                  (\w -> source w `Set.notMember` newCovered)
                                  analysisTodo
          }
        , msgs
        )
      where
        AnalysisStatic{..} = stateStatic

        newPicked :: [(V3Id, RelevantLesson)]
        msgs      :: [String]
        (newPicked, msgs) = unzip $ map tryPickNew analysisPicked

        newCovered :: Set Simpl
        newCovered = Set.fromList $ map source $ concatMap (rel . snd) newPicked

        tryPickNew :: (V3Id, RelevantLesson) -> ((V3Id, RelevantLesson), String)
        tryPickNew (oldId, old) =
            case f' (oldId, old) of
              Left  msg            -> ((oldId, old), msg)
              Right (_newId, _new) -> undefined -- TODO

        f' :: (V3Id, RelevantLesson) -> Either String (V3Id, RelevantLesson)
        f' (oldId, old) =
            f st (oldId, old) (Map.toList $ Map.delete oldId candidates)
          where
            oldRel     = Set.fromList $ map source (rel old)
            allCovered = \newRel -> all (\w -> w `elem` newRel) (rel old)
            candidates = Map.mapMaybe
                           (mkRelevant allCovered oldRel)
                           analysisAllLessons

-- | For debugging: never suggest improvements, just show matching candidates
showCandidates :: LessonPredicate -> Improvement
showCandidates _ st (oldId, old) _ | null (rel old) = Left $ unlines [
      "### Warning: empty relevant set in " ++ pretty' (summarise st (oldId, old))
    ]
showCandidates p st old candidates =
    if null filtered
      then Left []
      else Left $ unlines [
          "### Improving: " ++ pretty' (summarise st old)
        , "Candidates:"
        , concatMap (\c -> "* " ++ pretty' c ++ "\n") filtered
        ]
  where
    summarized = map (summarise st) candidates
    filtered   = filter p summarized

{-------------------------------------------------------------------------------
  Export
-------------------------------------------------------------------------------}

-- | Info we need to export a lesson
data ExportInfo = ExportInfo {
      exportId    :: V3Id   -- ^ ID of the lesson
    , exportTitle :: String -- ^ Lesson title
    , exportLevel :: Level  -- ^ Lesson level
    , exportVocab :: [Word] -- ^ All vocabulary we should study for this lesson
    }

class ToExportInfo a where
  toExportInfo :: a -> ExportInfo

exportPleco :: String -> String -> [ExportInfo] -> IO ()
exportPleco fileName topCat exportInfo = withFile fileName AppendMode $ \h ->
    forM_ exportInfo $ \ExportInfo{..} -> do
      hPutStrLn h $ "//" ++ topCat ++ "/" ++ exportTitle ++ " (" ++ dumpStr exportLevel ++ ")"
      forM_ exportVocab $ hPutStrLn h . source

exportMarkdown :: FilePath -> String -> [ExportInfo] -> IO ()
exportMarkdown fp header exportInfo = withFile fp AppendMode $ \h -> do
    hPutStrLn h $ header
    hPutStrLn h $ replicate (length header) '-'
    forM_ exportInfo $ \ExportInfo{..} -> do
      content :: API.LessonContent <- decodeFile $ "content/" ++ v3IdString exportId
      let url = "https://chinesepod.com/lessons/" ++ API.lessonContentSlug content
      hPutStrLn h $ "* [" ++ exportTitle ++ " (" ++ dumpStr exportLevel ++ ")](" ++ url ++ ")"

downloadAudio :: FilePath -> [ExportInfo] -> IO ()
downloadAudio dest exportInfo = do
    forM_ (zip [1..] exportInfo) $ \(i, ExportInfo{exportId}) -> do
      content :: API.LessonContent <- decodeFile $ "content/" ++ v3IdString exportId
      let slug               = API.lessonContentSlug content
          Just mp3FullLesson = API.lessonContentCdQualityMp3 content
          pref               = printf "%03d" (i :: Int) ++ "-"
          filename           = pref ++ slug ++ ".mp3"
          pathLesson         = dest </> "lesson"   </> filename
          pathDialogue       = dest </> "dialogue" </> filename

      unlessFileExists pathLesson $
        callProcess "curl" [mp3FullLesson, "-o", pathLesson]

      case API.lessonContentDialogueMp3  content of
        Nothing ->
          return () -- QW don't have dialogues
        Just mp3Dialogue -> unlessFileExists pathDialogue $
          callProcess "curl" [mp3Dialogue, "-o", pathDialogue]
  where
    -- Technically, this has a race condition, but who cares
    unlessFileExists :: FilePath -> IO () -> IO ()
    unlessFileExists fp act = do
      exists <- doesFileExist fp
      if exists then putStrLn $ "Skipping " ++ fp
                else act

{-------------------------------------------------------------------------------
  Convenience functions for exporting picked lessons
-------------------------------------------------------------------------------}

instance ToExportInfo (Summary (V3Id, RelevantLesson)) where
  toExportInfo l@LessonSummary{..} = ExportInfo {
        exportId    = lessonId
      , exportTitle = lessonTitle
      , exportLevel = lessonLevel
      , exportVocab = lessonRel ++ map fst (lessonAllIrrel l)
      }

pickedExportInfo :: LessonPredicate -> IO [ExportInfo]
pickedExportInfo p = aux <$> getStateSummary
  where
    aux :: Summary State -> [ExportInfo]
    aux = map toExportInfo
        . sortByKey exportSortKey
        . filter p
        . statePicked

pickedToPleco :: String -> String -> LessonPredicate -> IO ()
pickedToPleco fp topCat p = pickedExportInfo p >>= exportPleco fp topCat

pickedToMarkdown :: FilePath -> String -> LessonPredicate -> IO ()
pickedToMarkdown fp header p = pickedExportInfo p >>= exportMarkdown fp header

downloadPicked :: FilePath -> LessonPredicate -> IO ()
downloadPicked dest p = pickedExportInfo p >>= downloadAudio dest

-- | Sort by level, then by ID
exportSortKey :: SortKey (Summary (V3Id, RelevantLesson))
exportSortKey = SortKey $ \LessonSummary{..} -> (lessonLevel, lessonId)

{-------------------------------------------------------------------------------
  Convenience functions for exporting automatic ranking
-------------------------------------------------------------------------------}

instance ToExportInfo Ranked where
  toExportInfo Ranked{rankedLesson = Lesson{..}, ..} = ExportInfo {
        exportId    = rankedId
      , exportTitle = title
      , exportLevel = level
      , exportVocab = key ++ supDialog
      }

instance ToExportInfo RankedCumulative where
  toExportInfo = toExportInfo . ranked

rankedExportInfo :: HSKLevel -> (RankedCumulative -> Bool) -> Int
                 -> IO [ExportInfo]
rankedExportInfo l p n = (map toExportInfo . snd) <$> getRanking l p n

rankedToPleco :: String -> String
              -> HSKLevel -> (RankedCumulative -> Bool) -> Int
              -> IO ()
rankedToPleco fp topCat l p n =
    rankedExportInfo l p n >>= exportPleco fp topCat

rankedToMarkdown :: FilePath -> String
                 -> HSKLevel -> (RankedCumulative -> Bool) -> Int
                 -> IO ()
rankedToMarkdown fp header l p n =
    rankedExportInfo l p n >>= exportMarkdown fp header

downloadRanked :: FilePath
               -> HSKLevel -> (RankedCumulative -> Bool) -> Int
               -> IO ()
downloadRanked dest l p n =
    rankedExportInfo l p n >>= downloadAudio dest

{-------------------------------------------------------------------------------
  Statistics
-------------------------------------------------------------------------------}

-- | Simple statistics
--
-- The "harmless" words are not inclided in the irrelevant statistics here.
data Stats = Stats {
      statsNumPicked  :: Int
    , statsNumRel     :: Int
    , statsNumIrrel   :: Int
    , statsRelVsIrrel :: Double
    }
  deriving (Generic)

instance PrettyVal Stats

printStats :: IO ()
printStats = printStats' (const True)

printStats' :: LessonPredicate -> IO ()
printStats' p = do
    picked <- filter p . statePicked <$> getStateSummary

    let totalRel, totalIrrel :: Set Simpl
        totalRel    = Set.fromList $ concatMap getRel     picked
        totalIrrel  = Set.fromList $ concatMap getHarmful picked

        statsNumPicked  = length picked
        statsNumRel     = length totalRel
        statsNumIrrel   = length totalIrrel
        statsRelVsIrrel = (fromIntegral statsNumRel / fromIntegral (statsNumRel + statsNumIrrel)) * 100
    putStrLn $ dumpStr Stats{..}
  where
    getRel, getHarmful :: Summary (V3Id, RelevantLesson) -> [Simpl]
    getRel     LessonSummary{..} = map source         lessonRel
    getHarmful LessonSummary{..} = map (source . fst) lessonHarmful

{-------------------------------------------------------------------------------
  Complete analysis state
-------------------------------------------------------------------------------}

data State = State {
      stateStatic   :: AnalysisStatic
    , stateDynamic  :: AnalysisDynamic
    , stateHarmless :: Set Simpl
    , stateFocus    :: Focus
    }

getState :: IO State
getState = uncurry State <$> readIORef globalAnalysisState
                         <*> readIORef globalHarmless
                         <*> readIORef globalFocus

updateDynamic :: (State -> AnalysisDynamic -> (AnalysisDynamic, b)) -> IO b
updateDynamic f = do
    st <- getState
    atomicModifyIORef globalAnalysisState $ \(static, dyn) ->
      first (static, ) $ f st dyn

updateDynamic_ :: (State -> AnalysisDynamic -> AnalysisDynamic) -> IO ()
updateDynamic_ f = updateDynamic $ \st -> (, ()) . f st

{-------------------------------------------------------------------------------
  Summarising
-------------------------------------------------------------------------------}

class Summarise a where
  data Summary a :: *

  -- | Construct a summary, given the curent analysis state
  summarise :: State -> a -> Summary a

instance Summarise (V3Id, RelevantLesson) where
  data Summary (V3Id, RelevantLesson) = LessonSummary {
        lessonId       :: V3Id
      , lessonTitle    :: String
      , lessonIsVideo  :: Bool
      , lessonReleased :: LocalTime
      , lessonLevel    :: Level
      , lessonRel      :: [Word]

        -- | Irrelevant but harmless
      , lessonHarmless :: [(Word, [HSKLevel])]

        -- | Truly irrelevant
      , lessonHarmful  :: [(Word, [HSKLevel])]
      }

  summarise State{..} (lessonId, RelevantLesson{..}) = LessonSummary {
        lessonId       = lessonId
      , lessonTitle    = title
      , lessonIsVideo  = isVideo
      , lessonReleased = released
      , lessonLevel    = level
      , lessonRel      = rel
      , lessonHarmless = harmless
      , lessonHarmful  = harmful
      }
    where
      AnalysisStatic{analysisAllLessons} = stateStatic
      Lesson{..} = analysisAllLessons `mapAt` lessonId

      isHarmless :: Word -> Bool
      isHarmless w = source w `Set.member` stateHarmless

      (harmless, harmful) = partition (isHarmless . fst) irrel

lessonAllIrrel :: Summary (V3Id, RelevantLesson) -> [(Word, [HSKLevel])]
lessonAllIrrel LessonSummary{..} = lessonHarmless ++ lessonHarmful

instance Summarise Simpl where
  data Summary Simpl = WordSummary {
        wordSimpl       :: Simpl
      , wordAppearsIn   :: [V3Id]
      , wordInAvailable :: [Summary (V3Id, RelevantLesson)]
      }

  summarise st@State{..} source = WordSummary{..}
    where
      wordSimpl       = source
      wordAppearsIn   = analysisInverse `mapAt` source
      wordInAvailable = filter ((`elem` wordAppearsIn) . lessonId) available

      AnalysisStatic{..} = stateStatic
      StateSummary{stateInFocusAvailable = available} = summarise st st

instance Summarise V3Id where
  data Summary V3Id =
      -- | We already picked this lesson
      LessonPicked (Summary (V3Id, RelevantLesson))

      -- | This lesson is still available to pick
    | LessonAvailable (Summary (V3Id, RelevantLesson))

      -- | This lesson is neither picked nor available
    | LessonUnavailable Lesson

      -- | This 'V3Id' is unknown
    | LessonUnknownV3Id V3Id

  summarise st@State{..} lessonId
    | Just relevantLesson <- lookup lessonId analysisPicked =
        LessonPicked (summarise st (lessonId, relevantLesson))
    | Just relevantLesson <- Map.lookup lessonId analysisAvailable =
        LessonAvailable (summarise st (lessonId, relevantLesson))
    | Just lesson <- Map.lookup lessonId analysisAllLessons =
        LessonUnavailable lesson
    | otherwise =
        LessonUnknownV3Id lessonId
    where
      AnalysisStatic{analysisAllLessons} = stateStatic
      AnalysisDynamic{analysisPicked, analysisAvailable} = stateDynamic


instance Summarise State where
  data Summary State = StateSummary {
        stateAvailable        :: [Summary (V3Id, RelevantLesson)]
      , stateInFocusAvailable :: [Summary (V3Id, RelevantLesson)]
      , statePicked           :: [Summary (V3Id, RelevantLesson)]
      , stateTodo             :: [Summary Simpl]
      , stateIsZoomedIn       :: Bool
      }

  summarise st State{..} = StateSummary {..}
    where
      stateAvailable        = map (summarise st) (Map.toList analysisAvailable)
      statePicked           = map (summarise st) analysisPicked
      stateTodo             = map (summarise st . source) analysisTodo
      stateInFocusAvailable = filter (focusPredicate stateFocus) stateAvailable
      stateIsZoomedIn       = case stateFocus of
                                FocusAll   -> False
                                _otherwise -> True

      AnalysisDynamic{..} = stateDynamic

{-------------------------------------------------------------------------------
  (Stateful) convenience function to deal with summarising
-------------------------------------------------------------------------------}

showWithState :: Pretty a => PrettyOpts a -> (State -> a) -> IO ()
showWithState opts f = getState >>= putStrLn . pretty opts . f

showWithState' :: Pretty a => (State -> a) -> IO ()
showWithState' f = getState >>= putStrLn . pretty' . f

getStateSummary :: IO (Summary State)
getStateSummary = (\st -> summarise st st) <$> getState

-- | Show state summary
showStateSummary :: IO ()
showStateSummary = showWithState' $ \st -> summarise st st

-- | Show information about a given lesson
showLessonSummary :: V3Id -> IO ()
showLessonSummary lessonId = showWithState' $ \st -> summarise st lessonId

-- | Show information about a given word
showWordSummary :: Simpl -> IO ()
showWordSummary word = showWithState True $ \st -> summarise st word

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

class Pretty a where
  type family PrettyOpts a :: *

  pretty :: PrettyOpts a -> a -> String
  defaultPrettyOpts :: a -> PrettyOpts a

pretty' :: Pretty a => a -> String
pretty' a = pretty (defaultPrettyOpts a) a

instance Pretty (Summary (V3Id, RelevantLesson)) where
  type PrettyOpts (Summary (V3Id, RelevantLesson)) = Bool

  defaultPrettyOpts _ = False

  pretty includeHarmless LessonSummary{..} = concat [
        v3IdString lessonId
      , " ("
      , lessonTitle
      , ", "
      , show $ lessonLevel
      , ", "
      , formatTime defaultTimeLocale "%F" lessonReleased
      , if lessonIsVideo then ", video" else ""
      , "): "
      , intercalate "," $ map source lessonRel
      , " vs "
      , intercalate "," $ map sourceWithLevel lessonHarmful
      , if includeHarmless
          then "/" ++ intercalate "," (map sourceWithLevel lessonHarmless)
          else ""
      , " ("
      , show $ length lessonRel
      , " vs "
      , show $ length lessonHarmful
      , if includeHarmless
          then "/" ++ show (length lessonHarmless)
          else ""
      , ")"
      ]
    where
      sourceWithLevel :: (Word, [HSKLevel]) -> String
      sourceWithLevel (Word{..}, levels) = source ++ show levels

instance Pretty (Summary Simpl) where
  -- | Should we summarise the available lessons this word appears in?
  -- (If not, we just show /how many/ available lesson this word appears in.)
  type PrettyOpts (Summary Simpl) = Bool
  defaultPrettyOpts _ = False

  pretty False WordSummary{..} = concat [
        wordSimpl
      , " ("
      , show $ length wordAppearsIn
      , "/"
      , show $ length wordInAvailable
      , ")"
      ]
  pretty True WordSummary{..} = unlines [
        "This word is in the vocab of the following available lessons:"
      , "-------------------------------------------------------------"
      , intercalate "\n"
          $ map (pretty False)
          $ sortByKey countLessonRelIrrel
          $ wordInAvailable
      , ""
      ]

instance Pretty (Summary State) where
  type PrettyOpts (Summary State) = (
        SortKey (Summary (V3Id, RelevantLesson))
      , SortKey (Summary Simpl)
      )

  defaultPrettyOpts _ = (countLessonRelIrrel, countWordAppearsIn)

  pretty (lessonKey, wordKey)
         StateSummary{
             stateInFocusAvailable = available
           , stateIsZoomedIn       = zoomedIn
           , statePicked           = picked
           , stateTodo             = todo
           } = unlines [
        "Available lessons (" ++ show (length available) ++ ")"
          ++ if zoomedIn then " (zoomed in)" else ""
      , "---------------------------------------------------------------"
      , intercalate "\n" $ map (pretty False) $ sortByKey lessonKey $ available
      , ""

      , "Picked lessons (" ++ show (length picked) ++ ")"
      , "---------------------------------------------------------------"
      , intercalate "\n" $ map (pretty False) picked
      , ""

      , "Words left (" ++ show (length todo) ++ ")"
      , "---------------------------------------------------------------"
      , intercalate "," $ map pretty' $ sortByKey wordKey $ todo
      , ""
      ]

instance Pretty (Summary V3Id) where
  type PrettyOpts (Summary V3Id) = ()
  defaultPrettyOpts _ = ()

  pretty () (LessonPicked relevantLesson) = unlines [
        "We picked this lesson:"
      , pretty' relevantLesson
      ]
  pretty () (LessonAvailable relevantLesson) = unlines [
        "This lesson is available:"
      , pretty' relevantLesson
      ]
  pretty () (LessonUnavailable lesson) = unlines [
        "This lesson is unavailable:"
      , dumpStr lesson
      ]
  pretty () (LessonUnknownV3Id lessonId) = unlines [
        "This V3Id is unknown: " ++ v3IdString lessonId
      ]

instance Pretty RankedCumulative where
  type PrettyOpts RankedCumulative = ()
  defaultPrettyOpts _ = ()

  pretty () RankedCumulative{ranked = Ranked{rankedLesson = Lesson{..}, ..}, ..} =
      concat [
          v3IdString rankedId
        , " ("
        , title
        , ", "
        , show level
        , ", "
        , formatTime defaultTimeLocale "%F" released
        , if isVideo then ", video" else ""
        , ") "
        , "score: " ++ show rankedScore
        , " ("
        , show (Set.size rankedNew)      ++ " new, "
        , show (Set.size rankedAllChars) ++ " total, "
        , show (Set.size rankedInLevel)  ++ " in this level, "
        , show (Set.size rankedInLower)  ++ " in lower levels, "
        , show (Set.size rankedInHigher) ++ " in higher levels, "
        , show (Set.size rankedNotInHSK) ++ " not in HSK"
        , ")"
        ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

none :: (a -> Bool) -> [a] -> Bool
none p = not . any p

-- | All different ways to split a list into sublists (without reordering)
splits :: forall a. [a] -> [[[a]]]
splits []     = [[]]
splits [x]    = [[[x]]]
splits (x:xs) = map (\(ys:yss) -> (x:ys):yss) (splits xs)
             ++ map ([x] :)                   (splits xs)

mapAt :: (Ord k, Show k) => Map k a -> k -> a
mapAt mp k = case Map.lookup k mp of
               Just a  -> a
               Nothing -> error $ "Unknown key " ++ show k

data SortKey a = forall b. Ord b => SortKey (a -> b)

instance Contravariant SortKey where
  contramap f (SortKey key) = SortKey (key . f)

sortByKey :: SortKey a -> [a] -> [a]
sortByKey (SortKey key) = sortBy (comparing key)

-- | Pick the largest element, returning the rest of the list in original order
--
-- If there are multiple elements that are compare EQ, we return the first.
pickBest :: forall a.
            (a -> Bool)          -- ^ Suitable element?
         -> (a -> a -> Ordering) -- ^ Compare elements
         -> [a]                  -- ^ Candidates
         -> Maybe (a, [a])
pickBest p f = \xs ->
    let xss = filter (p . picked) $ reverse $ pickAll xs
    in if null xss
      then Nothing
      else Just $ aux (maximumBy (f `on` picked) xss)
  where
    aux :: ([a], a, [a]) -> (a, [a])
    aux (xs, y, zs) = (y, xs ++ zs)

    picked :: ([a], a, [a]) -> a
    picked (_, y, _) = y

-- | All different ways to pick an element from a list
pickAll :: [a] -> [([a], a, [a])]
pickAll = mapMaybe aux . split2
  where
    aux :: ([a], [a]) -> Maybe ([a], a, [a])
    aux (_  , []  ) = Nothing
    aux (xs , y:ys) = Just (xs, y, ys)

-- | All different ways to split a list into 2 lists (without reordering)
split2 :: [a] -> [([a], [a])]
split2 []     = [([], [])]
split2 (x:xs) = ([], (x:xs)) : map (first (x:)) (split2 xs)
