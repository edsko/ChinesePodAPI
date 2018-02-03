-- | Analyze the vocab lists and try to match them against dialogues
--
-- This module is intended for use in @ghci@.
module Servant.ChinesePod.Analysis (
    -- * Initialization
    initState
  , resetVocab
    -- * Saving and loading
    -- ** Analysis state
  , save
  , open
  , openV1
    -- ** List of harmless words
  , saveHarmless
  , openHarmless
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
    -- * Dealing with harmless words
  , showHarmless
  , markHarmless
  , analyzeIrrelevant
    -- * Local optimization
  , Improvement
  , improveLocal
  , showCandidates
    -- * Export
  , exportPleco
  , exportPleco'
  , exportMarkdown
  , exportMarkdown'
  , downloadAudio
  , downloadAudio'
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
    -- Re-exports
  , module State
  , module Vocab
  , module HSK
  , module Data.IORef
  ) where

import Prelude hiding (Word, words)
import Control.Monad
import Data.Bifunctor (first)
import Data.Binary (encodeFile, decodeFile)
import Data.Functor.Contravariant
import Data.IORef
import Data.List (intercalate, sortBy, partition, nub, isInfixOf)
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
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
import Servant.ChinesePod.HSK.HSK2012                 as HSK
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

hskLevel :: Simpl -> [(HSKLevel, Word)]
hskLevel simpl = Map.findWithDefault [] simpl hskIndex

hskIndex :: Map Simpl [(HSKLevel, Word)]
hskIndex = Map.unionsWith (++) [
      indexFor 1 hsk1
    , indexFor 2 hsk2
    , indexFor 3 hsk3
    , indexFor 4 hsk4
    , indexFor 5 hsk5
    , indexFor 6 hsk6
    ]
  where
    indexFor :: HSKLevel -> [Word] -> Map Simpl [(HSKLevel, Word)]
    indexFor level = Map.fromList . map go
      where
        go :: Word -> (Simpl, [(HSKLevel, Word)])
        go word@Word{..} = (source, [(level, word)])

withLevel :: Word -> (Word, [HSKLevel])
withLevel w = (w, map fst . hskLevel . source $ w)

showHskLevel :: Simpl -> IO ()
showHskLevel = putStrLn . dumpStr . hskLevel

-- | Try to find split a word into smaller words which are in HSK levels
hskSplits :: Simpl -> [(HSKLevel, Word)]
hskSplits = nub . concatMap hskLevel . nub . concat . splits

showHskSplits :: Simpl -> IO ()
showHskSplits w = do
    putStrLn $ "* HSK splits for " ++ w
    putStrLn $ dumpStr (hskSplits w)

searchHsk :: String -> [(HSKLevel, Word)]
searchHsk str = filter matches
              $ concat
              $ Map.elems hskIndex
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

exportPleco :: String -> String -> IO ()
exportPleco = exportPleco' (const True)

exportPleco' :: LessonPredicate -> String -> String -> IO ()
exportPleco' p fileName topCategory = do
    picked <- filter p . statePicked <$> getStateSummary
    withFile fileName AppendMode $ \h ->
      forM_ (sortBy (comparing exportSortKey) picked) $ \l@LessonSummary{..} -> do
        hPutStrLn h $ "//" ++ topCategory ++ "/" ++ lessonTitle ++ " (" ++ dumpStr lessonLevel ++ ")"
        forM_ (lessonRel ++ map fst (lessonAllIrrel l)) $ hPutStrLn h . source

exportMarkdown :: String -> String -> IO ()
exportMarkdown = exportMarkdown' (const True)

exportMarkdown' :: LessonPredicate -> String -> String -> IO ()
exportMarkdown' p fileName header = do
    picked <- filter p . statePicked <$> getStateSummary
    withFile fileName AppendMode $ \h -> do
      hPutStrLn h $ header
      hPutStrLn h $ replicate (length header) '-'
      forM_ (sortBy (comparing exportSortKey) picked) $ \LessonSummary{..} -> do
        content :: API.LessonContent <- decodeFile $ "content/" ++ v3IdString lessonId
        let url = "https://chinesepod.com/lessons/" ++ API.lessonContentSlug content
        hPutStrLn h $ "* [" ++ lessonTitle ++ " (" ++ dumpStr lessonLevel ++ ")](" ++ url ++ ")"

downloadAudio :: FilePath -> IO ()
downloadAudio = downloadAudio' (const True)

downloadAudio' :: LessonPredicate -> FilePath -> IO ()
downloadAudio' p dest = do
    picked <- filter p . statePicked <$> getStateSummary
    forM_ (zip [1..] (sortBy (comparing exportSortKey) picked)) $ \(i, LessonSummary{..}) -> do
      content :: API.LessonContent <- decodeFile $ "content/" ++ v3IdString lessonId
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

-- | Sort by level, then by ID
exportSortKey :: Summary (V3Id, RelevantLesson) -> (Level, V3Id)
exportSortKey LessonSummary{..} = (lessonLevel, lessonId)

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
