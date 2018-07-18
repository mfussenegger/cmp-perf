
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.DeepSeq                  (force)
import           Control.Exception                (evaluate)
import           Control.Monad                    (replicateM)
import           Data.List                        (minimumBy)
import           Data.Ord                         (comparing)
import qualified Data.Text                        as T
import qualified Data.Vector.Unboxed              as V
import           GHC.Base                         (String)
import           Options.Applicative              (Parser (..), auto,
                                                   execParser, fullDesc, help,
                                                   helper, info, long, metavar,
                                                   option, progDesc,
                                                   showDefault, strOption,
                                                   value, (<**>))
import qualified Statistics.Distribution.StudentT as S
import qualified Statistics.Function              as S
import qualified Statistics.Sample                as S
import qualified Statistics.Test.StudentT         as S
import qualified Statistics.Test.Types            as ST
import           System.IO                        (hGetContents)
import           System.Process                   (CreateProcess (..),
                                                   StdStream (..), shell,
                                                   waitForProcess,
                                                   withCreateProcess)


type Samples = V.Vector Double


data Args = Args
  { old   :: T.Text
  , new   :: T.Text
  , forks :: Int
  }


data Diff = Diff
  { meanPct :: Double
  , oldMean :: Double
  , oldMin  :: Double
  , oldMax  :: Double
  , newMin  :: Double
  , newMax  :: Double
  , newMean :: Double
  , test    :: Maybe (ST.Test S.StudentT) }
  deriving (Show)


args :: Parser Args
args = Args
    <$> strOption
        ( long "old"
        <> metavar "OLD"
        <> help "Command that returns samples for \"old\"" )
    <*> strOption
        ( long "new"
        <> metavar "NEW"
        <> help "Command that returns samples for \"new\"" )
    <*> option auto
        ( long "forks"
        <> help "How often the commands will be invoked to gather samples"
        <> showDefault
        <> value 1
        <> metavar "FORKS" )


execProc :: String -> IO Samples
execProc cmd =
  withCreateProcess createProc $ \_ (Just out) _ ph -> do
    samples <- textToSamples <$> hGetContents out
    evaluate $ force samples
    ex <- waitForProcess ph
    pure samples
  where
    textToSamples = V.fromList . map read . lines
    createProc = (shell cmd) { std_out = CreatePipe }


calcDiff :: Samples -> Samples -> Diff
calcDiff oldSamples newSamples = Diff{..}
  where
    (oldMin, oldMax) = S.minMax oldSamples
    (newMin, newMax) = S.minMax newSamples
    oldMean = S.mean oldSamples
    newMean = S.mean newSamples
    oldStdDev = S.stdDev oldSamples
    newStdDev = S.stdDev newSamples
    meanPct = percDiff oldMean newMean
    test = S.welchTTest S.BGreater oldSamples newSamples


percDiff :: Double -> Double -> Double
percDiff x y = (abs (x - y) / ((x + y) / 2)) * 100


cmpPerf :: Args -> IO ()
cmpPerf (Args old new forks) = do
  listOfOldSamples <- replicateM forks $ do
    putStrLn ("Gathering metrics for: " <> T.unpack old)
    execProc $ T.unpack old
  listOfNewSamples <- replicateM forks $ do
    putStrLn ("Gathering metrics for: " <> T.unpack new)
    execProc $ T.unpack new
  let
    bestOldSamples = minimumBy (comparing S.mean) listOfOldSamples
    bestNewSamples = minimumBy (comparing S.mean) listOfNewSamples
  print $ calcDiff bestOldSamples bestNewSamples


main :: IO ()
main = cmpPerf =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
      <> progDesc "Compare the (runtime) values returned by two different programs" )
