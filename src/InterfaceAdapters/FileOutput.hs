module InterfaceAdapters.FileOutput where

import Control.Concurrent.STM
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Time.Clock
import Data.Time.Format
import GHC.IO.Handle (Handle)
import Polysemy
import Polysemy.Input
import System.FilePath
import System.IO (IOMode (..), openBinaryFile)

fileDateSuffixFormat :: String
fileDateSuffixFormat = "%FT%H-%M-%S_%q"

data RotationFrequency = Day | Hour | Minute
data RotationBucket = RotationBucket
    { freq :: RotationFrequency
    , _id :: UTCTime
    }

toMod :: RotationFrequency -> Integer
toMod Day = 60 * 60 * 24
toMod Hour = 60 * 60
toMod Minute = 60

projectDate :: RotationFrequency -> UTCTime -> UTCTime
projectDate Day (UTCTime d _) = UTCTime d 0
projectDate x (UTCTime d s) = UTCTime d projected
  where
    secs = floor s
    projected = fromInteger $ secs - (secs `mod` toMod x)

toRotationBucket :: RotationFrequency -> UTCTime -> RotationBucket
toRotationBucket f d = RotationBucket f $ projectDate f d

buildFileName ::
    -- | Output Dir
    String ->
    -- | file prefix
    String ->
    -- | ext
    String ->
    -- | timestamp
    UTCTime ->
    FilePath
buildFileName dir file ext date =
    let suffix = formatTime defaultTimeLocale fileDateSuffixFormat date
     in dir </> (file ++ suffix) <.> ext

outputFileHandle :: String -> String -> String -> IO Handle
outputFileHandle dir file ext = do
    date <- getCurrentTime
    let destFile = buildFileName dir file ext date
    openBinaryFile destFile WriteMode

getOutputFileHandle ::
    (Member (Embed IO) r) =>
    -- | Output Directory
    String ->
    -- | Output File Prefix
    String ->
    -- | File ext
    String ->
    Sem r Handle
getOutputFileHandle outputDirectory outputFilePrefix fileExt = do
    date <- embed getCurrentTime
    let fileName = outputFilePrefix ++ formatTime defaultTimeLocale fileDateSuffixFormat date ++ fileExt
    embed $ openBinaryFile (outputDirectory ++ "/" ++ fileName) WriteMode

writeToFile :: (Member (Embed IO) r, Member (Input Handle) r) => BS.ByteString -> Sem r ()
writeToFile msg = do
    h <- input
    embed $ BS.hPutStrLn h msg

withHandle :: (Member (Embed STM) r, Member (Input (TMVar Handle)) r) => (Handle -> Sem r a) -> Sem r a
withHandle action = do
    handleVar <- input
    h <- embed $ takeTMVar handleVar
    res <- action h
    embed $ putTMVar handleVar h
    pure res