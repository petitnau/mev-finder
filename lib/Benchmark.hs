module Benchmark where
import Test.Tasty.Runners (timed)
import Text.Printf
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.IO (hFlush)
import GHC.IO.Handle.FD (stdout, stderr)

timeIt :: IO a -> IO a
timeIt = timeItNamed "CPU time"

timeItNamed :: String -> IO a -> IO a
timeItNamed name ioa = do
    (t, a) <- timed ioa
    liftIO $ printf (name ++ ": %6.2fs\n") t
    liftIO $ hFlush stdout
    liftIO $ hFlush stderr
    return a
