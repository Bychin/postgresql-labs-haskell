module Timer where

import Data.Time.Clock.POSIX

timeMs :: IO Integer
timeMs = getCurrentTime >>= pure . (1000 *) . utcTimeToPOSIXSeconds >>= pure . round
