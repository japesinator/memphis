module Main where

import Lib
import Types
import Form
import qualified Data.HashMap as H
import Data.Proxy
import Network.Wai.Handler.Warp
import Servant

main :: IO ()
main = putsite --run 8081 . serve (Proxy :: Proxy PatientAPI) . eitherServer $ H.fromList []
