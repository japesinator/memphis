module Lib where

import Types
import Control.Monad.State.Class
import Control.Monad.State.Lazy
import qualified Data.HashMap as H
import Servant

patientEntry :: MonadState ST m => Int -> PInfo -> m Bool
patientEntry i p = state $ \h -> (True, H.insert i (p, Nothing) h)

doctorEntry :: MonadState ST m => Int -> DInfo -> m Bool
doctorEntry i d = get >>= \m -> case H.lookup i m of Nothing     -> return False
                                                     Just (p, _) -> state $ \h -> (True, H.insert i (p, Just d) h)

patientResults :: MonadState ST m => Int -> m (Maybe String)
patientResults i = return . fmap (show . fst) . H.lookup i =<< get

doctorResults :: MonadState ST m => Int -> m (Maybe DInfo)
doctorResults = flip fmap get . ((snd =<<) .) . H.lookup

eitherServer :: ST -> Server PatientAPI
eitherServer s = enter (Nat $ \x -> fmap fst (runStateT x s)) $ patientEntry :<|> doctorEntry :<|> patientResults :<|> doctorResults
