module Types where

import Data.Aeson
import qualified Data.HashMap as H
import GHC.Generics
import Servant

data Irregularity = Irregularity {
    burning       :: Bool
  , stinging      :: Bool
  , sharpPain     :: Bool
  , dullPain      :: Bool
  , rash          :: Bool
  , numbness      :: Bool
  , tightness     :: Bool
  , inflammation  :: Bool
  , itching       :: Bool
  , discharge     :: Bool
  , discoloration :: Bool
  , other         :: Maybe String
  , notes         :: Maybe String
} deriving Generic

instance FromJSON Irregularity
instance ToJSON   Irregularity

data PInfo = PInfo {
  -- Basic info
    height :: Int
  , weight :: Int
  , age    :: Int
  , gender :: String
  , trans  :: Bool

  -- R u ded
  , hospitalizedRecently :: Maybe String
  , surgery              :: Maybe String
  , medication           :: Maybe String
  , chronicIllness       :: Maybe String

  -- Obvious health stuff
  , allergies    :: Maybe String
  , asthma       :: Maybe String
  , heartDisease :: Maybe String
  , firstPeriod  :: Maybe Int
  , lastPeriod   :: Maybe String

  -- Irregularities
  , head      :: Maybe Irregularity
  , throat    :: Maybe Irregularity
  , upperBack :: Maybe Irregularity
  , lowerBack :: Maybe Irregularity
  , stomach   :: Maybe Irregularity
  , chest     :: Maybe Irregularity
  , arms      :: Maybe Irregularity
  , legs      :: Maybe Irregularity
  , joints    :: Maybe Irregularity
  , genitals  :: Maybe Irregularity
  , ears      :: Maybe Irregularity

  -- Other symptoms
  , coughing   :: Bool
  , sneezing   :: Bool
  , nausea     :: Bool
  , dizziness  :: Bool
  , vision     :: Bool
  , hearing    :: Bool
  , vomiting   :: Bool
  , breathing  :: Bool
  , congestion :: Bool
  , urination  :: Bool
  , defecation :: Bool
  , fever      :: Bool
  , shaking    :: Bool
  , chills     :: Bool
  , heat       :: Bool
  , exhaustion :: Bool
  , insomnia   :: Bool
  , bloating   :: Bool
  , breathing  :: Bool
  , runnyNose  :: Bool
  , mucusColor :: Bool
  , soreThroat :: Bool

  -- Because we miss things
  , qualityOfLife :: Int
  , notes         :: Maybe String
} deriving Generic

instance FromJSON PInfo
instance ToJSON   PInfo

data DInfo = DInfo {
    commonCold     :: Bool
  , sinusInfection :: Bool
  , earInfection   :: Bool
  , pinkeye        :: Bool
  , strepThroat    :: Bool
  , uti            :: Bool
  , pregnancy      :: Bool
  , herpes         :: Bool
  , flu            :: Bool
  , shingles       :: Bool
  , chickenPox     :: Bool
  , mono           :: Bool
  , pneumonia      :: Bool
  , dehydration    :: Bool
  , menopause      :: Bool
  , yeastInfection :: Bool
  , acidReflux     :: Bool
  , sprainedJoint  :: Bool
  , fracturedBone  :: Bool
  , notes          :: String
} deriving Generic

instance FromJSON DInfo
instance ToJSON   DInfo

data Diagnosis = Diagnosis {
    name        :: String
  , description :: String
  , facts       :: [String]
  , symptoms    :: [String]
  , treatment   :: [String]
  , doctorIf    :: [String]
}

type PatientAPI = "patient-entry"   :> Capture "patient" Int :> ReqBody '[JSON] PInfo :> Post '[JSON] Bool
             :<|> "doctor-entry"    :> Capture "patient" Int :> ReqBody '[JSON] DInfo :> Post '[JSON] Bool
             :<|> "patient-results" :> Capture "patient" Int                          :> Get  '[JSON] (Maybe PInfo)
             :<|> "doctor-results"  :> Capture "patient" Int                          :> Get  '[JSON] (Maybe DInfo)

type ST = H.Map Int (PInfo, Maybe DInfo)
