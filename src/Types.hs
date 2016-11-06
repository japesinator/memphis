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
  , notes         :: Maybe String
} deriving Generic

instance Show Irregularity where
  show (Irregularity burning stinging sharpPain dullPain rash numbness tightness inflammation itching discharge discoloration notes) =
       f burning "burning"
    ++ f stinging "stinging"
    ++ f sharpPain "sharp pain"
    ++ f dullPain "dull pain"
    ++ f rash "rash"
    ++ f numbness "numbness"
    ++ f tightness "tightness"
    ++ f inflammation "inflammation"
    ++ f itching "itching"
    ++ f discharge "discharge"
    ++ f discoloration "discoloration"
    ++ case notes of
            Nothing  -> ""
            (Just a) -> "  Also note: " ++ a ++ "\n"
      where
        f b s = if b then "  Patient complains of " ++ s ++ "\n" else ""

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
  , head        :: Maybe Irregularity
  , throat      :: Maybe Irregularity
  , back        :: Maybe Irregularity
  , stomach     :: Maybe Irregularity
  , chest       :: Maybe Irregularity
  , arms        :: Maybe Irregularity
  , legs        :: Maybe Irregularity
  , joints      :: Maybe Irregularity
  , genitals    :: Maybe Irregularity
  , ears        :: Maybe Irregularity
  , extremities :: Maybe Irregularity

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
  , runnyNose  :: Bool
  , mucusColor :: Bool
  , soreThroat :: Bool
  , bodyAche   :: Bool
  , lymphNodes :: Bool
  , moodSwings :: Bool
  , appetite   :: Bool
  , weightGain :: Bool
  , sweating :: Bool

  -- Because we miss things
  , qualityOfLife :: Int
  , notes         :: Maybe String
} deriving Generic

instance Show PInfo where
  show (PInfo height weight age gender trans 
              hospitalizedRecently surgery medication chronicIllness
              allergies asthma heartDisease firstPeriod lastPeriod
              head throat back stomach chest arms legs joints genitals ears extremities
              coughing sneezing nausea dizziness vision hearing vomiting breathing congestion urination defecation fever shaking chills heat exhaustion insomnia bloating runnyNose mucusColor soreThroat bodyAche lymphNodes moodSwings appetite weightGain sweating qol notes) = f height "Height"
                                  ++ f weight "Weight"
                                  ++ f age "Age"
                                  ++ f gender "Gender"
                                  ++ if trans then "Patient is transgender\n" else ""
                                  ++ g hospitalizedRecently "Patient was hospitalized recently"
                                  ++ g surgery "Patient had recent surgery"
                                  ++ g medication "Patient takes medication"
                                  ++ g chronicIllness "Patient has history of chronic illness"
                                  ++ h head "head"
                                  ++ h throat "throat"
                                  ++ h back "back"
                                  ++ h stomach "stomach"
                                  ++ h chest "chest"
                                  ++ h arms "arms"
                                  ++ h legs "legs"
                                  ++ h joints "joints"
                                  ++ h genitals "genitals"
                                  ++ h ears "ears"
                                  ++ h extremities "extremities"
                                  ++ i coughing "coughing"
                                  ++ i sneezing "sneezing"
                                  ++ i nausea "nausea"
                                  ++ i dizziness "dizziness"
                                  ++ i vision "vision issues"
                                  ++ i hearing "hearing issues"
                                  ++ i vomiting "vomiting"
                                  ++ i breathing "breathing issues"
                                  ++ i congestion "congestion"
                                  ++ i urination "urination issues"
                                  ++ i defecation "defecation issues"
                                  ++ i fever "fever"
                                  ++ i shaking "shaking"
                                  ++ i chills "chills"
                                  ++ i heat "feeling of heat"
                                  ++ i exhaustion "exhaustion"
                                  ++ i insomnia "insomnia"
                                  ++ i bloating "bloating"
                                  ++ i runnyNose "runny nose"
                                  ++ i mucusColor "abnormal mucus color"
                                  ++ i soreThroat "sore throat"
                                  ++ i bodyAche "body aching"
                                  ++ i lymphNodes "swollen lymph nodes"
                                  ++ i moodSwings "mood swings"
                                  ++ i appetite "change in appetite"
                                  ++ i weightGain "weight gain"
                                  ++ i sweating "excessive sweating"
                                  ++ "Patient rates their quality of life as " ++ show qol ++ "/5\n"
                                  ++ case notes of
                                    Nothing  -> ""
                                    (Just a) -> "  Also note: " ++ a
                                  ++ "\n" where
                                    f a s = s ++ ": " ++ show a ++ "\n"
                                    g Nothing _ = ""
                                    g (Just s) t = t ++ ": " ++ s ++ "\n"
                                    h Nothing _ = ""
                                    h (Just i) s = "In " ++ s ++ ":\n" ++ show i
                                    i b s = if b then "Patient reports " ++ s ++ "\n" else ""

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
  , hypothermia    :: Bool
  , heatstroke     :: Bool
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
             :<|> "patient-results" :> Capture "patient" Int                          :> Get  '[JSON] (Maybe String)
             :<|> "doctor-results"  :> Capture "patient" Int                          :> Get  '[JSON] (Maybe DInfo)

type ST = H.Map Int (PInfo, Maybe DInfo)
