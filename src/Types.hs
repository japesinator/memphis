module Types where

import qualified Diagnosis as D

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
  , notes         :: String
} deriving (Generic, Show, Read)

showI (Irregularity burning stinging sharpPain dullPain rash numbness tightness inflammation itching discharge discoloration notes) =
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
    ++ if notes == "" then "" else "* Also note: " ++ notes ++ "\n"
      where
        f b s = if b then "* Patient complains of " ++ s ++ "\n" else ""

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
  , hospitalizedRecently :: String
  , surgery              :: String
  , medication           :: String
  , chronicIllness       :: String

  -- Obvious health stuff
  , allergies    :: String
  , asthma       :: String
  , heartDisease :: String
  , firstPeriod  :: String
  , lastPeriod   :: String

  -- Irregularities
  , head        :: Irregularity
  , throat      :: Irregularity
  , back        :: Irregularity
  , stomach     :: Irregularity
  , chest       :: Irregularity
  , arms        :: Irregularity
  , legs        :: Irregularity
  , joints      :: Irregularity
  , genitals    :: Irregularity
  , ears        :: Irregularity
  , extremities :: Irregularity
  , skin        :: Irregularity

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
  , sweating   :: Bool

  -- Because we miss things
  , qualityOfLife :: Int
  , notes         :: String
} deriving (Generic, Show, Read)

showP :: PInfo -> String
showP (PInfo height weight age gender trans 
              hospitalizedRecently surgery medication chronicIllness
              allergies asthma heartDisease firstPeriod lastPeriod
              head throat back stomach chest arms legs joints genitals ears extremities skin
              coughing sneezing nausea dizziness vision hearing vomiting breathing congestion urination defecation fever shaking chills heat exhaustion insomnia bloating runnyNose mucusColor soreThroat bodyAche lymphNodes moodSwings appetite weightGain sweating qol notes) = f height "Height"
                                  ++ f weight "Weight"
                                  ++ f age "Age"
                                  ++ "Gender: " ++ gender ++ "\n"
                                  ++ if trans then "Patient is transgender\n" else ""
                                  ++ g hospitalizedRecently "Patient was hospitalized recently"
                                  ++ g surgery "Patient had recent surgery"
                                  ++ g medication "Patient takes medication"
                                  ++ g chronicIllness "Patient has history of chronic illness"
                                  ++ g allergies "Patient has allergies"
                                  ++ g asthma "Patient has asthma"
                                  ++ g heartDisease "Patient has heart disease"
                                  ++ g firstPeriod "Patient had first period"
                                  ++ g lastPeriod "Patient had last period"
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
                                  ++ h skin "skin"
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
                                  ++ if notes == "" then "" else "Also note: " ++ notes
                                  ++ "\n" where
                                    f a s = s ++ ": " ++ show a ++ "\n"
                                    g s t = if s == "" then "" else t ++ ": " ++ s ++ "\n"
                                    h i s = if showI i == "" then "" else "In " ++ s ++ ":\n" ++ showI i
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
  , menopause      :: Bool
  , yeastInfection :: Bool
  , heatExhaustion :: Bool
  , notes          :: String
} deriving (Generic, Show, Read)

instance FromJSON DInfo
instance ToJSON   DInfo

showD1 :: DInfo -> String
showD1 (DInfo commonCold sinusInfection earInfection pinkeye strepThroat uti pregnancy herpes flu shingles chickenPox mono pneumonia menopause yeastInfection heatExhaustion notes) =
    f commonCold D.commonCold
 ++ f sinusInfection D.sinusInfection
 ++ f earInfection   D.earInfection
 ++ f pinkeye        D.pinkeye
 ++ f strepThroat    D.strepThroat
 ++ f uti            D.uti
 ++ f pregnancy      D.pregnancy
 ++ f herpes         D.herpes
 ++ f flu            D.flu
 ++ f shingles       D.shingles
 ++ f chickenPox     D.chickenPox
 ++ f mono           D.mono
 ++ f pneumonia      D.pneumonia
 ++ f menopause      D.menopause
 ++ f yeastInfection D.yeastInfection
 ++ f heatExhaustion D.heatExhaustion
 ++ "The doctor also noted: " ++ notes ++ "\n" where
   f b d = if b then D.showD d else ""

showD2 :: DInfo -> String
showD2 (DInfo commonCold sinusInfection earInfection pinkeye strepThroat uti pregnancy herpes flu shingles chickenPox mono pneumonia menopause yeastInfection heatExhaustion notes) =
    f commonCold D.commonCold
 ++ f sinusInfection D.sinusInfection
 ++ f earInfection   D.earInfection
 ++ f pinkeye        D.pinkeye
 ++ f strepThroat    D.strepThroat
 ++ f uti            D.uti
 ++ f pregnancy      D.pregnancy
 ++ f herpes         D.herpes
 ++ f flu            D.flu
 ++ f shingles       D.shingles
 ++ f chickenPox     D.chickenPox
 ++ f mono           D.mono
 ++ f pneumonia      D.pneumonia
 ++ f menopause      D.menopause
 ++ f yeastInfection D.yeastInfection
 ++ f heatExhaustion D.heatExhaustion
 ++ "The doctor also noted: " ++ notes ++ "\n" where
   f b d = if b then "The doctor suspected " ++ D.name d ++ "\n" else ""

type PatientAPI = "patient-entry"   :> Capture "patient" Int :> ReqBody '[JSON] PInfo :> Post '[JSON] Bool
             :<|> "doctor-entry"    :> Capture "patient" Int :> ReqBody '[JSON] DInfo :> Post '[JSON] Bool
             :<|> "patient-results" :> Capture "patient" Int                          :> Get  '[JSON] (Maybe String)
             :<|> "doctor-results"  :> Capture "patient" Int                          :> Get  '[JSON] (Maybe DInfo)

type ST = H.Map Int (PInfo, Maybe DInfo)
