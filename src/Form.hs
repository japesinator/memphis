{-# LANGUAGE OverloadedStrings #-}

module Form where

import Types
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Happstack.Server
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Digestive.Happstack

irregularityForm :: Monad m => Form String m Irregularity
irregularityForm = Irregularity
  <$> "Burning"                 .: bool Nothing
  <*> "Stinging"                .: bool Nothing
  <*> "Sharp pain"              .: bool Nothing
  <*> "Dull pain"               .: bool Nothing
  <*> "Rash"                    .: bool Nothing
  <*> "Numbness"                .: bool Nothing
  <*> "Tightness"               .: bool Nothing
  <*> "Inflammation"            .: bool Nothing
  <*> "Itching"                 .: bool Nothing
  <*> "Discharge"               .: bool Nothing
  <*> "Discoloration"           .: bool Nothing
  <*> "Other (please describe)" .: string Nothing

pinfoForm :: Monad m => Form String m PInfo
pinfoForm = PInfo
  <$> "Height (inches)"                 .: stringRead "not an integer" Nothing
  <*> "Weight (pounds)"                 .: stringRead "not an integer" Nothing
  <*> "Age"                             .: stringRead "not an integer" Nothing
  <*> "Gender"                          .: string Nothing
  <*> "Do you identify as transgender?" .: bool Nothing

  <*> "Have you been hospitalized recently?"     .: string Nothing
  <*> "Have you had surgery recently?"           .: string Nothing
  <*> "Are you currently taking any medication?" .: string Nothing
  <*> "Do you suffer from chronic illness?"      .: string Nothing

  <*> "Do you have any allergies?"                                          .: string Nothing
  <*> "Do you have asthma?"                                                 .: string Nothing
  <*> "Do you have heart disease?"                                          .: string Nothing
  <*> "If you menstruate, how old were you when you had your first period?" .: string Nothing
  <*> "If you menstruate, when was your last period?"                       .: string Nothing

  <*> "Have you experienced the following in your head?"        .: irregularityForm
  <*> "Have you experienced the following in your throat?"      .: irregularityForm
  <*> "Have you experienced the following on your back?"        .: irregularityForm
  <*> "Have you experienced the following in your stomach?"     .: irregularityForm
  <*> "Have you experienced the following in your chest?"       .: irregularityForm
  <*> "Have you experienced the following in your arms?"        .: irregularityForm
  <*> "Have you experienced the following in your legs?"        .: irregularityForm
  <*> "Have you experienced the following in your joints?"      .: irregularityForm
  <*> "Have you experienced the following in your genitals?"    .: irregularityForm
  <*> "Have you experienced the following in your ears?"        .: irregularityForm
  <*> "Have you experienced the following in your extremities?" .: irregularityForm
  <*> "Have you experienced the following on your skin?"        .: irregularityForm

  <*> "Have you experienced coughing"               .: bool Nothing
  <*> "Have you experienced sneezing"               .: bool Nothing
  <*> "Have you experienced nausea"                 .: bool Nothing
  <*> "Have you experienced dizziness"              .: bool Nothing
  <*> "Have you experienced vision issues"          .: bool Nothing
  <*> "Have you experienced hearing issues"         .: bool Nothing
  <*> "Have you experienced vomiting"               .: bool Nothing
  <*> "Have you experienced breathing issues"       .: bool Nothing
  <*> "Have you experienced congestion"             .: bool Nothing
  <*> "Have you experienced urination issues"       .: bool Nothing
  <*> "Have you experienced defecation issues"      .: bool Nothing
  <*> "Have you experienced fever"                  .: bool Nothing
  <*> "Have you experienced shaking"                .: bool Nothing
  <*> "Have you experienced chills"                 .: bool Nothing
  <*> "Have you experienced heat"                   .: bool Nothing
  <*> "Have you experienced exhaustion"             .: bool Nothing
  <*> "Have you experienced insomnia"               .: bool Nothing
  <*> "Have you experienced bloating"               .: bool Nothing
  <*> "Have you had a runny nose"                   .: bool Nothing
  <*> "Have you had abnormal mucus color"           .: bool Nothing
  <*> "Have you had a sore throat"                  .: bool Nothing
  <*> "Have you experienced body aches"             .: bool Nothing
  <*> "Have you experienced swollen lymph nodes"    .: bool Nothing
  <*> "Have you experienced mood swings"            .: bool Nothing
  <*> "Have you experienced difference in appetite" .: bool Nothing
  <*> "Have you experienced weight gain"            .: bool Nothing
  <*> "Have you experienced abnormal sweating"      .: bool Nothing

  <*> "How would you rate your quality of life (1-5)?" .: stringRead "not an integer" Nothing
  <*> "Is there anything else you'd like to mention"   .: string Nothing

dinfoForm :: Monad m => Form String m DInfo
dinfoForm = DInfo
  <$> "Diagnose the common cold" .: bool Nothing
  <*> "Diagnose a sinus infection" .: bool Nothing
  <*> "Diagnose an ear infection" .: bool Nothing
  <*> "Diagnose pinkeye" .: bool Nothing
  <*> "Diagnose strep throat" .: bool Nothing
  <*> "Diagnose a UTI" .: bool Nothing
  <*> "Diagnose pregnancy" .: bool Nothing
  <*> "Diagnose herpes" .: bool Nothing
  <*> "Diagnose the flu" .: bool Nothing
  <*> "Diagnose shingles" .: bool Nothing
  <*> "Diagnose chicken pox" .: bool Nothing
  <*> "Diagnose mono" .: bool Nothing
  <*> "Diagnose pneumonia" .: bool Nothing
  <*> "Diagnose menopause" .: bool Nothing
  <*> "Diagnose a yeast infection" .: bool Nothing
  <*> "Diagnose heat exhaustion" .: bool Nothing
  <*> "Other notes" .: check "" (/= "") (string Nothing)

irregularityView :: View H.Html -> H.Html
irregularityView v = do
  label         "Burning" v "Burning"
  inputCheckbox "Burning" v
  H.br
  label         "Stinging" v "Stinging"
  inputCheckbox "Stinging" v
  H.br
  label         "Sharp pain" v "Sharp pain"
  inputCheckbox "Sharp pain" v
  H.br
  label         "Dull pain" v "Dull pain"
  inputCheckbox "Dull pain" v
  H.br
  label         "Rash" v "Rash"
  inputCheckbox "Rash" v
  H.br
  label         "Numbness" v "Numbness"
  inputCheckbox "Numbness" v
  H.br
  label         "Tightness" v "Tightness"
  inputCheckbox "Tightness" v
  H.br
  label         "Inflammation" v "Inflammation"
  inputCheckbox "Inflammation" v
  H.br
  label         "Itching" v "Itching"
  inputCheckbox "Itching" v
  H.br
  label         "Discharge" v "Discharge"
  inputCheckbox "Discharge" v
  H.br
  label         "Discoloration" v "Discoloration"
  inputCheckbox "Discoloration" v
  H.br
  label         "Other (Please describe)" v "Other (please describe)"
  inputText     "Other (please describe)" v

pinfoView :: View H.Html -> H.Html
pinfoView v = do
  label "Height (inches)" v "Height (inches)"
  inputText "Height (inches)" v
  H.br
  label "Weight (pounds)" v "Weight (pounds)"
  inputText "Weight (pounds)" v
  H.br
  label "Age" v "Age"
  inputText "Age" v
  H.br
  label "Gender" v "Gender"
  inputText "Gender" v
  H.br
  label "Do you identify as transgender?" v "Do you identify as transgender?"
  inputCheckbox "Do you identify as transgender?" v
  H.br
  H.br

  label "Have you been hospitalized recently?" v "Have you been hospitalized recently?"
  inputText "Have you been hospitalized recently?" v
  H.br
  label "Have you had surgery recently?" v "Have you had surgery recently?"
  inputText "Have you had surgery recently?" v
  H.br
  label "Are you currently taking any medication?" v "Are you currently taking any medication?"
  inputText "Are you currently taking any medication?" v
  H.br
  label "Do you suffer from chronic illness?" v "Do you suffer from chronic illness?"
  inputText "Do you suffer from chronic illness?" v
  H.br
  H.br

  label "Do you have any allergies?" v "Do you have any allergies?"
  inputText "Do you have any allergies?" v
  H.br
  label "Do you have asthma?" v "Do you have asthma?"
  inputText "Do you have asthma?" v
  H.br
  label "Do you have heart disease?" v "Do you have heart disease?"
  inputText "Do you have heart disease?" v
  H.br
  label "If you menstruate, how old were you when you had your first period?" v "If you menstruate, how old were you when you had your first period?"
  inputText "If you menstruate, how old were you when you had your first period?" v
  H.br
  label "If you menstruate, when was your last period?" v "If you menstruate, when was your last period?"
  inputText "If you menstruate, when was your last period?" v
  H.br
  H.br

  H.p "Have you experienced the following in your head?"
  irregularityView $ subView "Have you experienced the following in your head?" v
  H.br
  H.p "Have you experienced the following in your throat?"
  irregularityView $ subView "Have you experienced the following in your throat?" v
  H.br
  H.p "Have you experienced the following on your back?"
  irregularityView $ subView "Have you experienced the following on your back?" v
  H.br
  H.p "Have you experienced the following in your stomach?"
  irregularityView $ subView "Have you experienced the following in your stomach?" v
  H.br
  H.p "Have you experienced the following in your chest?"
  irregularityView $ subView "Have you experienced the following in your chest?" v
  H.br
  H.p "Have you experienced the following in your arms?"
  irregularityView $ subView "Have you experienced the following in your arms?" v
  H.br
  H.p "Have you experienced the following in your legs?"
  irregularityView $ subView "Have you experienced the following in your legs?" v
  H.br
  H.p "Have you experienced the following in your joints?"
  irregularityView $ subView "Have you experienced the following in your joints?" v
  H.br
  H.p "Have you experienced the following in your genitals?"
  irregularityView $ subView "Have you experienced the following in your genitals?" v
  H.br
  H.p "Have you experienced the following in your ears?"
  irregularityView $ subView "Have you experienced the following in your ears?" v
  H.br
  H.p "Have you experienced the following in your extremities?"
  irregularityView $ subView "Have you experienced the following in your extremities?" v
  H.br
  H.p "Have you experienced the following on your skin?"
  irregularityView $ subView "Have you experienced the following on your skin?" v
  H.br
  H.br

  label "Have you experienced coughing" v "Have you experienced coughing"
  inputCheckbox "Have you experienced coughing" v
  H.br
  label "Have you experienced sneezing" v "Have you experienced sneezing"
  inputCheckbox "Have you experienced sneezing" v
  H.br
  label "Have you experienced nausea" v "Have you experienced nausea"
  inputCheckbox "Have you experienced nausea" v
  H.br
  label "Have you experienced dizziness" v "Have you experienced dizziness"
  inputCheckbox "Have you experienced dizziness" v
  H.br
  label "Have you experienced vision issues" v "Have you experienced vision issues"
  inputCheckbox "Have you experienced vision issues" v
  H.br
  label "Have you experienced hearing issues" v "Have you experienced hearing issues"
  inputCheckbox "Have you experienced hearing issues" v
  H.br
  label "Have you experienced vomiting" v "Have you experienced vomiting"
  inputCheckbox "Have you experienced vomiting" v
  H.br
  label "Have you experienced breathing issues" v "Have you experienced breathing issues"
  inputCheckbox "Have you experienced breathing issues" v
  H.br
  label "Have you experienced congestion" v "Have you experienced congestion"
  inputCheckbox "Have you experienced congestion" v
  H.br
  label "Have you experienced urination issues" v "Have you experienced urination issues"
  inputCheckbox "Have you experienced urination issues" v
  H.br
  label "Have you experienced defecation issues" v "Have you experienced defecation issues"
  inputCheckbox "Have you experienced defecation issues" v
  H.br
  label "Have you experienced fever" v "Have you experienced fever"
  inputCheckbox "Have you experienced fever" v
  H.br
  label "Have you experienced shaking" v "Have you experienced shaking"
  inputCheckbox "Have you experienced shaking" v
  H.br
  label "Have you experienced chills" v "Have you experienced chills"
  inputCheckbox "Have you experienced chills" v
  H.br
  label "Have you experienced heat" v "Have you experienced heat"
  inputCheckbox "Have you experienced heat" v
  H.br
  label "Have you experienced exhaustion" v "Have you experienced exhaustion"
  inputCheckbox "Have you experienced exhaustion" v
  H.br
  label "Have you experienced insomnia" v "Have you experienced insomnia"
  inputCheckbox "Have you experienced insomnia" v
  H.br
  label "Have you experienced bloating" v "Have you experienced bloating"
  inputCheckbox "Have you experienced bloating" v
  H.br
  label "Have you had a runny nose" v "Have you had a runny nose"
  inputCheckbox "Have you had a runny nose" v
  H.br
  label "Have you had abnormal mucus color" v "Have you had abnormal mucus color"
  inputCheckbox "Have you had abnormal mucus color" v
  H.br
  label "Have you had a sore throat" v "Have you had a sore throat"
  inputCheckbox "Have you had a sore throat" v
  H.br
  label "Have you experienced body aches" v "Have you experienced body aches"
  inputCheckbox "Have you experienced body aches" v
  H.br
  label "Have you experienced swollen lymph nodes" v "Have you experienced swollen lymph nodes"
  inputCheckbox "Have you experienced swollen lymph nodes" v
  H.br
  label "Have you experienced mood swings" v "Have you experienced mood swings"
  inputCheckbox "Have you experienced mood swings" v
  H.br
  label "Have you experienced difference in appetite" v "Have you experienced difference in appetite"
  inputCheckbox "Have you experienced difference in appetite" v
  H.br
  label "Have you experienced weight gain" v "Have you experienced weight gain"
  inputCheckbox "Have you experienced weight gain" v
  H.br
  label "Have you experienced abnormal sweating" v "Have you experienced abnormal sweating"
  inputCheckbox "Have you experienced abnormal sweating" v
  H.br
  H.br

  label "How would you rate your quality of life (1-5)?" v "How would you rate your quality of life (1-5)?"
  inputText "How would you rate your quality of life (1-5)?" v
  H.br
  label "Is there anything else you'd like to mention" v "Is there anything else you'd like to mention"
  inputText "Is there anything else you'd like to mention" v
  H.br

dinfoView :: View H.Html -> H.Html
dinfoView v = do
  label "Diagnose the common cold" v "Diagnose the common cold"
  inputCheckbox "Diagnose the common cold" v
  H.br
  label "Diagnose a sinus infection" v "Diagnose a sinus infection"
  inputCheckbox "Diagnose a sinus infection" v
  H.br
  label "Diagnose an ear infection" v "Diagnose an ear infection"
  inputCheckbox "Diagnose an ear infection" v
  H.br
  label "Diagnose pinkeye" v "Diagnose pinkeye"
  inputCheckbox "Diagnose pinkeye" v
  H.br
  label "Diagnose strep throat" v "Diagnose strep throat"
  inputCheckbox "Diagnose strep throat" v
  H.br
  label "Diagnose a UTI" v "Diagnose a UTI"
  inputCheckbox "Diagnose a UTI" v
  H.br
  label "Diagnose pregnancy" v "Diagnose pregnancy"
  inputCheckbox "Diagnose pregnancy" v
  H.br
  label "Diagnose herpes" v "Diagnose herpes"
  inputCheckbox "Diagnose herpes" v
  H.br
  label "Diagnose the flu" v "Diagnose the flu"
  inputCheckbox "Diagnose the flu" v
  H.br
  label "Diagnose shingles" v "Diagnose shingles"
  inputCheckbox "Diagnose shingles" v
  H.br
  label "Diagnose chicken pox" v "Diagnose chicken pox"
  inputCheckbox "Diagnose chicken pox" v
  H.br
  label "Diagnose mono" v "Diagnose mono"
  inputCheckbox "Diagnose mono" v
  H.br
  label "Diagnose pneumonia" v "Diagnose pneumonia"
  inputCheckbox "Diagnose pneumonia" v
  H.br
  label "Diagnose menopause" v "Diagnose menopause"
  inputCheckbox "Diagnose menopause" v
  H.br
  label "Diagnose a yeast infection" v "Diagnose a yeast infection"
  inputCheckbox "Diagnose a yeast infection" v
  H.br
  label "Diagnose heat exhaustion" v "Diagnose heat exhaustion"
  inputCheckbox "Diagnose heat exhaustion" v
  H.br
  label "Other notes" v "Other notes"
  inputText "Other notes" v
  H.br

patientInfo :: ServerPart Response
patientInfo = do
  decodeBody $ defaultBodyPolicy "/tmp" 4096 4096 4096
  r <- runForm "test" pinfoForm
  case r of
    (view, Nothing) -> do
      let view' = fmap H.toHtml view
      ok $ toResponse $ form view' "/patient-info" ! A.action "/patient-info" $ do
        pinfoView view'
        H.br
        inputSubmit "Submit"
    (_, Just release) -> do
      _ <- liftIO $ writeFile "patient" (show release)
      ok $ toResponse $ do
        H.h1 "Successfully recieved info"
        H.p "To finish your conversation please call XXX-XXX-XXXX or join hangouts.google.com/XXXX to speak with a real doctor about what you just discussed"

doctorInfo :: ServerPart Response
doctorInfo = do
  decodeBody $ defaultBodyPolicy "/tmp2" 4096 4096 4096
  r <- runForm "test2" dinfoForm
  case r of
    (view, Nothing) -> do
      p <- liftIO $ readFile "patient"
      let view' = fmap H.toHtml view
      ok $ toResponse $ form view' "/doctor-info" ! A.action "/doctor-info" $ do
        mapM_ (H.p . H.toHtml) $ lines $ showP (read p :: PInfo)
        H.br
        dinfoView view'
        H.br
        inputSubmit "Submit"
    (_, Just release) -> do
      _ <- liftIO $ writeFile "doctor" (show release)
      ok $ toResponse $ do
        H.h1 "Successfully recieved info"
        H.p "Thank you for helping those in need!"

patientResults :: ServerPart Response
patientResults = do
  decodeBody $ defaultBodyPolicy "/tmp2" 4096 4096 4096
  d <- liftIO $ readFile "doctor"
  return $ toResponse $ mapM_ (H.p . H.toHtml) $ lines $ showD1 (read d :: DInfo)

doctorResults :: ServerPart Response
doctorResults = do
  decodeBody $ defaultBodyPolicy "/tmp2" 4096 4096 4096
  d <- liftIO $ readFile "doctor"
  p <- liftIO $ readFile "patient"
  return $ toResponse $ do
    mapM_ (H.p . H.toHtml) $ lines $ showP  (read p :: PInfo)
    mapM_ (H.p . H.toHtml) $ lines $ showD2 (read d :: DInfo)

site :: ServerPart Response
site = msum [ dir "patient-info" patientInfo
            , dir "doctor-info"  doctorInfo
            , dir "patient-results" patientResults
            , dir "doctor-results" doctorResults
            ]

putsite :: IO ()
putsite = simpleHTTP nullConf site
