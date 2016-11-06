module Diagnosis where

data Diagnosis = Diagnosis {
    name        :: String
  , description :: String
  , facts       :: [String]
  , symptoms    :: [String]
  , treatment   :: [String]
  , doctorIf    :: [String]
}

showD :: Diagnosis -> String
showD (Diagnosis name description facts symptoms treatment doctorIf) =
     "You may have " ++ name ++ "\n"
  ++ unlines facts
  ++ "Symptoms include: \n" ++ unlines (map ("* " ++) symptoms)
  ++ "To treat this at home, consider: \n" ++ unlines (map ("* " ++) treatment)
  ++ "You should consider seeing a medical professional if: \n" ++ unlines (map ("* " ++) doctorIf)
  ++ "\n"

commonCold :: Diagnosis
commonCold = Diagnosis
	"The Common Cold"
	"A common viral infectious disease mainly affecting the nose, throat, and sinuses."
	["Can be caused by over 200 strains of viruses", "can spread through the sneezes or coughs of an infected person, contaminated objects, and hand-to-hand contact", "Symptoms usually appear 1-3 days after contact with the virus"]
	["Cough", "Runny/Stuffy Nose", "Congestion", "Head Dull Pain", "Head Sharp Pain", "Fever", "Discolored Mucus", "Sore Throat"]
	["Nasal Decongestants", "Cough Syrup", "Fever Reducers/Pain Relievers such as acetaminophen (Tylenol) or ibuprofen (Motrin, Advil)", "Drink plenty of fluids", "Vitamin C", "Eat regularly", "Sleep 8-10 hours"]
	["If the patient is a child with a fever lasting 2+ days", "If patient is an adult with a fever lasting 5+ days", "In the case of ear pain, severe or worsening symptoms, or difficulty breathing"]


sinusInfection :: Diagnosis
sinusInfection = Diagnosis
	"Sinus Infection (Acute Sinitus)"
	"Inflammation/swelling in the nasal cavities, often caused by a viral or bacterial infection."
	["Mucus in a sinus infection becomes discolored (yellow or green) due to white blood cells that died fighting the infectious cells", "Sore throat/coughing is due to mucus draining down the back of the throat"]
	["Head Dull Pain", "Head Sharp Pain", "Stuffy/Runny nose", "Congestion", "Discolored Mucus", "Coughing", "Sore Throat"]
	["Nasal Decongestants","Fever Reducers/Pain Relievers such as acetaminophen (Tylenol) or ibuprofen (Motrin, Advil)", "Drink plenty of fluids", "Over the counter nasal corticosteroid spray (Flonase, Veramyst, Nasonex)", "Saline Nasal Spray", "Eat regularly", "Sleep 8-10 hours"]
	["Symptoms last more than 10 days", "Symptoms worsen suddenly", "Symptoms disappear and reappear", "Mucus is black in color"]


earInfection :: Diagnosis
earInfection = Diagnosis
	"Ear Infection (Otitis)"
	"A bacterial or viral infection usually affecting the middle ear."
	["Ear infection generally occurs after another illness that causes congestion and swelling of the nasal passages or throat, such as a common cold or flu.", "Children under the age of 6 are the most at risk for ear infection."]
	["Head Dull Pain", "Head Sharp Pain", "Ear Pain", "Discharge of the Ear", "Hearing Impairment", "Fever", "Dizziness"]
	["Apply a warm cloth to affected ear (lessens discomfort)", "Fever Reducers/Pain Relievers such as acetaminophen (Tylenol) or ibuprofen (Motrin, Advil)", "Nasal Decongestants", "Antibiotics may be prescribed by a doctor depending on severity"]
	["Patient is a child under the age of 2", "Fever is over 102 degrees Fahrenheit", "Symptoms persist for 2+ days", "Symptoms worsen", "Symptoms disappear and reappear"]


pinkeye :: Diagnosis
pinkeye = Diagnosis 
	"Pink Eye (Conjunctivitis)"
	"Inflammation of the conjunctiva, the clear lens on the outer part of the eyeball and the tissue inside of the eyelid."
	["Eyes appear pink because inflammation makes the blood vessels more prominent", "Can be caused by bacteria, viruses, or allergies"]	
	["Eye Pain", "Eye Discoloration", "Eye Discharge", "Itchy Eyes", "Burning Eyes", "Vision Impairment", "Inflammation of the Eyes"]
	["Cold compress applied to eyes", "Artificial tears", "Refrain from wearing contacts", "Antihistamine eye drops", "Oral antihistamines (Benadryl, Claritin, etc.)", "Antiviral/antibiotic if prescribed by a doctor"]
	["Patient is a newborn", "Symptoms worsen", "Impaired vision not due to tears or discharge is present", "Patient has an immunodeficiency disorder"]
	
strepThroat :: Diagnosis
strepThroat = Diagnosis
	"Strep Throat (Streptococcal Pharyngitis)"
	"A bacterial infection of the throat."
	["Strep Throat can affect those of all ages, but most commonly is experienced by children ages 5-15.", "A doctor MUST conduct a strep test in order to confirm the diagnosis of Strep Throat.", "Symptoms normally occur 5 days after exposure to the bacteria."]
	["Throat Inflammation", "Throat Discoloration", "Throat Rash", "Chills", "Head Dull Pain", "Head Sharp Pain", "Fever", "Swollen Lymph Nodes"]
	["SEE A MEDICAL PROFESSIONAL IN ORDER TO CONFIRM DIAGNOSIS", "Antibiotics", "Hot liquids/teas", "Fever Reducers/Pain Relievers such as acetaminophen (Tylenol) or ibuprofen (Motrin, Advil)", "8-10 hours of rest"]
	["You could possible have strep throat"]


uti :: Diagnosis
uti = Diagnosis
	"Urinary Tract Infection (UTI)"
	"A bacterial infection of the urinary tract."
	["While a UTI can be experienced by both men and women, women are more likely to experience a UTI.", "Contrary to popular belief, there is no proof that home remedies can cure a UTI.", "Infection from a UTI can spread to kidneys, which is why it is important to see a doctor as soon as a UTI is suspected.", "Some of the most common reasons for contracting a UTI involve spreading of bacteria from the anus to the urethra in either sexual intercourse or unsanitary bathroom habits."]
	["Irregular Urination", "Genital Area Burning", "Genital Area Itching", "Genital Area Bleeding", "Genital Area Pain", "Genital Area Discharge", "Back Pain", "Fever", "Shaking", "Chills"]
	["SEE A MEDICAL PROFESSIONAL IN ORDER TO CONFIRM DIAGNOSIS", "Antibiotics as prescribed by a doctor", "Drink plenty of fluids", "Urinate as often as the urge arises", "Apply heating pack to abdominal area", "Refrain from drinking alcohol, coffee, and caffeine"]
	["You prefer your genitalia intact"]


pregnancy :: Diagnosis
pregnancy = Diagnosis
	"Pregnancy"
	"Having an offspring developing inside the body, as a result of fertilization of an egg cell by a sperm cell."
	["Every person experiences pregnancy a little differently", "Human pregnancy lasts about 9 months", "In general, only people under the age of 50 can get pregnant"]
	["Irregular Urination", "Irregular Defecation", "Bloating", "Nausea", "Vomiting", "Headaches", "Back Pain", "Dizziness", "Mood Swings", "Weight Gain", "Change in Appetite", "Chest Enlargement", "Skipped Period"]
	["NOTE: Treatment suggestions are for those who intend to carry pregnancy to term. Terminating pregnancy is an option as well and can be done by consulting a local clinic, hospital, or doctor's office.", "Confirm with an at-home pregnancy test or doctor's appointment", "Gentle exercise on a regular basis", "Eating plenty of fruit, vegetables, and fiber", "Drink plenty of water", "Refrain from drinking alcohol, smoking, and other drug use"]
	["You miss multiple periods."]


herpes :: Diagnosis
herpes = Diagnosis
	"Genital Herpes (Herpes Simplex Virus, HSV-2)"
	"A common sexually transmitted disease that causes rash in the affected area."
	["Herpes also has another form, which generally affects the mouth area, called HSV-1.", "It is estimated that about 2/3 of the population of the US has Herpes in some form.", "It is estimated that 90% of Americans have come into contact with some form of Herpes.", "Herpes has no cure and is a recurring infection, it will affect the person infected throughout their life.", "Herpes is in the same family as Chicken Pox."]
	["Genital Area Discharge", "Genital Area Itching", "Genital Area Burning", "Genital Area Rash", "Genital Area Sharp Pain", "Genital Area Tingling", "Irregular Urination"]
	["Antiviral Medication can be prescribed by a doctor for suppressing outbreaks or keeping symptoms in check.", "Sexual partners should be warned prior to intercourse"]
	["You would like to not have herpes"]


flu :: Diagnosis
flu = Diagnosis
	"Flu (Influenza)"
	"A common contagious viral infection which comes on quite suddenly."
	["The Influenza virus is very adaptable, which leads to many different strains. This is why doctors suggest a flu shot on a yearly basis.", "Peak 'Flu Season' is from October to March."]
	["Fever", "Cough", "Sore Throat", "Runny/Stuffy Nose", "Full Body Ache", "Head Dull Pain", "Head Sharp Pain", "Chills", "Fatigue", "Irregular Defecation", "Vomiting", "Nausea", "Excessive Sweating"]
	["Drink plenty of fluids", "Fever Reducers/Pain Relievers such as acetaminophen (Tylenol) or ibuprofen (Motrin, Advil)", "Nasal Decongestant", "Rest for 8-10 hours", "Antiviral medication may be prescribed by a doctor"]
	["Symptoms begin to disappear and then reappear worse", "Symptoms are very severe", "Fever is extremely high (102 degrees Fahrenheit)"]
	


shingles :: Diagnosis
shingles = Diagnosis
	"Shingles"
	"A condition caused by the reanimation of the dormant Chickenpox virus (Varicella-Zoster) that had settled in the nerve cells after Chickenpox subsides."
	["Damage to nerve cells caused by Shingles can lead to postherpetic neuralgia, a condition in which damaged nerve fibers send constant pain signals to the brain.", "Shingles itself is not deadly, but the complications due to shingles may be deadly."]
	["Skin Rash", "Skin Tingling", "Skin Burning", "Skin Itching", "Fever", "Head Dull Pain", "Head Sharp Pain", "Exhaustion"]
	["It is HIGHLY RECOMMENDED that a medical professional is contacted", "Antiviral drugs and pain medication as prescribed by a doctor", "Cold compress", "Cold bath"]
	["Rash occurs around the face and eyes", "Pain becomes unbearable", "Patient is over 70 years old"]


chickenPox :: Diagnosis
chickenPox = Diagnosis
	"Chickenpox (Varicella-Zoster)"
	"A viral disease that is usually contracted in childhood, characterized by the iconic ‘pox’ rash that appears all over the skin."
	["The Chickenpox virus is also called Herpes-Zoster, and it is of the same family as the Herpes Simplex virus." , "Like Herpes, Chickenpox remains dormant in the body after the symptoms subside.", "Vaccinated people can still contract the disease, but it is often much more mild, with no fever."]
	["Skin Rash", "Fever", "Change in Appetite", "Head Dull Pain", "Head Sharp Pain", "Exhaustion"]
	["SEE A MEDICAL PROFESSIONAL", "Antiviral medications prescribed by a doctor", "Drink plenty of fluids", "Rest 8-10 hours", "Fever Reducers/Pain Relievers such as acetaminophen (Tylenol) or ibuprofen (Motrin, Advil)", "Nasal Decongestant"]
	[]


mono :: Diagnosis
mono = Diagnosis
	"Mono (Infectious Mononucleosis)"
	"Mono is not a disease in itself, but rather a group of symptoms commonly caused by the Epstein-Barr virus (EBV)."
	["While EBV is most associated with Mono, Mono can also be caused by other viruses.", "Mono lasts anywhere from two to six weeks."]
	["Exhaustion", "Sore Throat", "Fever", "Swollen Lymph Nodes", "Head Dull Pain", "Head Sharp Pain", "Skin Rash", "Abdomen Dull Pain"]
	["Drink plenty of water and fruit juices", "Fever Reducers/Pain Relievers such as acetaminophen (Tylenol) or ibuprofen (Motrin, Advil)", "Nasal Decongestant", "Rest as much as possible"]
	["Symptoms do not lessen after one week"]


pneumonia :: Diagnosis
pneumonia = Diagnosis
	"Pneumonia"
	"Pneumonia is when the air sacs in the lungs become inflamed, filling with fluid."
	["Pneumonia can be caused by a variety of organisms, including baceria, viruses, and fungi.", "Pneumonia can also occur due to the inhalation of food, drink, vomit, or saliva into the lungs."]
	["Coughing", "Sneezing", "Nausea", "Vomiting", "Irregular defecation", "Chest Pain", "Exhaustion", "Fever", "Excessive Sweating", "Shaking", "Chills"]
	["SEE A MEDICAL PROFESSIONAL", "Fever Reducers/Pain Relievers such as acetaminophen (Tylenol) or ibuprofen (Motrin, Advil)", "Cough Syrup", "Get as much rest as possible", "Medication as prescribed by a medical professional",  "Drink plenty of water"]
	["You possibly have pheumonia."]


menopause :: Diagnosis
menopause = Diagnosis
	"Menopause"
	"The end of the fertility cycle of a uterus."
	["Menopause is simply the end of a fertility cycle, but people can still remain healthy and active.", "Menopause can be concretely diagnosed one full year after the last menstrual cycle."]
	["Irregular Periods", "Genital Area Itching", "Heat Flashes", "Excessive Sweating", "Insomnia", "Mood Swings", "Change in Appetite", "Irregular Urination"]
	["NOTE: Menopause is not a ‘disease’, so there is no cure. Treatments are only to help cope with the symptoms.","Estrogen Therapy", "Low-dose antidepressants", "Sleep 8-10 hours", "Avoid caffeine", "Practice relaxation techniques", "Exercise regularly", "Eat a balanced diet", "Kegel exercises"]
	["If symptoms cause problems to daily life"]


yeastInfection :: Diagnosis
yeastInfection = Diagnosis
	"Yeast Infection"
	"A fungal infection of the genital region."
  ["Yeast infections can happen in both women and men, but are more frequent in women."]
  ["Genital Area Itching", "Genital Area Inflammation", "Genital Area Dull Pain", "Genital Area Discharge", "Genital Area Rash"]
  ["Over-the-counter antifungal cream", "Medication as prescribed by a doctor"]
  ["It is unclear if symptoms indicate a yeast infection", "Symptoms don’t decrease with over-the-counter antifungal cream", "This is the patient’s first time with yeast infection symptoms"]


heatExhaustion :: Diagnosis
heatExhaustion = Diagnosis
	"Heat Exhaustion"
	"A heat-related illness that can occur after exposure to high temperatures."
	["Heat Exhaustion is often accompanied by dehydration."]
  ["Irregular Urination", "Exhaustion", "Abdominal Dull Pain", "Abdominal Sharp Pain", "Skin Discoloration", "Excessive Sweating", "Dizziness", "Nausea", "Vomiting", "Head Sharp Pain", "Head Dull Pain"]
  ["Drink plenty of fluids, but not caffeine or alcohol", "Remove tight and unnecessary clothing", "Take a cool shower", "Apply cold compresses", "Sit and rest in a cool location, such as shade or air-conditioning"]
  ["The above measures are taken, but symptoms worsen"]
