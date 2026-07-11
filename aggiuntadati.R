source("library.R")

#importazione di clean data
merged <- read.csv("C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/Input/merged_data.csv")


#aggiustare variabili
merged$bmi <- as.numeric(merged$bmi)
merged$MEDAS <- as.numeric(merged$MEDAS)
#date da chr
merged$data_nascita= as.Date(merged$data_nascita, origin= "1899-12-30")
merged$DATA_ESORDIO        <- as.Date(merged$DATA_ESORDIO, origin = "1899-12-30")
merged$data_diagnosi       <- as.Date(merged$data_diagnosi, origin = "1899-12-30")
merged$data_prelievo_T0    <- as.Date(merged$data_prelievo_T0, origin = "1899-12-30")
merged$data_cambio_terapia <- as.Date(merged$data_cambio_terapia, origin = "1899-12-30")
merged$data_referto_T6  <- as.Date(merged$data_referto_T6,  origin = "1899-12-30")
merged$data_referto_T12 <- as.Date(merged$data_referto_T12, origin = "1899-12-30")
merged$data_referto_T18 <- as.Date(merged$data_referto_T18, origin = "1899-12-30")
merged$data_referto_T24 <- as.Date(merged$data_referto_T24, origin = "1899-12-30")
merged$data_referto_T30 <- as.Date(merged$data_referto_T30, origin = "1899-12-30")
merged$data_referto_T36 <- as.Date(merged$data_referto_T36, origin = "1899-12-30")
merged$sample_collection_date <- as.Date(merged$sample_collection_date, origin = "1899-12-30")
merged$glc_date_before_t0 <- as.Date(merged$glc_date_before_t0, origin="1899-12-30")
#da int a categoriche (relapse, lesion, cambio terapia)
merged$lesioni_T6 <- factor(merged$lesioni_T6)
merged$lesioni_T12 <- factor(merged$lesioni_T12)
merged$lesioni_T18 <- factor(merged$lesioni_T18)
merged$lesioni_T24 <- factor(merged$lesioni_T24)
merged$`lesioni_T30` <- factor(merged$`lesioni_T30`)
merged$`lesioni_T36` <- factor(merged$`lesioni_T36`)

merged$relapse_T6 <- factor(merged$relapse_T6)
merged$relapse_T12 <- factor(merged$relapse_T12)
merged$relapse_T18<- factor(merged$relapse_T18)
merged$relapse_T24 <- factor(merged$relapse_T24)
merged$relapse_T30 <- factor(merged$relapse_T30)
merged$relapse_T36 <- factor(merged$relapse_T36)

merged$cambio_terapia<- as.character(merged$cambio_terapia)

merged$bone_marrow_lesions <- as.factor(merged$bone_marrow_lesions)
merged$subtentorial_lesions <- as.factor(merged$subtentorial_lesions)
merged$gadolinium_contrast <- as.factor(merged$gadolinium_contrast)
merged$full_recovery <- as.factor(merged$full_recovery)

#da chr a categoriche
merged$naive <- factor(merged$naive,levels = c("no", "yes"))
merged$sex <- factor(merged$sex, levels = c("M", "F"))
merged$smoking_habit <- factor(merged$smoking_habit,levels = c("yes", "no"))
merged$antibiotic_use <- factor(merged$antibiotic_use,levels = c("yes", "no"))
merged$spike_in <- factor(merged$spike_in,levels = c("no", "yes"))
merged$glucocorticoid_treatment <- factor(merged$glucocorticoid_treatment,levels = c("positive", "negative"))
merged$gc_treatment <- factor(merged$gc_treatment,levels = c("positive", "negative"))
merged$prognosis_at_onset <- factor(merged$prognosis_at_onset,levels = c("positive", "negative"))
merged$simplified_batch<- factor (merged$simplified_batch, levels= c("1-3", "4-6"))


#aggiungo NUOVI dati 
merged$EDSS_T12[merged$ID == "MAV23"] <- 1.0 #CAMBIA CONTINUAMENTE
merged$previous_therapy[merged$ID == "MAV07"] <- "interferon_beta"
merged$naive[merged$ID == "MAV08"]<- "no"
merged$naive[merged$ID == "MAV12"]<- "no"
merged$naive[merged$ID == "MAV20"]<- "no"


merged$data_nascita[merged$ID == "MAV34"] <- as.Date("1987-09-22")
merged$bmi[merged$ID == "MAV12"] <- 20.31 
merged$smoking_habit[merged$ID == "MAV12"] <- "no"
merged$physical_activity[merged$ID == "MAV12"] <- "partially_active"

merged$bmi[merged$ID == "MAV16"] <- 20.08
merged$smoking_habit[merged$ID == "MAV16"] <- "yes"
merged$physical_activity[merged$ID == "MAV16"] <- "partially_active"

merged$data_nascita <- as.Date(merged$data_nascita)
merged$data_nascita[merged$ID == "MAV31"] <- as.Date("1975-10-07")
merged$data_nascita[merged$ID == "MAV33"] <- as.Date("1998-11-09")
merged$data_nascita[merged$ID == "MAV32"] <- as.Date("1976-01-26")

merged$DATA_ESORDIO <- as.Date(merged$DATA_ESORDIO)
merged$DATA_ESORDIO[merged$ID == "MAV31"] <- as.Date("2017-08-07")
merged$DATA_ESORDIO[merged$ID == "MAV33"] <- as.Date("2021-02-15")
merged$DATA_ESORDIO[merged$ID == "MAV32"] <- as.Date("2015-07-01")
merged$DATA_ESORDIO[merged$ID == "MAV34"] <- as.Date("2008-01-01")

merged$data_diagnosi <- as.Date(merged$data_diagnosi)
merged$data_diagnosi[merged$ID == "MAV31"] <- as.Date("2017-12-15")
merged$data_diagnosi[merged$ID == "MAV33"] <- as.Date("2021-08-20")
merged$data_diagnosi[merged$ID == "MAV32"] <- as.Date("2016-07-01")
merged$data_diagnosi[merged$ID == "MAV34"] <- as.Date("2019-11-01")
merged$data_prelievo_T0 <- as.Date(merged$data_prelievo_T0)
merged$data_prelievo_T0[merged$ID== "MAV22"] <- as.Date ("2021-11-01")
#inserisco age_at_diagnosis 
merged$age_at_diagnosis <- floor(
  as.numeric(interval(merged$data_nascita, merged$data_diagnosi) / years(1))
)

#age 
merged$age <- floor(
  as.numeric(interval(merged$data_nascita, Sys.Date()) / years(1))
)

#age at t0
merged$age_at_T0 <- floor(
  as.numeric(interval(merged$data_nascita, merged$data_prelievo_T0) / years(1))
)


merged$data_prelievo_T0[merged$ID == "MAV31"] <- as.Date("2021-07-01")
merged$EDSS_T0[merged$ID == "MAV31"] <- 2.5
merged$EDSS_T12[merged$ID == "MAV31"] <- 2.5
merged$EDSS_T24[merged$ID == "MAV31"] <- 2.5
merged$glc_date_before_t0[merged$ID == "MAV31"] <-
  merged$lesion_burden[merged$ID == "MAV31"] <- 1
merged$bone_marrow_lesions[merged$ID == "MAV31"] <- 1
merged$subtentorial_lesions[merged$ID == "MAV31"] <- 1
merged$gadolinium_contrast[merged$ID == "MAV31"] <- 0
merged$clinical_presentation[merged$ID == "MAV31"] <- "sensory_symptoms"
merged$full_recovery[merged$ID == "MAV31"] <- 0
merged$lesioni_T12[merged$ID == "MAV31"] <- 1
merged$relapse_T12[merged$ID == "MAV31"] <- 0
merged$lesioni_T24[merged$ID == "MAV31"] <- 0
merged$relapse_T24[merged$ID == "MAV31"] <- 0


merged$data_prelievo_T0[merged$ID == "MAV32"] <- as.Date("2021-08-21")
merged$EDSS_T0[merged$ID == "MAV32"] <- 4
merged$EDSS_T12[merged$ID == "MAV32"] <- 4
merged$EDSS_T24[merged$ID == "MAV32"] <- 4
merged$lesioni_T12[merged$ID == "MAV32"] <- 0
merged$lesioni_T24[merged$ID == "MAV32"] <- 0

merged$data_prelievo_T0[merged$ID== "MAV33"] <- as.Date("2021-09-06")
merged$EDSS_T0[merged$ID == "MAV33"] <- 2.5
merged$EDSS_T12[merged$ID == "MAV33"] <- 2
merged$EDSS_T24[merged$ID == "MAV33"] <- 2.5
merged$lesion_burden[merged$ID == "MAV33"] <- 1
merged$bone_marrow_lesions[merged$ID == "MAV33"] <- 1
merged$subtentorial_lesions[merged$ID == "MAV33"] <- 0
merged$gadolinium_contrast[merged$ID == "MAV33"] <- 0
merged$clinical_presentation[merged$ID == "MAV33"] <- "sensory_symptoms"
merged$full_recovery[merged$ID == "MAV33"] <- 1
merged$lesioni_T12[merged$ID == "MAV33"] <- 0
merged$relapse_T12[merged$ID == "MAV33"] <- 0
merged$lesioni_T24[merged$ID == "MAV33"] <- 0
merged$relapse_T24[merged$ID == "MAV33"] <- 0



merged$data_prelievo_T0[merged$ID == "MAV34"] <- as.Date("2022-04-19")
merged$EDSS_T0[merged$ID == "MAV34"] <- 2
merged$EDSS_T12[merged$ID == "MAV34"] <- 2
merged$EDSS_T24[merged$ID == "MAV34"] <- 2
merged$glc_date_before_t0[merged$ID == "MAV34"] <- as.Date("2022-02-01")
merged$lesion_burden[merged$ID == "MAV34"] <- 0
merged$bone_marrow_lesions[merged$ID == "MAV34"] <- 1
merged$gadolinium_contrast[merged$ID == "MAV34"] <- 0
merged$clinical_presentation[merged$ID == "MAV34"] <- "optic_neuritis"
merged$full_recovery[merged$ID == "MAV34"] <- 1
merged$lesioni_T12[merged$ID == "MAV34"] <- 0
merged$relapse_T12[merged$ID == "MAV34"] <- 0
merged$lesioni_T24[merged$ID == "MAV34"] <- 0
merged$relapse_T24[merged$ID == "MAV34"] <- 0

merged$bmi[merged$ID == "MAV15"] <- 36
merged$smoking_habit[merged$ID == "MAV15"] <- "yes"
merged$physical_activity[merged$ID == "MAV15"] <- "sedentary"


merged$smoking_habit [merged$ID == "MAV25"] <- "no"
merged$physical_activity [merged$ID == "MAV25"] <- "sedentary"
merged$bmi [merged$ID == "MAV25"] <- 25.7


merged$MEDAS [merged$ID == "MAV10"] <- 35
merged$MEDAS [merged$ID == "MAV21"] <- 35
merged$MEDAS [merged$ID == "MAV22"] <- 42

merged$smoking_habit[merged$ID == "MAV31"] <- "no"
merged$smoking_habit [merged$ID == "MAV32"] <- "yes"
merged$smoking_habit [merged$ID == "MAV32"] <- "no"

merged$MEDAS[merged$ID == "MAV23"] <- 42.9#CAPIRE MEDAS (71?)
merged$bmi[merged$ID == "MAV23"] <- 20.43
merged$smoking_habit[merged$ID == "MAV23"]<- "no"
merged$physical_activity[merged$ID== "MAV23"] <- "active"

merged$lesion_burden <- factor(merged$lesion_burden)

#aggiunta dati al 14.05 
merged$MEDAS[merged$ID== "MAV19"] <- 35
merged$naive[merged$ID == "MAV22"] <- "yes"

merged$relapse_T6[merged$ID== "MAV07"] <- 0
merged$relapse_T6[merged$ID== "MAV08"] <- 1
merged$relapse_T6[merged$ID== "MAV09"] <- NA
merged$relapse_T6[merged$ID== "MAV11"] <- 0
merged$relapse_T6[merged$ID== "MAV14"] <- NA
merged$relapse_T6[merged$ID== "MAV15"] <- NA
merged$relapse_T6[merged$ID== "MAV17"] <- NA
merged$relapse_T6[merged$ID== "MAV19"] <- 0
merged$relapse_T6[merged$ID== "MAV20"] <- NA
merged$relapse_T6[merged$ID== "MAV21"] <- 0
merged$relapse_T6[merged$ID== "MAV23"] <- 0
merged$relapse_T6[merged$ID== "MAV24"] <- 0
merged$relapse_T6[merged$ID== "MAV25"] <- 0
merged$relapse_T6[merged$ID== "MAV31"] <- NA
merged$relapse_T6[merged$ID== "MAV32"] <- NA
merged$relapse_T6[merged$ID== "MAV33"] <- NA
merged$relapse_T6[merged$ID== "MAV34"] <- NA
merged$relapse_T6[merged$ID== "MAV29"] <- 0
merged$relapse_T6[merged$ID== "MAV30"] <- 0
merged$relapse_T6[merged$ID== "MAV26"] <- 1

merged$cortisone_T6[merged$ID == "MAV08"] <- 15 

merged$EDSS_T12[merged$ID == "MAV23"] <- 1.0
merged$EDSS_T12[merged$ID == "MAV33"] <- 2.5

merged$relapse_T12[merged$ID == "MAV08"] <- NA 
merged$relapse_T12[merged$ID == "MAV12"] <- 0
merged$relapse_T12[merged$ID == "MAV16"] <- NA
merged$relapse_T12[merged$ID == "MAV18"] <- NA
merged$relapse_T12[merged$ID == "MAV19"] <- 0
merged$relapse_T12[merged$ID == "MAV20"] <- NA
merged$relapse_T12[merged$ID == "MAV25"] <- 0
merged$relapse_T12[merged$ID == "MAV26"] <- 0
merged$relapse_T12[merged$ID == "MAV28"] <- 0


merged$relapse_T18[merged$ID == "MAV08"] <- NA
merged$relapse_T18[merged$ID == "MAV09"] <- NA
merged$relapse_T18[merged$ID == "MAV10"] <- 0
merged$relapse_T18[merged$ID == "MAV12"] <- 0
merged$relapse_T18[merged$ID == "MAV13"] <- 0
merged$relapse_T18[merged$ID == "MAV14"] <- NA
merged$relapse_T18[merged$ID == "MAV16"] <- 0
merged$relapse_T18[merged$ID == "MAV17"] <- 0
merged$relapse_T18[merged$ID == "MAV20"] <- NA
merged$relapse_T18[merged$ID == "MAV21"] <- 0
merged$relapse_T18[merged$ID == "MAV22"] <- 0
merged$relapse_T18[merged$ID == "MAV23"] <- 0
merged$relapse_T18[merged$ID == "MAV24"] <- 0
merged$relapse_T18[merged$ID == "MAV25"] <- 0
merged$relapse_T18[merged$ID == "MAV26"] <- 0
merged$relapse_T18[merged$ID == "MAV27"] <- 0
merged$relapse_T18[merged$ID == "MAV30"] <- NA



merged$relapse_T24[merged$ID == "MAV05"] <- NA
merged$relapse_T24[merged$ID == "MAV06"] <- NA
merged$relapse_T24[merged$ID == "MAV07"] <- NA
merged$relapse_T24[merged$ID == "MAV08"] <- NA
merged$relapse_T24[merged$ID == "MAV09"] <- NA
merged$relapse_T24[merged$ID == "MAV13"] <- 0
merged$relapse_T24[merged$ID == "MAV16"] <- 1
merged$relapse_T24[merged$ID == "MAV17"] <- 0
merged$relapse_T24[merged$ID == "MAV18"] <- NA
merged$relapse_T24[merged$ID == "MAV19"] <- NA
merged$relapse_T24[merged$ID == "MAV20"] <- NA
merged$relapse_T24[merged$ID == "MAV23"] <- 0
merged$relapse_T24[merged$ID == "MAV24"] <- 0
merged$relapse_T24[merged$ID == "MAV25"] <- NA
merged$relapse_T24[merged$ID == "MAV26"] <- 1
merged$relapse_T24[merged$ID == "MAV27"] <- NA
merged$relapse_T24[merged$ID == "MAV28"] <- 0
merged$relapse_T24[merged$ID == "MAV30"] <- NA
merged$relapse_T24[merged$ID == "MAV32"] <- 0


merged$relapse_T30[merged$ID == "MAV09"] <- 1
merged$relapse_T30[merged$ID == "MAV08"] <- NA
merged$relapse_T30[merged$ID == "MAV07"] <- NA

merged$relapse_T30[merged$ID == "MAV10"] <- 0
merged$relapse_T30[merged$ID == "MAV11"] <- 0
merged$relapse_T30[merged$ID == "MAV12"] <- 0
merged$relapse_T30[merged$ID == "MAV13"] <- 0
merged$relapse_T30[merged$ID == "MAV14"] <- 0
merged$relapse_T30[merged$ID == "MAV15"] <- 0
merged$relapse_T30[merged$ID == "MAV21"] <- 1
merged$relapse_T30[merged$ID == "MAV24"] <- NA
merged$relapse_T30[merged$ID == "MAV25"] <- 0
merged$relapse_T30[merged$ID == "MAV26"] <- 1
merged$relapse_T30[merged$ID == "MAV32"] <- NA
merged$relapse_T30[merged$ID == "MAV27"] <- NA



merged$relapse_T36[merged$ID == "MAV07"] <- NA
merged$relapse_T36[merged$ID == "MAV10"] <- 0
merged$relapse_T36[merged$ID == "MAV20"] <- NA
merged$relapse_T36[merged$ID == "MAV21"] <- NA
merged$relapse_T36[merged$ID == "MAV23"] <- NA
merged$relapse_T36[merged$ID == "MAV25"] <- 0
merged$relapse_T36[merged$ID == "MAV27"] <- NA
merged$relapse_T36[merged$ID == "MAV31"] <- NA
merged$relapse_T36[merged$ID == "MAV30"] <- NA

merged$cambio_terapia[merged$ID == "MAV31"] <- 0
merged$cambio_terapia[merged$ID == "MAV33"] <- 0

merged$data_cambio_terapia[merged$ID == "MAV29"] <- as.Date("2024-08-01")

merged$NUOVA_TERAPIA[merged$ID== "MAV07"] <- "inizia mavenclad il 4/2/2021"

merged$naive[merged$ID== "MAV15"] <- "no"
merged$naive[merged$ID== "MAV16"] <- "yes"
merged$naive[merged$ID== "MAV22"] <- "yes"
merged$naive[merged$ID== "MAV33"] <- "yes"
merged$naive[merged$ID== "MAV32"] <- "no"
merged$previous_therapy[merged$ID== "MAV32"] <- "Lemtrada"
merged$naive[merged$ID== "MAV31"] <- "no"
merged$previous_therapy[merged$ID== "MAV31"] <- "Rebif"
merged$naive[merged$ID== "MAV27"] <- "no"
merged$naive[merged$ID== "MAV23"] <- "yes"
merged$naive[merged$ID== "MAV24"] <- "no"
merged$naive[merged$ID== "MAV25"] <- "yes"


merged$smoking_habit[merged$ID== "MAV31"] <- "no"
merged$smoking_habit[merged$ID== "MAV32"] <- "yes"
merged$smoking_habit[merged$ID== "MAV33"] <- "no"

merged$gc_treatment[merged$ID== "MAV34"] <- "positive"
merged$gc_treatment[merged$ID== "MAV33"] <- "negative"
merged$gc_treatment[merged$ID== "MAV32"] <- "negative"
merged$gc_treatment[merged$ID== "MAV31"] <- "negative"
merged$gc_treatment[merged$ID== "MAV30"] <- "negative"



#eliminare TEC09 
merged <- merged %>%
  filter(ID != "TEC09")

class(merged$lesion_burden)
str(merged)


#####AGGIUNTA DATI TEC CENTRI ESTERNI####
#TEC25 
merged$DATA_ESORDIO [merged$ID== "TEC25"] <- as.Date("2021-02-15")
merged$data_nascita [merged$ID == "TEC25"] <-as.Date("1997-07-21")
merged$data_prelievo_T0 [merged$ID == "TEC25"] <- as.Date("2021-06-03")
merged$data_diagnosi[merged$ID == "TEC25"] <- as.Date("2021-03-15")

merged$EDSS_T12[merged$ID == "TEC25"] <- 1.5
merged$EDSS_T24[merged$ID == "TEC25"] <- 1.5
merged$lesioni_T12[merged$ID == "TEC25"] <- 1
merged$relapse_T12[merged$ID == "TEC25"] <- 0
merged$lesioni_T24[merged$ID == "TEC25"] <- 0
merged$relapse_T24[merged$ID == "TEC25"] <- 0
merged$full_recovery[merged$ID == "TEC25"] <- 1

merged$lesion_burden[merged$ID == "TEC25"] <- 1
merged$bone_marrow_lesions[merged$ID == "TEC25"] <- 0
merged$gadolinium_contrast[merged$ID == "TEC25"] <- 0
merged$subtentorial_lesions[merged$ID == "TEC25"] <- 1
merged$clinical_presentation[merged$ID == "TEC25"] <- "sensory_symptoms"
merged$cambio_terapia[merged$ID =="TEC25"]<- 1
merged$data_cambio_terapia[merged$ID == "TEC25"] <- as.Date("2021-09-01")
merged$NUOVA_TERAPIA[merged$ID == "TEC25"] <- "MAVENCLAD"


#TEC26
merged$DATA_ESORDIO [merged$ID== "TEC26"] <- as.Date("2021-08-15")
merged$data_nascita [merged$ID == "TEC26"] <-as.Date("1996-04-04")
merged$data_prelievo_T0 [merged$ID == "TEC26"] <- as.Date("2021-09-08")
merged$data_diagnosi[merged$ID == "TEC26"] <- as.Date("2021-09-08")

merged$EDSS_T12[merged$ID == "TEC26"] <- 2
merged$EDSS_T24[merged$ID == "TEC26"] <- 2
merged$lesioni_T12[merged$ID == "TEC26"] <- 0
merged$relapse_T12[merged$ID == "TEC26"] <- 0
merged$lesioni_T24[merged$ID == "TEC26"] <- 0
merged$relapse_T24[merged$ID == "TEC26"] <- 0
merged$full_recovery[merged$ID == "TEC26"] <- 0


merged$lesion_burden[merged$ID == "TEC26"] <- 0
merged$bone_marrow_lesions[merged$ID == "TEC26"] <- 1
merged$gadolinium_contrast[merged$ID == "TEC26"] <- 1
merged$subtentorial_lesions[merged$ID == "TEC26"] <- 0
merged$clinical_presentation[merged$ID == "TEC26"] <- "brain_stem"
merged$cambio_terapia[merged$ID =="TEC26"]<- 0

#TEC27 
merged$DATA_ESORDIO [merged$ID== "TEC27"] <- as.Date("2021-10-15")
merged$data_nascita [merged$ID == "TEC27"] <-as.Date("1975-01-11")
merged$data_prelievo_T0 [merged$ID == "TEC27"] <- as.Date("2021-03-31")
merged$data_diagnosi[merged$ID == "TEC27"] <- as.Date("2021-03-31")


merged$EDSS_T12[merged$ID == "TEC27"] <- 2.5
merged$EDSS_T24[merged$ID == "TEC27"] <- 2.5
merged$lesioni_T12[merged$ID == "TEC27"] <- 0
merged$relapse_T12[merged$ID == "TEC27"] <- 0
merged$lesioni_T24[merged$ID == "TEC27"] <- 0
merged$relapse_T24[merged$ID == "TEC27"] <- 0
merged$full_recovery[merged$ID == "TEC27"] <- 1

merged$lesion_burden[merged$ID == "TEC27"] <- 0
merged$bone_marrow_lesions[merged$ID == "TEC27"] <- 1
merged$gadolinium_contrast[merged$ID == "TEC27"] <- 0
merged$subtentorial_lesions[merged$ID == "TEC27"] <- 0
merged$clinical_presentation[merged$ID == "TEC27"] <- "sensory_symptoms"
merged$cambio_terapia[merged$ID =="TEC27"]<- 0

#TEC28
merged$DATA_ESORDIO [merged$ID== "TEC28"] <- as.Date("2020-03-01")
merged$data_nascita [merged$ID == "TEC28"] <-as.Date("1989-05-26")
merged$data_prelievo_T0 [merged$ID == "TEC28"] <- as.Date("2021-08-02")
merged$data_diagnosi[merged$ID == "TEC28"] <- as.Date("2021-07-15")

merged$EDSS_T0 [merged$ID == "TEC28"] <- 2
#
merged$EDSS_T12[merged$ID == "TEC28"] <- 3
merged$EDSS_T24[merged$ID == "TEC28"] <- 3.5
merged$lesioni_T12[merged$ID == "TEC28"] <- 0
merged$relapse_T12[merged$ID == "TEC28"] <- 0
merged$cortisone_T12[merged$ID == "TEC28"] <-0
merged$lesioni_T24[merged$ID == "TEC28"] <- 0
merged$relapse_T24[merged$ID == "TEC28"] <- 0
merged$cortisone_T24[merged$ID == "TEC28"] <-0
#
merged$full_recovery[merged$ID == "TEC28"] <- 1

#TEC29
merged$DATA_ESORDIO [merged$ID== "TEC29"] <- as.Date("2020-06-15")
merged$data_nascita [merged$ID == "TEC29"] <-as.Date("1964-12-23")
merged$data_prelievo_T0 [merged$ID == "TEC29"] <- as.Date("2021-08-02")
merged$data_diagnosi[merged$ID == "TEC29"] <- as.Date("2021-07-15")

merged$EDSS_T0 [merged$ID == "TEC29"] <- 3
merged$EDSS_T12[merged$ID == "TEC29"] <- 3
merged$EDSS_T24[merged$ID == "TEC29"] <- 3.5
merged$lesioni_T12[merged$ID == "TEC29"] <- 0
merged$relapse_T12[merged$ID == "TEC29"] <- 0
merged$cortisone_T12[merged$ID == "TEC29"] <-0
merged$lesioni_T24[merged$ID == "TEC29"] <- 0
merged$relapse_T24[merged$ID == "TEC29"] <- 0
merged$cortisone_T24[merged$ID == "TEC29"] <-0
merged$full_recovery[merged$ID == "TEC29"] <- 0


merged$lesion_burden[merged$ID == "TEC29"] <- 1
merged$bone_marrow_lesions[merged$ID == "TEC29"] <- 0
merged$gadolinium_contrast[merged$ID == "TEC29"] <- 1
merged$subtentorial_lesions[merged$ID == "TEC29"] <- 0
merged$clinical_presentation[merged$ID == "TEC29"] <- "brain_stem"
merged$cambio_terapia[merged$ID =="TEC29"]<- 0

#TEC30
merged$MEDAS [merged$ID== "TEC30"] <- 57
merged$DATA_ESORDIO [merged$ID== "TEC30"] <- as.Date("2020-12-30")
merged$data_nascita [merged$ID == "TEC30"] <-as.Date("")
merged$data_prelievo_T0 [merged$ID == "TEC30"] <- as.Date("2022-01-27")
merged$data_diagnosi[merged$ID == "TEC30"] <- as.Date("2021-01-01")

merged$EDSS_T0 [merged$ID == "TEC30"] <- 0

merged$EDSS_T12[merged$ID == "TEC30"] <- 0
merged$EDSS_T24[merged$ID == "TEC30"] <- 0
merged$lesioni_T12[merged$ID == "TEC30"] <- 0
merged$relapse_T12[merged$ID == "TEC30"] <- 0
merged$cortisone_T12[merged$ID == "TEC30"] <-0
merged$lesioni_T24[merged$ID == "TEC30"] <- 0
merged$relapse_T24[merged$ID == "TEC30"] <- 0
merged$cortisone_T24[merged$ID == "TEC30"] <-0
merged$full_recovery[merged$ID == "TEC30"] <- 1

merged$lesion_burden[merged$ID == "TEC30"] <- 0
merged$bone_marrow_lesions[merged$ID == "TEC30"] <- 1
merged$gadolinium_contrast[merged$ID == "TEC30"] <- 0
merged$subtentorial_lesions[merged$ID == "TEC30"] <- 1
merged$clinical_presentation[merged$ID == "TEC30"] <- "brain_stem"
merged$cambio_terapia[merged$ID =="TEC30"]<- 0

#TEC31
merged$DATA_ESORDIO [merged$ID== "TEC31"] <- as.Date("1987-06-01")
merged$data_nascita [merged$ID == "TEC31"] <-as.Date("")
merged$data_prelievo_T0 [merged$ID == "TEC31"] <- as.Date("2022-03-29")
merged$data_diagnosi[merged$ID == "TEC31"] <- as.Date("1996-06-01")

merged$EDSS_T0 [merged$ID == "TEC31"] <- 1
merged$EDSS_T12[merged$ID == "TEC31"] <- 1
merged$EDSS_T24[merged$ID == "TEC31"] <- 1.5
merged$lesioni_T12[merged$ID == "TEC31"] <- 0
merged$relapse_T12[merged$ID == "TEC31"] <- 0
merged$cortisone_T12[merged$ID == "TEC31"] <-0
merged$lesioni_T24[merged$ID == "TEC31"] <- 0
merged$relapse_T24[merged$ID == "TEC31"] <- 0
merged$cortisone_T24[merged$ID == "TEC31"] <-0
merged$full_recovery[merged$ID == "TEC31"] <- 1

merged$MEDAS [merged$ID == "TEC31"] <- 71
merged$cambio_terapia[merged$ID =="TEC31"]<- 0
merged$bmi [merged$ID == "TEC31"] <- 25.45
merged$smoking_habit [merged$ID == "TEC31"] <- "no"
merged$physical_activity [merged$ID == "TEC31"] <- "active"

merged$lesion_burden[merged$ID == "TEC31"] <- 0
merged$bone_marrow_lesions[merged$ID == "TEC31"] <- 1
merged$gadolinium_contrast[merged$ID == "TEC31"] <- 0
merged$subtentorial_lesions[merged$ID == "TEC31"] <- 0
merged$clinical_presentation[merged$ID == "TEC31"] <- "optic_neuritis"
merged$cambio_terapia[merged$ID =="TEC31"]<- 0

#TEC32
merged$DATA_ESORDIO [merged$ID== "TEC32"] <- as.Date("2019-01-01")
merged$data_nascita [merged$ID == "TEC32"] <-as.Date("1966-05-25")
merged$data_prelievo_T0 [merged$ID == "TEC32"] <- as.Date("2022-07-13")
merged$data_diagnosi[merged$ID == "TEC32"] <- as.Date("2022-07-01")

merged$EDSS_T0 [merged$ID == "TEC32"] <- 2
merged$EDSS_T24[merged$ID == "TEC32"] <- 2
merged$lesioni_T12[merged$ID == "TEC32"] <- 0
merged$relapse_T12[merged$ID == "TEC32"] <- 0
merged$cortisone_T12[merged$ID == "TEC32"] <-0
merged$lesioni_T24[merged$ID == "TEC32"] <- 0
merged$relapse_T24[merged$ID == "TEC32"] <- 0
merged$cortisone_T24[merged$ID == "TEC32"] <-0
merged$full_recovery[merged$ID == "TEC32"] <- 0

merged$MEDAS [merged$ID == "TEC32"] <- 
  merged$bmi [merged$ID == "TEC32"] <- 
  merged$smoking_habit [merged$ID == "TEC32"] <- ""
merged$physical_activity [merged$ID == "TEC32"] <- ""

merged$lesion_burden[merged$ID == "TEC32"] <- 1
merged$bone_marrow_lesions[merged$ID == "TEC32"] <- 1
merged$gadolinium_contrast[merged$ID == "TEC32"] <- 0
merged$subtentorial_lesions[merged$ID == "TEC32"] <- 0
merged$clinical_presentation[merged$ID == "TEC32"] <- "sensory_symptoms"
merged$cambio_terapia[merged$ID =="TEC32"]<- 1
merged$data_cambio_terapia [merged$ID == "TEC32"] <- as.Date("2023-09-29")
merged$NUOVA_TERAPIA [merged$ID == "TEC32"] <- "OCRELIZUMAB"




#TEC13
merged$MEDAS [merged$ID == "TEC13"] <- 57
merged$bmi [merged$ID == "TEC13"] <- 21.83
merged$smoking_habit [merged$ID == "TEC13"] <- "yes"
merged$physical_activity [merged$ID == "TEC13"] <- "active"




merged$data_referto_T6[merged$ID == "TEC26"] <- as.Date("2022-02-22")
merged$relapse_T6[merged$ID == "TEC01"] <- 0
merged$relapse_T6[merged$ID == "TEC02"] <- 0
merged$relapse_T6[merged$ID == "TEC03"] <- 0



merged$MEDAS[60]

merged$relapse_T12[merged$ID == "TEC28"] <- NA
merged$relapse_T6[merged$ID == "TEC08"] <- 0
merged$relapse_T6[merged$ID == "TEC11"] <- 0
merged$relapse_T6[merged$ID == "TEC22"] <- 0
merged$relapse_T6[merged$ID == "TEC24"] <- 1

merged$EDSS_T12[merged$ID == "TEC10"] <- 2
merged$relapse_T12[merged$ID == "TEC01"] <- 0
merged$relapse_T12[merged$ID == "TEC02"] <- 0
merged$relapse_T12[merged$ID == "TEC05"] <- 0
merged$relapse_T12[merged$ID == "TEC08"] <- 1
merged$relapse_T12[merged$ID == "TEC16"] <- 0
merged$relapse_T12[merged$ID == "TEC17"] <- 0
merged$relapse_T12[merged$ID == "TEC18"] <- 0
merged$relapse_T12[merged$ID == "TEC21"] <- 0
merged$relapse_T12[merged$ID == "TEC22"] <- 0
merged$EDSS_T18[merged$ID== "TEC02"] <- 1

merged$relapse_T18[merged$ID== "TEC01"] <- 0
merged$relapse_T18[merged$ID== "TEC01"] <- 0
merged$relapse_T18[merged$ID== "TEC05"] <- 0
merged$relapse_T18[merged$ID== "TEC16"] <- 0
merged$relapse_T18[merged$ID== "TEC20"] <- 0
merged$relapse_T18[merged$ID== "TEC21"] <- 0

merged$relapse_T24[merged$ID== "TEC01"] <- 0
merged$relapse_T24[merged$ID== "TEC02"] <- 0
merged$relapse_T24[merged$ID == "TEC15"] <- NA
merged$relapse_T24[merged$ID== "TEC04"] <- 0
merged$relapse_T24[merged$ID== "TEC05"] <- 0
merged$relapse_T24[merged$ID== "TEC10"] <- 0
merged$relapse_T24[merged$ID== "TEC14"] <- 0
merged$relapse_T24[merged$ID== "TEC08"] <- 1
merged$relapse_T24[merged$ID== "TEC16"] <- 0
merged$relapse_T24[merged$ID== "TEC20"] <- 0
merged$relapse_T24[merged$ID== "TEC22"] <- 1
merged$relapse_T24[merged$ID== "TEC18"] <- 0
merged$relapse_T30[merged$ID== "TEC01"] <- 0
merged$relapse_T30[merged$ID== "TEC05"] <- 0
merged$relapse_T30[merged$ID== "TEC22"] <- 0
merged$relapse_T36[merged$ID== "TEC01"] <- 0
merged$relapse_T36[merged$ID== "TEC02"] <- 0
merged$relapse_T36[merged$ID== "TEC03"] <- 0
merged$relapse_T36[merged$ID== "TEC04"] <- 1
merged$relapse_T36[merged$ID== "TEC05"] <- 0
merged$relapse_T36[merged$ID== "TEC06"] <- 0
merged$relapse_T36[merged$ID== "TEC07"] <- 0
merged$relapse_T36[merged$ID== "TEC10"] <- 0
merged$relapse_T36[merged$ID== "TEC17"] <- 0
merged$relapse_T36[merged$ID== "TEC16"] <- 0


merged$cambio_terapia[merged$ID == "TEC05"] <- 1
merged$cambio_terapia[merged$ID == "TEC08"] <- 1
merged$cambio_terapia[merged$ID == "TEC19"] <- 1
merged$cambio_terapia[merged$ID == "TEC20"] <- 1
merged$cambio_terapia[merged$ID == "TEC23"] <- 1
merged$cambio_terapia[merged$ID == "TEC25"] <- 1
merged$cambio_terapia[merged$ID == "TEC32"] <- 1



merged$cambio_terapia[merged$ID == "TEC01"] <- 0
merged$cambio_terapia[merged$ID == "TEC02"] <- 0
merged$cambio_terapia[merged$ID == "TEC03"] <- 0
merged$cambio_terapia[merged$ID == "TEC04"] <- 0
merged$cambio_terapia[merged$ID == "TEC06"] <- 0
merged$cambio_terapia[merged$ID == "TEC07"] <- 0
merged$cambio_terapia[merged$ID == "TEC10"] <- 0
merged$cambio_terapia[merged$ID == "TEC11"] <- 0
merged$cambio_terapia[merged$ID == "TEC12"] <- 0
merged$cambio_terapia[merged$ID == "TEC14"] <- 0
merged$cambio_terapia[merged$ID == "TEC15"] <- 0
merged$cambio_terapia[merged$ID == "TEC16"] <- 0
merged$cambio_terapia[merged$ID == "TEC17"] <- 0
merged$cambio_terapia[merged$ID == "TEC24"] <- 0
merged$cambio_terapia[merged$ID == "TEC26"] <- 0
merged$cambio_terapia[merged$ID == "TEC27"] <- 0
merged$cambio_terapia[merged$ID == "TEC28"] <- 0
merged$cambio_terapia[merged$ID == "TEC29"] <- 0
merged$cambio_terapia[merged$ID == "TEC30"] <- 0
merged$cambio_terapia[merged$ID == "TEC31"] <- 0

merged$naive[merged$ID == "TEC13"] <- "no"
merged$naive[merged$ID == "TEC23"] <- "yes"
merged$previous_therapy[merged$ID == "TEC13"] <- "interferon_beta"
merged$relapse_T30[merged$ID == "TEC15"] <- NA
merged$bmi[merged$ID == "TEC32"] <- NA
merged$antibiotic_use[merged$ID == "TEC25"] <- "no"
merged$antibiotic_use[merged$ID == "TEC29"] <- "no"

merged$sample_type[grepl("^TEC", merged$ID)] <- "sample"
merged$simplified_batch[grepl("^TEC", merged$ID)] <- "1-3"
merged$disease_activity[grepl("^TEC", merged$ID)] <- "lower_activity"

merged$glc_date_before_t0[merged$ID == "TEC29"] <- as.Date("2021-07-15")
merged$gc_treatment[merged$ID == "TEC29"] <- "positive"

merged$lesion_burden[merged$ID == "TEC18"] <- 0
merged$bone_marrow_lesions[merged$ID == "TEC18"] <- 0
merged$subtentorial_lesions[merged$ID == "TEC18"] <- 1
merged$subtentorial_lesions[merged$ID == "TEC32"] <- NA
merged$gadolinium_contrast[merged$ID == "TEC18"] <- 0
merged$clinical_presentation[merged$ID == "TEC18"] <- "brain_stem"
merged$full_recovery[merged$ID == "TEC18"] <- 0


merged <- merged %>%
  mutate(ID_tmp = ID) %>%
  relocate(ID_tmp, .after = full_recovery)

merged$ID_tmp <- NULL

#MEDAS RACHELE 8/7
merged$MEDAS[merged$ID== "TEC29"] <- 50
merged$MEDAS[merged$ID== "MAV31"] <- 57
merged$MEDAS[merged$ID== "TEC26"] <- 57
merged$MEDAS[merged$ID== "TEC29"] <- 50


##### salvataggio di merged
#AGGIUNGI E SALVA
merged$gc_treatment[merged$ID== "MAV34"] <- "positive"
merged$gc_treatment[merged$ID== "MAV33"] <- "negative"
merged$gc_treatment[merged$ID== "MAV32"] <- "negative"
merged$gc_treatment[merged$ID== "MAV31"] <- "negative"
merged$gc_treatment[merged$ID== "MAV30"] <- "negative"


#nuovi dati 30/06
merged$cambio_terapia[merged$ID== "MAV32"] <- "0"
merged$cambio_terapia[merged$ID== "MAV31"] <- "0"
merged$cambio_terapia[merged$ID== "MAV33"] <- "0"

merged$gc_treatment[merged$ID== "MAV32"] <- "negative"
merged$gc_treatment[merged$ID== "MAV31"] <- "negative"
merged$gc_treatment[merged$ID== "MAV33"] <- "negative"

#COLONNA GROUP
merged <- merged %>%
  mutate(
    GROUP = case_when(
      therapy == "cladribine" ~ "MAV",
      therapy == "dimethyl_fumarate" ~ "TEC",
      TRUE ~ NA_character_
    )
  )

#aggiunta dati al 9 luglio con dataset alessandro
#TEC
merged$EDSS_T30[merged$ID == "TEC02"] <- 1
merged$relapse_T30[merged$ID == "TEC02"] <- 0
merged$cortisone_T30[merged$ID == "TEC02"] <- 0
merged$EDSS_T30[merged$ID == "TEC02"] <- 1
merged$EDSS_T18[merged$ID == "TEC04"] <- 1
merged$lesioni_T18[merged$ID == "TEC04"] <- 0
merged$relapse_T18[merged$ID == "TEC04"] <- 0
merged$cortisone_T18[merged$ID == "TEC04"] <- 0
merged$EDSS_T30[merged$ID == "TEC04"] <- 1
merged$EDSS_T6[merged$ID == "TEC05"] <- 1
merged$lesioni_T6[merged$ID == "TEC05"] <- 0
merged$relapse_T6[merged$ID == "TEC05"] <- 0
merged$cortisone_T6[merged$ID == "TEC05"] <- 0
merged$EDSS_T6[merged$ID == "TEC06"] <- 1
merged$lesioni_T12[merged$ID == "TEC06"] <- 0
merged$relapse_T12[merged$ID == "TEC06"] <- 0
merged$EDSS_T18[merged$ID == "TEC06"] <- 1
merged$lesioni_T18[merged$ID == "TEC06"] <- 0
merged$relapse_T18[merged$ID == "TEC06"] <- 0
merged$lesioni_T24[merged$ID == "TEC06"] <- 0
merged$relapse_T24[merged$ID == "TEC06"] <- 0
merged$EDSS_T30[merged$ID == "TEC06"] <- 0
merged$lesioni_T30[merged$ID == "TEC06"] <- 0
merged$relapse_T30[merged$ID == "TEC06"] <- 0
merged$lesioni_T12[merged$ID == "TEC07"] <- 0
merged$relapse_T12[merged$ID == "TEC07"] <- 0
merged$cortisone_T12[merged$ID == "TEC07"] <- 0
merged$EDSS_T18[merged$ID == "TEC07"] <- 1
merged$lesioni_T18[merged$ID == "TEC07"] <- 0
merged$relapse_T18[merged$ID == "TEC07"] <- 0
merged$cortisone_T18[merged$ID == "TEC07"] <- 0
merged$lesioni_T24[merged$ID == "TEC07"] <- 0
merged$relapse_T24[merged$ID == "TEC07"] <- 0
merged$cortisone_T24[merged$ID == "TEC07"] <- 0

merged$lesioni_T18[merged$ID == "TEC08"] <- 0
merged$relapse_T18[merged$ID == "TEC08"] <- 1

merged$EDSS_T6[merged$ID == "TEC10"] <- 1.5
merged$EDSS_T18[merged$ID == "TEC10"] <- 2

merged$EDSS_T30[merged$ID == "TEC10"] <- 1.5
merged$lesioni_T30[merged$ID == "TEC10"] <- 0
merged$relapse_T30[merged$ID == "TEC10"] <- 0
merged$cortisone_T30[merged$ID == "TEC10"] <- 0

merged$EDSS_T6[merged$ID == "TEC15"] <- 0
merged$lesioni_T6[merged$ID == "TEC15"] <- 0
merged$relapse_T6[merged$ID == "TEC15"] <- 0
merged$cortisone_T6[merged$ID == "TEC15"] <- 0

merged$EDSS_T6[merged$ID == "TEC16"] <- 1
merged$lesioni_T6[merged$ID == "TEC16"] <- 1
merged$relapse_T6[merged$ID == "TEC16"] <- 0
merged$cortisone_T6[merged$ID == "TEC16"] <- 0
merged$EDSS_T30[merged$ID == "TEC16"] <- 1
merged$lesioni_T30[merged$ID == "TEC16"] <- 0
merged$relapse_T30[merged$ID == "TEC16"] <- 0
merged$cortisone_T30[merged$ID == "TEC16"] <- 0

merged$EDSS_T6[merged$ID == "TEC17"] <- 0
merged$lesioni_T6[merged$ID == "TEC17"] <- 0
merged$relapse_T6[merged$ID == "TEC17"] <- 0
merged$cortisone_T6[merged$ID == "TEC17"] <- 0
merged$lesioni_T18[merged$ID == "TEC17"] <- 0
merged$relapse_T18[merged$ID == "TEC17"] <- 0
merged$cortisone_T18[merged$ID == "TEC17"] <- 0
merged$lesioni_T24[merged$ID == "TEC17"] <- 0
merged$relapse_T24[merged$ID == "TEC17"] <- 0
merged$cortisone_T24[merged$ID == "TEC17"] <- 0
merged$lesioni_T30[merged$ID == "TEC17"] <- 0
merged$relapse_T30[merged$ID == "TEC17"] <- 0
merged$cortisone_T30[merged$ID == "TEC17"] <- 0

merged$EDSS_T6[merged$ID == "TEC18"] <- 1.5
merged$relapse_T6[merged$ID == "TEC18"] <- 0
merged$cortisone_T6[merged$ID == "TEC18"] <- 0
merged$EDSS_T18[merged$ID == "TEC18"] <- 1.5
merged$relapse_T18[merged$ID == "TEC18"] <- 0
merged$cortisone_T18[merged$ID == "TEC18"] <- 0

merged$EDSS_T6[merged$ID== "TEC19"] <- 1
merged$lesioni_T12[merged$ID== "TEC20"] <- 0
merged$relapse_T12[merged$ID== "TEC20"] <- 0
merged$cortisone_T12[merged$ID== "TEC20"] <- 0

merged$EDSS_T6[merged$ID== "TEC21"] <- 1.5
merged$lesioni_T6[merged$ID== "TEC21"] <- 0
merged$relapse_T6[merged$ID== "TEC21"] <- 0
merged$cortisone_T6[merged$ID== "TEC21"] <- 0

merged$EDSS_T18[merged$ID== "TEC22"] <- 1.5
merged$lesioni_T18[merged$ID== "TEC22"] <- 0
merged$relapse_T18[merged$ID== "TEC22"] <- 0
merged$cortisone_T18[merged$ID== "TEC22"] <- 0

merged$EDSS_T36[merged$ID== "TEC22"] <- 1
merged$lesioni_T36[merged$ID== "TEC22"] <- 0
merged$relapse_T36[merged$ID== "TEC22"] <- 0
merged$cortisone_T36[merged$ID== "TEC22"] <- 0


merged$EDSS_T6[merged$ID== "TEC23"] <- 0
merged$lesioni_T6[merged$ID== "TEC23"] <- 0
merged$relapse_T6[merged$ID== "TEC23"] <- 0
merged$cortisone_T6[merged$ID== "TEC23"] <- 0

merged$EDSS_T18[merged$ID== "TEC23"] <- 0
merged$lesioni_T18[merged$ID== "TEC23"] <- 0
merged$relapse_T18[merged$ID== "TEC23"] <- 0
merged$cortisone_T18[merged$ID== "TEC23"] <- 0

merged$EDSS_T24[merged$ID== "TEC23"] <- 0
merged$lesioni_T24[merged$ID== "TEC23"] <- 0
merged$relapse_T24[merged$ID== "TEC23"] <- 0
merged$cortisone_T24[merged$ID== "TEC23"] <- 0

merged$EDSS_T6[merged$ID== "TEC24"] <- 1
merged$relapse_T12[merged$ID== "TEC24"] <- 1

merged$EDSS_T6[merged$ID== "TEC25"] <- 1.5
merged$EDSS_T18[merged$ID== "TEC25"] <- 1.5

merged$EDSS_T6[merged$ID== "TEC26"] <- 2
merged$lesioni_T6[merged$ID== "TEC26"] <- 0
merged$relapse_T6[merged$ID== "TEC26"] <- 0
merged$EDSS_T18[merged$ID== "TEC26"] <- 2
merged$lesioni_T18[merged$ID== "TEC26"] <- 0
merged$relapse_T18[merged$ID== "TEC26"] <- 0

merged$EDSS_T6[merged$ID== "TEC27"] <- 2
merged$lesioni_T6[merged$ID== "TEC27"] <- 0
merged$relapse_T6[merged$ID== "TEC27"] <- 0
merged$cortisone_T6[merged$ID== "TEC27"] <- 0
merged$cortisone_T12[merged$ID== "TEC27"] <- 0
merged$EDSS_T18[merged$ID== "TEC27"] <- 2.5
merged$lesioni_T18[merged$ID== "TEC27"] <- 0
merged$relapse_T18[merged$ID== "TEC27"] <- 0
merged$cortisone_T18[merged$ID== "TEC27"] <- 0
merged$cortisone_T24[merged$ID== "TEC27"] <- 0

merged$EDSS_T6[merged$ID== "TEC28"] <- 2
merged$lesioni_T6[merged$ID== "TEC28"] <- 0
merged$relapse_T6[merged$ID== "TEC28"] <- 0
merged$cortisone_T6[merged$ID== "TEC28"] <- 0
merged$relapse_T12[merged$ID== "TEC28"] <- 0
merged$EDSS_T18[merged$ID== "TEC28"] <- 3
merged$lesioni_T18[merged$ID== "TEC28"] <- 0
merged$relapse_T18[merged$ID== "TEC28"] <- 0
merged$cortisone_T18[merged$ID== "TEC28"] <- 0
merged$sex[merged$ID== "TEC32"] <- "F"

merged$EDSS_T6[merged$ID== "TEC29"] <- 3
merged$EDSS_T18[merged$ID== "TEC29"] <- 3
merged$lesioni_T18[merged$ID== "TEC29"] <- 0
merged$relapse_T18[merged$ID== "TEC29"] <- 0
merged$cortisone_T18[merged$ID== "TEC29"] <- 0

merged$EDSS_T6[merged$ID== "TEC30"] <- 0
merged$lesioni_T6[merged$ID== "TEC30"] <- 0
merged$relapse_T6[merged$ID== "TEC30"] <- 0
merged$cortisone_T6[merged$ID== "TEC30"] <- 0
merged$EDSS_T18[merged$ID== "TEC30"] <- 0
merged$lesioni_T18[merged$ID== "TEC30"] <- 0
merged$relapse_T18[merged$ID== "TEC30"] <- 0
merged$cortisone_T18[merged$ID== "TEC30"] <- 0


merged$EDSS_T6[merged$ID== "TEC31"] <- 1
merged$lesioni_T6[merged$ID== "TEC31"] <- 0
merged$relapse_T6[merged$ID== "TEC31"] <- 0
merged$cortisone_T6[merged$ID== "TEC31"] <- 0
merged$EDSS_T18[merged$ID== "TEC31"] <- 1
merged$lesioni_T18[merged$ID== "TEC31"] <- 0
merged$relapse_T18[merged$ID== "TEC31"] <- 0
merged$cortisone_T18[merged$ID== "TEC31"] <- 0


merged$EDSS_T6[merged$ID== "TEC32"] <- 2
merged$lesioni_T6[merged$ID== "TEC32"] <- 0
merged$relapse_T6[merged$ID== "TEC32"] <- 0
merged$cortisone_T6[merged$ID== "TEC32"] <- 0
merged$EDSS_T12[merged$ID== "TEC32"] <- 2
merged$lesioni_T12[merged$ID== "TEC32"] <- 0
merged$relapse_T12[merged$ID== "TEC32"] <- 0
merged$cortisone_T12[merged$ID== "TEC32"] <- 0
merged$EDSS_T18[merged$ID== "TEC32"] <- 2
merged$lesioni_T18[merged$ID== "TEC32"] <- 0
merged$relapse_T18[merged$ID== "TEC32"] <- 0
merged$cortisone_T18[merged$ID== "TEC32"] <- 0
merged$naive[merged$ID== "TEC32"] <- "no"
merged$naive[merged$ID== "TEC29"] <- "no"
merged$naive[merged$ID== "TEC25"] <- "no"
merged$gc_treatment[merged$ID== "TEC11"] <- "positive"

#MAV
merged$naive[merged$ID== "MAV25"] <- "no"
merged$naive[merged$ID== "MAV27"] <- "yes"
merged$naive[merged$ID== "MAV32"] <- "yes"
merged$naive[merged$ID== "MAV33"] <- "no"
merged$gc_treatment[merged$ID== "MAV07"] <- "positive"
merged$gc_treatment[merged$ID== "MAV09"] <- "positive"

merged$naive[merged$ID== "MAV34"] <- "no"

merged$EDSS_T6[merged$ID== "MAV09"] <- 3.5
merged$lesioni_T6[merged$ID== "MAV09"] <- 0
merged$relapse_T6[merged$ID== "MAV09"] <- 0

merged$EDSS_T6[merged$ID== "MAV10"] <- 5
merged$lesioni_T6[merged$ID== "MAV10"] <- 1
merged$relapse_T6[merged$ID== "MAV10"] <- 0
merged$cortisone_T6[merged$ID== "MAV10"] <- 0

merged$EDSS_T24[merged$ID== "MAV12"] <- 0
merged$lesioni_T24[merged$ID== "MAV12"] <- 0
merged$relapse_T24[merged$ID== "MAV12"] <- 0
merged$cortisone_T24[merged$ID== "MAV12"] <- 0

merged$EDSS_T6[merged$ID== "MAV14"] <- 1.5
merged$lesioni_T6[merged$ID== "MAV14"] <- 0
merged$relapse_T6[merged$ID== "MAV14"] <- 0
merged$cortisone_T6[merged$ID== "MAV14"] <- 0
merged$EDSS_T18[merged$ID== "MAV14"] <- 1.5
merged$lesioni_T18[merged$ID== "MAV14"] <- 0
merged$relapse_T18[merged$ID== "MAV14"] <- 0
merged$cortisone_T18[merged$ID== "MAV14"] <- 0

merged$EDSS_T6[merged$ID== "MAV15"] <- 5
merged$lesioni_T6[merged$ID== "MAV15"] <- 0
merged$relapse_T6[merged$ID== "MAV15"] <- 0
merged$cortisone_T6[merged$ID== "MAV15"] <- 0
merged$EDSS_T24[merged$ID== "MAV15"] <- 4.5
merged$lesioni_T24[merged$ID== "MAV15"] <- 0
merged$relapse_T24[merged$ID== "MAV15"] <- 0
merged$cortisone_T24[merged$ID== "MAV15"] <- 0

merged$EDSS_T6[merged$ID== "MAV16"] <- 3.5

merged$EDSS_T6[merged$ID== "MAV17"] <- 2
merged$lesioni_T6[merged$ID== "MAV17"] <- 0
merged$relapse_T6[merged$ID== "MAV17"] <- 0
merged$cortisone_T6[merged$ID== "MAV17"] <- 0

merged$EDSS_T18[merged$ID== "MAV19"] <- 0
merged$lesioni_T18[merged$ID== "MAV19"] <- 0
merged$relapse_T18[merged$ID== "MAV19"] <- 0
merged$cortisone_T18[merged$ID== "MAV19"] <- 0

merged$EDSS_T6[merged$ID== "MAV20"] <- 1

merged$EDSS_T6[merged$ID== "MAV21"] <- 2
merged$EDSS_T18[merged$ID== "MAV21"] <- 1

merged$lesioni_T12[merged$ID== "MAV23"] <- 0
merged$relapse_T12[merged$ID== "MAV23"] <- 0
merged$cortisone_T12[merged$ID== "MAV23"] <- 0

merged$lesioni_T12[merged$ID== "MAV24"] <- 0
merged$relapse_T12[merged$ID== "MAV24"] <- 0
merged$cortisone_T12[merged$ID== "MAV24"] <- 0
merged$lesioni_T24[merged$ID== "MAV24"] <- 0
merged$relapse_T24[merged$ID== "MAV24"] <- 0
merged$cortisone_T24[merged$ID== "MAV24"] <- 0
merged$EDSS_T30[merged$ID== "MAV24"] <- 1
merged$lesioni_T30[merged$ID== "MAV24"] <- 0
merged$relapse_T30[merged$ID== "MAV24"] <- 0
merged$cortisone_T30[merged$ID== "MAV24"] <- 0

merged$EDSS_T24[merged$ID== "MAV25"] <- 0
merged$lesioni_T24[merged$ID== "MAV25"] <- 0
merged$relapse_T24[merged$ID== "MAV25"] <- 0
merged$cortisone_T24[merged$ID== "MAV25"] <- 0

merged$EDSS_T6[merged$ID== "MAV27"] <- 0
merged$EDSS_T18[merged$ID== "MAV27"] <- 0

merged$EDSS_T6[merged$ID== "MAV28"] <- 1.5
merged$lesioni_T6[merged$ID== "MAV28"] <- 0
merged$relapse_T6[merged$ID== "MAV28"] <- 0
merged$cortisone_T6[merged$ID== "MAV28"] <- 0
merged$EDSS_T18[merged$ID== "MAV28"] <- 0
merged$lesioni_T18[merged$ID== "MAV28"] <- 0
merged$relapse_T18[merged$ID== "MAV28"] <- 0
merged$cortisone_T18[merged$ID== "MAV28" ] <- 0

merged$EDSS_T6[merged$ID== "MAV31"] <- 2.5
merged$lesioni_T6[merged$ID== "MAV31"] <- 0
merged$relapse_T6[merged$ID== "MAV31"] <- 0
merged$EDSS_T18[merged$ID== "MAV31"] <- 2.5
merged$relapse_T18[merged$ID== "MAV31"] <- 0

merged$EDSS_T6[merged$ID== "MAV32"] <- 4
merged$lesioni_T6[merged$ID== "MAV32"] <- 0
merged$relapse_T6[merged$ID== "MAV32"] <- 0
merged$EDSS_T18[merged$ID== "MAV32"] <- 4
merged$lesioni_T18[merged$ID== "MAV32"] <- 0
merged$relapse_T18[merged$ID== "MAV32" ] <- 0

merged$EDSS_T6[merged$ID== "MAV33"] <- 2.5
merged$lesioni_T6[merged$ID== "MAV33"] <- 0
merged$relapse_T6[merged$ID== "MAV33"] <- 0
merged$EDSS_T18[merged$ID== "MAV33"] <- 2.5
merged$lesioni_T18[merged$ID== "MAV33"] <- 0
merged$relapse_T18[merged$ID== "MAV33"] <- 0

merged$EDSS_T6[merged$ID== "MAV34"] <- 2
merged$lesioni_T6[merged$ID== "MAV34"] <- 0
merged$relapse_T6[merged$ID== "MAV34"] <- 0
merged$EDSS_T18[merged$ID== "MAV34"] <- 2
merged$lesioni_T18[merged$ID== "MAV34"] <- 0
merged$relapse_T18[merged$ID== "MAV34"] <- 0



#SOVRASCRIZIONE MERGED
write.csv2(merged,
           "C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/merged.csv",
           row.names = FALSE, 
           fileEncoding = "UTF-8")

#apertura di merged
merged <- read.csv2("input/merged.csv")






