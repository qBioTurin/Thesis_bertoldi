#per aprire correttamente il file
library(readxl)
metadati_pazienti <- read_excel("input/metadata_pazienti.xlsx")
meta_paper <- read_excel("input/metadata_paper.xlsx")

metadati_pazienti<- metadati_pazienti_lesioni_ecc_2%>% filter(grepl("^(TEC|MAV)", ID))

metadati_pazienti <- metadati_pazienti[ , -2]

str(metadati_pazienti)
#DATE DA POSIX
metadati_pazienti$`data di nascita`<- as.Date(metadati_pazienti$`data di nascita`, origin= "1899-12-30")

metadati_pazienti$data_diagnosi<- as.Date(metadati_pazienti$data_diagnosi, origin= "1899-12-30")

metadati_pazienti$data_referto_T6<- as.Date(metadati_pazienti$data_referto_T6, origin= "1899-12-30")

metadati_pazienti$data_referto_T12<- as.Date(metadati_pazienti$data_referto_T12, origin= "1899-12-30")

metadati_pazienti$data_referto_T18<- as.Date(metadati_pazienti$data_referto_T18, origin= "1899-12-30")

metadati_pazienti$data_referto_T30<- as.Date(metadati_pazienti$data_referto_T30, origin= "1899-12-30")

metadati_pazienti$data_referto_T36<- as.Date(metadati_pazienti$data_referto_T36, origin= "1899-12-30")

#DATE DA NUMERIC
metadati_pazienti$`DATA ESORDIO` <- as.Date(metadati_pazienti$`DATA ESORDIO`, origin = "1899-12-30")

#DATE DA CHARACTER
metadati_pazienti$data_prelievo_T0 <- as.Date(as.numeric(metadati_pazienti$data_prelievo_T0), origin="1899-12-30")
metadati_pazienti$data_referto_T24 <- as.Date(as.numeric(metadati_pazienti$data_referto_T24), origin= "1899-12-30")

metadati_pazienti$data_cambio_terapia <- as.Date(as.numeric(metadati_pazienti$data_cambio_terapia), origin= "1899-12-30")

#da numeriche a categoriche 
metadati_pazienti$lesioni_T6 <- factor(metadati_pazienti$lesioni_T6)
metadati_pazienti$lesioni_T12 <- factor(metadati_pazienti$lesioni_T12)
metadati_pazienti$`lesioni T18` <- factor(metadati_pazienti$`lesioni T18`)
metadati_pazienti$`lesioni T24` <- factor(metadati_pazienti$`lesioni T24`)
metadati_pazienti$`lesioni T30` <- factor(metadati_pazienti$`lesioni T30`)
metadati_pazienti$`lesioni T36` <- factor(metadati_pazienti$`lesioni T36`)

metadati_pazienti$relapse_T6 <- factor(metadati_pazienti$relapse_T6)
metadati_pazienti$relapse_T12 <- factor(metadati_pazienti$relapse_T12)
metadati_pazienti$`relapse T18`<- factor(metadati_pazienti$`relapse T18`)
metadati_pazienti$`relapse T24` <- factor(metadati_pazienti$`relapse T24`)
metadati_pazienti$`relapse T30` <- factor(metadati_pazienti$`relapse T30`)
metadati_pazienti$`relapse T36` <- factor(metadati_pazienti$`relapse T36`)

metadati_pazienti$cambio_terapia<- factor(metadati_pazienti$cambio_terapia)


#CAMBIO NOMI COLONNE
metadati_pazienti<- metadati_pazienti %>% rename (NUOVA_TERAPIA = `nuova TERAPIA`)
metadati_pazienti<- metadati_pazienti %>% rename (data_nascita = `data di nascita`)
metadati_pazienti<- metadati_pazienti %>% rename (DATA_ESORDIO = `DATA ESORDIO`)
metadati_pazienti<- metadati_pazienti %>% rename(lesioni_T18 = `lesioni T18`)
metadati_pazienti<- metadati_pazienti %>% rename (relapse_T18 = `relapse T18`)
metadati_pazienti<- metadati_pazienti %>% rename (cortisone_T18 = `cortisone T18`)
metadati_pazienti<- metadati_pazienti %>% rename (lesioni_T24 = `lesioni T24`)
metadati_pazienti<- metadati_pazienti %>% rename (relapse_T24 = `relapse T24`)
metadati_pazienti<- metadati_pazienti %>% rename (cortisone_T24 = `cortisone T24`)
metadati_pazienti<- metadati_pazienti %>% rename (lesioni_T30 = `lesioni T30`)
metadati_pazienti<- metadati_pazienti %>% rename(relapse_T30 = `relapse T30`)
metadati_pazienti<- metadati_pazienti %>% rename(cortisone_T30 = `cortisone T30`)
metadati_pazienti<- metadati_pazienti %>% rename(lesioni_T36 = `lesioni T36`)
metadati_pazienti<- metadati_pazienti %>% rename (relapse_T36 = `relapse T36`)
metadati_pazienti<- metadati_pazienti %>% rename (cortisone_T36 = `cortisone T36`)
metadati_pazienti<- metadati_pazienti %>% rename (NUOVA_TERAPIA = `nuova TERAPIA`)


meta_paper<- metadata_paper%>% filter(grepl("^(TEC|MAV)", id))

meta_paper <- meta_paper %>% 
  rename(ID = id)

meta_paper <- meta_paper[ , -c(1,3,13,23,24,25,26,27,37,38,39,40,41,42,47)]
ncol(meta_paper)

meta_paper$ID <- sub("T0$", "", meta_paper$ID)

merged_data <- full_join(metadati_pazienti, meta_paper, by = "ID")


merged_data$naive[merged_data$ID == "MAV07"] <- "no"


#COMPLETARE COLONNA THERAPY 
merged_data <- merged_data %>%
  mutate(therapy = ifelse(ID == "TEC", "dimethyl_fumarate", therapy))

merged_data <- merged_data %>%
  mutate(therapy = ifelse(ID == "MAV", "cladribine", therapy))

merged_data$therapy[grepl("TEC", merged_data$ID)] <- "dimethyl_fumarate"
merged_data$therapy[grepl("MAV", merged_data$ID)] <- "cladribine"

#INSERIMENTO SEX MANCANTI 
merged_data$sex[merged_data$ID == "MAV07"] <- "F"
merged_data$sex[merged_data$ID == "MAV08"] <- "M"
merged_data$sex[merged_data$ID == "MAV12"] <- "F"
merged_data$sex[merged_data$ID == "MAV15"] <- "F"
merged_data$sex[merged_data$ID == "MAV16"] <- "F"
merged_data$sex[merged_data$ID == "MAV20"] <- "F"
merged_data$sex[merged_data$ID == "MAV22"] <- "M"
merged_data$sex[merged_data$ID == "MAV23"] <- "F"
merged_data$sex[merged_data$ID == "MAV24"] <- "F"
merged_data$sex[merged_data$ID == "MAV25"] <- "F"
merged_data$sex[merged_data$ID == "MAV31"] <- "F"
merged_data$sex[merged_data$ID == "MAV34"] <- "M"

merged_data$sex[merged_data$ID == "TEC13"] <- "F"
merged_data$sex[merged_data$ID == "TEC18"] <- "M"
merged_data$sex[merged_data$ID == "TEC23"] <- "M"
merged_data$sex[merged_data$ID == "TEC25"] <- "F"
merged_data$sex[merged_data$ID == "TEC29"] <- "M"

#INSERIRE AGE AGGIORNATA RISPETTO COLONNA 
install.packages("lubridate")
library(lubridate)
merged_data$age <- floor(interval(merged_data$data_nascita, Sys.Date()) / years(1))

class(merged_data$sample_collection_date)

#aggiungo data_prelievo_T0 mancante
library(dplyr)
merged_data<- merged_data %>%
  mutate(data_prelievo_T0 = if_else(ID == "TEC19",
                                    as.Date("2022-10-18"),
                                    data_prelievo_T0))
merged_data<- merged_data %>%
  mutate(data_prelievo_T0 = if_else(ID == "TEC23",
                                    as.Date("2023-06-26"),
                                    data_prelievo_T0))

#cambiare simplified_batch in 1-3
class(merged_data$simplified_batch)
merged_data$simplified_batch[merged_data$simplified_batch == "3-Jan"] <- "1-3"

merged_data$simplified_batch <- factor(merged_data$simplified_batch,
                              levels = c("1-3", "4-5"))


Input/
  .gitignore
