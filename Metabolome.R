#apertura file metabolomica 
source("library.R")

 # matrice metabolomica
mat <- read_excel("C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/metabolome_countmatrix.xlsx")

#salvataggio mat_t (trasposta)
write.csv2(mat_t,
           "C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/mat_t.csv",
           row.names = FALSE, 
           fileEncoding = "UTF-8")
#apertura di mat_t
mat_t <- read.csv2("input/mat_t.csv")

#salvataggio mat_t_filtered (rule 50%)
write.csv2(mat_t_filtered,
           "C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/mat_t_filtered.csv",
           row.names = FALSE, 
           fileEncoding = "UTF-8")

#apertura di mat_t_filtered
mat_t_filtered <- read.csv2("input/mat_t_filtered.csv")

# annotazioni
annot <- read_excel("C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/metabolome_keyannotation.xlsx")


# batch
batch <- read_excel("C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/metabolome_batch.xlsx")
#salvataggio e apertura di BATCH DEFINITIVO
write.csv2(batch,
           "C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/batch.csv",
           row.names = FALSE, 
           fileEncoding = "UTF-8")

batch <- read.csv2("input/batch.csv")

#METADATI COORTE SANI 
metadati_sani <- readxl::read_excel("metadati_sani.xlsx")

#modifiche dataset 
metadati_sani <- metadati_sani %>%
  mutate(MEDAS = MEDAS * 100)

#sovrascrivo il file
write.csv2(metadati_sani,
           "C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/metadati_sani.csv",
           row.names = FALSE, 
           fileEncoding = "UTF-8")

#apertura metadati_sani SOVRASCRITTO in csv
metadati_sani <- read.csv2("input/metadati_sani.csv")

# sovrascrizione Combat_batch2
write.csv2(Combat_batch2,
           "C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/Combat_batch2.csv",
           row.names = FALSE, 
           fileEncoding = "UTF-8")
#apertura Combat_batch2
Combat_batch2 <- read.delim("input/Combat_batch2.csv", sep = ";", header = TRUE)

##########################################################################################

#modifico e elimino nomi di samples (ctr, new)
colnames(mat)[colnames(mat) == "RR_old"] <- "ctr_16"
colnames(mat)[colnames(mat) == "MB_old"] <- "ctr_17"
colnames(mat)[colnames(mat) == "AM_old"] <- "ctr_15"

mat <- mat[, colnames(mat) != "ctr_15_new...29"]
mat <- mat[, colnames(mat) != "ctr_15_new...30"]
mat <- mat[, colnames(mat) != "ctr_15_new...31"]
mat <- mat[, colnames(mat) != "TEC_02_t0_old"]
mat <- mat[, colnames(mat) != "TEC_07_t0_old"]
mat <- mat[, colnames(mat) != "TEC_22_t0_old"]

colnames(mat)[colnames(mat) == "TEC_07_t0_new"] <- "TEC_07_t0"
colnames(mat)[colnames(mat) == "TEC_02_t0_new"] <- "TEC_02_t0"
colnames(mat)[colnames(mat) == "TEC_22_t0_new"] <- "TEC_22_t0"

colnames(mat_t)[colnames(mat_t) == "Labels"] <- "Class"

mat_t <- mat %>% #mat_t è la matrice originale MA trasposta 
  column_to_rownames(var = names(mat)[1]) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Samples")

mat_t_clean <- mat_t[mat_t$Labels != "qc", ] #mat__clean è la matrice solo con sani e malati (NO QC)



#MATCHING NOMI TRA BATCH E mat_t (senza qc) (modifico batch)
#ctr
batch$id <- ifelse(
  grepl("ctrl", batch$id),
  paste0(
    "ctr_",
    gsub(".*ctrl\\s*", "", batch$id)  # prende solo il numero dopo ctrl
  ),
  batch$id
)

#MAV
batch$id <- ifelse(
  grepl("^MAV[0-9]+T[0-9]+$", batch$id),
  gsub("^([A-Z]+)([0-9]+)T([0-9]+)$", "\\1_\\2_t\\3", batch$id),
  batch$id
)

#MS
batch$id <- ifelse(
  grepl("^MS[0-9]+$", batch$id),
  gsub("^MS([0-9]+)$", "MS_\\1_t0", batch$id),
  batch$id
)

#TEC
batch$id <- ifelse(
  grepl("^TEC[0-9]+T[0-9]+$", batch$id),
  gsub("^TEC([0-9]+)T([0-9]+)$", "TEC_\\1_t\\2", batch$id),
  batch$id
)

#modifiche id completamente irregolari 
batch$id[grepl("ctr_34", batch$id)] <- "ctr_34"
batch$id[batch$id == "MS_3_t0"] <- "MS_3"
batch$id[batch$id == "MS_4_t0"] <- "MS_4"
batch$id[batch$id == "MS_5_t0"] <- "MS_5"
batch$id[batch$id == "MS_6_t0"] <- "MS_6"
batch$id[batch$id == "MS_7_t0"] <- "MS_7"
batch$id[batch$id == "MS_8_t0"] <- "MS_8"
batch$id[batch$id == "MS_9_t0"] <- "MS_9"

batch$id[batch$id == "MS_23_t0"] <- "MS_23"
batch$id[batch$id == "MS_24_t0"] <- "MS_24"
batch$id[batch$id == "MS_25_t0"] <- "MS_25"

#inserisco colonna BATCH_metabolomica in mat_t
mat_t$batch_metabolomica <- batch$batch_metabolomica[
  match(mat_t$Samples, batch$id)
]

mat_t <- mat_t[, c(
  1, 2,
  ncol(mat_t),
  3:(ncol(mat_t)-1)
)]

mat_t$batch_metabolomica[grepl("batch1", mat_t$Samples)] <- 1
mat_t$batch_metabolomica[grepl("batch2", mat_t$Samples)] <- 2

#AGGIUNTA COLONNA THERAPY 
mat_t$Therapy <- ifelse(grepl("MAV", mat_t$Samples), "MAV",
                        ifelse(grepl("TEC", mat_t$Samples), "TEC", NA))

mat_t <- mat_t %>%
  relocate(Therapy, .after = Class)

#aggiungo manualmente pazienti MS con altre terapie o MAV/TEC
mat_t$Therapy[mat_t$Samples== "MS_17_t0"] <- "TEC"
mat_t$Therapy[mat_t$Samples== "MS_13_t0"] <- "TEC"
mat_t$Therapy[mat_t$Samples== "MS_18_t0"] <- "altre terapie"
mat_t$Therapy[mat_t$Samples== "MS_1_t0"] <- "altre terapie"
mat_t$Therapy[mat_t$Samples== "MS_2_t0"] <- "altre terapie"
mat_t$Therapy[mat_t$Samples== "MS_4"] <- "TEC"
mat_t$Therapy[mat_t$Samples== "MS_11_t0"] <- "TEC"
mat_t$Therapy[mat_t$Samples== "MS_5"] <- "TEC"
mat_t$Therapy[mat_t$Samples== "MS_6"] <- "altre terapie"
mat_t$Therapy[mat_t$Samples== "MS_10_t0"] <- "altre terapie"
mat_t$Therapy[mat_t$Samples== "MS_9"] <- "altre terapie"




#80% rule: FILTRAGGIO DEI METABOLITI mantengo solo i metaboliti con valori
#diversi da zero in almeno il 50% di uno dei due gruppi (ctr vs patients)

# Campioni biologici usati SOLO per il calcolo del filtro
bio <- mat_t %>%
  filter(Class %in% c("ctr", "patients"))

# Colonne dei metaboliti
metabolites <- setdiff(names(mat_t),
                       c("Samples", "Class", "Therapy", "batch_metabolomica"))

# 50% rule
keep <- sapply(metabolites, function(met) {
  
  pct_ctr <- mean(bio[bio$Class == "ctr", met] != 0, na.rm = TRUE)
  
  pct_pat <- mean(bio[bio$Class == "patients", met] != 0, na.rm = TRUE)
  
  pct_ctr >= 0.50 | pct_pat >= 0.50
})

# Dataset finale: TUTTI i campioni (compresi i QC)
mat_t_filtered <- mat_t %>%
  select(Samples, Class, Therapy, batch_metabolomica,
         all_of(metabolites[keep]))

# Controllo
cat("Metaboliti iniziali:", length(metabolites), "\n")
cat("Metaboliti mantenuti:", sum(keep), "\n")
cat("Metaboliti rimossi:", sum(!keep), "\n")

table(mat_t_filtered$Class)


# Numero di metaboliti rimasti
ncol(mat_t_filtered) - 4

# Percentuale totale di zeri
metab <- mat_t_filtered[mat_t_filtered$Class %in% c("ctr", "patients"),
                        metabolites[keep]]
100 * sum(metab == 0, na.rm = TRUE) / sum(!is.na(metab))

# Percentuale massima di zeri per ciascun metabolita [RESTITUISCE 76% MA PUO' 
#ESSERE GIUSTO]
max(colMeans(metab == 0, na.rm = TRUE) * 100)

#CONTROLLO FINALE
bio_filtered <- mat_t_filtered %>%
  filter(Class %in% c("ctr", "patients"))

check <- sapply(metabolites[keep], function(met){
  
  pct_ctr <- mean(bio_filtered[bio_filtered$Class == "ctr", met] != 0)
  
  pct_pat <- mean(bio_filtered[bio_filtered$Class == "patients", met] != 0)
  
  (pct_ctr >= 0.50) | (pct_pat >= 0.50)
})

table(check)

#DISTRIBUZIONE DEGLI ZERO NEL DATASET FILTRATO
zero_pct <- colMeans(metab == 0, na.rm = TRUE) * 100
summary(zero_pct)

hist(zero_pct,
     breaks = 20,
     main = "Distribuzione della % di zeri per metabolita",
     xlab = "% di zeri")

table(cut(zero_pct,
          breaks = seq(0,100,10)))

#rioridno colonn per metaboanalyst
mat_t_filtered <- mat_t_filtered %>%
  rename(batch = batch_metabolomica) %>%
  mutate(batch = paste0("B", batch))

mat_t_filtered <- mat_t_filtered[, c(
  setdiff(names(mat_t_filtered), c("Therapy", "batch")),
  "batch",
  "Therapy"
)]

mat_t_filtered <- mat_t_filtered %>%
  relocate(batch, Therapy, .after = 2)

#correzione combat SENZA log2
Combat_batch <- read.delim("input/Combat_batch.csv", sep = ",", header = TRUE)

#MAT LOG
mat_log <- mat_t_filtered

mat_log[, 5:ncol(mat_log)] <- log2(
  as.matrix(mat_t_filtered[, 5:ncol(mat_t_filtered)]) + 1
)

write.csv2(mat_log,
           "C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/mat_log.csv",
           row.names = FALSE, 
           fileEncoding = "UTF-8")

mat_log <- read.csv2("input/mat_log.csv")

#correzione combat con log2 

Combat_batch2 <- read.csv2(
  "input/Combat_batch2.csv",
  header = TRUE,
  check.names = FALSE
)



#CREO DATASET PARALLELO CON COLONNA THERAPY 
Combat_patients <- read.csv2("Combat_batch2.csv", check.names = FALSE)

Combat_patients$CLASS <- Combat_patients$THERAPY
Combat_patients <- Combat_patients %>%
   mutate(
         THERAPY = case_when(
        grepl("^TEC", NAME) ~ "TEC",
       grepl("^MAV", NAME) ~ "MAV",
      grepl("^MS",  NAME) ~ NA_character_,
         TRUE ~ NA_character_
      )
       )


Combat_patients <- Combat_patients[, c(
  "NAME",
  "THERAPY",
  setdiff(names(Combat_patients), c("NAME", "THERAPY"))
)]

Combat_patients <- Combat_patients[
  !grepl("^(QC|ctr)", Combat_patients$NAME),
]

Combat_patients$THERAPY[Combat_patients$NAME== "MS_17_t0"] <- "TEC"
Combat_patients$THERAPY[Combat_patients$NAME== "MS_13_t0"] <- "TEC"
Combat_patients$THERAPY[Combat_patients$NAME== "MS_18_t0"] <- "altre terapie"
Combat_patients$THERAPY[Combat_patients$NAME== "MS_1_t0"] <- "altre terapie"
Combat_patients$THERAPY[Combat_patients$NAME== "MS_2_t0"] <- "altre terapie"
Combat_patients$THERAPY[Combat_patients$NAME== "MS_4"] <- "TEC"
Combat_patients$THERAPY[Combat_patients$NAME== "MS_11_t0"] <- "TEC"
Combat_patients$THERAPY[Combat_patients$NAME== "MS_5"] <- "TEC"
Combat_patients$THERAPY[Combat_patients$NAME== "MS_6"] <- "altre terapie"
Combat_patients$THERAPY[Combat_patients$NAME== "MS_10_t0"] <- "altre terapie"
Combat_patients$THERAPY[Combat_patients$NAME== "MS_9"] <- "altre terapie"

Combat_patients$THERAPY[is.na(Combat_patients$THERAPY)] <- "altre terapie"

#sovrascrivo Combat_patients
write.csv2(
  Combat_patients,
  "Combat_patients.csv",
  row.names = FALSE
)



#creo dataset solo con controlli e pazienti 
Combat_batch_pt_ctr <- Combat_batch2[-(1:12), ]

#sovrascrivo 
write.csv2(Combat_batch_pt_ctr,
           "C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/Combat_batch_pt_ctr.csv",
           row.names = FALSE, 
           fileEncoding = "UTF-8")

#apertura 

Combat_batch_pt_ctr <- read.delim(
  "input/Combat_batch_pt_ctr.csv",
  sep = ";",
  header = TRUE,
  blank.lines.skip = TRUE
)


