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
