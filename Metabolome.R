#apertura file metabolomica 
source("library.R")

# matrice metabolomica
mat <- read_excel("C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/metabolome_countmatrix.xlsx")

# annotazioni
annot <- read_excel("C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/metabolome_keyannotation.xlsx")


# batch
batch <- read_excel("C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/input/metabolome_batch.xlsx")


#aggiusto valori numerici in mat
group <- as.character(unlist(mat[1, ]))  # salva gruppi
mat <- mat[-1, ] #escludi riga 

mat <- as.data.frame(mat)
rownames(mat) <- mat[,1]
mat <- mat[,-1]

mat[] <- lapply(mat, function(x) as.numeric(as.character(x)))

#BATCH1
#qc colonne batch 1
qc_batch1_cols <- grep("^qc_feci_batch1", colnames(mat), value = TRUE)

#media qc per ogni metabolita
qc_mean_batch1 <- rowMeans(mat[, qc_batch1_cols])


length(qc_mean_batch1)
nrow(mat)

#BATCH 2
qc_batch2_cols <- grep("^qc_feci_batch2", colnames(mat), value = TRUE)

qc_mean_batch2 <- rowMeans(mat[, qc_batch2_cols])


#Sostituizione degli 0 in QC
qc_mean_batch1[qc_mean_batch1 == 0] <- NA
qc_mean_batch2[qc_mean_batch2 == 0] <- NA

#separazione dei samples dai QC
mat_samples <- mat[, !colnames(mat) %in% c(qc_batch1_cols, qc_batch2_cols)]




#MATCHING NOMI TRA BATCH E MAT_SAMPLES (modifico batch)
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
batch$id[batch$id == "ctr_15"] <- "ctr_15_new...29"
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


batch$id[batch$id == "TEC_02_t0"] <- "TEC_02_t0_new"
batch$id[batch$id == "TEC_07_t0"] <- "TEC_07_t0_new"
batch$id[batch$id == "TEC_22_t0"] <- "TEC_22_t0_new"



#inserire samples mancanti in batch 
nuove_righe <- data.frame(
  id = c("ctr_15_new...30", "ctr_15_new...31"),
  batch_metabolomica = c(2, 2)
)

batch <- rbind(
  batch[1:42, ],
  nuove_righe,
  batch[43:nrow(batch), ]
)

#controllo matching
setequal(batch$id, colnames(mat))
setdiff(batch$id, colnames(mat_samples))
setdiff(colnames(mat_samples), batch$id)

#CREO DATASET SEPARATI PER SAMPLES-> BATCH1 E SAMPLES->BATCH2
common <- intersect(batch$id, colnames(mat_samples))

batch1_ids <- common[batch$batch_metabolomica[match(common, batch$id)] == 1]
batch2_ids <- common[batch$batch_metabolomica[match(common, batch$id)] == 2]

mat_batch1 <- mat_samples[, batch1_ids]
mat_batch2 <- mat_samples[, batch2_ids]

setdiff(batch$id[batch$batch_metabolomica == 2], colnames(mat_samples))                       

#controllo se combaciano i metaboliti
length(qc_mean_batch1) == nrow(mat_batch1)
length(qc_mean_batch2) == nrow(mat_batch2)

#NORMALIZZO SEPARATAMENTE
mat_batch1_norm <- mat_batch1 / qc_mean_batch1
mat_batch2_norm <- mat_batch2 / qc_mean_batch2

#ricombino i dataset (le colonne sono distribuite diversamente rispetto a mat_samples)
mat_samples_norm <- cbind(mat_batch1_norm, mat_batch2_norm)


