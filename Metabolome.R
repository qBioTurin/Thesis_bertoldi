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

#separazione dei samples dai QC
mat_samples <- mat[, !colnames(mat) %in% c(qc_batch1_cols, qc_batch2_cols)]




#MATCHING NOMI TRA BATCH E MAT_SAMPLES (modifico batch)

batch$id_clean <- toupper(batch$id)
                       







