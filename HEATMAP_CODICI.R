source("library.R")
Batch80_norm_log_clean <- read.delim("input/Batch80_norm_log_clean.csv", sep =
                                       ";", header = TRUE, check.names = FALSE)

merged <- read.csv2("input/merged.csv")

patients_key <- read.csv2("input/patients_key.csv")
metadati_sani <- read.csv2("input/metadati_sani.csv")

#sostituzione temporanea di ID in merged 
library(dplyr)

merged <- merged %>%
  left_join(patients_key, by = "ID") %>%
  mutate(ID = ifelse(!is.na(New_ID), New_ID, ID)) %>%
  select(-New_ID)

#####################
##nuovo codice heatmap base

top10 <- c(
  "Kailuin H",
  "PC(14:0/20:0)",
  "[Ala2, Val11]cyclosporin",
  "Ulongamide D",
  "Luminmide C",
  "Neoxanthin",
  "PAX13",
  "PC(18:1(9Z)/18:1(9Z))",
  "Phthalic anhydride",
  "Cordyceamide A",
  "Neo-debromoaplysiatoxin A",
  "Gephyronic acid (hemiketal-beta)"
)

# =========================
# Selezione metaboliti
# =========================

heat_mat <- t(
  as.matrix(
    Batch80_norm_log_clean[, top10]
  )
)

# i campioni diventano le colonne
colnames(heat_mat) <- Batch80_norm_log_clean$ID

# =========================
# Annotazioni campioni
# =========================

annotation_col <- data.frame(
  CLASS = Batch80_norm_log_clean$CLASS
)

rownames(annotation_col) <- Batch80_norm_log_clean$ID

# =========================
# Colori gruppi
# =========================

ann_colors <- list(
  CLASS = c(
    "ctr" = "#4DAF4A",
    "patients" = "#E41A1C"
  )
)

# =========================
# Heatmap
# =========================
my_colors <- colorRampPalette(
  c("blue", "lightblue", "lightyellow", "orange", "red")
)(100)

pheatmap(
  heat_mat,
  scale = "row",
  annotation_col = annotation_col,
  annotation_colors = ann_colors,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  show_colnames = TRUE,
  fontsize_row = 13,
  fontsize_col = 6, 
  border_color = "grey",
  color = my_colors
)



#############
Batch80_norm_log_clean <- read.delim("input/Batch80_norm_log_clean.csv", sep =
                                       ";", header = TRUE, check.names = FALSE)
merged <- read.csv2("input/merged.csv")
patients_key <- read.csv2("input/patients_key.csv")
metadati_sani <- read.csv2("input/metadati_sani.csv")

#heatmap con header
merged <- merged %>%
  left_join(patients_key, by = "ID") %>%
  mutate(ID = ifelse(!is.na(New_ID), New_ID, ID)) %>%
  select(-New_ID)
# Pazienti
header_pat <- merged %>%
  select(ID, age_at_T0, MEDAS, bmi, sex)

# Controlli sani
header_ctrl <- metadati_sani %>%
  select(ID, age_at_T0, MEDAS, bmi, sex)

# Conversione a numerico
header_pat$MEDAS <- as.numeric(header_pat$MEDAS)
header_ctrl$MEDAS <- as.numeric(header_ctrl$MEDAS)

header_pat$bmi <- as.numeric(header_pat$bmi)
header_ctrl$bmi <- as.numeric(header_ctrl$bmi)

# Unione pazienti + controlli
header_all <- bind_rows(header_pat, header_ctrl)

# Costruzione header finale usando TUTTI i soggetti della heatmap
header_df <- Batch80_norm_log_clean %>%
  select(ID, CLASS) %>%
  left_join(header_all, by = "ID")

# Ordine dei soggetti nella heatmap
sample_ids <- Batch80_norm_log_clean$ID

# Mantiene l'ordine dei campioni della heatmap
header_df <- data.frame(ID = sample_ids) %>%
  left_join(header_df, by = "ID")

rownames(header_df) <- header_df$ID

# Rimuovi ID dalle annotazioni
header_df <- header_df[, c("CLASS","age_at_T0", "MEDAS", "bmi", "sex")]




colnames(header_df)

# =========================
# 2. Definizione colori
# =========================

ha <- HeatmapAnnotation(
  CLASS = header_df$CLASS,
  age_at_T0 = header_df$age_at_T0,
  MEDAS = header_df$MEDAS,
  bmi = header_df$bmi,
  sex = header_df$sex,
  col = list(
    CLASS = c(
      "ctr" = "#4DAF4A",
      "patients" = "#e31a1c"
    ),
    
    age_at_T0 = colorRamp2(
      c(min(header_df$age_at_T0, na.rm = TRUE),
        max(header_df$age_at_T0, na.rm = TRUE)),
      c("lightblue", "darkblue")
    ),
    
    MEDAS = colorRamp2(
      c(min(header_df$MEDAS, na.rm = TRUE),
        max(header_df$MEDAS, na.rm = TRUE)),
      c("white", "darkgreen")
    ),
    
    bmi = colorRamp2(
      c(min(header_df$bmi, na.rm = TRUE),
        max(header_df$bmi, na.rm = TRUE)),
      c("yellow", "red")
    ),
    sex = c(
      "M" = "#1f78b4",
      "F" = "#FF69B4"
    )
  ),
  na_col = "white"
)


table(header_df$CLASS, useNA = "always")

# =========================
# TOP 10 METABOLITES
# =========================
top10 <- c(
  "Kailuin H",
  "PC(14:0/20:0)",
  "[Ala2, Val11]cyclosporin",
  "Ulongamide D",
  "Luminmide C",
  "Neoxanthin",
  "PAX13",
  "PC(18:1(9Z)/18:1(9Z))",
  "Phthalic anhydride",
  "Cordyceamide A",
  "Neo-debromoaplysiatoxin A",
  "Gephyronic acid (hemiketal-beta)"
)


# =========================
# 3. Matrice heatmap
# =========================
mat <- Batch80_norm_log_clean %>%
select(all_of(top10)) %>%
as.matrix()

rownames(mat) <- Batch80_norm_log_clean$ID

mat <- t(mat)

# =========================
# 4. Heatmap
# =========================
my_colors1 <- colorRampPalette(
  c("blue", "lightblue", "lightyellow", "orange", "red")
)(100)

Heatmap(
  mat,
  name = "Expression",
  col = my_colors1,
  top_annotation = ha,
  show_column_names = TRUE,
  show_row_names = TRUE,
  column_names_rot = 90,
  column_names_gp = grid::gpar(fontsize = 6),
  row_names_gp = grid::gpar( fontsize = 10,
 fontface = "bold"
  ),
  cluster_columns = TRUE,
  cluster_rows = TRUE,
 rect_gp = grid::gpar(
   col = "grey", # colore del bordo
   lwd = 0.5 # spessore
 )
)




#################
#DISCRETIZZAZIONE PER BMI, MEDAS, age at T0
merged <- merged %>%
  mutate(
    bmi_cat = case_when(
      bmi < 18.5 ~ "Underweight (bmi<18.49)",
      bmi >= 18.5 & bmi < 25 ~ "Normal weight (18.5>bmi>24.9)",
      bmi >= 25 ~ "Overweight (bmi>24.5)",
      TRUE ~ NA_character_
    ))

merged <- merged %>%
  mutate(
    MEDAS_cat = case_when(
      MEDAS < 42.9 ~ "Low MEDAS (MEDAS < 42.9)",
      MEDAS >= 42.9 & MEDAS < 57.0 ~ "Intermediate MEDAS (42.9 < MEDAS < 57)",
      MEDAS >= 57~ "High MEDAS (MEDAS > 57)",
      TRUE ~ NA_character_
    ))


merged <- merged %>%
  mutate(
    age_cat = case_when(
      age_at_T0 < 30 ~ "Young (age < 30)",
      age_at_T0 >= 30 & age_at_T0 < 45 ~ "Middle-aged (30 < age < 45)",
      age_at_T0 >= 45 ~ "Older (age > 45)",
      TRUE ~ NA_character_
    ))


metadati_sani <- metadati_sani %>%
  mutate(
    bmi_cat = case_when(
      bmi < 18.5 ~ "Underweight (bmi<18.49)",
      bmi >= 18.5 & bmi < 25 ~ "Normal weight (18.5>bmi>24.9)",
      bmi >= 25 ~ "Overweight (bmi>24.5)",
      TRUE ~ NA_character_
    ))

metadati_sani <- metadati_sani %>%
  mutate(
    MEDAS_cat = case_when(
      MEDAS < 42.9 ~ "Low MEDAS (MEDAS < 42.9)",
      MEDAS >= 42.9 & MEDAS < 57.0 ~ "Intermediate MEDAS (42.9 < MEDAS < 57)",
      MEDAS >= 57~ "High MEDAS (MEDAS > 57)",
      TRUE ~ NA_character_
    ))

metadati_sani <- metadati_sani %>%
  mutate(
    age_cat = case_when(
      age_at_T0 < 30 ~ "Young (age < 30)",
      age_at_T0 >= 30 & age_at_T0 < 45 ~ "Middle-aged (30 < age < 45)",
      age_at_T0 >= 45 ~ "Older (age > 45)",
      TRUE ~ NA_character_
    ))

#heatmap con header
merged <- merged %>%
  left_join(patients_key, by = "ID") %>%
  mutate(ID = ifelse(!is.na(New_ID), New_ID, ID)) %>%
  select(-New_ID)
# Pazienti
header_pat <- merged %>%
  select(ID, age_cat, MEDAS_cat, bmi_cat, sex)

# Controlli sani
header_ctrl <- metadati_sani %>%
  select(ID, age_cat, MEDAS_cat, bmi_cat, sex)

# Conversione a numerico
#header_pat$MEDAS <- as.numeric(header_pat$MEDAS)
#header_ctrl$MEDAS <- as.numeric(header_ctrl$MEDAS)

#header_pat$bmi <- as.numeric(header_pat$bmi)
#header_ctrl$bmi <- as.numeric(header_ctrl$bmi)

# Unione pazienti + controlli
header_all <- bind_rows(header_pat, header_ctrl)

# Costruzione header finale usando TUTTI i soggetti della heatmap
header_df <- Batch80_norm_log_clean %>%
  select(ID, CLASS) %>%
  left_join(header_all, by = "ID")

# Ordine dei soggetti nella heatmap
sample_ids <- Batch80_norm_log_clean$ID

# Mantiene l'ordine dei campioni della heatmap
header_df <- data.frame(ID = sample_ids) %>%
  left_join(header_df, by = "ID")

rownames(header_df) <- header_df$ID

# Rimuovi ID dalle annotazioni
header_df <- header_df[, c("CLASS","age_cat", "MEDAS_cat", "bmi_cat", "sex")]




colnames(header_df)

# =========================
# 2. Definizione colori
# =========================

bmi_cols <- c(
  "Normal weight (18.5>bmi>24.9)" = "#fd8d3c",
  "Overweight (bmi>24.5)" = "#e31a1c",
  "Underweight (bmi<18.49)" = "#ffffcc"
)

medas_cols <- c(
  "Low MEDAS (MEDAS < 42.9)" = "#fee08b",
  "Intermediate MEDAS (42.9 < MEDAS < 57)" = "#66bd63",
  "High MEDAS (MEDAS > 57)" = "#1a9850"
)

age_cols <- c(
  "Young (age < 30)" = "#A6CEE3",
  "Middle-aged (30 < age < 45)" = "#1F78B4",
  "Older (age > 45)" = "#08306B"
)

library(ComplexHeatmap)

ha <- HeatmapAnnotation(
  CLASS = header_df$CLASS,
  age_cat = header_df$age_cat,
  MEDAS_cat = header_df$MEDAS_cat,
  bmi_cat = header_df$bmi_cat,
  sex = header_df$sex,
  
  col = list(
    CLASS = c(
      "ctr" = "#4DAF4A",
      "patients" = "#e31a1c"
    ),
    
    age_cat = age_cols,
    
    MEDAS_cat = medas_cols,
    
    bmi_cat = bmi_cols,
    
    sex = c(
      "M" = "#1f78b4",
      "F" = "#FF69B4"
    )
  ),    annotation_legend_param = list(
    
    age_cat = list(
      at = c("Young (age < 30)", "Middle-aged (30 < age < 45)", "Older (age > 45)")
    ),
    
    MEDAS_cat = list(
      at = c("Low MEDAS (MEDAS < 42.9)",
             "Intermediate MEDAS (42.9 < MEDAS < 57)",
             "High MEDAS (MEDAS > 57)")
    ),
    
    bmi_cat = list(
      at = c("Underweight (bmi<18.49)",
             "Normal weight (18.5>bmi>24.9)",
             "Overweight (bmi>24.5)")
    )
  ),
  
  
  na_col = "white"
)


# =========================
# TOP 10 METABOLITES
# =========================
top10 <- c(
  "Kailuin H",
  "PC(14:0/20:0)",
  "[Ala2, Val11]cyclosporin",
  "Ulongamide D",
  "Luminmide C",
  "Neoxanthin",
  "PAX13",
  "PC(18:1(9Z)/18:1(9Z))",
  "Phthalic anhydride",
  "Cordyceamide A",
  "Neo-debromoaplysiatoxin A",
  "Gephyronic acid (hemiketal-beta)"
)


# =========================
# 3. Matrice heatmap
# =========================
mat <- Batch80_norm_log_clean %>%
  select(all_of(top10)) %>%
  as.matrix()

rownames(mat) <- Batch80_norm_log_clean$ID

mat <- t(mat)

mat <- t(scale(t(mat)))
# =========================
# 4. Heatmap
# =========================
my_colors1 <- colorRampPalette(
  c("blue", "lightblue", "lightyellow", "orange", "red")
)(100)

ht <- Heatmap(
  mat,
  name = "Expression",
  col = my_colors1,
  top_annotation = ha,
  show_column_names = TRUE,
  show_row_names = TRUE,
  column_names_rot = 90,
  column_names_gp = grid::gpar(fontsize = 6),
  row_names_gp = grid::gpar( fontsize = 8,
                             fontface = "bold"
  ),
  cluster_columns = TRUE,
  cluster_rows = TRUE,
  rect_gp = grid::gpar(
    col = "grey", # colore del bordo
    lwd = 0.5 # spessore
  ) )



draw(
  ht,
  heatmap_legend_side = "bottom",
  annotation_legend_side = "bottom",
  merge_legends = TRUE
)


summary(metadati_sani$age)
summary(merged$age)

summary(merged$age_at_T0)
