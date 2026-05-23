source("library.R")

#considerazioni su variabili da analizzare
colnames(merged_data)

#importazione di clean data
merged <- read.csv("C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/Input/merged_data.csv")
str(merged)

#variabili nuovamente importate male 

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

merged$cambio_terapia<- factor(merged$cambio_terapia)

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


#analisi descrittitva dell'intera coorte
summary(merged$age)


#####età####
ggplot(merged, aes(x = age)) +
  geom_histogram(bins = 20, fill = "steelblue") +
  geom_vline(aes(xintercept = median(age, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal()
#quale tra i due?
ggplot(merged, aes(x = age)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
  geom_density(aes(y = after_stat(count)), color = "darkblue") +
  geom_vline(aes(xintercept = median(age, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean(age, na.rm = TRUE)),
             color = "green", linetype = "dotted", size = 1) +
  theme_minimal()


#distribuzione età per pazienti TEC (at T0)
merged %>%
  filter(str_starts(ID, "TEC")) %>%
  summarise(
    n = n(),
    eta_media = mean(age_at_T0, na.rm = TRUE),
    eta_mediana = median(age_at_T0, na.rm = TRUE),
    sd = sd(age_at_T0, na.rm = TRUE)
  )

#distribuzione età per pazienti MAV (at T0)
merged %>%
  filter(str_starts(ID, "MAV")) %>%
  summarise(
    n = n(),
    eta_media = mean(age_at_T0, na.rm = TRUE),
    eta_mediana = median(age_at_T0, na.rm = TRUE),
    sd = sd(age_at_T0, na.rm = TRUE)
  )



#Boxplot dell’età per farmaco 

ggplot(merged, aes(x = GROUP, y = age)) +
  # Distribuzione (forma)
  geom_violin(fill = "lightblue", alpha = 0.3, color = NA) +
  # Boxplot
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  # Dati grezzi
  geom_jitter(width = 0.15, alpha = 0.4) +
  # Media (punto rosso)
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "red") +
  # Test statistico
  stat_compare_means(
    method = "wilcox.test",   #wilcox è più adatto ai miei dati
    label = "p.format"        # formato p-value leggibile
  ) +
  theme_minimal() +
  labs(
    title = "Distribuzione dell'età all'inizio della terapia",
    x = "Terapia",
    y = "Età (anni)"
  )

#distribuzione età nei pazienti TEC alla diagnosi 
merged %>%
  filter(str_starts(ID, "TEC")) %>%
  summarise(
    n = n(),
    eta_media = mean(age_at_diagnosis, na.rm = TRUE),
    eta_mediana = median(age_at_diagnosis, na.rm = TRUE),
    sd = sd(age_at_diagnosis, na.rm = TRUE)
  )


#distribuzione età nei pazienti MAV alla diagnosi 
merged %>%
  filter(str_starts(ID, "MAV")) %>%
  summarise(
    n = n(),
    eta_media = mean(age_at_diagnosis, na.rm = TRUE),
    eta_mediana = median(age_at_diagnosis, na.rm = TRUE),
    sd = sd(age_at_diagnosis, na.rm = TRUE)
  )


#Boxplot dell’età per farmaco alla diagnosi 

ggplot(merged, aes(x = GROUP, y = age_at_diagnosis)) +
  # Distribuzione (forma)
  geom_violin(fill = "lightblue", alpha = 0.3, color = NA) +
  # Boxplot
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  # Dati grezzi
  geom_jitter(width = 0.15, alpha = 0.4) +
  # Media (punto rosso)
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3,
    color = "red"
  ) +
  # Test statistico (Wilcoxon)
  stat_compare_means(
    method = "wilcox.test",
    label = "p.format"
  ) +
  theme_minimal() +
  labs(
    title = "Distribuzione dell'età alla diagnosi",
    x = "Terapia",
    y = "Età (anni)"
  )

  
######

######EDSS baseline#####
ggplot(merged, aes(x = EDSS_T0)) +
  geom_histogram(bins = 15, fill = "orange") +
  geom_vline(
    aes(xintercept = median(EDSS_T0, na.rm = TRUE)),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  theme_minimal()

#creo colonna group per differenziare i gruppi 
merged <- merged %>%
  mutate(
    GROUP = str_extract(ID, "^[A-Z]+")
  )

#EDSS baseline histogram per TEC e MAV

stats_EDSS_baseline <- merged %>%
  group_by(GROUP) %>%
  summarise(
    mediana = median(EDSS_T0, na.rm = TRUE),
    media = mean(EDSS_T0, na.rm = TRUE)
  )


ggplot(merged, aes(x = EDSS_T0)) +
  geom_histogram(
    binwidth = 0.5,
    fill = "steelblue",
    color = "black"
  ) +
  geom_vline(
    data = stats,
    aes(xintercept = mediana),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  # Media (linea verde puntinata)
  geom_vline(
    data = stats,
    aes(xintercept = media),
    color = "green",
    linetype = "dotted",
    linewidth = 1
  ) +
  facet_wrap(~ GROUP, ncol = 1) +
  theme_minimal()


#EDSS baseline TEC e MAV -> density plot (per la forma)
ggplot(merged, aes(x = EDSS_T0, color = GROUP, fill = GROUP)) +
  geom_density(alpha = 0.3, linewidth = 1) +
   geom_vline(
    data = stats,
    aes(xintercept = mediana, color = GROUP),
    linetype = "dashed",
    linewidth = 1
  )+
  theme_minimal()



#EDSS baseline TEC e MAV -> boxplot + jitter (per la dispersione)

ggplot(merged, aes(x = GROUP, y = EDSS_T0, fill = GROUP)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  stat_compare_means(method = "wilcox.test", label = "p.format") +
  theme_minimal()

####

###andamento EDSS nel tempo con traiettorie individuali####

ggplot(edss_long, aes(x = mesi, y = EDSS, group = ID)) +
  geom_line(alpha = 0.2, color = "grey") +   # linee individuali leggere
  geom_point(alpha = 0.3, size = 1) +        # punti (molto utili!)
  stat_summary(fun = mean, geom = "line", 
               color = "red", size = 1.5) +  # media
  labs(title = "Evoluzione EDSS nel tempo",
       x = "Mesi",
       y = "EDSS") +
  theme_minimal()

#con differenziamento trattamento
ggplot(edss_long, aes(x = mesi, y = EDSS, group = ID)) +
  geom_line(aes(color = trattamento), alpha = 0.1) +
  stat_summary(aes(color = trattamento),
               fun = mean, geom = "line", size = 1.5) +
  labs(title = "EDSS nel tempo per trattamento")

      
#boxplot generale di andamento 
ggplot(edss_long, aes(x = factor(mesi), y = EDSS)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribuzione EDSS nel tempo",
       x = "Mesi",
       y = "EDSS")

#boxplot generale andamento per trattamento 
ggplot(edss_long, aes(x = factor(mesi), y = EDSS, fill = trattamento)) +
  geom_boxplot() +
  labs(title = "EDSS nel tempo per trattamento",
       x = "Mesi",
       y = "EDSS")

#andamento differenziato per farmaco (line plot with error bars)
ggplot(edss_long, aes(x = mesi, y = EDSS, color = trattamento)) +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 1) +
  labs(title = "Andamento medio EDSS per trattamento")

#variazione EDSS a 12 mesi
merged <- merged %>%
  mutate(delta_EDSS_12 = EDSS_T12 - EDSS_T0)

ggplot(merged, aes(x = GROUP, y = delta_EDSS_12)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Variazione EDSS a 12 mesi")

wilcox.test(delta_EDSS_12 ~ GROUP, data = merged)
#####

######distribuzione del sesso#####
ggplot(merged, aes(x = sex)) +
  geom_bar(fill = "purple", color = "black") +
  labs(
    x = "Sesso",
    y = "Numero di soggetti",
    title = "Distribuzione del sesso"
  ) +
  theme_minimal()

#distribuzione sesso tra TEC e MAV 

ggplot(merged, aes(x = sex, fill = sex)) +
  geom_bar(color = "black") +
  facet_wrap(~ GROUP) +
  theme_minimal()


#con percentuali 
ggplot(merged, aes(x = sex, fill = sex)) +
  geom_bar(aes(y = after_stat(prop), group = GROUP)) +
  facet_wrap(~ GROUP) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()



######

#####naive vs non-naive per farmaco#### 

ggplot(merged, aes(x = naive, fill = substr(ID, 1, 3))) +
  geom_bar(position = "dodge") +
  labs(fill = "Farmaco", x = "Naive", y = "Numero pazienti") +
  theme_minimal()
######

########## andamento EDSS nel tempo nei TEC #######

TEC <- merged %>% 
  filter(grepl("^TEC", ID))

#mettere edss in formato lungo (x grafici longitudinali)

TEC_long <- TEC %>%
  pivot_longer(
    cols = c(EDSS_T0, EDSS_T6, EDSS_T12, EDSS_T24, EDSS_T30, EDSS_T36),
    names_to = "Tempo",
    values_to = "EDSS"
  )
#pulizia della variabile "tempo"
TEC_long$Tempo <- factor(
  TEC_long$Tempo,
  levels = c("EDSS_T0", "EDSS_T6", "EDSS_T12", 
             "EDSS_T24", "EDSS_T30", "EDSS_T36"),
  labels = c("0", "6", "12", "24", "30", "36")
)



ggplot(TEC_long, aes(x = Tempo, y = EDSS, group = ID)) +
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "line",
               color = "blue",
               linewidth = 1.2) +
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "point",
               color = "blue",
               size = 3) +
  stat_summary(
    aes(group = 1),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.2
  ) +
  labs(
    title = "Andamento medio EDSS nei pazienti TEC",
    x = "Mesi di follow-up",
    y = "EDSS"
  ) +
  theme_minimal() 
#da valutare se considerare solo tempi (0,12,24 in relazione ai dati dei centri esterni)

#alternativa con mediana di EDSS (meglio per dati clinici??)

ggplot(TEC_long, aes(x = Tempo, y = EDSS)) +
  stat_summary(
    fun = median,
    geom = "line",
    group = 1,
    color = "darkred",
    linewidth = 1.2
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    color = "darkred",
    size = 3
  ) +
  labs(
    title = "Andamento mediano EDSS nei pazienti TEC",
    x = "Mesi di follow-up",
    y = "EDSS"
  ) +
  theme_minimal() #forse c'è un bias di follow up (numero di pazienti a t24)

#CONTROLLO NECESSARIO: QUANTI PAZIENTI PER OGNI TEMPO
TEC_long %>%
  group_by(Tempo) %>%
  summarise(N = sum(!is.na(EDSS)))
######

#######andamento EDSS nel tempo nei pazienti MAV ######

library(dplyr)

MAV <- merged %>% 
  filter(grepl("^MAV", ID))


library(tidyr)

MAV_long <- MAV %>%
  pivot_longer(
    cols = c(EDSS_T0, EDSS_T6, EDSS_T12, 
             EDSS_T24, EDSS_T30, EDSS_T36),
    names_to = "Tempo",
    values_to = "EDSS"
  )
#pulizia variabile "tempo"
MAV_long$Tempo <- factor(
  MAV_long$Tempo,
  levels = c("EDSS_T0", "EDSS_T6", "EDSS_T12",
             "EDSS_T24", "EDSS_T30", "EDSS_T36"),
  labels = c("0", "6", "12", "24", "30", "36")
)


library(ggplot2)

ggplot(MAV_long, aes(x = Tempo, y = EDSS)) +
  stat_summary(
    fun = median,
    geom = "line",
    group = 1,
    color = "darkred",
    linewidth = 1.3
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    color = "darkred",
    size = 3
  ) +
  labs(
    title = "Andamento mediano EDSS nei pazienti MAV",
    x = "Mesi di follow-up",
    y = "EDSS"
  ) +
  theme_minimal()

#CONTROLLO NECESSARIO: QUANTI PAZIENTI PER OGNI TEMPO

MAV_long %>%
  group_by(Tempo) %>%
  summarise(N = sum(!is.na(EDSS))) #al 21/4 i dati sono squilibrati per i tempi 
######


####andamento delle lesioni in TEC e MAV####
library(dplyr)
library(tidyr)
library(stringr)

lesioni_prop <- lesioni_long %>%
  group_by(terapia, mesi) %>%
  summarise(
    prop_lesioni = mean(lesioni == 1, na.rm = TRUE),
    .groups = "drop")

library(ggplot2)

ggplot(lesioni_prop, aes(x = mesi, y = prop_lesioni, color = terapia)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Mesi dal baseline",
    y = "Pazienti con lesioni (%)",
    title = "Andamento dell'attività lesionale nel tempo"
  ) +
  theme_minimal()


#controllo numero di dati -> alcuni time hanno meno pazienti

lesioni_long %>%
  group_by(terapia, mesi) %>%
  summarise(
    n_totale = n(),
    n_lesioni = sum(lesioni == 1, na.rm = TRUE),
    percent_lesioni = round(mean(lesioni == 1, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(terapia, mesi)


#####

####BLOCCO 1 — Correlazioni tra variabili BASELINE####

#Età all’inizio della terapia ↔ EDSS al baseline (I pazienti più anziani partono con maggiore disabilità?)

#utilizzo di Spearman perchè le variabili sono ordinali e non continue 
cor.test(
  merged$age_at_T0,
  merged$EDSS_T0,
  method = "spearman",
  use = "complete.obs"
)

#Età alla diagnosi ↔ EDSS_T0 (età di diagnosi tardiva è associata a EDSS maggiore?)

cor.test(
  merged$age_at_diagnosis,
  merged$EDSS_T0,
  method = "spearman",
  #use = "complete.obs"
  exact = FALSE
)

 
#BMI↔ EDSS_T0

cor.test(
  merged$bmi,
  merged$EDSS_T0,
  method = "spearman",
  use = "complete.obs"
)


#MEDAS ↔ EDSS_T0

cor.test(
  merged$MEDAS,
  merged$EDSS_T0,
  method = "spearman",
  use = "complete.obs"
)


#Età ↔ BMI (controllo demografico) (i pazienti più anziani hanno bmi più alto?)

cor.test(
  merged$age_at_T0,
  merged$bmi,
  method = "spearman",
  use = "complete.obs"
)

#EDSS_T0 ↔ presenza di lesioni al baseline (I pazienti con lesioni al baseline hanno EDSS più alto?)

library(ggplot2)

#gadolinium
ggplot(merged, aes(x = factor(gadolinium_contrast), y = EDSS_T0)) +
  geom_boxplot() +
  labs(
    x = "Gadolinium contrast (T0)",
    y = "EDSS T0",
    title = "EDSS al baseline e RM con gadolinio"
  ) +
  theme_minimal()     #i pazienti con gadolinio positivo tendono ad avere EDSS più alto al baseline

wilcox.test(
  EDSS_T0 ~ gadolinium_contrast,
  data = merged
)

#lesion_burden
ggplot(merged, aes(x = factor(lesion_burden), y = EDSS_T0)) +
  geom_boxplot() +
  labs(
    x = "Lesion Burden (T0)",
    y = "EDSS T0",
    title = "EDSS al baseline e carico di lesioni"
  ) +
  theme_minimal()

wilcox.test(
  EDSS_T0 ~ lesion_burden,
  data = merged)
#pazienti con più lesioni burden riflettono maggiore EDSS iniziale (significatività)

#bone_marrow_lesions
ggplot(merged, aes(x = factor(bone_marrow_lesions), y = EDSS_T0)) +
  geom_boxplot() +
  labs(
    x = "Bone Marrow lesions (T0)",
    y = "EDSS T0",
    title = "EDSS al baseline e lesioni a midollo spinale"
  ) +
  theme_minimal()

wilcox.test(
  EDSS_T0 ~ bone_marrow_lesions,
  data = merged)


#subtentorial_lesions
ggplot(merged, aes(x = factor(subtentorial_lesions), y = EDSS_T0)) +
  geom_boxplot() +
  labs(
    x = " Subtentorial lesions (T0)",
    y = "EDSS T0",
    title = "EDSS al baseline e lesioni subtentoriali"
  ) +
  theme_minimal()

wilcox.test(
  EDSS_T0 ~ subtentorial_lesions,
  data = merged)


######

######BLOCCO 2 — Stile di vita ↔ outcome#####

#Fumo <- EDSS_T0

library(ggplot2)
ggplot(merged, aes(x = smoking_habit, y = EDSS_T0)) +
  geom_boxplot() +
  labs(x = "smoking habit", y = "EDSS_T0") +
  theme_minimal()


wilcox.test(
  EDSS_T0 ~ smoking_habit,
  data = merged
)


#Attività fisica <- EDSS_T0

ggplot(merged, aes(x = physical_activity , y = EDSS_T0)) +
  geom_boxplot() +
  labs(x = "Attività fisica", y = "EDSS_T0") +
  theme_minimal()

kruskal.test(
  EDSS_T0 ~ physical_activity,
  data = merged
)


#Stile di vita ↔ ATTIVITÀ RM BASELINE

#MEDAS ↔ lesion_burden

wilcox.test(
  MEDAS ~ lesion_burden,
  data = merged
)

#BMI ↔ lesion_burden

wilcox.test(
  bmi ~ lesion_burden,
  data = merged
)


#Attività fisica ↔ lesion_burden

kruskal.test(
  lesion_burden ~ physical_activity,
  data = merged
)

#Fumo ↔ lesion_burden
fisher.test(table(merged$smoking_habit, merged$lesion_burden))

 


#Modello lineare misto


edss_long <- merged %>%
  pivot_longer(
    cols = starts_with("EDSS_T"),
    names_to = "timepoint",
    values_to = "EDSS"
  ) %>%
  mutate(
    mesi = as.numeric(gsub("EDSS_T", "", timepoint)),
    trattamento = ifelse(grepl("TEC", ID),
                         "Tecfidera",
                         "Mavenclad")
  )



#utilizzo di edss_long
edss_long$MEDAS <- as.numeric(as.character(edss_long$MEDAS))

library(lme4)

model <- lmer(
  EDSS ~ months * MEDAS + bmi + smoking_habit + physical_activity + (1 | ID),
  data = edss_long
)

summary(model)



#stessa analisi ma distinguendo Tecfidera vs Mavenclad (TEC è il riferimento)

#L’andamento dell’EDSS nel tempo e l’effetto della dieta (MEDAS) 
#sono diversi nei pazienti trattati con Tecfidera vs Mavenclad, 
#una volta tenuto conto di BMI, fumo e attività fisica? 

edss_long$farmaco <- factor(edss_long$farmaco)
edss_long$farmaco <- relevel(edss_long$farmaco, ref = "TEC")

model_tx <- lmer(
  EDSS ~ months * MEDAS * farmaco +
    bmi + smoking_habit + physical_activity +
    (1 | ID),
  data = edss_long
)
summary(model_tx)

#####

