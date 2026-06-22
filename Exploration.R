source("library.R")

#considerazioni su variabili da analizzare
colnames(merged_data)

#importazione di clean data
merged <- read.csv("C:/Users/cecil/OneDrive/Documenti/TIROCINIO/Thesis_bertoldi/Input/merged_data.csv")


#analisi descrittitva dell'intera coorte
summary(merged$age)
table(merged$sex, useNA = "ifany")
table(merged$CENTRO)
table(merged$GROUP)

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
    data = stats_EDSS_baseline,
    aes(xintercept = mediana),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  # Media (linea verde puntinata)
  geom_vline(
    data = stats_EDSS_baseline,
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
    data = stats_EDSS_baseline,
    aes(xintercept = mediana, color = GROUP),
    linetype = "dashed",
    linewidth = 1
  )+
  theme_minimal()



#EDSS baseline TEC e MAV -> boxplot + jitter (per la dispersione)
ggplot(merged, aes(x = GROUP, y = EDSS_T0, fill = GROUP)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.12, alpha = 0.4, size = 2, color = "black") +
  stat_compare_means(
    method = "wilcox.test",
    label = "p.format",
    label.y = max(merged$EDSS_T0, na.rm = TRUE) + 0.5,
    size = 4
  ) +
  scale_fill_manual(values = c("MAV" = "#E07A73", "TEC" = "#4DB6B3")) +
  labs(
    title = "Baseline EDSS by Treatment Group",
    subtitle = "Wilcoxon rank-sum test",
    x = "Treatment group",
    y = "EDSS at baseline"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
####

###andamento EDSS nel tempo con traiettorie individuali####
ggplot(edss_long, aes(x = mesi, y = EDSS, group = ID)) +
  geom_line(alpha = 0.25, color = "grey50", linewidth = 0.6) +
  geom_point(alpha = 0.4, size = 1, color = "grey40") +
  stat_summary(
    aes(group = 1),
    fun.data = mean_se,
    geom = "ribbon",
    fill = "red",
    alpha = 0.2,
    color = NA
  ) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    
    color = "red",
    linewidth = 1.5
  ) +
  labs(
    title = "EDSS evolution over time",
    subtitle = "grey: patients | red: mean ± SE",
    x = "Months",
    y = "EDSS"
  ) +
  theme_minimal(base_size = 13)

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

#nel report 
sex_prop <- merged %>%
  filter(!is.na(sex)) %>%
  group_by(GROUP, sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(GROUP) %>%
  mutate(percent = n / sum(n))
#nel report
ggplot(sex_prop, aes(x = GROUP, y = percent, fill = sex)) +
geom_col(position = "dodge") +
  geom_text(
    aes(label = scales::percent(percent)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = c(
    "F" = "#E07A73",
    "M" = "#4DB6B3"
  )) +
  labs(
    title = "Sex distribution by treatment group",
    x = "Treatment group",
    y = "Percentage",
    fill = "Sex"
  ) +
  theme_minimal()
git


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

#andamento medio EDSS in TEC
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

###ATTENZIONE: per il report è meglio un unico grafico

ggplot(edss_long, aes(x = mesi, y = EDSS, color = farmaco)) +
  stat_summary(
    fun = median,
    geom = "line",
    linewidth = 1.3
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3
  ) +
  labs(
    title = "Median EDSS over time by treatment",
    x = "Months of follow-up",
    y = "EDSS",
    color = "Treatment"
  ) +
  theme_minimal()

#inserisco anche modello longitudinale per il report 
model3 <- lmer(EDSS ~ mesi * farmaco + (1 | ID), data = edss_long)
summary(model3)


######


####andamento delle lesioni in TEC e MAV####
#per creare lesioni_long
lesioni_long <- merged %>%
  pivot_longer(
    cols = starts_with("lesioni_"),
    names_to = "mesi",
    values_to = "lesioni"
  ) %>%
  mutate(
    mesi = str_remove(mesi, "lesioni_T"),   
    mesi = as.numeric(mesi),
    terapia = GROUP
  )
#creazione lesioni_prop
lesioni_prop <- lesioni_long %>%
  group_by(terapia, mesi) %>%
  summarise(
    prop_lesioni = mean(lesioni == 1, na.rm = TRUE),
    .groups = "drop")

ggplot(lesioni_prop, aes(x = mesi, y = prop_lesioni, color = terapia)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Months from baseline",
    y = "Patients with lesions (%)",
    title = "Lesions activity over time"
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

#gadolinium
ggplot(merged, aes(x = factor(gadolinium_contrast), y = EDSS_T0)) +
  geom_boxplot() +
  labs(
    x = "Gadolinium contrast (T0)",
    y = "EDSS T0",
    title = "Baseline EDSS and gadolinium-enhancing MRI findings"
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
    title = "Baseline EDSS by lesion burden"
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

ggplot(merged %>% filter(!is.na(smoking_habit)),
  aes(x = smoking_habit, y = EDSS_T0, fill = smoking_habit)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2, color = "black") +
  stat_compare_means(method = "wilcox.test", label = "p.format", size = 4) +
  scale_fill_manual(values = c("yes" = "#E07A73", "no" = "#4DB6B3")) +
  labs(
    title = "Baseline EDSS by smoking status",
    x = "Smoking status",
    y = "Baseline EDSS"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )


wilcox.test(
  EDSS_T0 ~ smoking_habit,
  data = merged
)

#smoking habit <- EDSS long
ggplot(edss_long, aes(x = mesi, y = EDSS, color = smoking_habit)) +
  stat_summary(fun = mean, geom = "line")

ggplot(edss_long, aes(x = mesi, y = EDSS, color = smoking_habit)) +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 1) +
  labs(title = "EDSS nel tempo per abitudine al fumo",
       x = "Mesi",
       y = "EDSS") +
  theme_minimal()

#controllo
head(edss_long[, c("timepoint", "mesi")])

kruskal.test(EDSS ~ smoking_habit, data = edss_long %>% filter(mesi == 12))

#uso di modello longitudinale (fumo influenza EDSS nel tempo?)

model <- lmer(EDSS ~ mesi + smoking_habit + (1 | ID),
              data = edss_long)
summary(model)


#Attività fisica <- EDSS_T0

ggplot(merged %>% filter(!is.na(physical_activity)),
       aes(x = physical_activity, y = EDSS_T0, fill = physical_activity)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2, color = "black") +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 4) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Baseline EDSS by physical activity",
    x = "Physical activity",
    y = "Baseline EDSS"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

kruskal.test(
  EDSS_T0 ~ physical_activity,
  data = merged
)

#Attività fisica <- EDSS long

ggplot(edss_long, aes(x = mesi, y = EDSS, color = physical_activity)) +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 1) +
  labs(title = "EDSS nel tempo per attività fisica",
       x = "Mesi",
       y = "EDSS") +
  theme_minimal()


kruskal.test(
  EDSS ~ physical_activity,
  data = edss_long %>% filter(mesi == 12)
)
kruskal.test(
  EDSS ~ physical_activity,
  data = edss_long %>% filter(mesi == 24)
)
kruskal.test(
  EDSS ~ physical_activity,
  data = edss_long %>% filter(mesi == 36)
)

#modello longitudinale (l’attività fisica cambia la velocità di progressione?)
model1 <- lmer(EDSS ~ mesi * physical_activity + (1 | ID),
              data = edss_long)

summary(model1)


####Stile di vita ↔ andamento LESIONI ####

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
                         "Mavenclad"))
edss_long <- edss_long %>%
  mutate(farmaco = case_when(
    grepl("^MAV", ID) ~ "MAV",
    grepl("^TEC", ID) ~ "TEC"
  ))


#utilizzo di edss_long
edss_long$MEDAS <- as.numeric(as.character(edss_long$MEDAS))


model2 <- lmer(
  EDSS ~ months * MEDAS + bmi + smoking_habit + physical_activity + (1 | ID),
  data = edss_long
)

summary(model2)



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



#####
#TABELLA SUMMARY 

N <- nrow(merged)

tab <- merged %>%
  select(
    GROUP,
    age,
    sex,
    bmi,
    starts_with("EDSS_"),
    starts_with("relapse_"),
    naive,
    disease_activity,
    lesion_burden,
    smoking_habit,
    physical_activity,
    therapy
  ) %>%
  tbl_summary( by = GROUP, 
                 statistic = list(
                 all_continuous() ~ "{mean} ± {sd}",
                 starts_with("EDSS_") ~ "{median} ({p25}, {p75})",
                 all_categorical() ~ "{n} ({p}%)"
               ),
               label = list(
                 # EDSS
                 EDSS_T0  ~ "T0",
                 EDSS_T6  ~ "T6",
                 EDSS_T12 ~ "T12",
                 EDSS_T18 ~ "T18",
                 EDSS_T24 ~ "T24",
                 EDSS_T30 ~ "T30",
                 EDSS_T36 ~ "T36",
                 
                 # RELAPSE
                 relapse_T6  ~ "T6",
                 relapse_T12 ~ "T12",
                 relapse_T18 ~ "T18",
                 relapse_T24 ~ "T24",
                 relapse_T30 ~ "T30",
                 relapse_T36 ~ "T36", 
                 smoking_habit ~ "Smokers"
               ),
               missing_text = "Missing values"
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  # ✅ intestazione EDSS
  modify_table_body(
    ~ .x %>%
      dplyr::add_row(
        label = "**EDSS**",
        .before = which(.x$variable == "EDSS_T0")
      )
  ) %>%
  
  # ✅ intestazione RELAPSE
  modify_table_body(
    ~ .x %>%
      dplyr::add_row(
        label = "**Relapse**",
        .before = which(.x$variable == "relapse_T6")
      )
  )%>%
  
  modify_header(label = "**Variables**")%>%


add_p(
  test = list(
    age ~ "wilcox.test",
    bmi ~ "wilcox.test",
    starts_with("EDSS_") ~ "wilcox.test",
    all_categorical() ~ "fisher.test"
  ), 
  pvalue_fun = ~style_pvalue(.x, digits = 3)
)

tab

#####




##### correzioni del report ####

counts <- edss_long %>%
  group_by(mesi) %>%
  summarise(n = sum(!is.na(EDSS)))

edss_long <- merge(edss_long, counts, by = "mesi")


ggplot(edss_long, aes(x = mesi, y = EDSS, group = ID)) +
geom_line(alpha = 0.25, color = "grey50", linewidth = 0.6) +
  geom_point(alpha = 0.4, size = 1, color = "grey40") +
  stat_summary(
    aes(group = 1),
    fun.data = mean_se,
    geom = "ribbon",
    fill = "red",
    alpha = 0.2,
    color = NA
  ) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    color = "red",
    linewidth = 1.5
  ) +
  geom_text(
    data = counts,
    aes(x = mesi, y = -0.4, label = paste0("n=", n)),
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "EDSS evolution over time",
    subtitle = "grey: patients | red: mean ± SE",
    x = "Months",
    y = "EDSS"
  ) +
  coord_cartesian(
    ylim = c(-0.5, max(edss_long$EDSS, na.rm = TRUE))
  ) +
  theme_minimal(base_size = 13)


#ALTERNATIVA n=SPESSORE
#necessario dare a counts edss_means
counts <- edss_long %>%
  dplyr::group_by(mesi) %>%
  dplyr::summarise(
    n = sum(!is.na(EDSS)),
    mean_EDSS = mean(EDSS, na.rm = TRUE)
  )


ggplot(edss_long, aes(x = mesi, y = EDSS, group = ID)) +
  geom_line(alpha = 0.12, color = "grey65", linewidth = 0.5) +
  geom_line(
    data = counts,
    aes(x = mesi, y = mean_EDSS, size = n, group=1),
    color = "#D55E00",
    inherit.aes = FALSE
  ) +
  scale_size(range = c(0.5, 2.5), guide = "none") +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 24, 30, 36),
    limits = c(0, 36)
  ) +
  coord_cartesian(ylim = c(0, 7)) +
labs(
  title = "EDSS evolution over time",
  subtitle = "Grey: patients | orange: mean (thickness = n)",
  x = "Months",
  y = "EDSS"
) +
  theme_minimal(base_size = 13)

counts <- edss_long %>%
  dplyr::group_by(mesi) %>%
  dplyr::summarise(
    n = sum(!is.na(EDSS)),
    mean_EDSS = mean(EDSS, na.rm = TRUE)
  )



###RELAPSE in formato long per sovrapposizione

relapse_long <- merged %>%
  pivot_longer(
    cols = starts_with("relapse_T"),
    names_to = "timepoint",
    values_to = "relapse"
  ) %>%
  mutate(
    mesi = as.numeric(gsub("relapse_T", "", timepoint)),
    farmaco = case_when(
      grepl("^MAV", ID) ~ "MAV",
      grepl("^TEC", ID) ~ "TEC"
    )
  )

relapse_counts <- relapse_long %>%
  group_by(mesi, farmaco) %>%
  summarise(n_relapse = sum(relapse == 1), .groups = "drop")


relapse_events <- relapse_long %>%
  dplyr::filter(relapse == 1)



#SURVIVAL PLOT x RELAPSE
surv_data_relapse <- relapse_long %>%
  arrange(ID, mesi) %>%
  group_by(ID, farmaco) %>%
  summarise(
    # verifica se esiste almeno un relapse
    has_event = any(relapse == 1, na.rm = TRUE),
    
    # verifica se il paziente ha almeno un dato valido
    has_data = any(!is.na(relapse)),
    
    time = case_when(
      has_event ~ min(mesi[relapse == 1], na.rm = TRUE),
      has_data ~ max(mesi[!is.na(relapse)], na.rm = TRUE),
      TRUE ~ max(mesi, na.rm = TRUE)  # completamente NA
    ),
    
    event = ifelse(has_event, 1, 0),
    .groups = "drop"
  )


fit <- survfit(Surv(time, event) ~ farmaco, data = surv_data_relapse)

ggsurvplot(
  fit,
  data = surv_data_relapse,
  pval = TRUE,
  pval.method = FALSE,
  pval.coord = c(20, 0.2),
  risk.table = TRUE,
  risk.table.height = 0.3,
  risk.table.fontsize = 3, 
  tables.theme = theme_cleantable(),
  break.time.by = 6,
  conf.int = FALSE,
  xlab = "Time (months)",
  ylab = "Probability of no relapse",
  legend.title = "Treatment",
  legend.labs = c("MAV", "TEC"),
  title = "Time to First Relapse",
  palette = c("#E64B35", "#4DBBD5")
)

#SURVIVAL PLOT X LESIONS

#lesioni_long_survival

lesioni_long_survival <- merged %>%
  pivot_longer(
    cols = starts_with("lesioni_T"),
    names_to = "timepoint",
    values_to = "lesioni"
  ) %>%
  mutate(
    mesi = as.numeric(gsub("lesioni_T", "", timepoint)),
    farmaco = case_when(
      grepl("^MAV", ID) ~ "MAV",
      grepl("^TEC", ID) ~ "TEC"
    )
  )


surv_data_lesioni <- lesioni_long_survival %>%
  group_by(ID, farmaco) %>%
  summarise(
    event = as.integer(any(lesioni == 1, na.rm = TRUE)),
    
    time = {
      if (any(lesioni == 1, na.rm = TRUE)) {
        min(mesi[lesioni == 1], na.rm = TRUE)
      } else if (any(!is.na(lesioni))) {
        max(mesi[!is.na(lesioni)], na.rm = TRUE)
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  )


fit_lesioni <- survfit(
  Surv(time, event) ~ farmaco,
  data = subset(surv_data_lesioni, !is.na(time))
)


ggsurvplot(
  fit_lesioni,
  data = surv_data_lesioni,
  pval = TRUE,
  pval.method = FALSE,
  pval.coord = c(20, 0.2),
  risk.table = TRUE,
  risk.table.height = 0.3,
  risk.table.fontsize = 3,
  tables.theme = theme_cleantable(),
  break.time.by = 6,
  conf.int = FALSE,
  xlab = "Time (months)",
  ylab = "Probability of no new lesions",
  legend.title = "Treatment",
  legend.labs = c("MAV", "TEC"),
  title = "Time to First Lesion",
  palette = c("#E64B35", "#4DBBD5")
)



#SURVIVAL PLOT x EDSS
#edss_long
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
                         "Mavenclad"))
edss_long <- edss_long %>%
  mutate(farmaco = case_when(
    grepl("^MAV", ID) ~ "MAV",
    grepl("^TEC", ID) ~ "TEC"
  ))

#aggiungo colonna necessaria
edss_long <- edss_long %>%
  group_by(ID) %>%
  mutate(
    baseline = EDSS[mesi == min(mesi, na.rm = TRUE)]
  ) %>%
  ungroup()


surv_data_edss <- edss_long %>%
  group_by(ID, farmaco) %>%
  summarise(
    event = as.integer(any(EDSS - baseline >= 1, na.rm = TRUE)),
    
    time = {
      if (any(EDSS - baseline >= 1, na.rm = TRUE)) {
        min(mesi[EDSS - baseline >= 1], na.rm = TRUE)
      } else if (any(!is.na(EDSS))) {
        max(mesi[!is.na(EDSS)], na.rm = TRUE)
      } else {
        NA_real_
      }
    },
    
    .groups = "drop"
  )
    

fit_edss <- survfit(
  Surv(time, event) ~ farmaco,
  data = subset(surv_data_edss, !is.na(time))
)



ggsurvplot(
  fit_edss,
  data = subset(surv_data_edss, !is.na(time)),
  pval = TRUE,
  pval.coord = c(20, 0.2),
  risk.table = TRUE,
  risk.table.height = 0.35,
  risk.table.fontsize = 2.8,
  risk.table.y.text = FALSE,
  break.time.by = 6,
  conf.int = FALSE,
  xlab = "Time (months)",
  ylab = "Probability of no EDSS progression",
  legend.title = "Treatment",
  legend.labs = c("MAV", "TEC"),
  title = "Time to EDSS Progression",
  palette = c("#E64B35", "#4DBBD5")
)

#SURVIVAL x NEDA3
#creazione struttura

#relapse_event

relapse_long <- merged %>%
  pivot_longer(
    cols = starts_with("relapse_T"),
    names_to = "timepoint",
    values_to = "relapse"
  ) %>%
  mutate(
    mesi = as.numeric(gsub("relapse_T", "", timepoint)),
    farmaco = case_when(
      grepl("^MAV", ID) ~ "MAV",
      grepl("^TEC", ID) ~ "TEC"
    )
  )


relapse_event <- relapse_long %>%
  filter(relapse == 1) %>%
  group_by(ID) %>%
  summarise(
    t_relapse = min(mesi),
    relapse_event = 1,
    .groups = "drop"
  )



all_ids <- relapse_long %>%
  distinct(ID)

relapse_event <- all_ids %>%
  left_join(relapse_event, by = "ID")



relapse_event <- relapse_event %>%
  mutate(
    relapse_event = ifelse(is.na(relapse_event), 0, relapse_event)
  )

#inserisco colonne in merged

merged <- merged %>%
  left_join(relapse_event, by = "ID")

merged <- merged %>%
  left_join(surv_data_edss, by = "ID")


#surv_data_edss_neda
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
                         "Mavenclad"))
edss_long <- edss_long %>%
  mutate(farmaco = case_when(
    grepl("^MAV", ID) ~ "MAV",
    grepl("^TEC", ID) ~ "TEC"
  ))

edss_long_neda <- edss_long %>%
  group_by(ID) %>%
  
  mutate(
    baseline = EDSS[mesi == min(mesi, na.rm = TRUE)],
    delta = EDSS - baseline,
    prog = case_when(
      baseline == 0 & delta >= 1.5 ~ 1,
      baseline > 0 & baseline <= 5 & delta >= 1 ~ 1,
      baseline > 5 & delta >= 0.5 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup()


surv_data_edss_neda <- edss_long_neda %>%
  group_by(ID, farmaco) %>%
  summarise(
    edss_event = as.integer(any(prog == 1, na.rm = TRUE)),
    t_edss = if (any(prog == 1, na.rm = TRUE)) {
      min(mesi[prog == 1], na.rm = TRUE)
    } else {
      max(mesi, na.rm = TRUE)
    },
    .groups = "drop"
  )

#mri_event

mri_event <- lesioni_long %>%
  group_by(ID) %>%
  summarise(
    # evento sì/no
    mri_event = as.integer(any(lesioni == 1, na.rm = TRUE)),
    
    # tempo evento (o censura)
    t_mri = if (any(lesioni == 1, na.rm = TRUE)) {
      min(mesi[lesioni == 1], na.rm = TRUE)
    } else {
      max(mesi, na.rm = TRUE)
    },
    
    .groups = "drop"
  )


#follow-up
followup <- bind_rows(
  edss_long %>% select(ID, mesi),
  relapse_long %>% select(ID, mesi),
  lesioni_long %>% select(ID, mesi)
) %>%
  group_by(ID) %>%
  summarise(
    last_followup = max(mesi, na.rm = TRUE),
    .groups = "drop"
  )

#neda bozza
neda_data <- followup %>%
  left_join(relapse_event, by = "ID") %>%
  left_join(surv_data_edss_neda, by = "ID") %>%
  left_join(mri_event, by = "ID")

#includo gadolinium anche se non è long
neda_data <- neda_data %>%
  left_join(merged %>% select(ID, gadolinium_contrast), by = "ID")




#NEDA FINALE
neda_data <- neda_data %>%
  mutate(
    # calcolo tempo evento in modo robusto
    time_event = pmin(
      coalesce(t_relapse, Inf),
      coalesce(t_edss, Inf),
      coalesce(t_mri, Inf)
    ),
    # evento globale
    event = ifelse(
      relapse_event == 1 |
        edss_event == 1 |
        mri_event == 1 |
        gadolinium_contrast == 1,
      1, 0
    ),
    
    time = coalesce(
      ifelse(event == 1, time_event, last_followup),
      last_followup
    )
  )



neda_data <- neda_data %>%
  mutate(
    farmaco = gsub("farmaco=", "", farmaco)
  )
neda_data$farmaco <- factor(neda_data$farmaco, levels = c("MAV", "TEC"))

unique(neda_data$farmaco)
#SURVIVAL NEDA

fit_neda <- survfit(Surv(time, event) ~ farmaco, data = neda_data)

ggsurvplot(fit_neda,
          conf.int = TRUE,
          pval = TRUE,
          
          palette = c("#1f78b4", "#e31a1c"),
          size = 1.3,
          conf.int.alpha = 0.15,
          
          xlab = "Months",
          ylab = "NEDA-3 Probability",
          title = "NEDA-3 according to Therapy",
          
          ggtheme = theme_classic(base_size = 14),
          
          
          
          risk.table = TRUE,
          risk.table.height = 0.32,   # ✅ più spazio verticale
          risk.table.fontsize = 5,    # ✅ numeri più grandi
          
          risk.table.y.text = TRUE,
          risk.table.y.text.col = TRUE,
          
          # ✅ questo è fondamentale per separare righe
          tables.theme = theme_cleantable(),
          
          legend.labs = c("MAV", "TEC"),
          legend.title = "Therapy"
)

#mettere a posto ordinatamente l'asse x dei mesi, fare combaciare i number at risk con i tempi
           


#tentativo 1: grafico con rug 
ggplot(edss_long, aes(x = mesi, y = EDSS, color = farmaco)) +
  stat_summary(
    fun = median,
    geom = "line",
    linewidth = 1.3
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3
  ) +
  geom_rug(
    data = relapse_events,
    aes(x = mesi, color = farmaco),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.7
  ) +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 24, 30, 36)
  ) +
  coord_cartesian(ylim = c(0.8, 1.6)) +
  labs(
    title = "Median EDSS over time by treatment",
    subtitle = "Rug = relapse events",
    x = "Months of follow-up",
    y = "EDSS",
    color = "Treatment"
  ) +
  
  theme_minimal()



#tentativo 2 (mix di plot)
p1 <- ggplot(edss_long, aes(x = mesi, y = EDSS, color = farmaco)) +
  stat_summary(fun = median, geom = "line", linewidth = 1.3) +
  stat_summary(fun = median, geom = "point", size = 3) +
  scale_x_continuous(breaks = c(0,6,12,18,24,30,36), limits = c(0,36))+ 
  labs(y = "EDSS", x = NULL) +
  theme_minimal()

# relapse aggregati

relapse_counts <- relapse_long %>%
  dplyr::group_by(mesi, farmaco) %>%
  dplyr::summarise(
    n = sum(relapse == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    mesi = c(0, 6, 12, 18, 24, 30, 36),
    farmaco,
    fill = list(n = 0)
  )




p2 <- ggplot(relapse_counts,
             aes(x = mesi, y = n, fill = farmaco)) +
  geom_col(
    position = "dodge",
    alpha = 0.7
  ) +
  scale_y_continuous(
    limits = c(0, max(relapse_counts$n, na.rm = TRUE) + 1)
  ) +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 24, 30, 36)
  ) +
  labs(
    y = "Relapses",
    x = "Months"
  ) +
  theme_minimal()

p1 / p2

#tentativo 3:EDSS prima e dopo un relapse

first_relapse <- relapse_long %>%
  filter(relapse == 1) %>%
  group_by(ID) %>%
  summarise(relapse_time = min(mesi), .groups = "drop")

data_rel <- edss_long %>%
  left_join(first_relapse, by = "ID") %>%
  filter(!is.na(relapse_time)) %>%
  mutate(time_from_relapse = mesi - relapse_time)


data_rel_trim <- data_rel %>%
  filter(time_from_relapse >= -12 & time_from_relapse <= 12)

#plot con range totale
ggplot(data_rel, aes(x = time_from_relapse, y = EDSS)) +
  # media
  stat_summary(fun = mean, geom = "line", color = "#E64B35", linewidth = 1.3) +
  stat_summary(fun = mean, geom = "point", color = "#E64B35", size = 2) +
  # errore
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 1) +
  # linea verticale = relapse
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Time from relapse (months)",
    y = "Mean EDSS",
    title = "Change in EDSS Around Relapse"
  ) +
  theme_minimal(base_size = 14)



#plot con range piccolo
ggplot(data_rel_trim, aes(x = time_from_relapse, y = EDSS)) +
  
  # traiettorie individuali (trasparenza)
  geom_line(aes(group = ID), color = "gray", alpha = 0.3) +
  
  # media
  stat_summary(fun = mean, geom = "line", color = "#E64B35", linewidth = 1.3) +
  stat_summary(fun = mean, geom = "point", color = "#E64B35", size = 2) +
  
  # barre di errore (SE)
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 1) +
  
  # linea verticale del relapse
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  
  labs(
    x = "Time from relapse (months)",
    y = "EDSS",
    title = "EDSS Trajectories Around Relapse (±12 months)"
  ) +
  
  theme_minimal(base_size = 14)


#plot piu esplicativo (BOXPLOT)
data_rel2 <- data_rel %>%
  mutate(period = case_when(
    time_from_relapse < 0 ~ "Before relapse",
    time_from_relapse == 0 ~ "At relapse",
    time_from_relapse > 0 ~ "After relapse"
  ))

ggplot(data_rel2, aes(x = period, y = EDSS)) +
  
  geom_boxplot(fill = "#4DBBD5", alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  
  labs(
    x = "",
    y = "EDSS",
    
    title = "EDSS Before and After Relapse"
  ) +
  
  theme_minimal(base_size = 14)

#plot piu esplicativo x trattamento (BOXPLOT)

ggplot(data_rel2 %>% filter(!is.na(EDSS)),
       aes(x = period, y = EDSS, fill = farmaco)) +
  geom_boxplot(
    alpha = 0.7,
    position = position_dodge(width = 0.8)
  ) +
  geom_jitter(
    aes(color = farmaco),
    alpha = 0.5,
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = 0.8
    )
  ) +
  labs(
    x = "",
    y = "EDSS",
    title = "EDSS Before and After Relapse by Treatment"
  ) +
  scale_fill_manual(values = c("TEC" = "#4DBBD5", "MAV" = "#E64B35")) +
  scale_color_manual(values = c("TEC" = "#4DBBD5", "MAV" = "#E64B35")) +
  
  theme_minimal(base_size = 14)



##### VIOLIN PLOT di "factors associated with baseline EDSS" (lesion burden)

na_counts_lesion <- merged %>%
  group_by(lesion_burden) %>%
  summarise(
    n_NA = sum(is.na(EDSS_T0)),
    y_pos = max(EDSS_T0, na.rm = TRUE) + 1.5
  )


ggplot(merged, aes(x = factor(lesion_burden), y = EDSS_T0)) +
  geom_violin(trim = FALSE, fill = "grey80", color = "black") +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 1) +
  
  geom_text(
    data = na_counts_lesion,
    aes(x = factor(lesion_burden), y = y_pos,
        label = paste0("NA: ", n_NA)),
    inherit.aes = FALSE,
    
    size = 3,
    vjust = 0
  ) +
  
  labs(
    x = "Lesion Burden (T0)",
    y = "EDSS T0",
    title = "Baseline EDSS by lesion burden"
  ) +
  theme_minimal()

    

    