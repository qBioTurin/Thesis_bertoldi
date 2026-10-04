source("library.R")

merged <- read.csv2("input/merged.csv")

##NAIVE
merged$naive <- as.factor(merged$naive)
fisher.test(table(merged$naive, merged$GROUP))

##SEX
fisher.test(table(merged$sex, merged$GROUP))

##AGE AT T0
shapiro.test(merged$age_at_T0[merged$group == "MAV"])
shapiro.test(merged$age_at_T0[merged$group == "TEC"])
wilcox.test(age_at_T0 ~ group, data = merged)

##MEDAS continuo
shapiro.test(merged$MEDAS[merged$GROUP == "MAV"])
shapiro.test(merged$MEDAS[merged$GROUP == "TEC"])
merged$MEDAS <- as.numeric(as.character(merged$MEDAS))
wilcox.test(MEDAS ~ group, data = merged)
#MEDAS_cat
fisher.test(table(merged$MEDAS_cat, merged$GROUP))

##BMI continuo
merged$bmi <- as.numeric(as.character(merged$bmi))
shapiro.test(merged$bmi[merged$GROUP == "MAV"])
shapiro.test(merged$bmi[merged$GROUP == "TEC"])
wilcox.test(bmi ~ group, data = merged)
#bmi_cat
fisher.test(table(merged$bmi_cat, merged$GROUP))

#LESION BURDEN 
fisher.test(table(merged$lesion_burden, merged$GROUP))

#SUBTENTORIAL 
fisher.test(table(merged$subtentorial_lesions, merged$GROUP))

#GADOLINIUM
fisher.test(table(merged$gadolinium_contrast, merged$GROUP))

#SMOKING
fisher.test(table(merged$smoking_habit, merged$GROUP))

#PHYSISCAL ACTIVITY 
fisher.test(table(merged$physical_activity, merged$GROUP))

#GC TREATMENT 
fisher.test(table(merged$gc_treatment, merged$GROUP))


#EDSS BASELINE

mean(merged$EDSS_T0, na.rm = TRUE)
median(merged$EDSS_T0, na.rm = TRUE)

quantile(
  merged$EDSS_T0,
  probs = c(0.25, 0.75),
  na.rm = TRUE
)

range(merged$EDSS_T0, na.rm = TRUE)


sort(
  table(merged$EDSS_T0),
  decreasing = TRUE
) #frequenze edss


sum(merged$EDSS_T0 >= 3, na.rm = TRUE) #Numero di pazienti con EDSS ≥3


wilcox.test(EDSS_T0 ~ GROUP,
            data = merged) # Confronto MAV vs TEC

merged %>%
  group_by(GROUP) %>%
  summarise(
    N = n(),
    Mean = mean(EDSS_T0, na.rm = TRUE),
    SD = sd(EDSS_T0, na.rm = TRUE),
    Median = median(EDSS_T0, na.rm = TRUE),
    Q1 = quantile(EDSS_T0, 0.25, na.rm = TRUE),
    Q3 = quantile(EDSS_T0, 0.75, na.rm = TRUE),
    Min = min(EDSS_T0, na.rm = TRUE),
    Max = max(EDSS_T0, na.rm = TRUE)
  ) # Statistiche per gruppo





