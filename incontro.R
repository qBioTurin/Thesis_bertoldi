
#apertura di merged
merged <- read.csv2("input/merged.csv")
#PRIMO QUESITO 
#COLONNA EDSS FINALE
merged <- merged %>%
  rowwise() %>%
  mutate(
    EDSS_finale = last(na.omit(c(
      EDSS_T0,
      EDSS_T6,
      EDSS_T12,
      EDSS_T18,
      EDSS_T24,
      EDSS_T30,
      EDSS_T36
    )))
  ) %>%
  ungroup()

#EDSS DELTA
merged <- merged %>%
  mutate(
    delta_EDSS = EDSS_finale - EDSS_T0
  )
#COLONNA EDSS ANDAMENTO
merged <- merged %>%
  mutate(
    andamento_EDSS = case_when(
      
      delta_EDSS >= 0.5 ~ "Worsening",
      
      delta_EDSS <= -0.5 ~ "Improvement",
      
      abs(delta_EDSS) < 0.5 & EDSS_T0 < 4 ~
        "Low stable",
      
      abs(delta_EDSS) < 0.5 & EDSS_T0 >= 4 ~    
        "High stable"
    )
  )
#ANDAMENTO TABELLA1
table(merged$andamento_EDSS)

table(merged$GROUP,
      merged$andamento_EDSS)

tabella1 <- table(merged$GROUP,
              merged$andamento_EDSS)

fisher.test(tabella1)


#EVENTO COMPOSITO 
merged <- merged %>%
  mutate(
    
    evento_EDSS =
      delta_EDSS >= 0.5,
    
    evento_lesioni =
      lesioni_T6==1 |
      lesioni_T12==1 |
      lesioni_T18==1 |
      lesioni_T24==1 |
      lesioni_T30==1 |
      lesioni_T36==1,
    
    evento_relapse =
      relapse_T6==1 |
      relapse_T12==1 |
      relapse_T18==1 |
      relapse_T24==1 |
      relapse_T30==1 |
      relapse_T36==1
  )

merged <- merged %>%
  mutate(
    
    worsening =
      evento_EDSS |
      evento_lesioni |
      evento_relapse
  )

merged <- merged %>%
  mutate(worsening = replace_na(worsening, FALSE))

table(merged$worsening)

#evento di worsening
tabella2 <- table(merged$GROUP,
                  merged$worsening)
table (merged$GROUP,
       merged$worsening)
fisher.test(tabella2)

#BARPLOT CON TEST 
ggplot(subset (merged,!is.na(andamento_EDSS) & !is.na(GROUP)),
       aes(x = andamento_EDSS,
           fill = GROUP)) +
  
  geom_bar(position = "dodge") +
  
  labs(
    x = "Clinical trend",
    y = "Patients",
    fill = "Treatment"
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

#PERCENTUALE DI WORSENING
ggplot(merged,
       aes(x = GROUP,
           fill = worsening)) +
  
  geom_bar(position = "fill") +
  
  scale_y_continuous(labels = scales::percent) +
  
  labs(
    x = "Group",
    y = "Percentage",
    fill = "At least 1 worsening"
  ) +
  
  theme_minimal()


#GC TREATMENT
#colonna gc_at_least indica che ha fatto almeno un giorno di cortisone
merged <- merged %>%
  mutate(
    gc_at_least = factor(
      if_else(
        if_any(starts_with("cortisone_T"), ~ . > 0),
        "positive",
        "negative"
      ),
      levels = c("negative", "positive")
    )
  )


#frequenza
table(merged$GROUP, merged$gc_treatment)

#durata del trattamento
merged <- merged %>%
  mutate(
    gc = ifelse(gc_treatment == "positive", 1, 0)
  )

merged <- merged %>%
  mutate(
    gc_days_total = rowSums(
      across(starts_with("cortisone_T")),
      na.rm = TRUE
    )
  )  

merged <- merged %>%
  mutate(
    gc = ifelse(gc_treatment == "positive", 1, 0),
    group = GROUP,  # TEC / MAV
    gc_group = case_when(
      group == "TEC" & gc == 1 ~ "TEC_GC+",
      group == "TEC" & gc == 0 ~ "TEC_GC-",
      group == "MAV" & gc == 1 ~ "MAV_GC+",
      group == "MAV" & gc == 0 ~ "MAV_GC-"
    )
  )


#SURVIVAL PLOT
merged <- merged %>%
  mutate(
    event_6_C  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_C = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_C = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_C = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_C = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_C = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1| lesioni_T36 == 1
  )

merged <- merged %>%
  mutate(
    event_time_C = case_when(
      event_6_C  ~ 6,
      event_12_C ~ 12,
      event_18_C ~ 18,
      event_24_C ~ 24,
      event_30_C ~ 30,
      event_36_C ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_C = ifelse(is.na(event_time_C), 0, 1),
    time_C = ifelse(is.na(event_time_C), 36, event_time_C)
  )



surv_obj <- Surv(time = merged$time_C, event = merged$event_status_C)
fit <- survfit(surv_obj ~ gc_group, data = merged)

ggsurvplot(
  fit,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC-", "TEC GC+", "MAV GC-", "MAV GC+"),
  xlab = "Months",
  ylab = "Event-free survival"
)



#SURVIVAL SOLO EDSS-GC TREAT

merged <- merged %>%
  mutate(
    event_6_1   = (EDSS_T6  - EDSS_T0 >= 0.5),
    event_12_1  = (EDSS_T12 - EDSS_T6 >= 0.5),
    event_18_1  = (EDSS_T18 - EDSS_T12 >= 0.5),
    event_24_1  = (EDSS_T24 - EDSS_T18 >= 0.5),
    event_30_1  = (EDSS_T30 - EDSS_T24 >= 0.5),
    event_36_1  = (EDSS_T36 - EDSS_T30 >= 0.5)
  )

merged <- merged %>%
  mutate(
    event_time_1 = case_when(
      event_6_1   ~ 6,
      event_12_1  ~ 12,
      event_18_1  ~ 18,
      event_24_1  ~ 24,
      event_30_1  ~ 30,
      event_36_1  ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_1 = ifelse(is.na(event_time_1), 0, 1),
    time_1 = ifelse(is.na(event_time_1), 36, event_time_1)
  )

surv_obj1 <- Surv(time = merged$time_1, event = merged$event_status_1)
fit1 <- survfit(surv_obj1 ~ gc_group, data = merged)

ggsurvplot(
  fit1,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC-", "TEC GC+", "MAV GC-", "MAV GC+"),
  xlab = "Months",
  ylab = "EDSS progression-free survival"
)

#SURVIVAL RELAPSE- GC TREAT
merged <- merged %>%
  mutate(
    event_6_2   = relapse_T6  == 1,
    event_12_2  = relapse_T12  == 1,
    event_18_2  = relapse_T18  == 1,
    event_24_2  = relapse_T24  == 1,
    event_30_2  = relapse_T30  == 1,
    event_36_2  = relapse_T36  == 1
  )

merged <- merged %>%
  mutate(
    event_time_2 = case_when(
      event_6_2   ~ 6,
      event_12_2  ~ 12,
      event_18_2  ~ 18,
      event_24_2  ~ 24,
      event_30_2  ~ 30,
      event_36_2  ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_2 = ifelse(is.na(event_time_2), 0, 1),
    time_2 = ifelse(is.na(event_time_2), 36, event_time_2)
  )

surv_obj2 <- Surv(time = merged$time_2, event = merged$event_status_2)
fit2 <- survfit(surv_obj2 ~ gc_group, data = merged)

ggsurvplot(
  fit2,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC-", "TEC GC+", "MAV GC-", "MAV GC+"),
  xlab = "Months",
  ylab = "Relapse-free survival"
)


#SURVIVAL LESIONI-GC TREAT 
merged <- merged %>%
  mutate(
    event_6_3   = lesioni_T6  == 1,
    event_12_3  = lesioni_T12 == 1,
    event_18_3  = lesioni_T18  == 1,
    event_24_3  = lesioni_T24  == 1,
    event_30_3  = lesioni_T30  == 1,
    event_36_3  = lesioni_T36  == 1
  )

merged <- merged %>%
  mutate(
    event_time_3 = case_when(
      event_6_3   ~ 6,
      event_12_3  ~ 12,
      event_18_3  ~ 18,
      event_24_3  ~ 24,
      event_30_3  ~ 30,
      event_36_3  ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_3 = ifelse(is.na(event_time_3), 0, 1),
    time_3 = ifelse(is.na(event_time_3), 36, event_time_3)
  )

surv_obj3 <- Surv(time = merged$time_3, event = merged$event_status_3)
fit3 <- survfit(surv_obj3 ~ gc_group, data = merged)

ggsurvplot(
  fit3,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC-", "TEC GC+", "MAV GC-", "MAV GC+"),
  xlab = "Months",
  ylab = "Lesion-free survival"
)


#CATEGORIE DI DURATA CORTISONE
merged <- merged %>%
  mutate(
    gc_category = case_when(
      rowSums(across(starts_with("cortisone_T"), ~ . > 0), na.rm = TRUE) == 0 ~ "None",
      rowSums(across(starts_with("cortisone_T"), ~ . > 0), na.rm = TRUE) <= 2 ~ "1-2",
      TRUE ~ "more_2"
    )
  )

ggplot(merged, aes(x = gc_category)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~GROUP) +
  labs(
    x = "GC category",
    y = "Number of patients"
  ) +
  theme_classic()


#plot durata cortisone 
gc_long <- merged %>%
  select(
    ID,
    GROUP,
    cortisone_T6,
    cortisone_T12,
    cortisone_T18,
    cortisone_T24,
    cortisone_T30,
    cortisone_T36
  ) %>%
  pivot_longer(
    cols = starts_with("cortisone_T"),
    names_to = "Timepoint",
    values_to = "Days"
  ) %>%
  mutate(
    Timepoint = gsub("cortisone_", "", Timepoint),
    Days = replace_na(Days, 0)
  )


ggplot(gc_long,
       aes(x = factor(ID),
           y = Days,
           fill = Timepoint)) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = ifelse(Days > 0, Days, "")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  labs(
    x = "Patient ID",
    y = "Days of corticosteroid treatment",
    fill = "Timepoint"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

#violin plot durata cortisone 
gc_long2 <- merged %>%
  select(
    ID,
    GROUP,
    cortisone_T6,
    cortisone_T12,
    cortisone_T18,
    cortisone_T24,
    cortisone_T30,
    cortisone_T36
  ) %>%
  pivot_longer(
    cols = starts_with("cortisone_T"),
    names_to = "Timepoint",
    values_to = "Days"
  ) %>%
  mutate(
    Timepoint = gsub("cortisone_", "", Timepoint),
    Timepoint = factor(
      Timepoint,
      levels = c("T6","T12","T18","T24","T30","T36")
    )
  )
ggplot(gc_long2,
       aes(x = Timepoint,
           y = Days,
           fill =GROUP)) +
  
  geom_violin(position = position_dodge(width = 0.8),
              trim = FALSE,
              alpha = 0.7) +
  
  geom_boxplot(position = position_dodge(width = 0.8),
               width = 0.15,
               outlier.shape = NA,
               alpha = 0.8) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    position = position_dodge(width = 0.8),
    shape = 23,
    size = 2.5,
    fill = "white"
  ) +
  
  labs(
    x = "Timepoint",
    y = "Days of corticosteroid treatment",
    fill = "Group"
  ) +
  
  theme_classic()


#colonna cambio terapia
#quanti cambiano
merged <- merged %>%
  mutate(
    cambio_terapia = case_when(
      cambio_terapia == "1" ~ "Yes",
      cambio_terapia == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )
table(merged$cambio_terapia)

ggplot(subset(merged, !is.na(cambio_terapia) & !is.na(GROUP)),
       aes(x = cambio_terapia, fill = GROUP)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Therapy change",
    y = "Patients",
    fill = "GROUP"
  ) +
  theme_minimal()

    
#mesi al cambio
merged <- merged %>%
  mutate(
    data_prelievo_T0 = as.Date(data_prelievo_T0),
    data_cambio_terapia = as.Date(data_cambio_terapia),
    
    mesi_cambio = interval(data_prelievo_T0,
                           data_cambio_terapia) / months(1)
  )


table(merged$GROUP, merged$cambio_terapia)


#SURVIVAL PLOT
#creo cambio_group 
merged <- merged %>%
  mutate(
    cambio_group = case_when(
      GROUP == "MAV" &
        cambio_terapia == "Yes" ~ "MAV cambio +",
      
      GROUP == "MAV" &
        cambio_terapia == "No" ~ "MAV cambio -",
      
      GROUP == "TEC" &
        cambio_terapia == "Yes" ~ "TEC cambio +",
      
      GROUP == "TEC" &
        cambio_terapia == "No" ~ "TEC cambio -"
    )
  )

#survival
merged <- merged %>%
  mutate(
    event_6_CT  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_CT = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_CT = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_CT = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_CT = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1| lesioni_T36 == 1
  )
merged <- merged %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      event_24_CT ~ 24,
      event_30_CT ~ 30,
      event_36_CT ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 36, event_time_CT)
  )

surv_obj4 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit4 <- survfit(surv_obj4 ~ cambio_group, data = merged)

ggsurvplot(
  fit4,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC cambio-", "TEC cambio+", "MAV cambio-", "MAV cambio+"),
  xlab = "Months",
  ylab = "Event-free survival"
)


#SURVIVAL EDSS- CAMBIO TERAPIA
surv_obj5 <- Surv(time = merged$time_1, event = merged$event_status_1)
fit5 <- survfit(surv_obj5 ~ cambio_group, data = merged)

ggsurvplot(
  fit5,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC cambio-", "TEC cambio+", "MAV cambio-", "MAV cambio+"),
  xlab = "Months",
  ylab = "EDSS progression-free survival"
)

#SURVIVALRELAPSE- CAMBIO TERAPIA
surv_obj6 <- Surv(time = merged$time_2, event = merged$event_status_2)
fit6 <- survfit(surv_obj6 ~ cambio_group, data = merged)

ggsurvplot(
  fit6,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC cambio-", "TEC cambio+", "MAV cambio-", "MAV cambio+"),
  xlab = "Months",
  ylab = "Relapse-free survival"
)

#SURVIVAL LESIONI- CAMBIO GRUPPO
surv_obj7 <- Surv(time = merged$time_3, event = merged$event_status_3)
fit7 <- survfit(surv_obj7 ~ cambio_group, data = merged)

ggsurvplot(
  fit7,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC cambio-", "TEC cambio+", "MAV cambio-", "MAV cambio+"),
  xlab = "Months",
  ylab = "Lesion-free survival"
)



#GADOLINIUM -> analisi aggiuntive
table(merged$gadolinium_contrast)

merged <- merged %>%
  mutate(
    gadolinium_contrast = case_when(
      gadolinium_contrast == "1" ~ "Yes",
      gadolinium_contrast == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )

merged <- merged %>%
  mutate(
    gado_group = case_when(
      GROUP == "MAV" &
        gadolinium_contrast == "Yes" ~ "MAV gado +",
      
      GROUP == "MAV" &
        gadolinium_contrast == "No" ~ "MAV gado -",
      
      GROUP == "TEC" &
        gadolinium_contrast == "Yes" ~ "TEC gado +",
      
      GROUP == "TEC" &
        gadolinium_contrast == "No" ~ "TEC gado -"
    )
  )

#survival composito x gado
merged <- merged %>%
  mutate(
    event_6_CT  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_CT = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_CT = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_CT = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_CT = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1| lesioni_T36 == 1
  )
merged <- merged %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      event_24_CT ~ 24,
      event_30_CT ~ 30,
      event_36_CT ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 36, event_time_CT)
  )
surv_obj8 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit8 <- survfit(surv_obj8 ~ gado_group, data = merged)

ggsurvplot(
  fit8,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC gado-", "TEC gado+", "MAV gado-", "MAV gado+"),
  xlab = "Months",
  ylab = "Event-free survival"
)

#survival composito x bone_marrow
merged <- merged %>%
  mutate(
    bone_marrow_lesions = case_when(
      bone_marrow_lesions == "1" ~ "Yes",
      bone_marrow_lesions == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )

merged <- merged %>%
  mutate(
    bone_group = case_when(
      GROUP == "MAV" &
        bone_marrow_lesions == "Yes" ~ "MAV spinal +",
      
      GROUP == "MAV" &
        bone_marrow_lesions == "No" ~ "MAV spinal -",
      
      GROUP == "TEC" &
        bone_marrow_lesions == "Yes" ~ "TEC spinal +",
      
      GROUP == "TEC" &
        bone_marrow_lesions == "No" ~ "TEC spinal -"
    )
  )


surv_obj9 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit9 <- survfit(surv_obj9 ~ bone_group, data = merged)

ggsurvplot(
  fit9,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC spinal-", "TEC spinal+", "MAV spinal-", "MAV spinal+"),
  xlab = "Months",
  ylab = "Event-free survival"
)

#survival composito x lesion_burden
merged <- merged %>%
  mutate(
    lesion_burden = case_when(
      lesion_burden == "1" ~ "Yes",
      lesion_burden == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )

merged <- merged %>%
  mutate(
    burden_group = case_when(
      GROUP == "MAV" &
        lesion_burden == "Yes" ~ "MAV burden +",
      
      GROUP == "MAV" &
        lesion_burden == "No" ~ "MAV burden -",
      
      GROUP == "TEC" &
        lesion_burden == "Yes" ~ "TEC burden +",
      
      GROUP == "TEC" &
        lesion_burden == "No" ~ "TEC burden -"
    )
  )


surv_obj10 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit10 <- survfit(surv_obj10 ~ burden_group, data = merged)

ggsurvplot(
  fit10,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC burden-", "TEC burden+", "MAV burden-", "MAV burden+"),
  xlab = "Months",
  ylab = "Event-free survival"
)

merged <- merged %>%
  mutate(
    lesion_burden = case_when(
      lesion_burden == "1" ~ "Yes",
      lesion_burden == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )

#survival composito x subtentorial
merged <- merged %>%
  mutate(
    subtentorial_lesions = case_when(
      subtentorial_lesions == "1" ~ "Yes",
      subtentorial_lesions == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )
merged <- merged %>%
  mutate(
    sub_group = case_when(
      GROUP == "MAV" &
        subtentorial_lesions == "Yes" ~ "MAV subt +",
      
      GROUP == "MAV" &
        subtentorial_lesions == "No" ~ "MAV subt -",
      
      GROUP == "TEC" &
        subtentorial_lesions == "Yes" ~ "TEC subt +",
      
      GROUP == "TEC" &
        subtentorial_lesions == "No" ~ "TEC subt -"
    )
  )


surv_obj11 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit11 <- survfit(surv_obj11 ~ sub_group, data = merged)

ggsurvplot(
  fit11,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC subt-", "TEC subt+", "MAV subt-", "MAV subt+"),
  xlab = "Months",
  ylab = "Event-free survival"
)

