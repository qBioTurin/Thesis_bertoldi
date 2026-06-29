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

tabella2 <- table(merged$GROUP,
                  merged$worsening)
fisher.test(tabella2)

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

#BARPLOT CON TEST 
ggplot(merged,
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
    x = "Gruppo",
    y = "Percentuale",
    fill = "Almeno un worsening"
  ) +
  
  theme_minimal()

#GC TREATMENT
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
    event_6  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == "Yes" | lesioni_T6  == "Yes",
    event_12 = (EDSS_T12 - EDSS_T0 >= 0.5) | relapse_T12 == "Yes" | lesioni_T12 == "Yes",
    event_18 = (EDSS_T18 - EDSS_T0 >= 0.5) | relapse_T18 == "Yes" | lesioni_T18 == "Yes",
    event_24 = (EDSS_T24 - EDSS_T0 >= 0.5) | relapse_T24 == "Yes" | lesioni_T24 == "Yes",
    event_30 = (EDSS_T30 - EDSS_T0 >= 0.5) | relapse_T30 == "Yes" | lesioni_T30 == "Yes",
    event_36 = (EDSS_T36 - EDSS_T0 >= 0.5) | relapse_T36 == "Yes" | lesioni_T36 == "Yes"
  )

merged <- merged %>%
  mutate(
    event_time = case_when(
      event_6  ~ 6,
      event_12 ~ 12,
      event_18 ~ 18,
      event_24 ~ 24,
      event_30 ~ 30,
      event_36 ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status = ifelse(is.na(event_time), 0, 1),
    time = ifelse(is.na(event_time), 36, event_time)
  )



surv_obj3 <- Surv(time = merged$time, event = merged$event_status)
fit3 <- survfit(surv_obj ~ gc_group, data = merged)

ggsurvplot(
  fit3,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC-", "TEC GC+", "MAV GC-", "MAV GC+"),
  xlab = "Months",
  ylab = "Event-free survival"
)


#colonna cambio terapia
#quanti cambiano
table(merged$cambio_terapia)

#
merged <- merged %>%
  mutate(
    data_prelievo_T0 = as.Date(data_prelievo_T0),
    data_cambio_terapia = as.Date(data_cambio_terapia),
    
    mesi_cambio = interval(data_prelievo_T0,
                           data_cambio_terapia) / months(1)
  )

merged <- merged %>%
  mutate(
    cambio_terapia = case_when(
      cambio_terapia == "1" ~ "Yes",
      cambio_terapia == "0" ~ "No",
      TRUE ~ NA_character_
    )
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
    event_6  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == "Yes" | lesioni_T6  == "Yes",
    event_12 = (EDSS_T12 - EDSS_T0 >= 0.5) | relapse_T12 == "Yes" | lesioni_T12 == "Yes",
    event_18 = (EDSS_T18 - EDSS_T0 >= 0.5) | relapse_T18 == "Yes" | lesioni_T18 == "Yes",
    event_24 = (EDSS_T24 - EDSS_T0 >= 0.5) | relapse_T24 == "Yes" | lesioni_T24 == "Yes",
    event_30 = (EDSS_T30 - EDSS_T0 >= 0.5) | relapse_T30 == "Yes" | lesioni_T30 == "Yes",
    event_36 = (EDSS_T36 - EDSS_T0 >= 0.5) | relapse_T36 == "Yes" | lesioni_T36 == "Yes"
  )
merged <- merged %>%
  mutate(
    event_time = case_when(
      event_6  ~ 6,
      event_12 ~ 12,
      event_18 ~ 18,
      event_24 ~ 24,
      event_30 ~ 30,
      event_36 ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status = ifelse(is.na(event_time), 0, 1),
    time = ifelse(is.na(event_time), 36, event_time)
  )

surv_obj4 <- Surv(time = merged$time, event = merged$event_status)
fit4 <- survfit(surv_obj ~ cambio_group, data = merged)

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

#survival
merged <- merged %>%
  mutate(
    event_6  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == "Yes" | lesioni_T6  == "Yes",
    event_12 = (EDSS_T12 - EDSS_T0 >= 0.5) | relapse_T12 == "Yes" | lesioni_T12 == "Yes",
    event_18 = (EDSS_T18 - EDSS_T0 >= 0.5) | relapse_T18 == "Yes" | lesioni_T18 == "Yes",
    event_24 = (EDSS_T24 - EDSS_T0 >= 0.5) | relapse_T24 == "Yes" | lesioni_T24 == "Yes",
    event_30 = (EDSS_T30 - EDSS_T0 >= 0.5) | relapse_T30 == "Yes" | lesioni_T30 == "Yes",
    event_36 = (EDSS_T36 - EDSS_T0 >= 0.5) | relapse_T36 == "Yes" | lesioni_T36 == "Yes"
  )
merged <- merged %>%
  mutate(
    event_time = case_when(
      event_6  ~ 6,
      event_12 ~ 12,
      event_18 ~ 18,
      event_24 ~ 24,
      event_30 ~ 30,
      event_36 ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status = ifelse(is.na(event_time), 0, 1),
    time = ifelse(is.na(event_time), 36, event_time)
  )

surv_obj5 <- Surv(time = merged$time, event = merged$event_status)
fit5 <- survfit(surv_obj ~ cambio_group, data = merged)

ggsurvplot(
  fit5,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC gado-", "TEC gado+", "MAV gado-", "MAV gado+"),
  xlab = "Months",
  ylab = "Event-free survival"
)

