####TABELLA PT E CTR
source("library.R")
merged <- read.csv2("input/merged.csv")



merged <- merged %>%
mutate(
  gadolinium_contrast = recode(as.character(gadolinium_contrast),
                               "0" = "No",
                               "1" = "Yes")
)


metadati_sani <- read.csv2("input/metadati_sani.csv")

# =====================================================
# BMI categories (patients and controls)
# =====================================================

bmi_fun <- function(x) {
  case_when(
    x < 18.5 ~ "Underweight (<18.5)",
    x >= 18.5 & x < 25 ~ "Normal weight (18.5-24.9)",
    x >= 25 ~ "Overweight (≥25)",
    TRUE ~ NA_character_
  )
}

merged <- merged %>%
  mutate(bmi_cat = bmi_fun(bmi))

metadati_sani <- metadati_sani %>%
  mutate(bmi_cat = bmi_fun(bmi))
# =====================================================
# MEDAS categories (patients and controls)
# =====================================================

medas_fun <- function(x) {
  case_when(
    x < 42.9 ~ "Low MEDAS (<42.9)",
    x >= 42.9 & x <= 57 ~ "Intermediate MEDAS (42.9-57)",
    x > 57 ~ "High MEDAS (>57)",
    TRUE ~ NA_character_
  )
}

merged <- merged %>%
  mutate(MEDAS_cat = medas_fun(MEDAS))

metadati_sani <- metadati_sani %>%
  mutate(MEDAS_cat = medas_fun(MEDAS))

# =====================================================
# P-values Patients vs Controls
# =====================================================

p_age <- wilcox.test(
  age_at_T0 ~ cohort,
  data = bind_rows(
    merged %>%
      select(age_at_T0) %>%
      mutate(cohort = "pwMS"),
    metadati_sani %>%
      select(age_at_T0) %>%
      mutate(cohort = "HC")
  )
)$p.value

p_sex <- fisher.test(
  table(
    c(as.character(merged$sex),
      as.character(metadati_sani$sex)),
    c(rep("pwMS", nrow(merged)),
      rep("HC", nrow(metadati_sani)))
  )
)$p.value

p_bmi <- fisher.test(
  table(
    c(as.character(merged$bmi_cat),
      as.character(metadati_sani$bmi_cat)),
    c(rep("pwMS", nrow(merged)),
      rep("HC", nrow(metadati_sani)))
  )
)$p.value

p_medas <- fisher.test(
  table(
    c(as.character(merged$MEDAS_cat),
      as.character(metadati_sani$MEDAS_cat)),
    c(rep("pwMS", nrow(merged)),
      rep("HC", nrow(metadati_sani)))
  )
)$p.value
# =====================================================
# Statistics for controls
# =====================================================

n_ctrl <- nrow(metadati_sani)

ctrl_age <- sprintf(
  "%.1f ± %.1f",
  mean(metadati_sani$age_at_T0, na.rm = TRUE),
  sd(metadati_sani$age_at_T0, na.rm = TRUE)
)

ctrl_F_txt <- sprintf(
  "%d (%.1f%%)",
  sum(metadati_sani$sex == "F", na.rm = TRUE),
  100 * mean(metadati_sani$sex == "F", na.rm = TRUE)
)

ctrl_M_txt <- sprintf(
  "%d (%.1f%%)",
  sum(metadati_sani$sex == "M", na.rm = TRUE),
  100 * mean(metadati_sani$sex == "M", na.rm = TRUE)
)

ctrl_bmi <- metadati_sani %>%
  count(bmi_cat) %>%
  mutate(
    txt = sprintf("%d (%.1f%%)", n, 100 * n / sum(n))
  )

bmi_lookup <- setNames(
  ctrl_bmi$txt,
  ctrl_bmi$bmi_cat
)

ctrl_medas <- metadati_sani %>%
  count(MEDAS_cat) %>%
  mutate(
    txt = sprintf("%d (%.1f%%)", n, 100 * n / sum(n))
  )

medas_lookup <- setNames(
  ctrl_medas$txt,
  ctrl_medas$MEDAS_cat
)

# =====================================================
# Main table
# =====================================================

tab2 <- merged %>%
  select(
    GROUP,
    age_at_T0,
    sex,
    bmi_cat,
    MEDAS_cat,
    naive,
    gc_treatment,
    lesion_burden,
    subtentorial_lesions,
    gadolinium_contrast,
    smoking_habit,
    physical_activity,
  
  ) %>%
  tbl_summary(
    by = GROUP,
    missing = "no",
    statistic = list(
      age_at_T0 ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      age_at_T0 ~ c(1, 1),
      all_categorical() ~ c(0, 1)
    ),
    type = list(
      gc_treatment ~ "dichotomous"
    ),
    value = list(
      gc_treatment ~ "positive"
    ),
    label = list(
      age_at_T0 ~ "Age at baseline (years)",
      sex ~ "Sex",
      bmi_cat ~ "BMI category",
      MEDAS_cat ~ "MEDAS category",
      naive ~ "Treatment-naïve",
      gc_treatment ~ "Glucocorticoid at baseline",
      lesion_burden ~ "Lesion burden",
      subtentorial_lesions ~ "Subtentorial lesions",
      gadolinium_contrast ~ "Gadolinium-enhancing lesions",
      smoking_habit ~ "Smoking habit",
      physical_activity ~ "Physical activity"
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      age_at_T0 ~ "wilcox.test",
      all_categorical() ~ "fisher.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        HC = case_when(
          variable == "age_at_T0" &
            row_type == "label" ~ ctrl_age,
          
          variable == "sex" &
            label == "F" ~ ctrl_F_txt,
          
          variable == "sex" &
            label == "M" ~ ctrl_M_txt,
          
          variable == "bmi_cat" &
            row_type == "level" ~ bmi_lookup[label],
          
          variable == "MEDAS_cat" &
            row_type == "level" ~ medas_lookup[label],
          
          TRUE ~ ""
        ),
        
        `P pwMS vs HC` = case_when(
          variable == "age_at_T0" &
            row_type == "label" ~ style_pvalue(p_age, digits = 3),
          
          variable == "sex" &
            row_type == "label" ~ style_pvalue(p_sex, digits = 3),
          
          variable == "bmi_cat" &
            row_type == "label" ~ style_pvalue(p_bmi, digits = 3),
          
          variable == "MEDAS_cat" &
            row_type == "label" ~ style_pvalue(p_medas, digits = 3),
          
          TRUE ~ ""
        )
      )
  ) %>%
  modify_header(
    HC ~ paste0("**HC**<br>N = ", n_ctrl),
    `P pwMS vs HC` ~ "**p-value**"
  ) %>%
  modify_footnote(
    p.value ~ "Comparison between MAV and TEC",
    abbreviation = FALSE
  ) %>%
  modify_footnote(
    `P pwMS vs HC` ~ "Comparison between pwMS and healthy controls",
    abbreviation = FALSE
  ) %>%
  bold_labels()

tab2

file.exists("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
Sys.setenv(
  CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
)

tab2 %>%
  as_gt() %>%
  gt::gtsave("C:/Users/cecil/OneDrive/Desktop/tab_tent.png")


