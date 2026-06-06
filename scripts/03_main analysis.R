library(tidyverse)
library(lme4) 
library(modelsummary) 
library(sjPlot)

# Multilevel models: cultural participation ~ internet use ----

#Load data
df_final <- read_csv("data/processed/main_dataset.csv",
                    show_col_types = FALSE)

# Prepare variables ----
#Create year_country variable because gdp_pc and culture_exp change in the country between 2007 and 2013

df_final <- df_final %>%
  filter(!is.na(country), !is.na(year), !is.na(part_index)) %>%
  mutate(
    country_year = factor(str_c(country, "_", year)),
    country = factor(country),
 # education_cat as factor with "low" as reference category     
    education_cat = factor(education_cat,
                           levels = c("low", "medium", "high")),
    country = factor(country),
 year_2013 = as.numeric(year == 2013)
 
)

# Macro statistics: standardise the country variables using mean and sd at the country level
macro_stats <- df_final %>%
  group_by(country_year) %>%
  summarise(
    gdp_raw = mean(gdp_pc, na.rm = TRUE),
    culture_raw = mean(culture_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(gdp_raw), !is.na(culture_raw)) %>%
  summarise(
    mean_gdp  = mean(gdp_raw, na.rm = TRUE), 
    sd_gdp    = sd(gdp_raw, na.rm = TRUE),
    mean_cult = mean(culture_raw, na.rm = TRUE), 
    sd_cult   = sd(culture_raw, na.rm = TRUE)
  )

    # Standardise continuous predictors (x - mean(x)) / sd(x) → mean = 0, sd = 1
df_final <- df_final %>%
  mutate(   
  #For individual predictor we calculate the on all the dataset,
  #for country-level predictors we use the means at macro level
 internet_use_z = (internet_use_rev - mean(internet_use_rev, na.rm = TRUE)) /
      sd(internet_use_rev, na.rm = TRUE),
 age_z= (age - mean(age, na.rm = TRUE)) / sd(age, na.rm = TRUE),
   gdp_pc_z= (gdp_pc - macro_stats$mean_gdp) / macro_stats$sd_gdp,
 culture_exp_z= (culture_exp - macro_stats$mean_cult) / macro_stats$sd_cult
  )


df_final %>% 
  select(part_index, internet_use_z, year_2013, education_cat, female, age_z, gdp_pc_z, culture_exp_z, country_year) %>% 
  summarise(across(everything(), ~sum(is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "Variabile_nel_Modello", values_to = "Numero_di_NA_Totale")

# check
summary(df_final$internet_use_z)  # mean ≈ 0, sd ≈ 1
summary(df_final$gdp_pc_z)


# NULL MODEL (intercept only, no predictors): To compute ICC before adding predictors

model0 <- lmer(
  part_index ~ 1 + (1 | country),
  data = df_final,
)

sjPlot:: tab_model(model0)

# ICC tells us how much variance is at country level
# If ICC > 0.1 → multilevel model is justified

# MODEL 1: Multilevel base model ----
# Tests H1 (internet use), H3 (GDP), H4 (culture expenditure)

model1 <- lmer(
  part_index ~ internet_use_z + year_2013 + education_cat + female + age_z +
    gdp_pc_z + culture_exp_z +
    (1 | country_year),
  data = df_final,
)
sjPlot:: tab_model(model1)

# MODEL 2: Model with interaction internet_use × year ----
# Tests H2 (change over time)

model2 <- lmer(
  part_index ~ internet_use_z * year_2013 + education_cat +
    female + age_z + gdp_pc_z + culture_exp_z +
    (1 | country_year),
  data = df_final,
  REML = TRUE
)

sjPlot:: tab_model(model2)

# Save Models
saveRDS(model0, "tables/model0.rds")
saveRDS(model1, "tables/model1.rds")
saveRDS(model2, "tables/model2.rds")

# Final Table with the 3 models
tab_model(model0, model1, model2,
          dv.labels = c("Null model", "Model 1", "Model 2"),
          show.icc   = TRUE,
          show.re.var = TRUE,
          p.style    = "stars",
          title = "Multilevel Models of Youth Cultural Participation (2007-2013)",
          file       = "tables/regression_table.html")


