library(tidyverse)
library(lme4) 
library(modelsummary) 

# Multilevel models: cultural participation ~ internet use ----

#Load data
df_final <- read_csv("data/processed/main_dataset.csv",
                    show_col_types = FALSE)

# Prepare variables ----
# education_cat as factor with "low" as reference category
df_final <- df_final %>%
  mutate(
    education_cat = factor(education_cat,
                           levels = c("low", "medium", "high")),
    country = factor(country),
    
    # Standardise continuous predictors (x - mean(x)) / sd(x) → mean = 0, sd = 1
    internet_use_z = (internet_use_rev - mean(internet_use_rev, na.rm = TRUE)) /
      sd(internet_use_rev, na.rm = TRUE),
    gdp_pc_z       = (gdp_pc - mean(gdp_pc, na.rm = TRUE)) /
      sd(gdp_pc, na.rm = TRUE),
    culture_exp_z  = (culture_exp - mean(culture_exp, na.rm = TRUE)) /
      sd(culture_exp, na.rm = TRUE),
    age_z          = (age - mean(age, na.rm = TRUE)) /
      sd(age, na.rm = TRUE)
  )

# check
summary(df_final$internet_use_z)  # mean ≈ 0, sd ≈ 1
summary(df_final$gdp_pc_z)

# NULL MODEL (intercept only): To compute ICC before adding predictors

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
  part_index ~ internet_use_z + education_cat + female + age_z +
    gdp_pc_z + culture_exp_z +
    (1 | country),
  data = df_final,
)
sjPlot:: tab_model(model1)

# MODEL 2: Model with interaction internet_use × year ----
# Tests H2 (change over time)

model2 <- lmer(
  part_index ~ internet_use_z * year_2013 + education_cat +
    female + age_z + gdp_pc_z + culture_exp_z +
    (1 | country),
  data = df_final,
  REML = TRUE
)

sjPlot:: tab_model(model2)
