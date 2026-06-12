library(tidyverse)
library(lme4) 
library(modelsummary) 
library(sjPlot)

#Load data
df_final <- read_csv("data/processed/main_dataset.csv",
                     show_col_types = FALSE)

#Inspection and Distribution of the Dependent Variable: participation index ----
summary(df_final$part_index)
table(df_final$part_index)

# Histogram
ggplot(df_final, aes(x = part_index)) +
  geom_histogram(bins = 8, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Cultural Participation Index",
       x = "Number of activities (0-7)", y = "Count")

# Multilevel models: cultural participation ~ internet use ----


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

# check
summary(df_final$internet_use_z)  # mean ≈ 0, sd ≈ 1
summary(df_final$gdp_pc_z)


# NULL MODEL (intercept only, no predictors): To compute ICC before adding predictors

model0 <- lmer(
  part_index ~ 1 + (1 | country_year),
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

# MODEL 3: random slopes for internet use ----
model3 <- lmer(
  part_index ~ internet_use_z * year_2013 + education_cat +
    female + age_z + gdp_pc_z + culture_exp_z +
    (1 + internet_use_z | country_year),
  data = df_final,
  REML = TRUE
)

sjPlot:: tab_model(model3)


# Save Models
saveRDS(model0, "tables/model0.rds")
saveRDS(model1, "tables/model1.rds")
saveRDS(model2, "tables/model2.rds")
saveRDS(model3, "tables/model3.rds")

# Final Table with the 3 models
tab_model(model0, model1, model2,model3,
          dv.labels = c("Null model", "Model 1", "Model 2", "Model 3 (Random Slopes)"),
          show.icc   = TRUE,
          show.re.var = TRUE,
          p.style    = "stars",
          title = "Multilevel Models of Youth Cultural Participation (2007-2013)",
          file       = "tables/regression_table.html")

# PREDICTION PLOT: Interaction internet_use × year_2013 ----
# Following the approach shown in class (session 12)

plot_model(
  model2,
  type  = "int",
  terms = c("internet_use_z", "year_2013")
) +
  theme_minimal() +
  labs(
    title    = "Predicted Cultural Participation by Internet Use and Wave",
    subtitle = "Marginal predictions at mean values of other covariates",
    x        = "Internet Use (standardised)",
    y        = "Predicted Participation Index (0–7)",
    color    = "Wave",
    fill     = "Wave"
  ) +
  scale_color_manual(
    values = c("0" = "#1A3557", "1" = "#2E6DAD"),
    labels = c("0" = "2007",    "1" = "2013")
  )

# Save
ggsave("figures/fig1_interaction_plot.png",
       width = 7, height = 5, dpi = 300)

# MARGINAL EFFECTS ----

library(marginaleffects)

# Average Marginal Effect (AME) of internet use
# Single summary number: on average, how much does 
# a 1 SD increase in internet use change participation?
marginaleffects::avg_slopes(model2, variables = "internet_use_z")

# Marginal Effects at specific values of the moderator (year)
# How does the effect of internet use differ in 2007 vs 2013?
marginaleffects::avg_slopes(
  model2,
  variables = "internet_use_z",
  by        = "year_2013",
  newdata   = datagrid(year_2013 = c(0, 1))
)

# Save AME results
ame_results <- marginaleffects::avg_slopes(
  model2,
  variables = "internet_use_z",
  by        = "year_2013",
  newdata   = datagrid(year_2013 = c(0, 1))
)

# Plot AMEs
plot_slopes(
  model2,
  variables = "internet_use_z",
  condition = "year_2013"
) +
  theme_minimal() +
  scale_x_discrete(
    breaks = c(0, 1),
    labels = c("2007", "2013")
  ) +
  labs(
    title    = "Marginal Effect of Internet Use on Cultural Participation",
    subtitle = "Average Marginal Effects by wave (2007 vs 2013)",
    x        = "Wave",
    y        = "AME of Internet Use"
  )

ggsave("figures/fig2_ame_plot.png",
       width = 6, height = 4, dpi = 300)

# RANDOM SLOPES PLOT ----
# Shows variation in the effect of internet use across country-years
sjPlot::plot_model(model3,
                   type     = "re",
                   sort.est = "internet_use_z") +
  theme_minimal() +
  labs(
    title    = "Random Slopes for Internet Use by Country-Year",
    subtitle = "Variation in the effect of internet use across European contexts"
  )

ggsave("figures/fig4_random_slopes.png",
       width = 7, height = 8, dpi = 300)

# Disaggregated participation index by activity and year -----
df_final %>%
  group_by(year) %>%
  summarise(
    Cinema     = mean(cult_cinema_bin,    na.rm = TRUE),
    Theatre    = mean(cult_theatre_bin,   na.rm = TRUE),
    Concert    = mean(cult_concert_bin,   na.rm = TRUE),
    Museum     = mean(cult_museum_bin,    na.rm = TRUE),
    Monuments  = mean(cult_monuments_bin, na.rm = TRUE),
    Library    = mean(cult_library_bin,   na.rm = TRUE),
    Ballet     = mean(cult_ballet_bin,    na.rm = TRUE)
  ) %>%
  pivot_longer(-year, names_to = "activity", values_to = "participation_rate") %>%
  ggplot(aes(x = reorder(activity, participation_rate),
             y = participation_rate,
             fill = factor(year))) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("2007" = "red", "2013" = "#2E6DAD"),
                    labels = c("2007", "2013")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    title    = "Participation Rate by Cultural Activity and Wave",
    subtitle = "Share of young Europeans (15-29) participating at least once in last 12 months",
    x        = NULL,
    y        = "Share participating (%)",
    fill     = "Wave"
  )

ggsave("figures/fig5_activities_disaggregated.png",
       width = 7, height = 5, dpi = 300)
