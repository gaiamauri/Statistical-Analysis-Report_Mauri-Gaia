# 01-data-import.R----
# Download raw datasets from Eurostat and save to data/raw/
# install.packages("eurostat")
library(eurostat)
library(tidyverse)

# Dataset Dependent Variable: cultural and sport participation ----
df_scp01 <- get_eurostat("ilc_scp01", time_format = "num")

# Independent Variables Datasets ----
# Independent variable 1: Internet use----
df_internet <- get_eurostat("isoc_ci_ifp_iu", time_format = "num")
# Independent variable 2: GDP per capita----
df_gdp <- get_eurostat("nama_10_pc", time_format = "num")
# Independent variable 3: Government expenditure on culture----
df_culture <- get_eurostat("gov_10a_exp", time_format = "num")
# It is too heavy so i filter and reload it
#Inspect the dataset
names(df_culture)
df_culture %>%  distinct(cofog99) #Classification of Functions of Government: divide public spending by sector, we need GF08 = recreation, culture and religion 
df_culture %>%  distinct(unit) # measurement unit, we need PC_GDP= PIL percentage
df_culture %>%  distinct(na_item) # National Accounts item: type of  public spending, we need TE = Total Expenditure

#Clean and filter what is useful
df_culture_clean <- df_culture %>% 
  filter(
    cofog99     == "GF08",      # Recreation, culture and religion
    unit        == "PC_GDP",    #PIL percentage, to make cross-national comparisons
    na_item     == "TE",        # Total Expenditure
    sector      == "S13",       # General government: it inclued all levels of government (central, regional, local)
    TIME_PERIOD %in% c(2006, 2015, 2022)
  ) %>% 
  select(geo, TIME_PERIOD, values) |>
  rename(
    country      = geo,
    year         = TIME_PERIOD,
    culture_exp  = values
  )

# Save raw data to data/raw/
write_csv(df_scp01,    "data/raw/scp01_raw.csv")
write_csv(df_internet, "data/raw/internet_raw.csv")
write_csv(df_gdp,      "data/raw/gdp_raw.csv")
write_csv(df_culture_clean,  "data/raw/culture_raw.csv")

message("Raw data downloaded and saved to data/raw/")
