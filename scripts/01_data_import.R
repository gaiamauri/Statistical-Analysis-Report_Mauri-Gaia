# 01-data-import.R----
# Download raw datasets from Eurostat and save to data/raw/
# install.packages("eurostat")
# install.packages("haven")
library(eurostat)
library(tidyverse)
library(haven)

# Dataset Dependent Variable: Eurobarometer----

#Special Eurobarometer 278: European cultural values

df_2007 <- read_sav("data/raw/ZA4529_v3-0-1.sav")

#Special Eurobarometer 399: Cultural access and participation

df_2013 <- read_sav("data/raw/ZA5688_v6-0-0.sav")

# Independent Variables Datasets ----

# Independent variable 1: GDP per capita----
df_gdp <- get_eurostat("nama_10_pc", time_format = "num")

#Inspect the dataset
names(df_gdp)
df_gdp %>%  distinct(na_item)
df_gdp %>%  distinct(unit)

#Clean and filter what is useful
df_gdp_clean <- df_gdp %>% 
  filter( 
    na_item     == "B1GQ", #the code for GDP
    unit        == "CP_PPS_EU27_2020_HAB", #GDP per capita in Purchasing Power Parity (PPS), referred to EU 27 2020
    TIME_PERIOD %in% c(2007, 2013)
  ) %>% 
  select(geo, TIME_PERIOD, values) %>% 
  rename(
    country = geo,
    year    = TIME_PERIOD,
    gdp_pc  = values
  )

#Check the result
nrow(df_gdp_clean)

# Independent variable 2: Government expenditure on culture----
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
    TIME_PERIOD %in% c(2007, 2013)
  ) %>% 
  select(geo, TIME_PERIOD, values) |>
  rename(
    country      = geo,
    year         = TIME_PERIOD,
    culture_exp  = values
  )

# Save raw data to data/raw/
write_csv(df_gdp_clean,      "data/raw/gdp_raw.csv")
write_csv(df_culture_clean,  "data/raw/culture_raw.csv")
df_gdp_clean %>% filter(country == "BE")

message("Raw data downloaded and saved to data/raw/")

read_csv("data/raw/gdp_raw.csv") %>% filter(country == "BE")
