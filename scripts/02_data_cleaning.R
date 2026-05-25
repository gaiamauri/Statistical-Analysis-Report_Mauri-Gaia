# 02-data-cleaning.R----
# Clean, filter and merge all datasets into main_dataset.csv ----

library(tidyverse)

# Load raw data from data/raw/
df_scp01    <- read_csv("data/raw/scp01_raw.csv")
df_internet <- read_csv("data/raw/internet_raw.csv")
df_gdp      <- read_csv("data/raw/gdp_raw.csv")
df_culture  <- read_csv("data/raw/culture_raw.csv")

#DEPENDENT VARIABLE: youth cultural and sport participation----
#Inspect the dataset
df_scp01 %>% distinct(age)   # I need the name for age 16_29
df_scp01 %>% distinct(sex)   
df_scp01 %>% distinct(isced11) # Education level
df_scp01 %>% distinct(TIME_PERIOD)  # Years

df_youth <- df_scp01 %>% 
  as_tibble() %>% 
  select(
    age, sex,isced11, TIME_PERIOD, geo, values, frequenc)

df_youth <- df_youth %>%
  filter(
    age         == "Y16-29",
    sex         == "T",   
    isced11     == "TOTAL", #not filter for education level
    frequenc== "GE1", #filter just who participated at least once in the last 12 months
    TIME_PERIOD %in% c(2006, 2015, 2022)
  ) %>%
  select(geo, TIME_PERIOD, values) %>%
  rename(
    country    = geo,
    year       = TIME_PERIOD ,
    part_rate       = values)

#Inspect the cleaned dataset
str(df_youth)
nrow(df_youth)

# Number of countries
df_youth %>%  distinct(country)

# Number of waves for each country
df_youth %>% 
  group_by(country) %>% 
  summarise(n_waves = n()) %>% 
  print(n = 37)

# I decided to proceed excluding countries with less than 3 waves
# In the dataset there are also some non EU-countries: if they have 3 waves they remain in the dataset (just Norway)

df_youth <- df_youth %>% 
  group_by(country) %>% 
  filter(n() == 3) %>% 
  ungroup()

nrow(df_youth)
df_youth %>%  distinct(country) %>% print(n = 26)

#Dependent Variable Inspection----
# Descriptive statistics
summary(df_youth$part_rate)

# Annual Distribution
df_youth %>% 
  group_by(year) %>% 
  summarise(
    mean_part = mean(part_rate, na.rm = TRUE),
    sd_part   = sd(part_rate, na.rm = TRUE),
    min_part  = min(part_rate, na.rm = TRUE),
    max_part  = max(part_rate, na.rm = TRUE)
  )

# Independent Variables Datasets ----

# Independent Variable 1: Internet Use----

#Inspect the dataset
str(df_internet)
head(df_internet)
names(df_internet)

#Clean and filter what is useful
df_internet_clean <- df_internet %>% 
  filter(
    indic_is    == "I_ILT12", #Last internet use: in the last 12 months. % individuals who used internet in the last 12 months
    ind_type    == "IND_TOTAL", #Not filtered for age
    unit        == "PC_IND", #Percentage of individuals
    TIME_PERIOD %in% c(2006, 2015, 2022)
  ) %>% 
  select(geo, TIME_PERIOD, values) %>% 
  rename(
    country      = geo,
    year         = TIME_PERIOD,
    internet_use = values
  )

#Check the result
nrow(df_internet_clean)

df_internet_clean %>% 
  group_by(year) %>% 
  summarise(n_countries = n())

#Check if all the 26 countries of the df_youth are also in df_internet_clean

df_internet_clean %>% 
  filter(country %in% df_youth$country) %>% 
  group_by(year) %>% 
  summarise(
    n_countries = n(),
    n_missing   = sum(is.na(internet_use))
  )
#All present

# Independent Variable 2: GDP per capita----

#Inspect the dataset
names(df_gdp)
df_gdp %>%  distinct(na_item)
df_gdp %>%  distinct(unit)

#Clean and filter what is useful
df_gdp_clean <- df_gdp %>% 
  filter( 
    na_item     == "B1GQ", #the code for GDP
    unit        == "CP_PPS_EU27_2020_HAB", #GDP per capita in Purchasing Power Parity (PPS), referred to EU 27 2020
    TIME_PERIOD %in% c(2006, 2015, 2022)
  ) %>% 
  select(geo, TIME_PERIOD, values) %>% 
  rename(
    country = geo,
    year    = TIME_PERIOD,
    gdp_pc  = values
  )

#Check the result
nrow(df_gdp_clean)

df_gdp_clean %>% 
  group_by(year) %>% 
  summarise(n_countries = n())

#Check if all the 26 countries of the df_youth are also in df_gdp_clean

df_gdp_clean %>% 
  filter(country %in% df_youth$country) %>% 
  group_by(year) %>% 
  summarise(
    n_countries = n(),
    n_missing   = sum(is.na(gdp_pc))
  )
#All present

# Independent Variable 3: Government expenditure on culture----
# Already cleaned

# #Check if all the 26 countries of the df_youth are also in df_culture_clean
df_culture_clean %>% 
  filter(country %in% df_youth$country) %>% 
  group_by(year) %>% 
  summarise(
    n_countries = n(),
    n_missing   = sum(is.na(culture_exp))
  )
# We see that in 2026 one country is missing, find which one
countries_2006 <- df_culture_clean %>% 
  filter(year == 2006) %>% 
  pull(country)

df_youth %>% 
  distinct(country) %>% 
  filter(!country %in% countries_2006)

#Slovakia

# Merge the datasets ----
# Merge df_youth with df_internet_clean
df_merged <- df_youth %>% 
  left_join(df_internet_clean, by = c("country", "year"))

# Add df_gdp_clean
df_merged <- df_merged %>% 
  left_join(df_gdp_clean, by = c("country", "year"))

# Add df_culture_clean
df_merged <- df_merged %>% 
  left_join(df_culture_clean, by = c("country", "year"))

#Check
glimpse(df_merged)
nrow(df_merged)
#If there are NA's
df_merged %>% 
  summarise(across(everything(), ~ sum(is.na(.))))
# 1 NA in "part_rate": Germany 2022 and 1 NA in "culture_exp": Slovakia 2006

# SAVE processed dataset ----
write_csv(df_merged, "data/processed/main_dataset.csv")

message("Cleaned dataset saved to data/processed/main_dataset.csv")

