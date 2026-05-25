#Data cleaning and preparation -----
install.packages("eurostat")
library(eurostat)
library(tidyverse)

# Dataframe Dependent Variable ----
df_scp01 <- get_eurostat("ilc_scp01", time_format = "num")
str(df_scp01)
head(df_scp01)
names(df_scp01)

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

#Dependent Variable Inspection
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
    
# Independent Variables Dataframes ----
# Internet Use
df_internet <- get_eurostat("isoc_ci_ifp_iu", time_format = "num")

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

# GDP per capita
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

# Government expenditure on culture
df_culture <- get_eurostat("gov_10a_exp", time_format = "num")

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