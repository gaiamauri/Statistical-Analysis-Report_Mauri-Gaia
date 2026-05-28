# 02-data-cleaning.R----

# Clean, filter and merge all datasets into main_dataset.csv ----

library(tidyverse)
library(haven)

# Eurobarometer microdata (downloaded manually from GESIS)
df_2007 <- read_sav("data/raw/ZA4529_v3-0-1.sav")
df_2013 <- read_sav("data/raw/ZA5688_v6-0-0.sav")

# Country-level variables (downloaded from Eurostat in 01-data-import.R)
df_gdp_clean     <- read_csv("data/raw/gdp_raw.csv")
df_culture_clean <- read_csv("data/raw/culture_raw.csv")

#DEPENDENT VARIABLE: Index of cultural participation - number of cultural activities attended at least once in the last 12 months----

# Inspect Eurobarometer datasets ----
nrow(df_2007)
nrow(df_2013)
names(df_2007)
names(df_2013)

# Comparison df_2013 and df_2007 to check if are comparable
# Check the variables: one cultural activity (cinema), internet use, age, country
# The correspondences between categories and what they represent are possible thanks to the codebooks, available at the datasets' Gesis webpage

# Key variables of df_2013:
df_2013 %>% count(qb1_1) #cinema
df_2013 %>% count(qc2) # internet use frequency
df_2013 %>% count(isocntry) #country
df_2013 %>% count(d11) #age
df_2013 %>% count(d8) #education: it is a measure of the age at which the respondent finished school not the level of education directly 
df_2013 %>% count(d15a) #employment
df_2013 %>% count(d10) #gender

# Key Variables of df_2007
df_2007 %>% count(v95)    # cinema
df_2007 %>% count(v115)   # internet use frequency
df_2007 %>% count(v727) %>% print(n = 20) # country
df_2007 %>% count (v7) #age
df_2007 %>% count (v724) #education

# Clean df_2007 ----
# Rename categories using the GESIS codebook
df_2007_clean <- df_2007 %>%
  rename(
    country      = v7,     # country ISO code
    age          = v727,   # D11 age
    sex          = v726,   # D10 gender (1=male, 2=female)
    education    = v724,   # D8 age when stopped full-time education
    occupation   = v730,   # D15A occupation
    internet_use = v115,   # QA6 internet use frequency (1=every day, 6=never)
    cult_ballet  = v94,    # QA4_1 ballet/dance/opera
    cult_cinema  = v95,    # QA4_2 cinema
    cult_theatre = v96,    # QA4_3 theatre
    cult_concert = v98,    # QA4_5 concert
    cult_library = v99,    # QA4_6 public library
    cult_monuments = v100, # QA4_7 historical monuments
    cult_museum  = v101    # QA4_8 museums/galleries
) %>%
  # Filter youth 15-29
  filter(age >= 15 & age <= 29) %>%
  # Add year identifier
  mutate(year = 2007) %>%
  # Keep only relevant columns
  select(country, year, age, sex, education, occupation,
         internet_use, cult_ballet, cult_cinema, cult_theatre,
         cult_concert, cult_library, cult_monuments, cult_museum)

nrow(df_2007_clean)

# Clean df_2013 ----
# Rename categories using the GESIS codebook
df_2013_clean <- df_2013 %>%
# Remove numeric country column to avoid conflict with "isocntry" rename
  select(-country) %>%
  rename(
    country      = isocntry, # country ISO code
    age          = d11,      # D11 age exact
    sex          = d10,      # D10 gender (1=male, 2=female)
    education    = d8,       # D8 age when stopped full-time education
    occupation   = d15a,     # D15A occupation
    internet_use = qc2,      # QC2 frequency of internet use for cultural purposes
    cult_ballet  = qb1_1,    # QB1_1 ballet/opera
    cult_cinema  = qb1_2,    # QB1_2 cinema
    cult_theatre = qb1_3,    # QB1_3 theatre
    cult_concert = qb1_4,    # QB1_4 concert
    cult_library = qb1_5,    # QB1_5 public library
    cult_monuments = qb1_6,  # QB1_6 historical monuments
    cult_museum  = qb1_7     # QB1_7 museums/galleries
  ) %>%
  # Filter youth 15-29
  filter(age >= 15 & age <= 29) %>%
  # Add year identifier
  mutate(year = 2013) %>%
  # Keep only relevant columns
  select(country, year, age, sex, education, occupation,
         internet_use, cult_ballet, cult_cinema, cult_theatre,
         cult_concert, cult_library, cult_monuments, cult_museum)

nrow(df_2013_clean)

#Pool the two waves ----
# Remove SPSS value labels before binding (converts to plain numeric)
# Exclude country column (character) from numeric conversion
df_2007_clean <- df_2007_clean %>%
  mutate(across(-country, as.numeric))

df_2013_clean <- df_2013_clean %>%
  mutate(across(-country, as.numeric))

# Merge df_2007_clean and df_2013_clean, vertically (same columns, different respondents)
df_pooled <- bind_rows(df_2007_clean, df_2013_clean)

nrow(df_pooled)

# Recode variables----

df_pooled <- df_pooled %>%
  mutate(
    # Dependent variable: participation index (0-7)
    # Recode each activity: 1 = participated (any frequency), 0 = never
    # Original scale: 1=never, 2=1-2 times, 3=3-5 times, 4=more than 5 times
    cult_ballet_bin    = if_else(cult_ballet > 1,    1L, 0L, missing = NA_integer_),
    cult_cinema_bin    = if_else(cult_cinema > 1,    1L, 0L, missing = NA_integer_),
    cult_theatre_bin   = if_else(cult_theatre > 1,   1L, 0L, missing = NA_integer_),
    cult_concert_bin   = if_else(cult_concert > 1,   1L, 0L, missing = NA_integer_),
    cult_library_bin   = if_else(cult_library > 1,   1L, 0L, missing = NA_integer_),
    cult_monuments_bin = if_else(cult_monuments > 1, 1L, 0L, missing = NA_integer_),
    cult_museum_bin    = if_else(cult_museum > 1,    1L, 0L, missing = NA_integer_),
    
    # Participation index: sum of 7 binary activities (range 0-7)
    part_index = cult_ballet_bin + cult_cinema_bin + cult_theatre_bin +
      cult_concert_bin + cult_library_bin +
      cult_monuments_bin + cult_museum_bin,
    
    # Binary DV: participated in at least one activity
    part_any = if_else(part_index > 0, 1L, 0L, missing = NA_integer_),
    
    # Internet use: reverse scale so higher = more use
    # Original: 1=every day, 6=never → reverse: 1=never, 6=every day
    internet_use_rev = 7 - internet_use,
    
    # Education: recode age of end of studies into 3 categories
    # Low:    stopped at 15 or younger
    # Medium: stopped 16-19
    # High:   stopped at 20 or older (still studying = 0, treat as high)
    education_cat = case_when(
      education == 0              ~ "high",   # still studying
      education <= 15             ~ "low",
      education >= 16 & education <= 19 ~ "medium",
      education >= 20             ~ "high",
      TRUE                        ~ NA_character_
    ),
    
    # Sex: recode to binary (0=male, 1=female)
    female = if_else(sex == 2, 1L, 0L),
    
    # Year dummy
    year_2013 = if_else(year == 2013, 1L, 0L)
  )

# Standardise country codes---- 
# Germany is split into DE-E and DE-W in both waves
# Merge into single DE code
df_pooled <- df_pooled %>%
  mutate(country = if_else(country %in% c("DE-E", "DE-W"), "DE", country))

# UK is split into GB-GBN (Great Britain) e GB-NIR (Nothern Irland) in both waves
# Merge into single GB code
df_pooled <- df_pooled %>%
mutate(country = if_else(country %in% c("GB-GBN", "GB-NIR"), "GB", country))

# Standardise country codes to match Eurostat
df_pooled <- df_pooled %>%
  mutate(country = case_when(
    country == "GR" ~ "EL",  # Greece: Eurobarometer uses GR, Eurostat uses EL
    country == "GB" ~ "UK",  # UK: Eurobarometer uses GB, Eurostat uses UK
    TRUE ~ country            # all other countries unchanged
  ))
# Check countries: should be 28
df_pooled %>% distinct(country) %>% print(n = 30)

#Merge country-level variables----

df_final <- df_pooled %>%
  left_join(df_gdp_clean,     by = c("country", "year")) %>%
  left_join(df_culture_clean, by = c("country", "year"))

#Final check

glimpse(df_final)
nrow(df_final)

# NA count per variable
df_final %>%
  summarise(across(everything(), ~ sum(is.na(.))))

df_final %>%
  summarise(
    na_gdp     = sum(is.na(gdp_pc)),
    na_culture = sum(is.na(culture_exp))
  )
# UK is excluded from the analysis because culture_exp data are not available
# for either wave (2007 and 2013) in Eurostat gov_10a_exp dataset
# Slovakia (SK) is missing culture_exp only for 2007: 187 na_culture — kept in the analysis
df_final <- df_final %>%
  filter(country != "UK")

# Verify
nrow(df_final)
df_final %>% distinct(country) %>% print(n = 30)

df_final %>%
  summarise(
    na_gdp     = sum(is.na(gdp_pc)),
    na_culture = sum(is.na(culture_exp))
  )

# Descriptive statistics of key variables
df_final %>%
  group_by(year) %>%
  summarise(
    n             = n(),
    mean_part     = mean(part_index,    na.rm = TRUE),
    mean_internet = mean(internet_use_rev, na.rm = TRUE),
    mean_gdp      = mean(gdp_pc,        na.rm = TRUE)
  )
# Save processed data: final dataset

write_csv(df_final, "data/processed/main_dataset.csv")
message("Cleaned dataset saved to data/processed/main_dataset.csv")

