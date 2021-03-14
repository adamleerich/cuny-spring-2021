# Explore the data
library(tidyverse)
remove(list = ls(all = TRUE))

csv_path <- './data/cleaned-cdc-mortality-1999-2010-2.csv'
csv <- read.csv(csv_path)
head(csv)
str(csv)
table(csv$ICD.Chapter)
table(csv$State)


csv_renamed <- csv %>% 
  as_tibble %>% 
  rename(
    state_abbr = State,
    year = Year,
    cause_long = ICD.Chapter,
    count = Deaths,
    population = Population,
    rate_100e3 = Crude.Rate)


#' Create some reference data:
#'   1. list of states
#'   2. list of years
#'   3. table of populations
#'   4. causes of death

states_abbr <- csv_renamed$state_abbr %>% unique %>% sort
years <- csv_renamed$year %>% unique %>% sort
causes_long <- csv_renamed$cause_long %>% unique %>% sort

pop <- csv_renamed %>% 
  group_by(state_abbr, year, population) %>% 
  summarize()


print(causes_long)



# State + Year gives a unique population
pop %>% 
  group_by(state_abbr, year) %>% 
  summarize(count = n()) %>% 
  filter(count > 1)




# We are not missing any populations for any state/year combination
sum(table(pop[, 1:2]) != 1)
pop_na <- is.na(pop)
str(pop_na)
is.matrix(pop_na)




# Blow up CSV so it has a row for every possible combination
# of state/year/cause

all_keys <- tibble(
  state_abbr = rep(states_abbr, each = length(years) * length(causes_long)),
  year = rep(years, each = length(causes_long), times = length(states_abbr)),
  cause_long = rep(causes_long, times = length(years) * length(states_abbr))
)

any(duplicated(all_keys))

full <- csv_renamed %>% 
  select(state_abbr, year, cause_long, count) %>% 
  right_join(all_keys)



# Check that it worked

dim(full)
dim(all_keys)
dim(csv)
dim(csv_renamed)

sum(is.na(full$count)) + nrow(csv)
length(states_abbr) * length(years) * length(causes_long)

full$count[is.na(full$count)] <- 0
sum(csv$Deaths)
sum(full$count)

sum(is.na(full))

nrow(pop)
length(states_abbr) * length(years)
nrow(pop) * length(causes_long)




# Is crude rate count/pop?
# Answer: it is count per 100,000 people

L <- 1:5
(csv$Deaths[L] / csv$Population[L])
csv$Crude.Rate[L]


table(round(csv$Deaths / csv$Population * 10^5 - csv$Crude.Rate, 1))

csv %>% 
  as_tibble %>% 
  select(Deaths, Population, Crude.Rate) %>% 
  mutate(
    raw = Deaths / Population) %>% 
  mutate(
    rounded = round(raw, 6)) %>% 
  mutate(
    scaled = rounded * 100e3) %>% 
  mutate(
    diff = scaled - Crude.Rate)


# All crude rates are rounded to tenths place
table(round(csv$Crude.Rate * 10, 0) / 10 - csv$Crude.Rate)





# National population by year

natpop <- pop %>% 
  group_by(year) %>% 
  summarize(population = sum(population))



# Add state names
smap <- read.csv('./data/states.csv')
smap

any(!states_abbr %in% smap$state2)

smap2 <- smap %>% 
  rename(
    state_abbr = state2,
    state_full = state)



full_wide <- full %>% 
  inner_join(pop) %>% 
  inner_join(smap2)

dim(full_wide)
dim(full)
sum(full_wide$count)
sum(full$count)


full_nat <- full_wide %>% 
  group_by(year, cause_long) %>% 
  summarize(
    count = sum(count),
    population = sum(population))



smap2
nrow(full_wide) / 51
nrow(full_nat)



# Create abbreviated causes of death
clipr::write_clip(causes_long)





# Save objects

causes <- c(
  "Perinatal Period",                      "Certain conditions originating in the perinatal period",
  "Infectious and Parasitic Diseases",     "Certain infectious and parasitic diseases",
  "Special Causes",                        "Codes for special purposes",
  "Congenital",                            "Congenital malformations, deformations and chromosomal abnormalities",
  "Blood and Immune",                      "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
  "Circulatory",                           "Diseases of the circulatory system",
  "Digestive",                             "Diseases of the digestive system",
  "Ear Related",                           "Diseases of the ear and mastoid process",
  "Reproductive",                          "Diseases of the genitourinary system",
  "Musculoskeletal",                       "Diseases of the musculoskeletal system and connective tissue",
  "Nervous",                               "Diseases of the nervous system",
  "Respiratory",                           "Diseases of the respiratory system",
  "Skin Related",                          "Diseases of the skin and subcutaneous tissue",
  "Nutritional and Metabolic",             "Endocrine, nutritional and metabolic diseases",
  "External Causes",                       "External causes of morbidity and mortality",
  "Mental and Behavioral",                 "Mental and behavioural disorders",
  "Cancers",                               "Neoplasms",
  "Pregnancy and Childbirth",              "Pregnancy, childbirth and the puerperium",
  "Other",                                 "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified"
) %>% matrix(nrow = 2) %>% t %>% as.data.frame %>% 
  rename(
    cause_abbr = V1, 
    cause_long = V2)



head(smap2)
head(pop)
head(years)
head(causes)
head(natpop)
head(full_wide)
head(full_nat)

readr::write_rds(smap2,  './data/states.Rds')
readr::write_rds(pop,     './data/population_states.Rds')
readr::write_rds(years,   './data/years.Rds')
readr::write_rds(causes,  './data/causes.Rds')
readr::write_rds(natpop,  './data/population_nationwide.Rds')
readr::write_rds(full_wide,    './data/counts_state.Rds')
readr::write_rds(full_nat,    './data/counts_nationwide.Rds')



