# Explore the data
library(tidyverse)
remove(list = ls(all = TRUE))

cm_path <- './data/compressed-mortaility-1999-2016.csv'
icd_path <- './data/icd-chapters.csv'
states_path <- './data/states.csv'

cm <- read_csv(cm_path) %>% 
  rename(
    state_long = State,
    year = Year,
    cause_long = `ICD Chapter`,
    cause_code = `ICD Chapter Code`,
    count = Deaths,
    population = Population,
    rate_100e3 = `Crude Rate`,
    unreliable = Unreliable) %>% 
  mutate(unreliable = coalesce(unreliable, FALSE))

icd <- read_csv(icd_path)
states <- read_csv(states_path)



head(cm)
str(cm)



#' Create some reference data:
#'   1. list of states
#'   2. list of years
#'   3. table of populations
#'   4. causes of death

years <- cm$year %>% unique %>% sort
cause_codes <- cm$cause_code %>% unique %>% sort
states_long <- cm$state_long %>% unique %>% sort


pop <- cm %>% 
  group_by(state_long, year, population) %>% 
  summarize()





# State + Year gives a unique population
pop %>% 
  group_by(state_long, year) %>% 
  summarize(count = n()) %>% 
  filter(count > 1)




# We are not missing any populations for any state/year combination
sum(table(pop[, 1:2]) != 1)
pop_na <- is.na(pop)
str(pop_na)
is.matrix(pop_na)




# Blow up cm so it has a row for every possible combination
# of state/year/cause

all_keys <- tibble(
  state_long = rep(states_long, each = length(years) * length(cause_codes)),
  year = rep(years, each = length(cause_codes), times = length(states_long)),
  cause_code = rep(cause_codes, times = length(years) * length(states_long))
)

any(duplicated(all_keys))

full <- cm %>% 
  select(state_long, year, cause_code, count) %>% 
  right_join(all_keys)


# Check that it worked

dim(full)
dim(all_keys)
dim(cm)

sum(is.na(full$count)) + nrow(cm)
length(states_long) * length(years) * length(cause_codes)

full$count[is.na(full$count)] <- 0
sum(cm$count)
sum(full$count)

sum(is.na(full))

nrow(pop)
length(states_long) * length(years)
nrow(pop) * length(cause_codes)




table(round(cm$count / cm$population * 10^5 - cm$rate_100e3, 1))

cm %>% 
  as_tibble %>% 
  select(count, population, rate_100e3) %>% 
  mutate(
    raw = count / population) %>% 
  mutate(
    rounded = round(raw, 6)) %>% 
  mutate(
    scaled = rounded * 100e3) %>% 
  mutate(
    diff = scaled - rate_100e3)


# All crude rates are rounded to tenths place
table(round(cm$rate_100e3 * 10, 0) / 10 - cm$rate_100e3)





# National population by year

natpop <- pop %>% 
  group_by(year) %>% 
  summarize(population = sum(population))



any(!states_long %in% states$state)

states2 <- states %>% 
  rename(
    state_abbr = state2,
    state_long = state)



full_wide <- full %>% 
  inner_join(pop) %>% 
  inner_join(states2) %>% 
  inner_join(icd)

dim(full_wide)
dim(full)
sum(full_wide$count)
sum(full$count)


full_nat <- full_wide %>% 
  group_by(
    year,
    cause_code,
    cause_long,
    cause_abbr) %>% 
  summarize(
    count = sum(count),
    population = sum(population))



states2
nrow(full_wide) / 51
nrow(full_nat)



# add rates back
full_wide$rate <- full_wide$count / full_wide$population * 100e3
full_nat$rate <- full_nat$count / full_nat$population * 100e3


# Save objects



head(states2)
head(pop)
head(years)
head(icd)
head(natpop)
head(full_wide)
head(full_nat)

readr::write_rds(states2,   './data/states.Rds')
readr::write_rds(pop,       './data/population_states.Rds')
readr::write_rds(years,     './data/years.Rds')
readr::write_rds(icd,       './data/causes.Rds')
readr::write_rds(natpop,    './data/population_nationwide.Rds')
readr::write_rds(full_wide, './data/counts_state.Rds')
readr::write_rds(full_nat,  './data/counts_nationwide.Rds')



