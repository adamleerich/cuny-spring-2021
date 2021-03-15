library(shiny)
library(tidyverse)
library(ggplot2)
# rm(list = ls())


causes <- read_rds('./data/causes.Rds')
years <- read_rds('./data/years.Rds')
states <- read_rds('./data/states.Rds')
counts_state <- read_rds('./data/counts_state.Rds')
counts_us <- read_rds('./data/counts_nationwide.Rds')




# Question #1 Plot

y <- 2010
k <- 'Respiratory'

cs <- counts_state %>% 
  filter(year == y, cause_abbr == k) %>% 
  select(state_abbr, rate)

L <- order(cs$rate, decreasing = TRUE)

cs$state <- factor(cs$state_abbr, levels = cs$state_abbr[L])

ggplot(data = cs) +
  aes(x = state, y = rate) +
  geom_col()




# Question #2 Plot

s <- 'California'
k <- 'Circulatory'

cs2 <- counts_state %>% 
  filter(state_long == s, cause_abbr == k) %>% 
  select(year, rate) %>% 
  mutate(Area = s)

cn2 <- counts_us %>% 
  ungroup() %>% 
  filter(cause_abbr == k) %>% 
  select(year, rate) %>% 
  mutate(Area = 'Nationwide')




ggplot(data = rbind(cs2, cn2)) + 
  aes(x = year, y = rate, color = Area, linetype = Area) + 
  geom_line(size = 1.5)
