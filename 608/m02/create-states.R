library(tidyverse)

# state.abb
# state.area
# state.center
# state.division
# state.name
# state.region
# state.x77


s <- as.data.frame(state.x77)
s$Abb <- state.abb
s$Center_X <- state.center$x
s$Center_Y <- state.center$y
s$Division <- state.division
s$Name <- state.name
s$Region <- state.region

all(s$name == row.names(s))
# round(s$area - s$Area, 2)
row.names(s) <- NULL


s %>% rename(
  HS_Grad = `HS Grad`,
  Life_Exp = `Life Exp`) %>% 
  select(
    Name,
    Population,
    Income,
    Illiteracy,
    Life_Exp,
    HS_Grad,
    Abb,
    Area,
    Center_X,
    Center_Y,
    Division,
    Region) %>% 
  write_csv(file = 'states.csv')

