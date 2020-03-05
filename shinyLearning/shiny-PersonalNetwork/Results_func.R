
library(tidyverse)

test <- readRDS("C:/Users/philw/Desktop/shiny-PersonalNetwork/responses/18326_74194495053961871360.rds") %>% 
  mutate_all(as.character) %>% 
  pivot_longer(col=everything(), "fields", "values") %>% 
  separate("fields", into = c("prefix", "type")) %>% 
  mutate(InfluID = case_when(str_detect(prefix,"influ")  ~ gsub("influ", "", prefix), #stringr::str_sub(prefix,start=-1),
                             TRUE ~ ""),
         InfluID = as.numeric(InfluID))

self <- 
  test %>% 
  filter(prefix == "self")

influAMT <- 
  test %>% 
  filter(prefix == "amt") %>% 
  mutate(value = as.numeric(value)) %>% 
  pull(value)

Compar <-
  test %>% 
  filter(InfluID <= influAMT,
        !is.na(InfluID)) %>% 
  left_join(.,
            self, by = c("type")) %>% 
  mutate(match = ifelse(value.x == value.y, 1, 0)) %>%
  ungroup()

ComparSumm <- 
  Compar %>% 
  group_by(type) %>% 
  summarise(count = n(),
            Matches = sum(match)) %>% 
  ungroup() %>% 
  mutate(PerMatch = Matches/count)
