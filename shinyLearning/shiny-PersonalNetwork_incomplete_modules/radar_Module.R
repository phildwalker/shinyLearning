# Creating the radar chart to show the homophily

# Example from https://github.com/ricardo-bion/ggradar

# library(ggradar)
library(dplyr)
library(scales)
# library(tibble)

# p <- 
SampleData <-  mtcars %>% 
  tibble::as_tibble(rownames = "group") %>% 
  mutate_at(vars(-group), rescale) %>% 
  tail(1) %>% 
  select(1:10) 

# p + ggplot2::theme_bw()

# 
# ggradar(mtcars_radar)



plotRadar <- function(){
  library(dplyr)
  library(scales)
  
  mtcars %>% 
    tibble::as_tibble(rownames = "group") %>% 
    mutate_at(vars(-group), rescale) %>% 
    tail(1) %>% 
    select(1:10) %>% 
    ggradar::ggradar()
}