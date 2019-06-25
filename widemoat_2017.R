widemoat_2017 <- read.csv('widemoat_2017.csv', header = FALSE) %>% 
  .$V2 %>% 
  as.character()