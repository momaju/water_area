library(tidyverse)
library(janitor)
library(stringr)


#South America only

inland_waters_sa <- read_csv("raw/inland_waters11-14-2022.csv") %>% 
  clean_names() %>% 
  filter(area_code %in% c(9,19,21,40,44,58,169,170,91, 236))

inland_2019 <- inland_waters_sa %>% 
  filter(year == 2019)


inland_2020 <- inland_waters_sa %>% 
  filter(year == 2020)



# Renaming Bolívia and Venezuela --------------------------------------------------------



inland_waters_sa["area"][inland_waters_sa["area"] == 
"Bolivia (Plurinational State of)"] <- "Bolivia"

inland_waters_sa["area"][inland_waters_sa["area"] == 
"Venezuela (Bolivarian Republic of)"] <- "Venezuela"


# Produção de pescado -----------------------------------------------------



quantity_fw <- read_csv("raw/aquaculture_quantity_fw.csv")











