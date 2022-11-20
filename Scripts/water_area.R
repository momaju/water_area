library(tidyverse)
library(janitor)
library(stringr)


#South America only

inland_waters_sa <- read_csv("raw/inland_waters11-14-2022.csv") %>% 
  clean_names() %>% 
  filter(area_code %in% c(9,19,21,40,44,58,169,170,91, 236)) %>% 
  rename(country = "area") %>% 
  select(country, element_code, year, value)



inland_2020 <- inland_waters_sa %>% 
  filter(year == 2020)



# Renaming Bolívia and Venezuela --------------------------------------------------------



inland_waters_sa["country"][inland_waters_sa["country"] == 
"Bolivia (Plurinational State of)"] <- "Bolivia"

inland_waters_sa["country"][inland_waters_sa["country"] == 
"Venezuela (Bolivarian Republic of)"] <- "Venezuela"





# Produção de pescado -----------------------------------------------------

# quantity in Tonnes - live weight

quantity_fw <- read_csv("raw/aquaculture_quantity_fw.csv") %>% 
  clean_names() %>% 
  rename(country = "country_name_en", "2020" = "x2020", 
         "2019" = "x2019", "2018" = "x2018",
         "2017" = "x2017", "2016" = "x2016") %>% 
  select(country, '2020', '2019', '2018', '2017', '2016')

quantity_fw["country"][quantity_fw["country"] == 
                           "Bolivia (Plurinat.State)"] <- "Bolivia"

quantity_fw["country"][quantity_fw["country"] == 
                           "Venezuela (Boliv Rep of)"] <- "Venezuela"

quantity_fw_pivoted <- quantity_fw %>% 
  pivot_longer(
    cols = '2020':'2016',
    names_to = "yaear",
    values_to = "quantity"
  )
  











