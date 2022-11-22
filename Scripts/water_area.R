library(tidyverse)
library(janitor)
library(stringr)

# Os dados originais foram obtidos da FAO:
# 1 https://www.fao.org/faostat/en/#search/water, Dodos do arquivo inland waters.
# 2 https://www.fao.org/fishery/statistics-query/en/aquaculture/aquaculture_quantity


#South America only

inland_waters_sa <- read_csv("raw/inland_waters11-14-2022.csv") %>% 
  clean_names() %>% 
  filter(area_code %in% c(9,19,21,40,44,58,169,170,91, 236)) %>% 
  rename(country = "area", area_ha = "value") %>% 
  select(country, element_code, year, area_ha) %>% 
  mutate(country = replace(country, 
                           country == "Bolivia (Plurinational State of)",
                           "Bolivia")) %>% 
  mutate(country = replace(country, 
                           country == "Venezuela (Bolivarian Republic of)",
                           "Venezuela"))





# Barplots ----------------------------------------------------------------

# This is to see assert the difference in area from the different 
# element_code

inland_waters_sa %>% 
  ggplot(aes(x = element_code, y = area_ha, fill = country)) +
  geom_bar(stat = "identity", position=position_dodge())

# Which element_code should I use?


  


# Grouped means by element_code -------------------------------------------


inland_waters_sa %>% 
  group_by(element_code) %>% 
  summarise(area_media = mean(area))

# element_code  area_media
# <dbl>           <dbl>
# 1 5006          1772.
# 2 5007          2776.
# 3 5008          2602.

mean(inland_waters_sa$area)

# [1] 2589.374




# Produção de pescado -----------------------------------------------------

# quantity in Tonnes - live weight

quantity_fw <- read_csv("raw/aquaculture_quantity_fw.csv") %>% 
  clean_names() %>% 
  rename(country = "country_name_en", "2020" = "x2020", 
         "2019" = "x2019", "2018" = "x2018",
         "2017" = "x2017", "2016" = "x2016") %>% 
  select(country, '2020', '2019', '2018', '2017', '2016') %>% 
  mutate(country = replace(country, 
                           country == "Bolivia (Plurinat.State)",
                           "Bolivia")) %>% 
  mutate(country = replace(country, 
                           country == "Venezuela (Boliv Rep of)",
                           "Venezuela"))

  
  
  

quantity_fw_pivoted <- quantity_fw %>% 
  pivot_longer(
    cols = '2020':'2016',
    names_to = "yaear",
    values_to = "quantity"
  )
  











