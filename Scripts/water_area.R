

library(tidyverse)
library(janitor)
library(stringr)
library(countrycode)
library(ggimage)
library(ggtext)
library(png)
library(patchwork)
library(ggflags)
library(showtext)


# Os dados originais foram obtidos da FAO:
# 1 https://www.fao.org/faostat/en/#search/water, 
#  Dados do arquivo inland waters.
  #Areas values are in 1000 ha (hectares)
# 2 https://www.fao.org/fishery/statistics-query/en/aquaculture/aquaculture_quantity
  # Tonnes - live weight


#South America only

#As águas interiores podem ser usadas para se referir a lagos, rios, riachos, 
#córregos, lagoas, canais interiores, represas e outras águas sem litoral 
#(geralmente de água doce) (como o Mar Cáspio, Mar de Aral, etc.).


inland_waters_sa <- read_csv("raw/inland_waters11-14-2022.csv") %>% 
  clean_names() %>% 
  filter(area_code %in% c(9,19,21,40,44,58,69,91,169,170,207,234,236)) %>% 
  rename(country = "area", area_ha = "value") %>% 
  select(country, element_code, year, area_ha) %>% 
  mutate(country = replace(country, 
                           country == "Bolivia (Plurinational State of)",
                           "Bolivia")) %>% 
  mutate(country = replace(country, 
                           country == "Venezuela (Bolivarian Republic of)",
                           "Venezuela"))


# Adding countrycodes -------------------------------------------------------
# Requires package countrycode


inland_waters_sa$iso2 <- countrycode(sourcevar =  inland_waters_sa$country,
                                     destination = "iso2c", 
                                     origin  = "country.name")




                        

# Barplots ----------------------------------------------------------------

# This is to see assert the difference in area from the different 
# element_code

inland_waters_sa %>% 
  ggplot(aes(x = element_code, y = area_ha, fill = country)) +
  geom_bar(stat = "identity", position=position_dodge())

# Which element_code should I use?
# I will use element_code 5007 MODIS from NASA.


  


# Grouped means by element_code -------------------------------------------


inland_waters_sa %>% 
  group_by(element_code) %>% 
  summarise(area_media = mean(area_ha))

# element_code  area_media
# <dbl>           <dbl>
# 1 5006          1409.
# 2 5007          2175.
# 3 5008          2057.

mean(inland_waters_sa$area_ha)

# [1] 2040.428




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
    names_to = "year",
    values_to = "quantity"
  )



# Inland_waters_sa by year and element_code -------------------------------

inland_2020 <- inland_waters_sa %>% 
  filter(year == 2020 & element_code == 5007)


# Qauntity by year --------------------------------------------------------

quantity_2020 <- quantity_fw_pivoted %>% 
  filter(year == "2020")


# Joining the dataframes --------------------------------------------------

joined_in_qty <- inland_2020 %>% 
  inner_join(quantity_2020, 
             by = "country") %>% 
  mutate(produtividade = (quantity/(area_ha))) %>% 
  select(country, area_ha, quantity, produtividade, iso2)



# Adding fonts ------------------------------------------------------------

font_add_google(name = "Oxygen",
                family = "oxygen")
showtext_auto()


# Bubble Graph --------------------------------------------------------------

joined_in_qty %>% 
  ggplot(aes(area_ha, quantity, size = quantity, color = country)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = country), size = 4, vjust = - 4)+
  expand_limits(x=0, y=0) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", 
                                                  decimal.mark = ","), 
                     limits = c(0, 750000)) +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", 
                                                  decimal.mark = ","), 
                     limits = c(0, 15000)) +
  scale_size(range = c(10, 80)) +
  labs(title = "Produção da Aquicultura Em Águas Interiores na América do Sul",
       y = "Toneladas",
       x = "Área (1.000 Hectares)") +
  theme_light() +
  theme(legend.position = "none") 



# Gráfico de peodutividade ------------------------------------------------


joined_in_qty %>% 
  ggplot(aes(area_ha, produtividade, size = produtividade, color = country)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = country), size = 4, vjust = - 3)+
  expand_limits(x=0, y=0) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", 
                                                  decimal.mark = ","), 
                     limits = c(0, 150)) +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", 
                                                  decimal.mark = ","), 
                     limits = c(0, 15000)) +
  scale_size(range = c(4, 28)) +
  labs(title = "Produção da Aquicultura Em Águas Interiores na América do Sul",
       y = "Kg/Ha",
       x = "Área (1.000 Hectares)") +
  theme_light() +
  theme(legend.position = "none") 



# Barplot with flags and image -----------------------------------------------
# flags are from ggimage package

 
my_image <- readPNG("G:/My Drive/RWork/Projects/water_area/images/fish_icon.png", 
                    native = TRUE)

joined_in_qty %>% 
  ggplot(aes(x = reorder(country, produtividade), y = produtividade, 
             fill = country)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#1F77B4","#FF7F0E","#009c39", 
                               "#D62728","#9467BD","#8C564B",
                               "#7F7F7F","#BCBD22","#17BECF",
                               "#17BECF","#FFBB78","#FFBB78")) +
  ggimage::geom_flag(y = -10, aes(image = iso2))  +
  lims(y = c(-8, 125)) +
  labs(title =  "<span style = 'color: #009c39;'>O Brasil</span> Ainda Tem <br>
       Muita Água para Crescer",
       subtitle = "Aquicultura de Águas Interiores em Relação\na Superfície Total Existente (em 1.000ha)",
       x = "",
       y = "",
       caption = "FAO, 2020") +
  coord_flip() +
  geom_text(aes(label = round(produtividade,2)), hjust = -0.2, 
            fontface = "bold") +
  theme_light() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(colour = "gray60"),
        plot.title = element_markdown(size = 25, face = "bold"),
        axis.text.x =  element_blank(),
        axis.text.y = element_text(size = 15)) +
  inset_element(p = my_image,
                left = 0.3,
                bottom = 0.55,
                right = 0.95,
                top = 0.1)

# Same plot with different colors -----------------------------------------
# # flags are from ggimage package

my_image <- readPNG("G:/My Drive/RWork/Projects/water_area/images/fish_icon.png", 
                    native = TRUE)

joined_in_qty %>% 
  ggplot(aes(x = reorder(country, produtividade), y = produtividade, 
             fill = country)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#2e98fe","#2e98fe","#009c39", 
                               "#2e98fe","#2e98fe","#2e98fe",
                               "#2e98fe","#2e98fe","#2e98fe",
                               "#2e98fe","#2e98fe","#2e98fe")) +
  ggimage::geom_flag(y = -10, aes(image = iso2))  +
  lims(y = c(-8, 125)) +
  labs(title =  "<span style = 'color: #009c39;'>O Brasil</span> Ainda Tem <br>
       Muita Água para Crescer",
       subtitle = "Aquicultura de Águas Interiores em Relação\na Superfície Total Existente (em 1.000ha)",
       x = "",
       y = "",
       caption = "FAO, 2020") +
  coord_flip() +
  geom_text(aes(label = round(produtividade,2)), hjust = -0.2, 
            fontface = "bold") +
  theme_light() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(colour = "gray60"),
        plot.title = element_markdown(size = 25, face = "bold"),
        axis.text.x =  element_blank(),
        axis.text.y = element_text(size = 15)) +
  inset_element(p = my_image,
                left = 0.3,
                bottom = 0.55,
                right = 0.95,
                top = 0.1)

  

# Round Flags -------------------------------------------------------------
# Requires package ggflags

my_image <- readPNG("G:/My Drive/RWork/Projects/water_area/images/fish_icon.png", 
                    native = TRUE)

joined_in_qty %>% 
  # The geom_flag() function needs iso2c country codes in lower case format,
  mutate(iso2 = tolower(iso2)) %>% 
  ggplot(aes(x = reorder(country, produtividade), y = produtividade, 
             fill = country)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#2e98fe","#2e98fe","#009c39", 
                               "#2e98fe","#2e98fe","#2e98fe",
                               "#2e98fe","#2e98fe","#2e98fe",
                               "#2e98fe","#2e98fe","#2e98fe")) +
  geom_flag(y = -5, aes(country = iso2), size = 15)  +
  lims(y = c(-8, 125)) +
  labs(title =  "<span style = 'color: #009c39;'>O Brasil</span> Ainda Tem
       Muita Água para Crescer",
       subtitle = "Produtividade da aquicultura continental, em t/1.000 ha levando-se em conta 
a superfície total de águas interiores em cada país.",
       x = "",
       y = "",
       caption = "FAO, 2020") +
  coord_flip() +
  geom_text(aes(label = round(produtividade,2)), hjust = -0.2, 
            fontface = "bold", size = 5) +
  theme_light() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(colour = "gray60"),
        plot.title = element_markdown(size = 30, face = "bold",
                                      family = "oxygen"),
        plot.subtitle = element_text(size = 15),
        axis.text.x =  element_blank(),
        axis.text.y = element_text(size = 15)) +
  inset_element(p = my_image,
                left = 0.3,
                bottom = 0.55,
                right = 0.95,
                top = 0.1)











