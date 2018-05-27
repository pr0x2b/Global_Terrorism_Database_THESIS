
# special installation requirement
# devtools::install_github('ropensci/plotly')
# devtools::install_github("jbkunst/highcharter")


# load libraries and set global options
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, DT, openxlsx, RCurl, stringr, stringi, reshape, knitr, pryr, tictoc, 
               DescTools, StandardizeText, scales, lubridate, countrycode, leaflet, leaflet.extras,
               viridis, viridisLite,
               TSstudio, timetk, plotly, highcharter, treemap, d3heatmap, 
               countrycode, 
               shiny, ggmap, maptools, maps, 
               shinydashboard, shinythemes, shinyjs, shinyBS, shinyWidgets, shinycssloaders)

options(warn = -1, digits = 4, scipen = 999)

# load clean data (GTD)
df <- readRDS("gtd_clean.rds")

# countries mapped with iso3c codes for worldmap
countries <- readRDS("countries.rds") 

#-------------------------------------
# Deadlist group top10_hc1
#-------------------------------------

by_groups <- df %>% filter(group_name != "Unknown" & year >= 2010) %>% 
  replace_na(list(nkill = 0, nwound = 0)) %>% 
  select(group_name, region, year, month, nkill, nwound, part_of_multiple_attacks) %>% 
  group_by(group_name, region, year, month) %>% 
  filter(if_else(part_of_multiple_attacks == 1, 
                 nkill == max(nkill) & nwound == max(nwound), 
                 nkill == nkill & nwound == nwound)) %>%
  distinct(group_name, region, year, month, nkill, nwound, part_of_multiple_attacks) %>%
  mutate(impact = nkill + nwound) %>%
  group_by(group_name) %>%
  summarise(total = sum(impact)) %>% 
  arrange(desc(total)) %>% head(10)

top10_groups <- as.vector(by_groups$group_name)

df_leaflet <- df %>% 
  mutate(suicide_attack = if_else(suicide_attack == 1, "Yes", "No"),
         attack_success = if_else(attack_success == 1, "Yes", "No"),
         extended = if_else(extended == 1, "Yes", "No"),
         crit1_pol_eco_rel_soc = if_else(crit1_pol_eco_rel_soc == 1, "Yes", "No"),
         crit2_publicize = if_else(crit2_publicize == 1, "Yes", "No"),
         crit3_os_intl_hmn_law = if_else(crit3_os_intl_hmn_law == 1, "Yes", "No"),
         part_of_multiple_attacks = if_else(part_of_multiple_attacks == 1, "Yes", "No"),
         intl_logistical_attack = if_else(intl_logistical_attack == 1, "Yes", "No"),
         intl_ideological_attack = if_else(intl_ideological_attack == 1, "Yes", "No", "Unknown")
         )

df_leaflet_t10 <- df_leaflet %>% filter(group_name %in% top10_groups)
