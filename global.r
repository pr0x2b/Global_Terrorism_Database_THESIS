
#--------------------------------
# For thesisdown (writing part)
#--------------------------------
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tinytex)
# tinytex::install_tinytex()
##after restarting RStudio, confirm that you have LaTeX with
# tinytex:::is_tinytex()

# Thesisdown
# devtools::install_github("rstudio/bookdown")
# devtools::install_github("ismayc/thesisdown")
# devtools::install_github("ismayc/reedtemplates")
# devtools::install_github("crsh/citr")
# install.packages("webshot")
# webshot::install_phantomjs()


#--------------------------------
# For shiny dashborad
#--------------------------------

# special installation requirement
# devtools::install_github('ropensci/plotly')
# devtools::install_github("jbkunst/highcharter")
# devtools::install_github('rstudio/DT')

# devtools::install_github("RamiKrispin/TSstudio")
# devtools::install_github("RamiKrispin/MLstudio")
# library(MLstudio)

# load libraries and set global options
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, DT, openxlsx, RCurl, stringr, stringi, reshape, knitr, pryr, tictoc, kableExtra, R.utils, 
               DescTools, GGally, StandardizeText, scales, lubridate, countrycode, leaflet, leaflet.extras,
               viridis, viridisLite, RColorBrewer, ggfortify, plotly, highcharter, treemap, d3heatmap, 
               arules, arulesViz, TSstudio, timetk, tidyquant, tidyr, zoo, forecast, tseries, imputeTS, 
               countrycode, WDI, purrr, igraph, visNetwork, randomcoloR, treemapify,
               shiny, ggmap, maptools, maps, eply, 
               shinydashboard, shinythemes, shinyjs, shinyBS, shinyWidgets, shinycssloaders, tidyverse,
               parallel, caret, pROC, lightgbm)

options(warn = -1, digits = 4, scipen = 999)
set.seed(84)

# load clean data (GTD)
df <- readRDS("index/data/gtd_clean_v2.rds")

# countries mapped with iso3c codes for worldmap
countries <- readRDS("index/data/countries.rds") 

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

#------------------------------------------------------
# Data for leaflet plot
#------------------------------------------------------

df_leaflet <- df %>% 
  mutate(suicide_attack = if_else(suicide_attack == 1, "Yes", "No"),
         attack_success = if_else(attack_success == 1, "Yes", "No"),
         extended = if_else(extended == 1, "Yes", "No"),
         crit1_pol_eco_rel_soc = if_else(crit1_pol_eco_rel_soc == 1, "Yes", "No"),
         crit2_publicize = if_else(crit2_publicize == 1, "Yes", "No"),
         crit3_os_intl_hmn_law = if_else(crit3_os_intl_hmn_law == 1, "Yes", "No"),
         part_of_multiple_attacks = if_else(part_of_multiple_attacks == 1, "Yes", "No"),
         intl_logistical_attack = if_else(intl_logistical_attack == 1, "Yes", 
                                  if_else(intl_logistical_attack == 0, "No", "Unknown")),
         intl_ideological_attack = if_else(intl_ideological_attack == 1, "Yes", 
                                   if_else(intl_ideological_attack == 0,"No", "Unknown"))
         )

df_leaflet_t10 <- df_leaflet %>% filter(group_name %in% top10_groups)


#------------------------------------------------------
# Data for Network graph (pattern discovery)
#------------------------------------------------------

dfn <- df %>% 
  filter(group_name %in% top10_groups) %>%  # filter data by top 10 groups
  replace_na(list(nkill = 0, nwound = 0))   # replace NAs

# Shorten lengthy group names
dfn$group_name[dfn$group_name == "Kurdistan Workers' Party (PKK)"] <- "PKK"
dfn$group_name[dfn$group_name == "Al-Qaida in the Arabian Peninsula (AQAP)"] <- "AQAP"
dfn$group_name[dfn$group_name == "Houthi extremists (Ansar Allah)"] <- "Houthi_Extrm"
dfn$group_name[dfn$group_name == "Tehrik-i-Taliban Pakistan (TTP)"] <- "TTP"
dfn$group_name[dfn$group_name == "Al-Nusrah Front"] <- "Al-Nusrah"
dfn$group_name[dfn$group_name == "Islamic State of Iraq and the Levant (ISIL)"] <-"ISIL"
dfn$group_name[dfn$group_name == "Donetsk People's Republic"] <- "Donetsk_PR" 

dfn <- dfn %>%
  select(group_name, target_type, weapon_type, attack_type, suicide_attack, nkill) %>%
  filter(target_type != "Unknown" & target_type != "Other" & 
         weapon_type != "Unknown" & attack_type != "Unknown") %>%
  mutate(nkill = if_else(nkill == 0, "0",
                 if_else(nkill >= 1 & nkill <= 5, "1 to 5",
                 if_else(nkill > 5 & nkill <= 10, "6 to 10",
                 if_else(nkill > 10 & nkill <= 50, "11 to 50",  "more than 50")))))

#shorten lengthy names for visualization purpose
dfn$weapon_type[dfn$weapon_type == "Explosives/Bombs/Dynamite"] <- "Explosives"
dfn$attack_type[dfn$attack_type == "Facility/Infrastructure Attack"] <- "Facility/Infra."
dfn$target_type[dfn$target_type == "Private Citizens & Property"] <- "Civilians"
dfn$target_type[dfn$target_type == "Terrorists/Non-State Militia"] <- "Non-State Militia"
dfn$target_type[dfn$target_type == "Religious Figures/Institutions"] <- "Religious Figures"

#convert everything to factor
dfn[] <- lapply(dfn, factor)


#------------------------------------------------------
# Data for Classification models with LightGBM
#------------------------------------------------------

df_class <- df %>% 
  select(year, month, day,  
         region, country, provstate, city, attack_type, target_type, weapon_type, target_nalty, group_name,
         crit1_pol_eco_rel_soc, crit2_publicize, crit3_os_intl_hmn_law, part_of_multiple_attacks, individual_attack, 
         attack_success, suicide_attack, extended, intl_logistical_attack, intl_ideological_attack,
         nkill, nwound, arms_export, arms_import, population, gdp_per_capita,
         refugee_asylum, refugee_origin, net_migration, n_peace_keepers, conflict_index) %>%
  replace_na(list(nkill = 0, nwound = 0)) %>%
  mutate(suicide_attack = if_else(suicide_attack == 1, "Yes", "No"),
         attack_success = if_else(attack_success == 1, "Yes", "No"),
         extended = if_else(extended == 1, "Yes", "No"),
         individual_attack = if_else(individual_attack == 1, "Yes", "No"),
         part_of_multiple_attacks = if_else(part_of_multiple_attacks == 1, "Yes", "No"),
         crit1_pol_eco_rel_soc = if_else(crit1_pol_eco_rel_soc == 1, "Yes", "No"),
         crit2_publicize = if_else(crit2_publicize == 1, "Yes", "No"),
         crit3_os_intl_hmn_law = if_else(crit3_os_intl_hmn_law == 1, "Yes", "No"),
         intl_logistical_attack = if_else(intl_logistical_attack == 1, "Yes", 
                                    if_else(intl_logistical_attack == 0, "No", "Unknown")),
         intl_ideological_attack = if_else(intl_ideological_attack == 1, "Yes", 
                                      if_else(intl_ideological_attack == 0,"No", "Unknown"))) %>%
  na.omit()

