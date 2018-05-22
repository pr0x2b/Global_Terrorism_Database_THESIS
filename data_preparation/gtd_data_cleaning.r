
if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, pryr, openxlsx, tidyverse, data.table, DT, DescTools, RCurl, countrycode)
options(warn = -1, digits = 4, scipen = 999)

setwd("C:/Users/Pranav_Pandya/Desktop/Thesis")

#External data (country geocodes)
country_geocodes <- readRDS("data/country_geocodes.rds")

#data preparation (GTD)
df <- read.xlsx("data/globalterrorismdb_0617dist.xlsx", sheet = 1, colNames = TRUE) %>% 
  select(eventid, 
         year = iyear, 
         month = imonth, 
         day = iday, 
         country = country_txt, 
         region = region_txt, 
         provstate, 
         city, 
         latitude, # 2.7% NAs will be replaced with country level geocodes
         longitude,
         attack_type = attacktype1_txt, 
         weapon_type = weaptype1_txt, 
         target_type = targtype1_txt, 
         target_nalty= natlty1_txt, 
         group_name  = gname, 
         nkill,   # 5% NAs
         nwound,  # 9% NAs
         extended, 
         crit1_pol_eco_rel_soc = crit1, 
         crit2_publicize = crit2, 
         crit3_os_intl_hmn_law = crit3, 
         part_of_multiple_attacks = multiple, 
         attack_success = success, 
         suicide_attack = suicide, 
         individual_attack = individual,
         intl_logistical_attack = INT_LOG, # whether a perpetrator group crossed a border to carry out an attack
         intl_ideological_attack = INT_IDEO # whether a perpetrator group attacked a target of a different nationality
         ) %>%
  replace_na(list(provstate = "unknown",       # replace nas with unknown
                  city =  "unknown",
                  target_nalty = "unknown")) %>%
  mutate(ISO = countrycode(country, 'country.name', 'iso3c'), # standardize country name codes
         month = if_else(month == 0, 1, month), # replace month to 1 in 20 occurences where month is unknown
         day = if_else(day == 0, 1, day),       # replace day to 1 in 891 occurences where month is unknown
         date = paste(year, month, day, sep="-"),
         date = as.Date(date, format = "%Y-%m-%d"),
         weapon_type = if_else(weapon_type == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)", "Vehicle", weapon_type)) %>%
  left_join(country_geocodes) %>% 
  mutate(latitude = ifelse(is.na(latitude),country_latitude, latitude), # replace missing lat lons with country lat lons
         longitude = ifelse(is.na(longitude), country_longitude, longitude)) %>%
  select(-c(country_latitude, country_longitude)) %>%
  # replace missing lat lons in remaining (~14) disputed/dissolved countries with country level lat long from prev obs
  mutate(latitude = if_else(is.na(latitude) & country == "People's Republic of the Congo", -0.228, latitude),
         longitude = if_else(is.na(longitude) & country == "People's Republic of the Congo", 15.83, longitude),
         latitude = if_else(is.na(latitude) & country == "Democratic Republic of the Congo", -4.038, latitude),
         longitude = if_else(is.na(longitude) & country == "Democratic Republic of the Congo", 21.76, longitude),
         latitude = if_else(is.na(latitude) & country == "North Yemen", 15.55, latitude),
         longitude = if_else(is.na(longitude) & country == "North Yemen", 48.52, longitude),
         latitude = if_else(is.na(latitude) & country == "South Yemen", 12.81, latitude),
         longitude = if_else(is.na(longitude) & country == "South Yemen", 45.04, longitude),
         latitude = if_else(is.na(latitude) & country == "Western Sahara", 27.41, latitude),
         longitude = if_else(is.na(longitude) & country == "Western Sahara", -9.047, longitude),
         latitude = if_else(is.na(latitude) & country == "Guadeloupe", 16.27, latitude),
         longitude = if_else(is.na(longitude) & country == "Guadeloupe", -61.55, longitude),
         latitude = if_else(is.na(latitude) & country == "New Caledonia", -20.9, latitude),
         longitude = if_else(is.na(longitude) & country == "New Caledonia", 165.6, longitude),
         latitude = if_else(is.na(latitude) & country == "Martinique", 14.61, latitude),
         longitude = if_else(is.na(longitude) & country == "Martinique", -61.07, longitude),
         latitude = if_else(is.na(latitude) & country == "Zaire", -2.501, latitude),
         longitude = if_else(is.na(longitude) & country == "Zaire", 28.87, longitude),
         latitude = if_else(is.na(latitude) & country == "Kosovo", 43.17, latitude),
         longitude = if_else(is.na(longitude) & country == "Kosovo", 20.74, longitude),
         latitude = if_else(is.na(latitude) & country == "Czechoslovakia", 50.66, latitude),
         longitude = if_else(is.na(longitude) & country == "Czechoslovakia", 14.03, longitude),
         latitude = if_else(is.na(latitude) & country == "Yugoslavia", 42.57, latitude),
         longitude = if_else(is.na(longitude) & country == "Yugoslavia", 20.56, longitude)
         )
saveRDS(df, "gtd_clean.rds")

# iso3c file for worldmap
countries <- df %>% group_by(country) %>% summarise(total = round(n())) 
countries$iso3 <- countrycode(countries$country, origin = "country.name", destination = "iso3c")
saveRDS(countries, "countries.rds")

# External data

# geocodes
geocodes <- fread("https://github.com/oughton/geocode/raw/master/example/result.csv") %>%
  select(country = V1, country_latitude = V2, country_longitude = V3) %>%
  mutate(ISO = countrycode(country, 'country.name', 'iso3c')) %>%
  filter(!is.na(ISO)) %>%
  select(ISO, country_latitude, country_longitude)
  
saveRDS(geocodes, "country_geocodes.rds")  



# Ethnicity by countries (ETH Zurich)
# https://icr.ethz.ch/data/epr/

ethnic_groups <- fread("data/EPR-2014.csv") %>%
  select(country = statename, from, to, ethnic_group = group, size, status) %>%
  filter(from >= 1970)
saveRDS(ethnic_groups, "ethnic_groups.rds")

# world pop by united nations
world_pop <- fread("https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_TotalPopulationBySex.csv") %>%
  select(country = Location, iyear = Time, pop = PopTotal) %>%
  filter(iyear >= 1970 & iyear <= 2016)

saveRDS(world_pop, "world_pop.rds")

world_pop <- world_pop[!duplicated(world_pop),]

# country_lat_lons <- as.data.frame(gCentroid(getMap(resolution="high"), byid=TRUE)) 
# country_lat_lons <- country_lat_lons %>%
#   mutate(country = rownames(country_lat_lons)) %>%
#   select(country, longitude = x, latitude = y)



conflicted_regions <- c("Afghanistan", "Algeria", "Angola", "Armenia", "Azerbaijan", "Burkina Faso", 
                        "Burundi", "Cameroon", "Central African Republic", "Chad", "Colombia", 
                        "Democratic Republic of the Congo", "Egypt", "Ethiopia", 
                        "Georgia", "India", "Iran", "Iraq", "Israel", "Kenya", "Lebanon", "Libya", 
                        "Mali", "Mexico", "Mozambique", "Myanmar", "Niger", "Nigeria", "Pakistan", 
                        "Palestine", "Peru", "Philippines", "Saudi Arabia", "Somalia", 
                        "South Sudan", "Sudan", "Syria", "Tunisia", "Turkey", "Ukraine", "Yemen")

df <- df %>% filter(country_txt %in% conflicted_regions)

Desc(df)


