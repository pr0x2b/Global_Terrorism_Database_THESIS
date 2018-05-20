
# load libraries and set global options
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, DT, openxlsx, RCurl, stringr, stringi, reshape, knitr, pryr, tictoc, 
               DescTools, StandardizeText, scales, lubridate, countrycode, leaflet, viridis, viridisLite,
               plotly, highcharter, treemap,
               countrycode, 
               shiny, ggmap, maptools, maps, shinydashboard, shinythemes, shinyjs, shinyBS, shinyWidgets)

options(warn = -1, digits = 4, scipen = 999)



