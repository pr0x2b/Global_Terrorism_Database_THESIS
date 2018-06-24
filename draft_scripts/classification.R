#---------------------------------
# Modeling: h2o | classification
#---------------------------------

# Binary response vars
crit1_pol_eco_rel_soc   
crit2_publicize         
crit3_os_intl_hmn_law   
part_of_multiple_attacks
attack_success          
suicide_attack          
intl_logistical_attack 
extended

#multiclass
intl_ideological_attack
attack_type
target_type
weapon_type
group_name


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, knitr, pryr, tictoc, lubridate, timetk, tidyquant, h2o, lime)
options(warn = -1, digits = 4, scipen = 999)

df <- df_leaflet %>%
  select(year, month, day, nkill, nwound, individual_attack, arms_export, arms_import, population, gdp_per_capita,
         refugee_asylum, refugee_origin, net_migration, n_peace_keepers, conflict_index, 
         region, country, provstate, city, attack_type, target_type, weapon_type, target_nalty, group_name,
         crit1_pol_eco_rel_soc, crit2_publicize, crit3_os_intl_hmn_law, part_of_multiple_attacks, attack_success, suicide_attack, 
         intl_logistical_attack, intl_ideological_attack) %>%
  replace_na(list(nkill = 0, nwound = 0)) %>%
  group_by(group_name, region, year, month) %>% 
  filter(if_else(part_of_multiple_attacks == 1, 
                 nkill == max(nkill) & nwound == max(nwound), 
                 nkill == nkill & nwound == nwound)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(month = as.character(month),
         day = as.character(day),
         conflict_index = as.character(conflict_index),
         nkill = log1p(nkill), 
         nwound= log1p(nwound),
         arms_export = log1p(arms_export + 0.01),
         arms_import = log1p(arms_import + 0.01),
         population = log1p(population + 0.01),
         individual_attack = if_else(individual_attack == 1, "Yes", "No")) %>%
  na.omit()

#add count features
df <- as.data.table(df)
df[, n_group_year:=.N,            by=list(group_name, year)]
df[, n_country_year:=.N,          by=list(country, year)]
df[, n_region_year:=.N,           by=list(region, year)]
df[, n_attack_year:=.N,           by=list(attack_type, year)]
df[, n_target__year:=.N,          by=list(target_type, year)]
df[, n_weapon_year:=.N,           by=list(weapon_type, year)]
df[, n_group_country_year:=.N,    by=list(group_name, country, year)]
df[, n_group:=.N,                 by=list(group_name)]
df[, n_provstate:=.N,             by=list(provstate)]
df[, n_city:=.N,                  by=list(city)]
df <- as.data.frame(df)

df <- df %>%
  mutate_if(is.character, as.factor) %>%
  select(suicide_attack, everything())

library(h2o)
h2o.init(max_mem_size = "16g", enable_assertions = FALSE)

# Split data into Train/Validation/Test Sets

train_h2o <- df %>% filter(year <= 2014) %>% mutate(year = as.factor(as.character(year)))
valid_h2o <- df %>% filter(year == 2015) %>% mutate(year = as.factor(as.character(year)))
test_h2o  <- df %>% filter(year == 2016) %>% mutate(year = as.factor(as.character(year)))

train_h2o <- as.h2o(train_h2o, destination_frame = "train")
valid_h2o <- as.h2o(valid_h2o, destination_frame = "valid")
test_h2o  <- as.h2o(test_h2o, destination_frame = "test")

# Set names for h2o
y <- "suicide_attack"
x <- setdiff(names(train_h2o), y)

# Run the automated machine learning 
automl_models_h2o <- h2o.automl(x = x, 
                                y = y,
                                training_frame    = train_h2o,
                                validation_frame = valid_h2o,
                                leaderboard_frame = test_h2o,
                                max_runtime_secs  = 10
                              )

automl_models_h2o

# Extract leader model
automl_leader <- automl_models_h2o@leader
automl_leader

# Predict on hold-out set, test_h2o
pred_h2o <- h2o.predict(object = automl_leader, newdata = test_h2o)

# Prep for performance assessment
test_performance <- test_h2o %>%
  tibble::as_tibble() %>%
  select(suicide_attack) %>%
  add_column(pred = as.vector(pred_h2o$predict)) %>%
  mutate_if(is.character, as.factor)
test_performance

# Confusion table counts
confusion_matrix <- test_performance %>%
  table() 
confusion_matrix

# Performance analysis
tn <- confusion_matrix[1]
tp <- confusion_matrix[4]
fp <- confusion_matrix[3]
fn <- confusion_matrix[2]

accuracy <- (tp + tn) / (tp + tn + fp + fn)
misclassification_rate <- 1 - accuracy
recall <- tp / (tp + fn)
precision <- tp / (tp + fp)
null_error_rate <- tn / (tp + tn + fp + fn)

tibble(accuracy, misclassification_rate, recall, precision,null_error_rate) %>% transpose() 


library(lime)

class(automl_leader)

# Setup lime::model_type() function for h2o
model_type.H2OBinomialModel <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  #
  # x is our h2o model
  
  return("classification")
}

# Setup lime::predict_model() function for h2o
predict_model.H2OBinomialModel <- function(x, newdata, type, ...) {
  # Function performs prediction and returns dataframe with Response
  #
  # x is h2o model
  # newdata is data frame
  # type is only setup for data frame
  
  pred <- h2o.predict(x, as.h2o(newdata))
  
  # return probs
  return(as.data.frame(pred[,-1]))
  
}

# Test our predict_model() function
predict_model(x = automl_leader, newdata = as.data.frame(test_h2o[,-1]), type = 'raw') %>% tibble::as_tibble()

# Run lime() on training set
explainer <- lime::lime(
  as.data.frame(train_h2o[,-1]), 
  model          = automl_leader, 
  bin_continuous = FALSE)

data <- df_class %>% group_by(suicide_attack) %>% summarise(count = n()) %>% mutate(suicide_attack = ifelse(suicide_attack == 1, "Yes", "No"))

highchart() %>% 
  hc_title(text = "Target variable") %>%
  hc_xAxis(categories = data[[1]]) %>% 
  hc_yAxis(title = list(text = "Total count")) %>%
  hc_add_series(data = data[[2]], type = "column", showInLegend = F, colorByPoint = TRUE) %>%
  hc_add_theme(hc_theme_ffx()) %>%
  hc_tooltip(pointFormat = "{point.y}") 

data <- df_class %>% group_by(suicide_attack) %>% summarize(count = n())
names(data) <- c("target_var", "count")
