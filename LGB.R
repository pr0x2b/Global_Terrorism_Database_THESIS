
#----------------------------------------
# Modeling: Classification with Lightgbm
#----------------------------------------

# Binary response vars
crit1_pol_eco_rel_soc   
crit2_publicize         
crit3_os_intl_hmn_law   
part_of_multiple_attacks
attack_success          
suicide_attack         
extended

#multiclass 
intl_logistical_attack 
intl_ideological_attack
attack_type
target_type
weapon_type
group_name


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, knitr, pryr, tictoc, lubridate, timetk, tidyquant, lightgbm)
options(warn = -1, digits = 4, scipen = 999)

df_class <- df %>% filter(country == "Afghanistan") %>%
  select(year, month, day, nkill, nwound, arms_export, arms_import, population, gdp_per_capita,
         refugee_asylum, refugee_origin, net_migration, n_peace_keepers, conflict_index, 
         region, country, provstate, city, attack_type, target_type, weapon_type, target_nalty, group_name,
         crit1_pol_eco_rel_soc, crit2_publicize, crit3_os_intl_hmn_law, part_of_multiple_attacks, individual_attack, 
         attack_success, suicide_attack, intl_logistical_attack, intl_ideological_attack) %>%
  replace_na(list(nkill = 0, nwound = 0)) %>%
  group_by(group_name, region, year, month) %>% 
  filter(if_else(part_of_multiple_attacks == 1, 
                 nkill == max(nkill) & nwound == max(nwound), 
                 nkill == nkill & nwound == nwound)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(nkill = log1p(nkill), 
         nwound= log1p(nwound),
         arms_export = log1p(arms_export + 0.01),
         arms_import = log1p(arms_import + 0.01),
         population = log1p(population + 0.01)) %>%
  na.omit()

features= names(df_class)
for (f in features) {
  if (class(df_class[[f]])=="character") {
    levels <- unique(c(df_class[[f]]))
    df_class[[f]] <- as.integer(factor(df_class[[f]], levels=levels))
  }
}

#add count features
df_class <- as.data.table(df_class)
df_class[, n_group_year:=.N,            by=list(group_name, year)]
df_class[, n_country_year:=.N,          by=list(country, year)]
df_class[, n_region_year:=.N,           by=list(region, year)]
df_class[, n_attack_year:=.N,           by=list(attack_type, year)]
df_class[, n_target__year:=.N,          by=list(target_type, year)]
df_class[, n_weapon_year:=.N,           by=list(weapon_type, year)]
df_class[, n_group_country_year:=.N,    by=list(group_name, country, year)]
df_class[, n_group:=.N,                 by=list(group_name)]
df_class[, n_provstate:=.N,             by=list(provstate)]
df_class[, n_city:=.N,                  by=list(city)]
df_class <- as.data.frame(df_class)

df_class[] <- lapply(df_class, as.numeric)

train <- df_class %>% filter(year <= 2014)
valid <- df_class %>% filter(year == 2015)
test  <- df_class %>% filter(year == 2016) 

dtest <- as.matrix(test[, colnames(test)])

categorical_features = df_class %>% select(year, month, day, conflict_index, region, country, provstate, city, 
                                     attack_type, target_type, weapon_type, target_nalty, group_name,
                                     crit1_pol_eco_rel_soc, crit2_publicize, crit3_os_intl_hmn_law,
                                     part_of_multiple_attacks, individual_attack, attack_success, intl_logistical_attack,
                                     intl_ideological_attack) %>% names()

dtrain = lgb.Dataset(data = as.matrix(train[, colnames(train) != "suicide_attack"]), 
                     label = train$suicide_attack, categorical_feature = categorical_features)
dvalid = lgb.Dataset(data = as.matrix(valid[, colnames(valid) != "suicide_attack"]), 
                     label = valid$suicide_attack, categorical_feature = categorical_features)

spw <- as.data.frame(table(train$suicide_attack)) 
spw <- round(spw$Freq[1]/spw$Freq[2], 0)
# Ref.: https://github.com/Microsoft/LightGBM/issues/695

params = list(objective = "binary", 
              metric = "auc", 
              learning_rate= 0.001, 
              num_leaves= 255,
              # max_depth= 4,
              subsample= 0.8,
              subsample_freq= 1,
              colsample_bytree= 0.7,
              scale_pos_weight= spw) 

model <- lgb.train(params, 
                   dtrain, 
                   valids = list(validation = dvalid), 
                   nrounds = 1000, 
                   early_stopping_rounds = 50, 
                   eval_freq = 10)

cat("Validation AUC @ best iter: ", max(unlist(model$record_evals[["validation"]][["auc"]][["eval"]])))

# get feature importance
fi = lgb.importance(model, percentage = TRUE)

highchart() %>% 
  hc_title(text = "Feature importance by Gain (important for generating a prediction)") %>%
  hc_xAxis(categories = fi$Feature) %>%
  hc_add_series(name = "Gain", data = fi, type = "bar", hcaes(x = Feature, y = Gain)) %>%
  hc_add_theme(hc_theme_ffx())


highchart() %>%
  hc_title(text = "Feature importance by Cover, Gain and Frequency") %>%
  hc_xAxis(categories = fi$Feature) %>%
  hc_add_series(name = "Cover", data = fi, type = "bar", hcaes(x = Feature, y = Cover)) %>%
  hc_add_series(name = "Gain", data = fi, type = "bar", hcaes(x = Feature, y= Gain)) %>%
  hc_add_series(name = "Frequency", data = fi, type = "bar", hcaes(x = Feature, y = Frequency)) %>%
  hc_add_theme(hc_theme_538()) 

#extract interpretation for 1st observation in validation data
tree_interpretation <- lgb.interprete(model, data = as.matrix(valid[, colnames(valid)]), 1)
lgb.plot.interpretation(tree_interpretation[[1]])
