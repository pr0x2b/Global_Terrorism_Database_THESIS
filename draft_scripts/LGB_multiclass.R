
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

df_multi_class <- df_class %>% filter(group_name %in% top10_groups) %>%
  mutate(nkill = log1p(nkill), 
         nwound= log1p(nwound),
         arms_export = log1p(arms_export + 0.01),
         arms_import = log1p(arms_import + 0.01),
         population = log1p(population + 0.01))

# create target variable (binary)

df_multi_class$ISIL <- ifelse(df_multi_class$group_name == "Islamic State of Iraq and the Levant (ISIL)", 1, 0)
df_multi_class$Taliban <- ifelse(df_multi_class$group_name == "Taliban", 1, 0)
df_multi_class$PKK <- ifelse(df_multi_class$group_name == "Kurdistan Workers' Party (PKK)", 1, 0)
df_multi_class$Al_Shabaab <- ifelse(df_multi_class$group_name == "Al-Shabaab", 1, 0)
df_multi_class$Boko_Haram <- ifelse(df_multi_class$group_name == "Boko Haram", 1, 0)
df_multi_class$DPR_Donetsk <- ifelse(df_multi_class$group_name == "Donetsk People's Republic", 1, 0)
df_multi_class$Al_Nusrah_Front <- ifelse(df_multi_class$group_name == "Al-Nusrah Front", 1, 0)
df_multi_class$Houthi_extremists <- ifelse(df_multi_class$group_name == "Houthi extremists (Ansar Allah)", 1, 0)
df_multi_class$Tehrik_i_Taliban_PAK <- ifelse(df_multi_class$group_name == "Tehrik-i-Taliban Pakistan (TTP)", 1, 0)
df_multi_class$AQAP_Al_Quaida_ArabPen <- ifelse(df_multi_class$group_name == "Al-Qaida in the Arabian Peninsula (AQAP)", 1, 0)

df_multi_class$group_name <- NULL

#response/ target vars

targets <-c("ISIL", "Taliban", "PKK", "Al_Shabaab", "Boko_Haram", "DPR_Donetsk", 
            "Al_Nusrah_Front", "Houthi_extremists", "Tehrik_i_Taliban_PAK", "AQAP_Al_Quaida_ArabPen")

features= names(df_multi_class)
for (f in features) {
  if (class(df_multi_class[[f]])=="character") {
    levels <- unique(c(df_multi_class[[f]]))
    df_multi_class[[f]] <- as.integer(factor(df_multi_class[[f]], levels=levels))
  }
}

#add count features
df_multi_class <- as.data.table(df_multi_class)
df_multi_class[, n_country_year:=.N,          by=list(country, year)]
df_multi_class[, n_region_year:=.N,           by=list(region, year)]
df_multi_class[, n_attack_year:=.N,           by=list(attack_type, year)]
df_multi_class[, n_target__year:=.N,          by=list(target_type, year)]
df_multi_class[, n_weapon_year:=.N,           by=list(weapon_type, year)]
df_multi_class[, n_provstate:=.N,             by=list(provstate)]
df_multi_class[, n_city:=.N,                  by=list(city)]
df_multi_class <- as.data.frame(df_multi_class)

df_multi_class[] <- lapply(df_multi_class, as.numeric)


train <- df_multi_class %>% filter(year <= 2014)
valid <- df_multi_class %>% filter(year == 2015)
test  <- df_multi_class %>% filter(year == 2016) 

#create empty data.frame for predictions
test_preds <- test[, !colnames(test) %in% targets]

dtest <- as.matrix(test[, !colnames(test) %in% targets])

categorical_features = df_multi_class %>% select(year, month, day, conflict_index, region, country, provstate, city, 
                                     attack_type, target_type, weapon_type, target_nalty, 
                                     crit1_pol_eco_rel_soc, crit2_publicize, crit3_os_intl_hmn_law,
                                     part_of_multiple_attacks, individual_attack, attack_success, intl_logistical_attack,
                                     intl_ideological_attack) %>% names()


# Modelling
params = list(objective = "binary", 
              metric = "auc", 
              learning_rate= 0.01, 
              num_leaves= 7,
              max_depth= 4,
              subsample= 0.8,
              subsample_freq= 1,
              colsample_bytree= 0.7,
              scale_pos_weight= 1) 


for (target in targets) {
  cat("\nFitting", target, "...\n")
  
  y_train <- train[[target]]
  y_valid <- valid[[target]]
  
  dtrain = lgb.Dataset(data = as.matrix(train[, !colnames(train) %in% targets]), 
                       label = train$DPR_Donetsk, categorical_feature = categorical_features)
  dvalid = lgb.Dataset(data = as.matrix(valid[, !colnames(valid) %in% targets]), 
                       label = valid$DPR_Donetsk, categorical_feature = categorical_features)
  
  model <- lgb.train(params, 
                     dtrain, 
                     valids = list(validation = dvalid), 
                     nrounds = 1000, 
                     early_stopping_rounds = 50, 
                     eval_freq = 100)
  
  cat("Validation AUC @ best iter: ", max(unlist(model$record_evals[["validation"]][["auc"]][["eval"]])))

  fi <- lgb.importance(model, percentage = TRUE)
  
  test_preds[[target]] <- predict(model, data = dtest, n = model$best_iter)
}




# Ref.: https://github.com/Microsoft/LightGBM/issues/695

num_classes <- length(unique(df_multi_class$group_name))

params = list(objective = "multiclass", 
              metric = "multi_error", 
              num_class = num_classes,
              learning_rate= 0.01, 
              num_leaves= 7,
              max_depth= 4,
              subsample= 0.9,
              subsample_freq= 1,
              colsample_bytree= 0.9) 

model <- lgb.train(params, 
                   dtrain, 
                   valids = list(validation = dvalid), 
                   nrounds = 1000, 
                   early_stopping_rounds = 50, 
                   eval_freq = 10)

cat("Validation AUC @ best iter: ", max(unlist(model$record_evals[["validation"]][["multi_error"]][["eval"]])))

# get feature importance
fi = lgb.importance(model, percentage = TRUE)

# highchart() %>% 
#   hc_title(text = "Feature importance by Gain (important for generating a prediction)") %>%
#   hc_xAxis(categories = fi$Feature) %>%
#   hc_add_series(name = "Gain", data = fi, type = "bar", hcaes(x = Feature, y = Gain)) %>%
#   hc_add_theme(hc_theme_ffx())
# 
# 
# highchart() %>%
#   hc_title(text = "Feature importance by Cover, Gain and Frequency") %>%
#   hc_xAxis(categories = fi$Feature) %>%
#   hc_add_series(name = "Cover", data = fi, type = "bar", hcaes(x = Feature, y = Cover)) %>%
#   hc_add_series(name = "Gain", data = fi, type = "bar", hcaes(x = Feature, y= Gain)) %>%
#   hc_add_series(name = "Frequency", data = fi, type = "bar", hcaes(x = Feature, y = Frequency)) %>%
#   hc_add_theme(hc_theme_538()) 
# 
# #extract interpretation for 1st observation in validation data
# tree_interpretation <- lgb.interprete(model, data = as.matrix(valid[, colnames(valid)]), 1)
# lgb.plot.interpretation(tree_interpretation[[1]])
# 
# 
# tmp <- df_leaflet_t10
# near_zero_vars <- nearZeroVar(tmp[, names(tmp)[colnames(tmp)!= "individual_attack"]])
# data <- data[ , -near_zero_vars]


# ROC curve

preds <- predict(model, dtest, reshape = TRUE)

val_preds = predict(model, data = as.matrix(valid[, colnames(valid) != "group_name"]), n = model$best_iter)

t(valid[1, ]) #let's choose the first observation
cat(paste("predicted value from model: ", val_preds[[1]]))

#extract interpretation for 1st observation in validation data
valid_matrix <- as.matrix(valid[, colnames(valid)])
tree_interpretation <- lgb.interprete(model, data = valid_matrix, idxset = 1)

lgb.plot.interpretation(tree_interpretation[[1]])

tree_dt <- lgb.model.dt.tree(model)


tmp <- as.data.frame(rbindlist(tree_interpretation))
tmp$Contribution <- round(tmp$Contribution, 2)

highchart() %>% 
  hc_title(text = "Model Interpretation by features contributions") %>%
  hc_add_series_labels_values(tmp$Feature, tmp$Contribution, showInLegend=F,
                              dataLabels = list(enabled = TRUE),
                              colors = ifelse(tmp$Contribution >= 0, "#ce1e36", "#0d6bc6"),
                              type = "bar") %>% 
  hc_yAxis(title = list(text = "Contribution"), labels = list(format = "{value}")) %>% 
  hc_xAxis(categories = tmp$Feature, title = list(text = "Feature")) %>% 
  hc_add_theme(hc_theme_ffx()) %>%
  hc_tooltip(pointFormat = "{point.y}") 

