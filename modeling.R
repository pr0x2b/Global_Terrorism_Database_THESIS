
#---------------------------------
# Modeling
#---------------------------------

crit1_pol_eco_rel_soc   
crit2_publicize         
crit3_os_intl_hmn_law   
part_of_multiple_attacks
attack_success          
suicide_attack          
intl_logistical_attack 
intl_ideological_attack


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, knitr, pryr, tictoc, lubridate, timetk, tidyquant, lightgbm)
options(warn = -1, digits = 4, scipen = 999)

df <- df_leaflet %>%
  select(eventid, date, year, month, day, latitude, longitude, nkill, nwound, individual_attack, arms_export, arms_import, population, gdp_per_capita,
         refugee_asylum, refugee_origin, net_migration, n_peace_keepers, conflict_index, 
         region, country, provstate, city, attack_type, target_type, weapon_type, target_nalty, group_name,
         crit1_pol_eco_rel_soc, crit2_publicize, crit3_os_intl_hmn_law, part_of_multiple_attacks, attack_success, suicide_attack, 
         intl_logistical_attack, intl_ideological_attack) %>%
  replace_na(list(nkill = 0, nwound = 0)) %>%
  mutate(nkill = log1p(nkill), nwound= log1p(nwound))

# Plot response var - nkill with train, validation, and test sets
df %>%
  ggplot(aes(date, nkill)) +
  # Train Region
  annotate("text", x = ymd("1990-11-30"), y = 8,
           color = palette_light()[[1]], label = "Training Region") +
  # Validation Region
  geom_rect(xmin = as.numeric(ymd("2015-01-01")), 
            xmax = as.numeric(ymd("2015-12-31")),
            ymin = 0, ymax = Inf, alpha = 0.02,
            fill = palette_light()[[3]]) +
  annotate("text", x = ymd("2015-01-01"), y = 8,
           color = palette_light()[[1]], label = "Validation\nRegion") +
  # Test Region
  geom_rect(xmin = as.numeric(ymd("2016-01-01")), 
            xmax = as.numeric(ymd("2016-12-31")),
            ymin = 0, ymax = Inf, alpha = 0.02,
            fill = palette_light()[[4]]) +
  annotate("text", x = ymd("2016-11-01"), y = 8,
           color = palette_light()[[1]], label = "Test\nRegion") +
  # Data
  geom_line(col = "#477aad", alpha=0.3) +
  geom_point(col = "#477aad", alpha=0.1) +
  geom_ma(ma_fun = SMA, n = 100, size = 1, color = "tomato3") +
  annotate("text", x = as.Date("1996-01-01"), y = 3.0, color = "#477aad", label = 'atop(bold("nkill (logarithmic)"))', parse = TRUE) +
  annotate("text", x = as.Date("2001-01-01"), y = 0.2, color = "tomato3", label = 'atop(bold("nkill (moving avg)"))', parse = TRUE) +
  # Aesthetics
  theme_tq() +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  labs(title = "Global Terror attacks: from top 10 deadliest groups",
       subtitle = "Train, Validation, and Test Sets for Modelling") 

df <- df %>%
  mutate(latlong = latitude + longitude,
         arms_export = log1p(arms_export + 0.01),
         arms_import = log1p(arms_import + 0.01),
         population = log1p(population + 0.01)) %>%
  select(-c(date, latitude, longitude, nwound)) %>%
  na.omit()

features= names(df)
for (f in features) {
  if (class(df[[f]])=="character") {
    levels <- unique(c(df[[f]]))
    df[[f]] <- as.integer(factor(df[[f]], levels=levels))
  }
}

df[] <- lapply(df, as.numeric)
df <- as.data.table(df)
df[, n_group_year:=.N,       by=list(group_name, year)]
df[, n_country_year:=.N,       by=list(country, year)]
df[, n_region_year:=.N,       by=list(region, year)]
df[, n_attack_year:=.N,       by=list(attack_type, year)]
df[, n_target__year:=.N,       by=list(target_type, year)]
df[, n_weapon_year:=.N,       by=list(weapon_type, year)]
df[, n_group_country_year:=.N,       by=list(group_name, country, year)]
df[, n_country_armsimp_year:=.N,       by=list(country, arms_import, year)]
df[, n_country_armsexp_year:=.N,       by=list(country, arms_export, year)]
df[, n_group:=.N,       by=list(group_name)]
df[, n_provstate:=.N,   by=list(provstate)]
df[, n_city:=.N,       by=list(city)]
df <- as.data.frame(df)

train <- df %>% filter(year < 2013) %>% select(-c(eventid))
valid <- df %>% filter(year >= 2013 & year <= 2014) %>% select(-c(eventid))

# set aside test data with eventid as unique identifier for the predictions
test  <- df %>% filter(year >= 2015) 

dtest <- test %>% select(-c(eventid))
dtest <- as.matrix(dtest[, colnames(dtest)])

categorical_features = df %>% select(year, month, day, individual_attack, region, country, provstate, city, 
                                     attack_type, target_type, weapon_type, target_nalty, group_name,
                                     crit1_pol_eco_rel_soc, crit2_publicize, crit3_os_intl_hmn_law,
                                     part_of_multiple_attacks, attack_success, suicide_attack, intl_logistical_attack,
                                     intl_ideological_attack) %>% names()

dtrain = lgb.Dataset(data = as.matrix(train[, colnames(train) != "nkill"]), 
                     label = train$nkill, categorical_feature = categorical_features)
dvalid = lgb.Dataset(data = as.matrix(valid[, colnames(valid) != "nkill"]), 
                     label = valid$nkill, categorical_feature = categorical_features)


params = list(objective = "regression", 
              metric = "rmse", 
              learning_rate= 0.01, 
              num_leaves= 192, 
              # max_depth= 8,
              subsample= 0.8, 
              subsample_freq= 1, 
              colsample_bytree= 0.6,
              nthread = 4,
              verbose= 1)

model <- lgb.train(params, 
                   dtrain, 
                   valids = list(validation = dvalid), 
                   nrounds = 10000, 
                   early_stopping_rounds = 100, 
                   eval_freq = 100)

# training's rmse:0.643093	validation's rmse:0.712112

kable(lgb.importance(model, percentage = TRUE))
