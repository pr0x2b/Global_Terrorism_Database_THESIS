
#-------------------------
# timeseries
#-------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, knitr, pryr, tictoc, lubridate, timetk, tidyquant, tidyr,
               zoo, forecast, tseries, TSstudio, imputeTS, thief, tidyverse)
options(warn = -1, digits = 4, scipen = 999)

# load and subset the dataset
df1 <- subset(df, select = c(year, country))

# create time series
dfz <- count(df1, year)
dfz <- filter(dfz, year >=1994)
dfz <- zoo(dfz$n, order.by = df$year)


# Predictions 2014-2015 and Evaluation metrics
AR <- accuracy(f = predict(ar(df[1:20]), n.ahead = 2)$pred, x = df[21:22])
MA <- accuracy(f = predict(ma(df[1:20], order = 2), h = 2)$mean, x = df[21:22])
ARIMA <- accuracy(f = predict(auto.arima(df[1:20]), n.ahead = 2)$pred, x = df[21:22])
ETS <- accuracy(f = predict(ets(df[1:20])$fitted, h = 2)$mean, x = df[21:22])
metrics  <- rbind(as.data.frame(AR, row.names = "AR"),
                  as.data.frame(MA, row.names = "MA"),
                  as.data.frame(ARIMA, row.names = "ARIMA"),
                  as.data.frame(ETS, row.names = "ETS"))

kable(metrics)

#----------------------------------------------
# timeseries data
#----------------------------------------------

# by attack counts
tmp <- df %>% 
  filter(country == "Afghanistan",year >= 2000) %>%
  # add_row(date = as.Date("1993-01-01"), year = 1993) %>% #add missing year for timeseries purpose
  mutate(
         month = month(date),
         month_year = paste(year, month,sep="-"),
         month_year = zoo::as.yearmon(month_year)
         # quarter = Quarter(date),
         # quarter_year = paste(year, quarter, sep="-"),
         # quarter_year = as.yearqtr(quarter_year, "%Y-%q")
         ) %>%
  group_by(year, month) %>%
  summarise(attack_count = n()) %>%
  # mutate(attack_count = ifelse(year == 1993, 0, attack_count)) %>% # set 0 to attack_count where year is 1993 (which is missing)
  ungroup() %>%
  group_by(year) %>%
  tidyr::complete(month = full_seq(seq(1:12), 1L), fill = list(attack_count = 0)) %>%
  ungroup()

tmp <- tmp %>%
  mutate(month_year = paste(year, month, sep="-"),
         month_year = zoo::as.yearmon(month_year)) %>%
  select(month_year, attack_count)

# by nkill
# tmp <- df %>% 
#   filter(country == "Afghanistan" & year >= 2000) %>%
#   mutate(quarter = Quarter(date),
#          quarter_year = paste(year, quarter, sep=" "),
#          quarter_year = as.yearqtr(quarter_year, "%Y %q")) %>%
#   replace_na(list(nkill = 0)) 
# 
# tmp <- as.data.frame(t(table(tmp$quarter_year))) %>% 
#   select(quarter_year = Var2, nkill_count = Freq) %>%
#   mutate(quarter_year = as.character(quarter_year),
#          quarter_year = str_replace(quarter_year, "Q", ""),
#          quarter_year = as.yearqtr(quarter_year, "%Y %q"))

#by nkill
tmp <- df %>% 
  filter(country == "United States" & year >= 2000) %>%
  replace_na(list(nkill = 0)) %>% 
  group_by(group_name, region, year, month) %>% 
  filter(if_else(part_of_multiple_attacks == 1, nkill == max(nkill), nkill == nkill)) %>%
  distinct(group_name, region, year, month, nkill, part_of_multiple_attacks) %>%
  ungroup() %>%
  mutate(month_year = paste(year, month, sep="-"),
         month_year = zoo::as.yearmon(month_year)) %>%
  group_by(year, month) %>%
  summarise(nkill_count = sum(nkill)) %>%
  ungroup() %>%
  group_by(year) %>%
  tidyr::complete(month = full_seq(seq(1:12), 1L), fill = list(nkill_count = 0)) %>%
  ungroup()

tmp <- tmp %>%
  mutate(month_year = paste(year, month, sep="-"),
         month_year = zoo::as.yearmon(month_year)) %>%
  select(month_year, nkill_count)


# Create a ts object
dfts <- ts(tmp[, 2], start = Year(min(tmp$month_year)), frequency = 12) # 1=annual, 4=quartly, 12=monthly
dfts <- na.kalman(dfts)

ts_plot(dfts, line.mode = "lines+markers")

ts_seasonal(dfts, type = "normal")
ts_seasonal(dfts, type = "cycle")
ts_seasonal(dfts, type = "box")
ts_seasonal(dfts, type = "all")

ts_heatmap(dfts)
ts_surface(dfts)
ts_polar(dfts, width = 300, height = 500)

ts_decompose(dfts, type = "both")
ts_acf(dfts, lag.max = 36)
ts_pacf(dfts, lag.max = 36)
ts_lags(dfts)

# set the forecast horizon for 12 months
h <- 12

# Split the data into training and testing sets (leaving the last 12 months for testing)
split_dfts <- ts_split(dfts, sample.out = h)

train <- split_dfts$train
test <- split_dfts$test

head(train, 5)
tail(test, 5)

# Building a model on the training set
fit_arima <- auto.arima(train, lambda = BoxCox.lambda(train))

# Checking the residuals
check_res(fit_arima)

# Forecast evaluation with the test_forecast
fc <- forecast(fit_arima, h = h)
test_forecast(actual = dfts, forecast.obj = fc, test = test)

kable(t(round(accuracy(fc$mean, test), 3)), caption = "Mean accuracy on test data") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
  column_spec(2, bold = T, color = "black", background = "#dee2ed")

#---------------------------------------
# Neural network
#---------------------------------------
fit_nn = nnetar(train, repeats = 10)
check_res(fit_nn)
fc = forecast(fit_nn, h = h)
test_forecast(actual = dfts, forecast.obj = fc, test = test)

#---------------------------------------
# tbats
#---------------------------------------
fit_tbats = tbats(train)
autoplot(fit_tbats)
plot(forecast(fit_tbats, h = h))
fc = forecast(fit_tbats, h = h)
test_forecast(actual = dfts, forecast.obj = fc, test = test)

#---------------------------------------
# ETS
#---------------------------------------
fit_ets = ets(train)
check_res(fit_ets)
fc = forecast(fit_ets, h = h)
test_forecast(actual = dfts, forecast.obj = fc, test = test)


# full forecast
fit <- nnetar(dfts, repeats = 10)
fore <- forecast(fit, h = 12, level = c(80, 95), PI = TRUE)

tbl <- timetk::tk_tbl(fore$mean) 
names(tbl) <- c("time_period", "forecast")
tbl$forecast <- round(tbl$forecast)

knitr::kable(tbl, caption = "Forecasted attacks") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
  column_spec(2, bold = T, color = "white", background = "#a05050")

plot_ly() %>%
  add_lines(x = time(dfts), y = dfts,
            color = I("#487caf"), name = "observed") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
              color = I("gray90"), name = "95% confidence") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
              color = I("gray85"), name = "80% confidence") %>%
  add_lines(x = time(fore$mean), y = fore$mean, color = I("orange"), name = "prediction")

fit_arima <- auto.arima(train)
fit_nn <- nnetar(train, repeats = 10)
fit_tbats <- tbats(train)
fit_ets <- ets(train)

fc_arima      <- forecast(fit_arima, h = h)
fc_nn         <- forecast(fit_nn, h = h)
fc_tbats      <- forecast(fit_tbats, h = h)
fc_ets        <- forecast(fit_ets, h = h)
test          <- forecast_data$test

metrics  <- cbind(
  as.data.frame(t(round(accuracy(fc_arima$mean, test), 3))),
  as.data.frame(t(round(accuracy(fc_nn$mean, test), 3))),
  as.data.frame(t(round(accuracy(fc_tbats$mean, test), 3))),
  as.data.frame(t(round(accuracy(fc_ets$mean, test), 3)))
) %>% rownames_to_column(var = "metric")


names(metrics) <- c("metric", "Auto Arima", "NeuralNet", "TBATS", "ETS")

metrics  <- rbind(
  as.data.frame(round(accuracy(fc_arima$mean, test), 3)),
  as.data.frame(round(accuracy(fc_nn$mean, test), 3)),
  as.data.frame(round(accuracy(fc_tbats$mean, test), 3)),
  as.data.frame(round(accuracy(fc_ets$mean, test), 3))
) %>% add_column(metric = c("Auto Arima", "NeuralNet", "TBATS", "ETS"), .before = "ME") 

metrics <- as.data.frame(metrics, row.names = FALSE)

plot(metrics)
barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")

