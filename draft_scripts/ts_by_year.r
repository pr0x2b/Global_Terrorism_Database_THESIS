
#------------------------------------------------------
# Predicting number of attacks by countries and year
#-----------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, knitr, pryr, tictoc, lubridate, timetk, tidyquant, tidyr,
               zoo, forecast, tseries, TSstudio, imputeTS, thief, tidyverse)
options(warn = -1, digits = 4, scipen = 999)

# load and subset the dataset
dfcon <- df %>% select(year, country)

# create time series
dfts <- count(dfcon, year)
dfts <- filter(dfts, year >=1994) # year 1993 has no data
dfts <- zoo(dfts$n, order.by = dfts$year)

# Predictions 2014-2016 and Evaluation metrics
AR    <- accuracy(f = predict(ar(dfts[1:21]), n.ahead = 2)$pred, x = dfts[22:23])
MA    <- accuracy(f = predict(ma(dfts[1:21], order = 2), h = 2)$mean, x = dfts[22:23])
# NN    <- accuracy(f = predict(nnetar(dfts[1:21]), n.ahead = 2)$mean, x = dfts[22:23])
ARIMA <- accuracy(f = predict(auto.arima(dfts[1:21]), n.ahead = 2)$pred, x = dfts[22:23])
TBATS <- accuracy(f = predict(tbats(dfts[1:21]), n.ahead = 2)$mean, x = dfts[22:23])
ETS   <- accuracy(f = predict(ets(dfts[1:21])$fitted, h = 2)$mean, x = dfts[22:23])

metrics  <- rbind(as.data.frame(AR, row.names = "AR"),
                  as.data.frame(MA, row.names = "MA"),
                  # as.data.frame(NN, row.names = "NN"),
                  as.data.frame(ARIMA, row.names = "ARIMA"),
                  as.data.frame(TBATS, row.names = "TBATS"),
                  as.data.frame(ETS, row.names = "ETS"))


head(metrics)

# Selection of the most accurate function
index <- c("AR" = min(abs(AR)), 
           "MA" = min(abs(MA)), 
           # "NN" = min(abs(NN)),
           "ARIMA" = min(abs(ARIMA)), 
           "TBATS" = min(abs(TBATS)),
           "ETS" = min(abs(ETS)))

#comparison of best models
as.data.frame(index) %>% rownames_to_column(var = "method") %>% arrange(index) 

best_model <- names(index[index == min(index)]) # MA

# Fitted and Predicted values
AR <- ar(dfts)$resid + as.vector(dfts)
AR <- na.ma(AR, k = 4, weighting = "exponential") # replace nas with ma smoothing
MA <- ma(dfts, order = 2)
MA <- na.ma(MA, k = 4, weighting = "exponential") # replace nas with ma smoothing
ARIMA <- auto.arima(dfts, stationary = F, seasonal = F)$resid + as.vector(dfts)
# NN <- nnetar(dfts)$fitted + as.vector(dfts)
TBATS <- tbats(dfts)$fitted
ETS <- ets(dfts)$fitted

pred_vls <- data.frame("AR" = c(AR, predict(ar(dfts), n.ahead = 2)$pred),
                       "MA" = c(MA, predict(ma(dfts, order = 2), h = 2)$mean),
                       # "NN" = c(NN, predict(nnetar(dfts), h = 2)$mean),
                       "ARIMA" = c(ARIMA, predict(auto.arima(dfts, stationary = F, seasonal = F), n.ahead = 2)$pred),
                       "TBATS" = c(TBATS, predict(tbats(dfts)$fitted, h = 2)$mean),
                       "ETS" = c(ETS, predict(ets(dfts)$fitted, h = 2)$mean),
                       "Actual" = c(as.vector(dfts), NA, NA),
                       "Year" = c(1994:2018))

# Predicted future terrorist activity
pred_vls <- gather(pred_vls, method, value, - Year) %>%
  filter(method %in% c(best_model, "Actual")) %>%
  mutate(method = as.factor(method))

#### Plot "Predicted terrorist activity in the world" ###
ggplot(pred_vls, 
       aes(x = Year, y = value, colour = method))+
    geom_point(size = 2)+
    geom_line(size = 1)+
    ggtitle("Predicted terrorist activity in the world")+
    labs(x = "Year",
         y = "Terrorist attacks",
         colour = " ")+
    scale_x_continuous(breaks = c(1994:2018))+
    theme(legend.position = "right",
          legend.text = element_text(size = 14),
          axis.text.x = element_text(size = 14, angle = 90), 
          axis.title.x = element_text(size = 14),
          axis.text.y = element_text(size = 14), 
          axis.title.y = element_text(size = 14),
          title = element_text(size = 16)) 


#### Predicted future terrorist activity by countries ####

fc <- data.frame("country" = character(), 
                 "2017" = integer(), 
                 "2018" = integer(), 
                 "method" = character())

country_preds <- function(country_name){

    country <- filter(dfcon, country %in% country_name)
    
    # Create time series
    country <- count(country, year)
    country <- filter(country, year >=1994)
    
    # If the length of time series is not enough for prediction then skip this country
    if(length(country$year) < 5){
        next
    }
    
    # Add years with zero values
    year <- c(1994:2016)[!c(1994:2016) %in% country$year]
    if(length(year) != 0){
        country <- rbind(country, data.frame("year" = year, "n" = 0))
    }
    
    # Create time series
    country <- zoo(country$n, order.by = country$year)
    
    # Predictions 2014-2015 and Evaluation metrics
    AR <- accuracy(f = predict(ar(country[1:21], aic = F), n.ahead = 2)$pred, x = country[22:23])
    AR <- replace(AR, is.infinite(AR), NA)
    MA <- accuracy(f = predict(ma(country[1:21], order = 2), h = 2)$mean, x = country[22:23])
    MA <- replace(MA, is.infinite(MA), NA)
    ARIMA <- accuracy(f = predict(auto.arima(country[1:21], stationary = F, seasonal = F, allowdrift = F), n.ahead = 2)$pred, x = country[22:23])
    ARIMA <- replace(ARIMA, is.infinite(ARIMA), NA)
    ETS <- accuracy(f = predict(ets(country[1:21])$fitted, h = 2)$mean, x = country[22:23])
    ETS <- replace(ETS, is.infinite(ETS), NA)
    
    # Selection of the most accurate function
    index <- c("AR" = min(abs(AR), na.rm = T), 
               "MA" = min(abs(MA), na.rm = T), 
               "ARIMA" = min(abs(ARIMA), na.rm = T), 
               "ETS" = min(abs(ETS), na.rm = T))
    index <- names(index[index == min(index, na.rm = T)])
    
    # Fitted and Predicted values
    AR <- ar(country)$resid + as.vector(country)
    MA <- ma(country, order = 2)
    ARIMA <- auto.arima(country, stationary = F, seasonal = F, allowdrift = F)$resid + as.vector(country)
    ETS <- ets(country)$fitted
    pred_vls <- data.frame("AR" = c(AR, predict(ar(country), n.ahead = 2)$pred),
                           "MA" = c(MA, predict(ma(country, order = 2), h = 2)$mean),
                           "ARIMA" = c(ARIMA, predict(auto.arima(country, stationary = F, seasonal = F, allowdrift = F), n.ahead = 2)$pred),
                           "ETS" = c(ETS, predict(ets(country)$fitted, h = 2)$mean),
                           "Real data" = c(as.vector(country), NA, NA),
                           "Year" = c(1994:2018))
    
    # Predicted future terrorist activity by countries
    pred_vls <- gather(pred_vls, method, value, - Year)
    pred_vls <- filter(pred_vls, method == index)
    fc2 <- data.frame("country" = country_name, 
                      "X2017" = pred_vls$value[pred_vls$Year == 2017],
                      "X2018" = pred_vls$value[pred_vls$Year == 2018],
                      "method" = index)
    # fc <- rbind(fc, fc2)
    # rm(fc2)
}

fc2 <- lapply(dfcon$country, country_preds)


actual_data <- function(country_name){
  
  #extract actual attack counts
  rc <- data.frame("country" = character(), "2016" = integer())
  country <- filter(dfcon, country %in% country_name, year == 2016)
  country <- count(country, year)
  rc <- rbind(rc, data.frame("country" = country_name, "X2016" = country$n))
  return(rc)

}

rc <- actual_data(country_name = "Iraq")

# Join Fitted-Predicted dataset and Real data
fc <- inner_join(fc, rc)
fc <- fc[c("country", "X2016", "X2017", "X2018", "method")]
fc$X2017 <- round(fc$X2017, digits = 0)
fc$X2018 <- round(fc$X2018, digits = 0)
fc <- arrange(fc, desc(X2018))
colnames(fc) <- c("Country", "2016", "2017", "2018", "Method")
fc$`2017`[fc$`2017` < 0] <- 0
fc$`2018`[fc$`2018` < 0] <- 0


# Real data by countries
rc <- data.frame("country" = character(), "2016" = integer())

act_preds <- function(country_name){
    country <- filter(dfcon, country %in% country_name, year == 2016)
    country <- count(country, year)
    rc <- rbind(rc, data.frame("country" = "Iraq", "X2016" = country$n))
    
    # if(length(country$n) == 0){
    #     rc <- rbind(rc, data.frame("country" = country_name, "X2016" = 0))
    # }
    # else{
    #     rc <- rbind(rc, data.frame("country" = country_name, "X2016" = country$n))
    # }
}


# Join Fitted-Predicted dataset and Real data
fc <- inner_join(fc, rc)
fc <- fc[c("country", "X2016", "X2017", "X2018", "method")]
fc$X2017 <- round(fc$X2017, digits = 0)
fc$X2018 <- round(fc$X2018, digits = 0)
fc <- arrange(fc, desc(X2018))
colnames(fc) <- c("Country", "2016", "2017", "2018", "Method")
fc$`2017`[fc$`2017` < 0] <- 0
fc$`2018`[fc$`2018` < 0] <- 0

#### Table "Top-10 countries by predicted terrorist attacks in 2018" ####
fc[1:10,]

#### "The most frequency predictive methods" ####
method <- data.frame(summary(fc$Method))
method$Method <- row.names(method)
method$Percents <- method$summary.fc.Method./sum(method$summary.fc.Method.)*100
method$Percents <- round(method$Percents, digits = 0)
method <- arrange(method, desc(Percents))

