#-------------------------------Libraries--------------------
library(dplyr)
library(tidyverse)
library(tidyr)
library(forecast)
library(lubridate)
library(zoo)
library(ggplot2)
options(scipen = 999)
options(stringsAsFactors = TRUE)

#-------------------------Work Director----------------------
setwd("C:/Users/truon/Documents/BAN673/Walmart")

#-------------------------Read CSV File----------------------
WM_Calendar<- read.csv("calendar.csv")
WM_Evaluation<- read.csv("sales_train_evaluation.csv")
WM_Price<- read.csv("sell_prices.csv")

#-----------------Subsetting Each State from Sell_Price csv------------
#CA
Price_CA <- WM_Price[grep("^CA", WM_Price$store_id),]
Price_Food_CA <- Price_CA[grep("FOODS", Price_CA$item_id),]
#TX
Price_TX <- WM_Price[grep("^TX", WM_Price$store_id),]
Price_Food_TX <- Price_TX[grep("FOODS", Price_TX$item_id),]
#WI
Price_WI <- WM_Price[grep("^WI", WM_Price$store_id),]
Price_Food_WI <- Price_WI[grep("FOODS", Price_WI$item_id),]

#----------------------Subsetting Each State Food from Eval CSV------------
#CA
Eval_CA <- WM_Evaluation[grep("^CA", WM_Evaluation$store_id),]
Eval_Food_CA <- Eval_CA[grep("^FOODS", Eval_CA$item_id),]
#TX
Eval_TX <- WM_Evaluation[grep("^TX", WM_Evaluation$store_id),]
Eval_Food_TX <- Eval_TX[grep("^FOODS", Eval_TX$item_id),]
#WI
Eval_WI <- WM_Evaluation[grep("^WI", WM_Evaluation$store_id),]
Eval_Food_WI <- Eval_WI[grep("^FOODS", Eval_WI$item_id),]


#----------------------Pivot days each State--------
#CA
pivot_Eval_Food_CA <- pivot_longer(Eval_Food_CA,
                                   cols = starts_with("d_"),
                                   names_to = 'd',
                                   values_to = 'quantity',
                                   values_drop_na = TRUE)
#TX
pivot_Eval_Food_TX <- pivot_longer(Eval_Food_TX,
                                   cols = starts_with("d_"),
                                   names_to = 'd',
                                   values_to = 'quantity',
                                   values_drop_na = TRUE)
#WI
pivot_Eval_Food_WI <- pivot_longer(Eval_Food_WI,
                                   cols = starts_with("d_"),
                                   names_to = 'd',
                                   values_to = 'quantity',
                                   values_drop_na = TRUE)

#---------------------------Merge pivot df to calendar----------------------
#CA
CA_Food_Merge <- merge(pivot_Eval_Food_CA, WM_Calendar, by = 'd', all.x =  TRUE)
#TX
TX_Food_Merge <- merge(pivot_Eval_Food_TX, WM_Calendar, by = 'd', all.x =  TRUE)
#WI
WI_Food_Merge <- merge(pivot_Eval_Food_WI, WM_Calendar, by = 'd', all.x =  TRUE)




#---------------Merge each state to Price Df-------------------------------
#CA
Full_Merge_CA <- merge(CA_Food_Merge, Price_Food_CA, by= c('store_id', 'item_id',
                                                       'wm_yr_wk'), all.x = TRUE)
#TX
Full_Merge_TX <- merge(TX_Food_Merge, Price_Food_TX, by= c('store_id', 'item_id',
                                                        'wm_yr_wk'), all.x = TRUE)
#WI
Full_Merge_WI <- merge(WI_Food_Merge, Price_Food_WI, by= c('store_id', 'item_id',
                                                        'wm_yr_wk'), all.x = TRUE)
#------------------------Removing NA from each State DF------------------------
#CA
Full_Merge_CA[is.na(Full_Merge_CA)] <- 0
sum(is.na(Full_Merge_CA))
#TX
Full_Merge_TX[is.na(Full_Merge_TX)] <- 0
sum(is.na(Full_Merge_TX))
#WI
Full_Merge_WI[is.na(Full_Merge_WI)] <- 0
sum(is.na(Full_Merge_WI))


#-----------------------------Arrange/Filter date------------------------------
#CA
Full_Merge_CA <- Full_Merge_CA %>% mutate(date=as.Date(date, format = "%Y-%m-%d"))
Arrange_date_CA <- Full_Merge_CA %>% arrange(date)
Filtered_date_CA <- Arrange_date_CA %>% filter(date >= ("2011-02-01") & date <= ("2016-04-30"))
#TX
Full_Merge_TX <- Full_Merge_TX %>% mutate(date=as.Date(date, format = "%Y-%m-%d"))
Arrange_date_TX <- Full_Merge_TX %>% arrange(date)
Filtered_date_TX <- Arrange_date_TX %>% filter(date >= ("2011-02-01") & date <= ("2016-04-30"))
#WI
Full_Merge_WI <- Full_Merge_WI %>% mutate(date=as.Date(date, format = "%Y-%m-%d"))
Arrange_date_WI <- Full_Merge_WI %>% arrange(date)
Filtered_date_WI <- Arrange_date_WI %>% filter(date >= ("2011-02-01") & date <= ("2016-04-30"))


#---------------------Adding a Total Sales Variable----------------------------
#CA
Filtered_date_CA$TotalSales <- (Filtered_date_CA$quantity*Filtered_date_CA$sell_price)
#TX
Filtered_date_TX$TotalSales <- (Filtered_date_TX$quantity*Filtered_date_TX$sell_price)
#WI
Filtered_date_WI$TotalSales <- (Filtered_date_WI$quantity*Filtered_date_WI$sell_price)

#---------------------Removing Certain Variables--------------------------------
#CA
CA_DF <- Filtered_date_CA[,c(1:16,22:23)]
#TX
TX_DF <- Filtered_date_TX[,c(1:16,22:23)]
#WI
WI_DF <- Filtered_date_WI[,c(1:16,22:23)]

#---------------------Writing to CSV file for each State-----------
#CA
write.csv(CA_DF, "C:/Users/truon/Documents/BAN673/Walmart/CA_DF.csv")
#TX
write.csv(TX_DF, "C:/Users/truon/Documents/BAN673/Walmart/TX_DF.csv")
#WI
write.csv(WI_DF, "C:/Users/truon/Documents/BAN673/Walmart/WI_DF.csv")

#-------------------------Group Total Sales by Day And Month--------------------
#CA
Sales_By_Day_CA <- CA_DF %>% 
  group_by(date) %>% 
  summarise(Daily_Sales = sum(TotalSales))
#TX
Sales_By_Day_TX <- TX_DF %>% 
  group_by(date) %>% 
  summarise(Daily_Sales = sum(TotalSales))
#WI
Sales_By_Day_WI <- WI_DF %>% 
  group_by(date) %>% 
  summarise(Daily_Sales = sum(TotalSales))

#---------------Group Total Sales by Month and export to CSV--------------------
#CA
Sales_By_Month_CA <- CA_DF %>% 
  group_by(month,year) %>% 
  summarise(Monthly_Sales = sum(TotalSales))
Sales_By_Month_CA <- Sales_By_Month_CA[order(Sales_By_Month_CA$year,
                                             Sales_By_Month_CA$month),]
write.csv(Sales_By_Month_CA,"C:/Users/truon/Documents/BAN673/Walmart/CA_Month.csv")

#TX
Sales_By_Month_TX <- TX_DF %>% 
  group_by(month,year) %>% 
  summarise(Monthly_Sales_TX = sum(TotalSales))
Sales_By_Month_TX <- Sales_By_Month_TX[order(Sales_By_Month_TX$year,
                                             Sales_By_Month_TX$month),]
write.csv(Sales_By_Month_TX,"C:/Users/truon/Documents/BAN673/Walmart/TX_day_store_Re_date.csv")

#WI
Sales_By_Month_WI <- WI_DF %>% 
  group_by(month,year) %>% 
  summarise(Monthly_Sales = sum(TotalSales))
Sales_By_Month_WI <- Sales_By_Month_WI[order(Sales_By_Month_WI$year,
                                             Sales_By_Month_WI$month),]
write.csv(Sales_By_Month_WI,"C:/Users/truon/Documents/BAN673/Walmart/WI_DF_by_month.csv")

#****Start from here if you have your state monthly Revenue--------------------
#CA
Sales_By_Month <- read.csv("CA_Month.csv")
#TX
Texas <- read.csv("TX_day_store_Re_date.csv", header = TRUE)
#WI
food_WI.data <- read.csv("WI_DF_by_month.csv")

####Note: Each state will run it's own nValid and nTrain
####Note: Run libraries if you need too line 1.





#-California ----------Analysis----------------
setwd("C:/Users/truon/Documents/BAN673/Walmart")
Sales_By_Month_CA <- read.csv("C:/Users/truon/Documents/BAN673/Walmart/CA_Month.csv")

#-----------------------------Time Series
TS_CA <- ts(Sales_By_Month_CA$Monthly_Sales, start = c(2011,2),
            end = c(2016,4), frequency = 12)
TS_CA
#-----------------Predictability 
diff_CA_TS<- diff(TS_CA, lag = 1)
Acf(diff_CA_TS, lag.max = 12,
    main = "Autocorrelation of First differencing for CA Walmart Food Revenue")
#--------------STL
CA_STL <- stl(TS_CA, s.window = 'periodic')
autoplot(CA_STL, main = "CA Time Series Components")

#------------------------Setting a validation
nValid <- 12
nTrain <- length(TS_CA) - nValid
train.ts <- window(TS_CA, start = c(2011, 2), end = c(2011, nTrain+1))
valid.ts <- window(TS_CA, start = c(2011, nTrain + 2),
                   end = c(2011, nTrain + nValid+1))

#-California ----------Regression ----------------
#-----------------Regression with Linear Trend
CA_LinearTrend <- tslm(train.ts ~ trend)
summary(CA_LinearTrend)
CA_LinearTrend_Pred <- forecast(CA_LinearTrend, h = nValid, level = 0)
CA_LinearTrend_Pred

#Regression with Quadratic Trend
CA_QuadTrend <- tslm(train.ts ~ trend + I(trend^2))
summary(CA_QuadTrend)
CA_QuadTrend_Pred <- forecast(CA_QuadTrend, h = nValid, level = 0)
CA_QuadTrend_Pred

#Regression With Seasonality
CA_Season <- tslm(train.ts ~ season)
summary(CA_Season)
CA_Season_Pred <- forecast(CA_Season, h = nValid, level = 0)
CA_Season_Pred

#Regression With Linear Trend and Seasonality
CA_LinearTrend_Season <- tslm(train.ts ~ trend + season)
summary(CA_LinearTrend_Season)
CA_LinearTrend_Season_Pred <- forecast(CA_LinearTrend_Season, h = nValid, level =0)
CA_LinearTrend_Season_Pred

#Regression with Quad Trend and Seasonality
CA_QuadTrend_Season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(CA_QuadTrend_Season)
CA_QuadTrend_Season_Pred <- forecast(CA_QuadTrend_Season, h = nValid, level =0)
CA_QuadTrend_Season_Pred

#Accuracy
round(accuracy(CA_LinearTrend_Pred, valid.ts),3) #Best One
round(accuracy(CA_QuadTrend_Pred, valid.ts), 3)
round(accuracy(CA_Season_Pred, valid.ts), 3)
round(accuracy(CA_LinearTrend_Season_Pred, valid.ts),3) #Best One
round(accuracy(CA_QuadTrend_Season_Pred, valid.ts), 3)

#-California ----------Two-Level Regression with MA and AR---------------
##Trailing MA
CA_DS_LTS.Res <- CA_LinearTrend_Season$residuals
CA_DS_LTS.MA.1 <- rollmean(CA_DS_LTS.Res, k = 1, align = 'right')
CA_DS_LTS.MA.2 <- rollmean(CA_DS_LTS.Res, k = 2, align = 'right')
CA_DS_LTS.MA.3 <- rollmean(CA_DS_LTS.Res, k = 3, align = 'right')
CA_DS_LTS.MA.4 <- rollmean(CA_DS_LTS.Res, k = 4, align = 'right')
CA_DS_LTS.MA.5 <- rollmean(CA_DS_LTS.Res, k = 5, align = 'right')
CA_DS_LTS.MA.6 <- rollmean(CA_DS_LTS.Res, k = 6, align = 'right')
CA_DS_LTS.MA.7 <- rollmean(CA_DS_LTS.Res, k = 7, align = 'right')
CA_DS_LTS.MA.8 <- rollmean(CA_DS_LTS.Res, k = 8, align = 'right')
CA_DS_LTS.MA.9 <- rollmean(CA_DS_LTS.Res, k = 9, align = 'right')
CA_DS_LTS.MA.10 <- rollmean(CA_DS_LTS.Res, k = 10, align = 'right')
CA_DS_LTS.MA.11 <- rollmean(CA_DS_LTS.Res, k = 11, align = 'right')
CA_DS_LTS.MA.12 <- rollmean(CA_DS_LTS.Res, k = 12, align = 'right')


#Forecast
CA_DS_LTS.MA.1_Pred <- forecast(CA_DS_LTS.MA.1, h = 12, level = 0)
CA_DS_LTS.MA.2_Pred <- forecast(CA_DS_LTS.MA.2, h = 12, level = 0)
CA_DS_LTS.MA.3_Pred <- forecast(CA_DS_LTS.MA.3, h = 12, level = 0)
CA_DS_LTS.MA.4_Pred <- forecast(CA_DS_LTS.MA.4, h = 12, level = 0)
CA_DS_LTS.MA.5_Pred <- forecast(CA_DS_LTS.MA.5, h = 12, level = 0)
CA_DS_LTS.MA.6_Pred <- forecast(CA_DS_LTS.MA.6, h = 12, level = 0)
CA_DS_LTS.MA.7_Pred <- forecast(CA_DS_LTS.MA.7, h = 12, level = 0)
CA_DS_LTS.MA.8_Pred <- forecast(CA_DS_LTS.MA.8, h = 12, level = 0)
CA_DS_LTS.MA.9_Pred <- forecast(CA_DS_LTS.MA.9, h = 12, level = 0)
CA_DS_LTS.MA.10_Pred <- forecast(CA_DS_LTS.MA.10, h = 12, level = 0)
CA_DS_LTS.MA.11_Pred <- forecast(CA_DS_LTS.MA.11, h = 12, level = 0)
CA_DS_LTS.MA.12_Pred <- forecast(CA_DS_LTS.MA.12, h = 12, level = 0)

#Combined Regression Forecast
CA_TS_Forecast_LTS_1 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.1_Pred$mean
CA_TS_Forecast_LTS_2 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.2_Pred$mean
CA_TS_Forecast_LTS_3 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.3_Pred$mean
CA_TS_Forecast_LTS_4 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.4_Pred$mean
CA_TS_Forecast_LTS_5 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.5_Pred$mean
CA_TS_Forecast_LTS_6 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.6_Pred$mean
CA_TS_Forecast_LTS_7 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.7_Pred$mean
CA_TS_Forecast_LTS_8 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.8_Pred$mean
CA_TS_Forecast_LTS_9 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.9_Pred$mean
CA_TS_Forecast_LTS_10 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.10_Pred$mean
CA_TS_Forecast_LTS_11 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.11_Pred$mean
CA_TS_Forecast_LTS_12 <- CA_LinearTrend_Season_Pred$mean + CA_DS_LTS.MA.12_Pred$mean

CA_TS_Fitted.1 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.1_Pred$fitted
CA_TS_Fitted.2 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.2_Pred$fitted
CA_TS_Fitted.3 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.3_Pred$fitted
CA_TS_Fitted.4 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.4_Pred$fitted
CA_TS_Fitted.5 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.5_Pred$fitted
CA_TS_Fitted.6 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.6_Pred$fitted
CA_TS_Fitted.7 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.7_Pred$fitted
CA_TS_Fitted.8 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.8_Pred$fitted
CA_TS_Fitted.9 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.9_Pred$fitted
CA_TS_Fitted.10 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.10_Pred$fitted
CA_TS_Fitted.11 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.11_Pred$fitted
CA_TS_Fitted.12 <- CA_LinearTrend_Season_Pred$fitted + CA_DS_LTS.MA.12_Pred$fitted

#-----------Accuracy Linear Trend with Seasonality and Moving Average
LTS_MA.1<- round(accuracy(CA_TS_Forecast_LTS_1, valid.ts),3)
LTS_MA.2<- round(accuracy(CA_TS_Forecast_LTS_2, valid.ts),3)
LTS_MA.3<- round(accuracy(CA_TS_Forecast_LTS_3, valid.ts),3)
LTS_MA.4<- round(accuracy(CA_TS_Forecast_LTS_4, valid.ts),3)
LTS_MA.5<- round(accuracy(CA_TS_Forecast_LTS_5, valid.ts),3)
LTS_MA.6<- round(accuracy(CA_TS_Forecast_LTS_6, valid.ts),3)
LTS_MA.7<- round(accuracy(CA_TS_Forecast_LTS_7, valid.ts),3)
LTS_MA.8<- round(accuracy(CA_TS_Forecast_LTS_8, valid.ts),3)
LTS_MA.9<- round(accuracy(CA_TS_Forecast_LTS_9, valid.ts),3)
LTS_MA.10<- round(accuracy(CA_TS_Forecast_LTS_10, valid.ts),3)
LTS_MA.11<- round(accuracy(CA_TS_Forecast_LTS_11, valid.ts),3)
LTS_MA.12<- round(accuracy(CA_TS_Forecast_LTS_12, valid.ts),3)

LTS_MA.1
LTS_MA.2 
LTS_MA.3
LTS_MA.4 
LTS_MA.5
LTS_MA.6
LTS_MA.7
LTS_MA.8
LTS_MA.9 #Best one
LTS_MA.10
LTS_MA.11
LTS_MA.12

##AR
Acf(CA_LinearTrend_Season_Pred$residuals, lag.max = 12,
    main = 'Linear Trend and Seasonality Train Residuals')
Acf(CA_LinearTrend_Season_Pred$mean, lag.max = 12)

LTS_Res_AR1 <- Arima(CA_LinearTrend_Season$residuals, order = c(1,0,0))
summary(LTS_Res_AR1)
Acf(LTS_Res_AR1$residuals, lag.max = 12)
LTS_Res_AR1_Pred  <- forecast(LTS_Res_AR1, h = nValid, level = 0)

Acf(LTS_Res_AR1$residuals, lag.max = 12,
    main = 'AR1 for CA food training Residuals of Residuals')

#AR1 model for residual
LTS_AR1_Pred <- CA_LinearTrend_Season_Pred$mean+LTS_Res_AR1_Pred$mean
LinearTrendSeason_AR1<- round(accuracy(LTS_AR1_Pred, valid.ts),3)

#Accuracy -Best one
LinearTrendSeason_AR1
LTS_MA.9









#-California ----------ARIMA-----------------------
#ARIMA(1,2,2)(1,2,2)
CA_Food_AR12 <- Arima(train.ts, order = c(1,2,2), seasonal = c(1,2,2))
summary(CA_Food_AR12)
CA_Food_AR12_Pred <- forecast(CA_Food_AR12, h = nValid, level = 0)
CA_Food_AR12_Pred
Acf(CA_Food_AR12_Pred$residuals, lag.max = 12)
AR122_122 <- round(accuracy(CA_Food_AR12_Pred, valid.ts), 3)

#ARIMA(2,1,2)(2,1,2)
CA_Food_AR11 <- arima(train.ts, order = c(2,1,2), seasonal = c(2,1,2))
summary(CA_Food_AR11)
CA_Food_AR11_Pred <- forecast(CA_Food_AR11, h = nValid, level = 0)
CA_Food_AR11_Pred
AR212_212<- round(accuracy(CA_Food_AR11_Pred, valid.ts), 3)
Acf(CA_Food_AR11_Pred$residuals, lag.max = 12)

#ARIMA(2,1,1)(2,1,1)
CA_Food_AR10 <- arima(train.ts, order = c(2,1,1), seasonal = c(2,1,1))
summary(CA_Food_AR10)
CA_Food_AR10_Pred <- forecast(CA_Food_AR10, h = nValid, level = 0)
CA_Food_AR10_Pred
AR211_211 <- round(accuracy(CA_Food_AR10_Pred, valid.ts), 3)
Acf(CA_Food_AR10_Pred$residuals, lag.max = 12)

#Auto ARIMA
CA_Auto_ARIMA <- auto.arima(train.ts)
summary(CA_Auto_ARIMA)
CA_Auto_ARIMA_Pred <- forecast(CA_Auto_ARIMA, h = nValid, level = 0)
CA_Auto_ARIMA_Pred
AutoAR <- round(accuracy(CA_Auto_ARIMA_Pred,valid.ts),3)
Acf(CA_Auto_ARIMA$residuals, lag.max = 12)

#Accuracy for ARIMA models
AR122_122
AR211_211
AR212_212
AutoAR

#-California ----------Holt-Winter's Model--------------
#-------------------------Simple Exponential Smoothing
simple_Exp_CA<- ets(train.ts, model ='ANN', alpha = .2)
simple_Exp_CA
pred_simple_Exp_CA<- forecast(simple_Exp_CA, h = nValid, level =0)
pred_simple_Exp_CA

#-Holt's Exp Smoothing
Holt_Exp_CA <- ets(train.ts, model = 'AAN', alpha = .1, beta = .1)
Holt_Exp_CA
pred_Holt_Exp_CA<- forecast(Holt_Exp_CA, h = nValid, level = 0)
pred_Holt_Exp_CA

#-Holt's Winter Model Automated Error
HW_ZZZ_CA <- ets(train.ts, model ='ZZZ')
HW_ZZZ_CA
pred_HW_ZZZ_CA <- forecast(HW_ZZZ_CA,  h = nValid, level = 0)
pred_HW_ZZZ_CA

#-MAM
HW_MAM <- ets(train.ts, model ='MAM')
HW_MAM
pred_HW_MAM <- forecast(HW_MAM,  h = nValid, level = 0)
pred_HW_MAM

#-MAA
HW_MAA <- ets(train.ts, model ='MAA')
HW_MAA
pred_HW_MAA <- forecast(HW_MAA,  h = nValid, level = 0)
pred_HW_MAA

#-MMM
HW_MMM <- ets(train.ts, model ='MMM')
HW_MMM
pred_HW_MMM <- forecast(HW_MMM,  h = nValid, level = 0)
pred_HW_MMM

#-AAA
HW_AAA <- ets(train.ts, model ='AAA')
HW_AAA
pred_HW_AAA <- forecast(HW_AAA,  h = nValid, level = 0)
pred_HW_AAA


#-ANN
HW_ANN <- ets(train.ts, model ='ANN')
HW_ANN
pred_HW_ANN <- forecast(HW_ANN,  h = nValid, level = 0)
pred_HW_ANN

#Accuracy
round(accuracy(pred_simple_Exp_CA, valid.ts),3)
round(accuracy(pred_Holt_Exp_CA, valid.ts),3)
round(accuracy(pred_HW_ZZZ_CA, valid.ts),3)
round(accuracy(pred_HW_MAM, valid.ts),3)
round(accuracy(pred_HW_MAA, valid.ts),3)
round(accuracy(pred_HW_MMM, valid.ts),3)
round(accuracy(pred_HW_AAA, valid.ts),3)



#-California ----------Best Models With Entire Data Set-------------
#-Setting a training and Validation with Entire Data Set
nValid <- 12
nTrain <- length(TS_CA) - nValid
train.ts <- window(TS_CA, start = c(2011, 2), end = c(2011, nTrain+1))
valid.ts <- window(TS_CA, start = c(2011, nTrain + 2),
                   end = c(2011, nTrain + nValid+1))

train.ts
valid.ts
#----------Regression With Linear Trend and Seasonality
CA_DS_LinearTrendSeason <- tslm(TS_CA ~ trend + season)
summary(CA_DS_LinearTrendSeason)
CA_DS_LinearTrendSeason_Pred <- forecast(CA_DS_LinearTrendSeason, h = 12, level =0)
CA_DS_LinearTrendSeason_Pred
LinearTrendSeason <- round(accuracy(CA_DS_LinearTrendSeason_Pred$fitted, TS_CA), 3)
#AR1
CA_DS_LTS_AR1 <- Arima(CA_DS_LinearTrendSeason$residuals, order = c(1,0,0))
summary(CA_DS_LTS_AR1)
Acf(CA_DS_LTS_AR1$residuals, lag.max = 12,
    main = "Autocorrelation CA food LTS with AR[1] Residuals")
#Forecasting LTS_AR1
CA_DS_Residual_AR1_Pred <- forecast(CA_DS_LTS_AR1, h =12, level =0)
summary(CA_DS_Residual_AR1_Pred)
LinearTrendSeason_AR1<- round(accuracy(CA_DS_LinearTrendSeason$fitted
                                       + CA_DS_LTS_AR1$fitted, TS_CA),3)
#Accuracy
LinearTrendSeason
LinearTrendSeason_AR1

#-Holt Winter's Model
#-MAA
HW_MAA <- ets(TS_CA, model ='MAA')
HW_MAA
pred_HW_MAA <- forecast(HW_MAA,  h = 12, level = 0)
pred_HW_MAA
MAA <- round(accuracy(pred_HW_MAA$fitted, TS_CA),3)

#-AAA
HW_AAA <- ets(TS_CA, model ='AAA')
HW_AAA
pred_HW_AAA <- forecast(HW_AAA,  h = 12, level = 0)
pred_HW_AAA
AAA <- round(accuracy(pred_HW_AAA$fitted , TS_CA),3)

#Accuracy
MAA
AAA

#-ARIMA
#(2,1,2)(2,1,2)
CA_Food_AR11 <- arima(TS_CA, order = c(2,1,2), seasonal = c(2,1,2))
summary(CA_Food_AR11)
CA_Food_AR11_Pred <- forecast(CA_Food_AR11, h = 12, level = 0)
CA_Food_AR11_Pred
Acf(CA_Food_AR11_Pred$residuals, lag.max = 12)
Arima212<- round(accuracy(CA_Food_AR11_Pred$fitted, TS_CA), 3)

#(1,2,2)(1,2,2)
CA_Food_AR10 <- arima(TS_CA, order = c(1,2,2), seasonal = c(1,2,2))
summary(CA_Food_AR10)
CA_Food_AR10_Pred <- forecast(CA_Food_AR10, h = 12, level = 0)
CA_Food_AR10_Pred
Acf(CA_Food_AR10_Pred$residuals, lag.max = 12)
Arima122 <- round(accuracy(CA_Food_AR10_Pred$fitted, TS_CA), 3)

#Accuracy
Arima122
Arima212

#-----------Accuracy for best model
LinearTrendSeason
LinearTrendSeason_AR1
MAA
AAA
Arima122
Arima212
round(accuracy((snaive(TS_CA))$fitted, TS_CA), 3)
round(accuracy((naive(TS_CA))$fitted, TS_CA), 3)


#------Plotting best mode-
#Plot
plot(CA_Food_AR11_Pred,
     xlab = "Years", ylab = "Revenue($)", main = "ARIMA(212)(212) for Entire Dataset",
     ylim = c(500000,1200000), xlim = c(2011,2017.20), lwd = 2)
lines(CA_Food_AR11_Pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2011,1150000, legend = c("CA Food Time Series", 
                                "ARIMA Forecast for Training", 
                                "ARIMA Forecast for Validation"), 
       col = c("black", "blue" , "light blue"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")
###Arrows for plot
lines(c(2016.3, 2016.3), c(500000,1200000)) #Vertical line
text(2013.5, 1180000, "Training")
text(2016.75, 1180000, "Future")
arrows(2016.5 - 0.25, 1150000, 2011, 1150000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.35, 1150000, 2017.25, 1150000, code = 3, length = 0.1,
       lwd = 1, angle = 30)



#-Texas ----------Analysis--------
setwd("C:/Users/STSC/Documents/Semester3/Time Series/Project/Data")
Texas <- read.csv("TX_day_store_Re_date.csv", header = TRUE)

#-Texas ----------Time Series
#Masters
Texas.ts <- ts(Texas$Monthly_Sales_TX, 
               start = c(2011,2), end = c(2016,4), freq = 12)

Texas.ts

#Plot TimeSeries
plot(Texas.ts, xlab="Monthly data",ylab="Revenue",
     main="Monthly revenue for Texas from 2011-02 to 2016-04", col='blue')

#Acf for TX
Acf(Texas.ts, lag.max = 12, main = "Autocorrelation of TX  Walmart Food Revenue")
# This graph shows that there is a significant autocorrelation in the data
# test for predictability 

Acf(diff(Texas.ts, lag=1),lag.max=12,main='ACF Plot for 1 lag difference for Texas Revenue')
# this shows that data is not random and there is lag 12 shows seasonality 

#STL
texas.stl <- stl(Texas.ts, s.window = 'periodic')
autoplot(texas.stl, main = "Texas Time Series Components")
texas.stl
# we can see upward linear trend with additive seasonality in the data 


#-Texas ----------Partitioning Texas Data---------------
# partition of data 

length(Texas.ts)
nValid <- 12
nTrain <- length(Texas.ts) - nValid
train.ts <- window(Texas.ts, start = c(2011, 2), end = c(2011, nTrain+1))
valid.ts <- window(Texas.ts, start = c(2011, nTrain + 2), 
                   end = c(2011, nTrain + nValid+1))
valid.ts
train.ts

#-Texas ----------Trailing MA-----------------------------
ma.trailing.4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing.4.pred <- forecast(ma.trailing.4, h = 12)
ma.trailing.3 <- rollmean(train.ts, k = 3, align = "right")
ma.trailing.3.pred <- forecast(ma.trailing.3, h = 12)
ma.trailing.2 <- rollmean(train.ts, k = 2, align = "right")
ma.trailing.2.pred <- forecast(ma.trailing.2, h = 12)


round(accuracy(ma.trailing.4.pred, valid.ts), 3)
round(accuracy(ma.trailing.3.pred, valid.ts), 3) # best MA 
round(accuracy(ma.trailing.2.pred, valid.ts), 3)


#-Texas ----------Winter Holts Model----------------------
winter_holt<- ets(train.ts, model = "ZZZ") #  this shows there is (Multiplicative) trend, 
#(Additive damped) seasonality with Multiplicative  error 
winter_holt
pred_winter <- forecast(winter_holt, h = nValid, level = 0)
pred_winter

winter_holt1<- ets(train.ts, model = "MAN")
summary(winter_holt1)
pred_winter1 <- forecast(winter_holt1, h = nValid, level = 0)


winter_holt2<- ets(train.ts, model = "AAA") 
summary(winter_holt2)
pred_winter2 <- forecast(winter_holt2, h = nValid, level = 0)


round(accuracy(pred_winter, valid.ts), 3)
round(accuracy(pred_winter1, valid.ts), 3)
round(accuracy(pred_winter2, valid.ts), 3)


Acf(winter_holt$residuals, lag.max = 12, main = "Autocorrelations of Auto Winter Holt's Model For training Residuals")
# there are some residual left so we can use AR model for residuals 

Acf(winter_holt1$residuals, lag.max = 12, main = "Autocorrelations of Holt Winter's Model - MAN for training Residuals")
Acf(valid.ts - pred_winter2$mean, lag.max = 12, main = "Autocorrelations of Holt Winter's Model - MAN for Validation Residuals")


Acf(winter_holt2$residuals, lag.max = 12, main = "Autocorrelations of AAA winter Holt's Model For training Residuals")
# there is not significant auto correlation left, so we can try trailing MA for residuals 

#-Texas ----------Two-Level Winter Holt's with MA--------------
ma.trailing.res1 <- rollmean(winter_holt$residuals, k = 3, align = "right")
ma.trailing.res1.pred <- forecast(ma.trailing.res1, h = 12, level = 95)
ts.forecast.1 <- pred_winter$mean + ma.trailing.res1.pred$mean

ma.trailing.res2 <- rollmean(winter_holt2$residuals, k = 3, align = "right")
ma.trailing.res2.pred <- forecast(ma.trailing.res2, h = 12, level = 95)
ts.forecast.2 <- pred_winter2$mean + ma.trailing.res2.pred$mean

ma.trailing.res5 <- rollmean(winter_holt1$residuals, k = 3, align = "right") # Best MAN model
ma.trailing.res5.pred <- forecast(ma.trailing.res5, h = 12, level = 95)
ts.forecast.5 <- pred_winter1$mean + ma.trailing.res5.pred$mean


#-Texas ----------Two-Level Winter Holt's with AR-------------------
# Two level with AR
res.ar1 <- Arima(winter_holt$residuals, order = c(6,0,0))
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
ts.forecast.3 <- pred_winter$mean + res.ar1.pred$mean

res.ar2 <- Arima(winter_holt2$residuals, order = c(6,0,0))
summary(res.ar2)
res.ar2.pred <- forecast(res.ar2, h = nValid, level = 95)
ts.forecast.4 <- pred_winter2$mean + res.ar2.pred$mean


res.ar3 <- Arima(winter_holt1$residuals, order = c(6,0,0))
summary(res.ar1)
res.ar3.pred <- forecast(res.ar3, h = nValid, level = 95)
ts.forecast.6 <- pred_winter1$mean + res.ar3.pred$mean


# Acf plot of residuals 
Acf(res.ar2$residuals, lag.max = 12, main = "Training Residuals of residuals of AAA model ")
Acf(valid.ts - res.ar2.pred$mean, lag.max = 12, main = "Validation Residuals of residuals of AAA model ")

Acf(res.ar1$residuals, lag.max = 12, main = "Training Residuals of residuals of Auto holt's winter model")
Acf(res.ar3$residuals, lag.max = 12, main = "Training Residuals of residuals of MAN model")
Acf(valid.ts- res.ar3.pred$mean, lag.max = 12, main = "Validation Residuals of residuals of MAN model")

# Accuracy 
round(accuracy(ts.forecast.1, valid.ts), 3)
round(accuracy(ts.forecast.2, valid.ts), 3) # Best model with auto holts winter with MA residuals 
round(accuracy(ts.forecast.5, valid.ts), 3) # Best MAN with MA

round(accuracy(ts.forecast.3, valid.ts), 3)
round(accuracy(ts.forecast.4, valid.ts), 3)
round(accuracy(ts.forecast.6, valid.ts), 3) # Best MAN with AR

#-Texas ----------Applying WH and MA TO Entire Data set--------------
winter_holt.e<- ets(Texas.ts, model = "ZZZ")
pred_winter.e <- forecast(winter_holt.e, h = 12, level = 0)

winter_holt1.e<- ets(Texas.ts, model = "MAN") 
pred_winter1.e <- forecast(winter_holt1.e, h = 12, level = 0)

winter_holt2.e<- ets(Texas.ts, model = "AAA") 
pred_winter2.e <- forecast(winter_holt2.e, h = 12, level = 0)


ma.trailing.res3 <- rollmean(winter_holt.e$residuals, k = 3, align = "right")
ma.trailing.res3.pred <- forecast(ma.trailing.res3, h = 12, level = 95)
ts.forecast.5 <- pred_winter.e$fitted + ma.trailing.res3.pred$fitted

ma.trailing.res4 <- rollmean(winter_holt2.e$residuals, k = 3, align = "right")
ma.trailing.res4.pred <- forecast(ma.trailing.res4, h = 12, level = 95)
ts.forecast.6 <- pred_winter2.e$fitted + ma.trailing.res4.pred$fitted


round(accuracy(pred_winter.e$fitted, Texas.ts), 3)
round(accuracy(pred_winter1.e$fitted, Texas.ts), 3)
round(accuracy(pred_winter2.e$fitted, Texas.ts), 3)
round(accuracy(ts.forecast.5, Texas.ts), 3) # best model Two level holts winter auto with MA for residuls 
round(accuracy(ts.forecast.6, Texas.ts), 3)
round(accuracy((naive(Texas.ts))$fitted, Texas.ts), 3) 
round(accuracy((snaive(Texas.ts))$fitted, Texas.ts), 3)

#-Texas ----------Regression---------------
#------------------- Regression model with linear trend
train.lin <- tslm(train.ts ~ trend)
summary(train.lin)
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

#-------------------  Regression mode with quadratic trend
train.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(train.quad)
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)

#-------------------  Regression model with seasonality
train.season <- tslm(train.ts ~ season)
summary(train.season)
train.season.pred <- forecast(train.season, h = nValid, level = 0)

#-------------------  Regression model with linear trend and seasonality
train.lin.trend.season <- tslm(train.ts ~ trend  + season)
summary(train.lin.trend.season)
train.lin.trend.season.pred <- forecast(train.lin.trend.season, h = nValid, level = 0)

#-------------------  Regression model with quadratic trend and seasonality.
train.quad.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.quad.trend.season)
train.quad.trend.season.pred <- forecast(train.quad.trend.season, h = nValid, level = 0)

#------------------- Accuracy 
round(accuracy(train.lin.pred, valid.ts), 3) # best accuracy 1
round(accuracy(train.quad.pred, valid.ts), 3)
#round(accuracy(train.season.pred, valid.ts), 3)
round(accuracy(train.lin.trend.season.pred, valid.ts), 3) # best accuracy 2
round(accuracy(train.quad.trend.season.pred, valid.ts), 3)

#------------------- AFC plot of residuals 
Acf(train.lin$residuals, lag.max = 12, main = "Autocorrelations of Reg model with linear trend For training Residuals")
Acf(train.quad$residuals, lag.max = 12, main = "Autocorrelations of Reg model with quad trend training Residuals")
Acf(train.season$residuals, lag.max = 12, main = "Autocorrelations of Reg model with  seasonality training Residuals")
Acf(train.lin.trend.season$residuals, lag.max = 12, main = "Autocorrelations of Reg model with Lin trend and seasonalityFor training Residuals")
Acf(train.quad.trend.season$residuals, lag.max = 12, main = "Autocorrelations of Reg model with quad trend and seasonality For train Residuals")



#-Texas ----------Two Level Forecasting Regression with MA and AR---------------
#MA
ma.trailing.res5 <- rollmean(train.lin$residuals, k = 3, align = "right")
ma.trailing.res5.pred <- forecast(ma.trailing.res5, h = nValid, level = 95)
ts.forecast.7 <- train.lin.pred$mean + ma.trailing.res5.pred$mean

ma.trailing.res6 <- rollmean(train.lin.trend.season$residuals, k = 3, align = "right")
ma.trailing.res6.pred <- forecast(ma.trailing.res6, h = nValid, level = 95)
ts.forecast.8 <- train.lin.trend.season.pred$mean + ma.trailing.res6.pred$mean

#AR
res.ar3 <- Arima(train.lin$residuals, order = c(1,0,0))
res.ar3.pred <- forecast(res.ar3, h = nValid, level = 95)
ts.forecast.9 <- train.lin.pred$mean + res.ar3.pred$mean
summary(res.ar3)

Acf(res.ar3$residuals, lag.max=12, main="Autocorrelation of Reg. Model with Linear trend for training Residuals of Residuals")

res.ar4 <- Arima(train.lin.trend.season$residuals, order = c(1,0,0))
res.ar4.pred <- forecast(res.ar4, h = nValid, level = 95)
ts.forecast.10 <- train.lin.trend.season.pred$mean + res.ar4.pred$mean

#Accuracy 
round(accuracy(ts.forecast.7, valid.ts), 3)
round(accuracy(ts.forecast.8, valid.ts), 3) 
round(accuracy(ts.forecast.9, valid.ts), 3) # best AR with linear trend 
round(accuracy(ts.forecast.10, valid.ts), 3)


#-Texas ----------Regression - Applying Entire Data set --------------------
#Linear Trend
lin.trend <- tslm(Texas.ts ~ trend)
summary(lin.trend)
lin.trend.pred <- forecast(lin.trend, h = 12, level = 0)

#Linear Trend with Seasonality
lin.trend.ses <- tslm(Texas.ts ~ trend +season)
summary(lin.trend.ses)
lin.trend.ses.pred <- forecast(lin.trend.ses, h = 12, level = 0)

#AR
res.ar5 <- Arima(lin.trend$residuals, order = c(1,0,0))
res.ar5.pred <- forecast(res.ar5, h = 12, level = 95)
ts.forecast.11 <- lin.trend.pred$fitted + res.ar5.pred$fitted

#MA
ma.trailing <- rollmean(lin.trend$residuals, k = 3, align = "right")
ma.trailing.pred <- forecast(ma.trailing, h = 12, level = 95)
ts.forecast.12 <- lin.trend.pred$fitted + ma.trailing.pred$fitted

#AR
res.ar6 <- Arima(lin.trend.ses$residuals, order = c(1,0,0))
res.ar6.pred <- forecast(res.ar6, h = 12, level = 95)
ts.forecast.13 <- lin.trend.ses.pred$fitted + res.ar6.pred$fitted

#MA
ma.trailing1 <- rollmean(lin.trend.ses$residuals, k = 3, align = "right")
ma.trailing.pred1 <- forecast(ma.trailing1, h = 12, level = 95)
ts.forecast.14 <- lin.trend.ses.pred$fitted + ma.trailing.pred1$fitted

#Accuracy 
round(accuracy(lin.trend.pred$fitted, Texas.ts), 3)
round(accuracy(lin.trend.ses.pred$fitted, Texas.ts), 3)
round(accuracy(ts.forecast.11, Texas.ts), 3)
round(accuracy(ts.forecast.12, Texas.ts), 3)
round(accuracy(ts.forecast.13, Texas.ts), 3)
round(accuracy(ts.forecast.14, Texas.ts), 3)


#-Texas ----------ARIMA MODEL----------------------------
#AR1
ar1 <- auto.arima(train.ts)
summary(ar1) # this model gives ARIMA(0,1,0)(1,1,0)[12] mean seasonality for 12 months 
ar1.pred <- forecast(ar1, h = nValid, level = 95)
ar1.pred
Acf(ar1$residuals, lag.max = 12, main = "Autocorrelations of Auto ARIMA Model Residuals")

#AR2
ar2 <- Arima(train.ts, order = c(1,1,1)) 
summary(ar2)
ar2.pred <- forecast(ar2, h = nValid, level = 95)

#AR3
ar3 <- Arima(train.ts, order = c(2,2,2)) 
ar3.pred <- forecast(ar3, h = nValid, level = 95)

#AR4
ar4 <- Arima(train.ts, order = c(2,1,2),seasonal = c(1,1,2)) 
summary(ar4)
ar4.pred <- forecast(ar4, h = nValid, level = 95)

#AR41
ar41 <- Arima(train.ts, order = c(1,1,2),seasonal = c(2,1,2)) 
ar41.pred <- forecast(ar41, h = nValid, level = 95)

# Accuracy 
round(accuracy(ar1.pred, valid.ts), 3)
round(accuracy(ar2.pred, valid.ts), 3)
round(accuracy(ar3.pred, valid.ts), 3)
round(accuracy(ar4.pred, valid.ts), 3)
round(accuracy(ar41.pred, valid.ts), 3)

#-Texas ----------ARIMA- Applying to Entire Data set---------------------------
#AR5
ar5 <- auto.arima(Texas.ts)
summary(ar5) 
ar5.pred <- forecast(ar5, h = 12, level = 95)
ar5.pred

#AR6
ar6 <- Arima(Texas.ts, order = c(1,1,1)) 
summary(ar6)
ar6.pred <- forecast(ar6, h = 12, level = 95)

#AR7
ar7 <- Arima(Texas.ts, order = c(2,2,2)) 
ar7.pred <- forecast(ar7, h = 12, level = 95)

#AR8
ar8 <- Arima(Texas.ts, order = c(2,1,2),seasonal = c(1,1,2)) 
summary(ar8)
ar8.pred <- forecast(ar8, h = 12, level = 95)

#Accuracy
round(accuracy(ar5.pred$fitted, Texas.ts), 3)
round(accuracy(ar6.pred$fitted, Texas.ts), 3)
round(accuracy(ar7.pred$fitted, Texas.ts), 3)
round(accuracy(ar8.pred$fitted, Texas.ts), 3)

#-Texas ----------Forecast Plot of ARIMA Model(212)(112) for Entire Dataset-----
plot(Texas.ts, 
     xlab = "Time", ylab = "Revenue($)", ylim = c(250000, 1200000), bty = "l",
     xaxt = "n", xlim = c(2011, 2018), lwd = 2,
     main = "ARIMA Model(212)(112) for Entire Dataset") 
axis(1, at = seq(2011, 2018, 1), labels = format(seq(2011, 2018, 1)))
lines(ar8$fitted, col = "blue", lwd = 2)
lines(ar8.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2011,1000000, legend = c("Revenue Series", 
                                "ARIMA Forecast", 
                                "ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2016.3,2016.3), c(0, 1200000))
text(2013.5, 1150000, "Training")
text(2016.75, 1150000, "Future")
arrows(2016.3, 1100000, 2011, 1100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.3, 1100000, 2017.3, 1100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

### Second Best
plot(Texas.ts, 
     xlab = "Time", ylab = "Revenue($)", ylim = c(250000, 1200000), bty = "l",
     xaxt = "n", xlim = c(2011, 2018), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset") 
axis(1, at = seq(2011, 2018, 1), labels = format(seq(2011, 2018, 1)))
lines(ar8$fitted, col = "blue", lwd = 2)
lines(ar8.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2011,1000000, legend = c("Revenue Series", 
                                "ARIMA Forecast", 
                                "ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2016.3,2016.3), c(0, 1200000))
text(2013.5, 1150000, "Training")
text(2016.75, 1150000, "Future")
arrows(2016.3, 1100000, 2011, 1100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.3, 1100000, 2017.3, 1100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)









#-Wisconsin ----------Analysis-----------------------------
#Setting work directory
setwd("/Users/elise/Desktop/Class\ folder/BAN\ 673/Project")
food_WI.data <- read.csv("WI_DF_by_month.csv")

#-Wisconsin ----------Time Series Data---------------------------
food_WI.ts <- ts(food_WI.data$Monthly_Sales, 
                 start = c(2011, 2), end = c(2016, 4), freq = 12)
food_WI.ts

#-Wisconsin ----------Data Plot---------------------------
plot(food_WI.ts, 
     xlab = "Month", ylab = "Revenue($)", main = "Revenue generated by food in WI", col = "blue")

#-Wisconsin ----------Check Predictability------------------------
diff.WI <- diff(food_WI.ts, lag = 1)
Acf(diff.WI, lag.max = 12, 
    main = "Autocorrelation of First differencing for WI Walmart Food Revenue")


#-Wisconsin ----------Autocorrelation plot----------------------
Acf(food_WI.ts, lag.max = 12, main = "Autocorrelation for WI Walmart food Revenue")

#-Wisconsin ----------STL-------------------------
wi.stl <- stl(food_WI.ts, s.window = 'periodic')
autoplot(wi.stl, main = "WI Time Series Components")

#-Wisconsin ----------Data Partition-------------------------
nValid <- 12 
nTrain <- length(food_WI.ts) - nValid
train.ts <- window(food_WI.ts, start = c(2011, 2), end = c(2011, nTrain+1))
valid.ts <- window(food_WI.ts, start = c(2011, nTrain + 2), 
                   end = c(2011, nTrain + nValid+1))
train.ts
valid.ts

#-Wisconsin ----------Regression--------------------------
#Model 1: linear trend
wal.lin <- tslm(train.ts ~ trend)
summary(wal.lin)
wal.lin.pred <- forecast(wal.lin, h = nValid, level = 0)

#Model 2: Linear Trend + Seasonality
wal.lin.sea <- tslm(train.ts ~ trend + season)
summary(wal.lin.sea)
wal.lin.sea.pred <- forecast(wal.lin.sea, h = nValid, level = 0)

#Model 3: quadratic trend
wal.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(wal.quad)
wal.quad.pred <- forecast(wal.quad, h = nValid, level = 0)

#Model4: quadratic trend +seasonality
wal.quad.sea <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(wal.quad.sea)
wal.quad.sea.pred <- forecast(wal.quad.sea, h = nValid, level = 0)

#Evaluation for Regression model
round(accuracy(wal.lin.pred, valid.ts),3)
round(accuracy(wal.lin.sea.pred, valid.ts),3)
round(accuracy(wal.quad.pred, valid.ts),3)
round(accuracy(wal.quad.sea.pred, valid.ts),3)

#Linear Residual Acf
Acf(wal.lin.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for linear trend training residuals")
Acf(valid.ts - wal.lin.pred$mean, lag.max = 12, 
    main = "Autocorrelation for linear trend validation residuals")

#Quad Residual Acf
Acf(wal.quad.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for quadratic trend training residuals")
Acf(valid.ts - wal.quad.pred$mean, lag.max = 12, 
    main = "Autocorrelation for quadratic trend validation residuals")



#-Wisconsin ----------Two-Level Forecast For Regression----------------------------
#Linear trend +AR
res.lin.ar1 <- Arima(wal.lin$residuals, order = c(1,0,0))
summary(res.lin.ar1)

res.lin.ar1.pred <- forecast(res.lin.ar1, h = nValid, level = 0)
train.res.lin.ar1.pred <- wal.lin.pred$fitted + res.lin.ar1.pred$fitted
valid.res.lin.ar1.pred <- wal.lin.pred$mean + res.lin.ar1.pred$mean

#linear trend + MA
ma.trailing.res <- rollmean(wal.lin$residuals, k = 4, align = "right")
ma.trailing.res_12.pred <- forecast(ma.trailing.res, h = 12, level = 0)
wal.lin.pred$fitted
ma.trailing.res_12.pred$fitted
ts.fitted.12 <- wal.lin.pred$fitted + ma.trailing.res_12.pred$fitted
ts.forecast.12 <- wal.lin.pred$mean + ma.trailing.res_12.pred$mean

#quadratic trend + AR
res.quad.ar1 <- Arima(wal.quad$residuals, order = c(1,0,0))
summary(res.quad.ar1)
res.quad.ar1.pred <- forecast(res.quad.ar1, h = nValid, level = 0)
valid.res.quad.ar1.pred <- wal.quad.sea.pred$mean + res.quad.ar1.pred$mean

#quadratic trend + MA
quad.ma.trailing.res <- rollmean(wal.quad$residuals, k = 7, align = "right")
quad.ma.trailing.res_12.pred <- forecast(quad.ma.trailing.res, h = 12, level = 0)
quad.ts.forecast.12 <- wal.quad.pred$mean + quad.ma.trailing.res_12.pred$mean

# Evaluation for 2 level forecast
round(accuracy(train.res.lin.ar1.pred, train.ts), 3)
round(accuracy(valid.res.lin.ar1.pred, valid.ts), 3)
round(accuracy(valid.res.quad.ar1.pred, valid.ts), 3)
round(accuracy(ts.fitted.12, train.ts), 3)
round(accuracy(ts.forecast.12, valid.ts), 3)
round(accuracy(quad.ts.forecast.12, valid.ts), 3)

#Autocorrelation for Training residuals of residuals
Acf(res.lin.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for WI food Revenue training residuals of residuals")




#-Wisconsin ----------Holt-Winters--------------------------------
#Automated Holt-Winters
wi.ZZZ <- ets(train.ts, model = "ZZZ") 
wi.ZZZ
wi.ZZZ.pred <- forecast(wi.ZZZ, h = nValid, level = 0)

#HOLT'S EXPONENTIAL SMOOTHING additive error(A), additive trend (A), & no seasonality (N)
wi.AAN <- ets(train.ts, model = "AAN")
wi.AAN
wi.AAN.pred <- forecast(wi.AAN, h = nValid, level = 0)

#HOLT'S EXPONENTIAL SMOOTHING additive error(A), additive trend (A), & additive seasonality (A)
wi.AAA <- ets(train.ts, model = "AAA")
wi.AAA
wi.AAA.pred <- forecast(wi.AAA, h = nValid, level = 0)

# Evaluation for Holt-Winters
round(accuracy(wi.ZZZ.pred, valid.ts), 3)
round(accuracy(wi.AAN.pred, valid.ts), 3)
round(accuracy(wi.AAA.pred, valid.ts), 3)

# Check residual
acf(wi.AAN.pred$residuals, lag.max = 12, main = "Autocorrelation for Holt Winter AAN WI Training Residuals")
Acf(valid.ts - wi.AAN.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Holt Winter AAN WI Validation Residuals")
acf(wi.AAA.pred$residuals, lag.max = 12, main = "Autocorrelation for Holt Winter AAA WI Training Residuals")
Acf(valid.ts - wi.AAA.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Holt Winter AAA WI Validation Residuals")

#-Wisconsin ----------Two-Level Forecast With HW--------------
#### AAN + AR
res.wi.AAN.ar1 <- Arima(wi.AAN$residuals, order = c(1,0,0))
summary(res.wi.AAN.ar1)
res.wi.AAN.ar1.pred <- forecast(res.wi.AAN.ar1, h = nValid, level = 0)
valid.res.wi.AAN.ar1.pred <- wi.AAN.pred$mean + res.wi.AAN.ar1.pred$mean

#### AAN + MA
ma.AAN.res <- rollmean(wi.AAN$residuals, k = 2, align = "right")
ma.AAN.res.pred <- forecast(ma.AAN.res, h = 12, level = 0)
ma.AAN.res.forecast.12 <- wi.AAN.pred$mean + ma.AAN.res.pred$mean

#### AAA + AR
res.wi.AAA.ar1 <- Arima(wi.AAA$residuals, order = c(1,0,0))
summary(res.wi.AAA.ar1)
res.wi.AAA.ar1.pred <- forecast(res.wi.AAA.ar1, h = nValid, level = 0)
valid.res.wi.AAA.ar1.pred <- wi.AAA.pred$mean + res.wi.AAA.ar1.pred$mean

#### AAA + MA
ma.AAA.res <- rollmean(wi.AAA$residuals, k = 7, align = "right")
ma.AAA.res.pred <- forecast(ma.AAA.res, h = 12, level = 0)
ma.AAA.res.forecast.12 <- wi.AAA.pred$mean + ma.AAA.res.pred$mean

### Evaluation for 2 level forecast
round(accuracy(valid.res.wi.AAN.ar1.pred, valid.ts), 3)
round(accuracy(valid.res.wi.AAA.ar1.pred, valid.ts), 3)
round(accuracy(ma.AAN.res.forecast.12, valid.ts), 3)
round(accuracy(ma.AAA.res.forecast.12, valid.ts), 3)

#----Autocorrelation Training residual of residuals

Acf(res.wi.AAN.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for WI food Revenue training residuals of residuals")
Acf(res.wi.AAA.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for WI food Revenue training residuals of residuals")


#-Wisconsin ----------ARIMA------------
# auto Arima
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)

#ARIMA (1,1,1)
wal.arima1<- Arima(train.ts, order = c(1,1,1))
summary(wal.arima1)
wal.arima1.pred <- forecast(wal.arima1, h = nValid, level = 0)

#ARIMA (2,2,2)
wal.arima2<- Arima(train.ts, order = c(2,2,2))
summary(wal.arima2)
wal.arima2.pred <- forecast(wal.arima2, h = nValid, level = 0)

#ARIMA (2,2,2)(0,0,2)
wal.arima.sea1<- Arima(train.ts, order = c(2,2,2), seasonal = c(0,0,1))
summary(wal.arima.sea1)
wal.arima.sea1.pred <- forecast(wal.arima.sea1, h = nValid, level = 0)

#ARIMA (2,2,2)(0,1,0)
wal.arima.sea2<- Arima(train.ts, order = c(2,2,2), seasonal = c(0,1,0))
summary(wal.arima.sea2)
wal.arima.sea2.pred <- forecast(wal.arima.sea2, h = nValid, level = 0)

# Evaluation for ARIMA
round(accuracy(train.auto.arima.pred, valid.ts), 3)
round(accuracy(wal.arima1.pred, valid.ts), 3)
round(accuracy(wal.arima2.pred, valid.ts), 3)
round(accuracy(wal.arima.sea1.pred, valid.ts), 3)
round(accuracy(wal.arima.sea2.pred, valid.ts), 3)


#-Wisconsin ----------Apply whole dataset to model candidates--------------------------
#MA
all.ma.trailing.7 <- rollmean(food_WI.ts, k = 7, align = "right")
all.ma.trailing.7.pred <- forecast(all.ma.trailing.7, h = 12, level = 0)

#Lin trend + AR
all.wal.lin <- tslm(food_WI.ts ~ trend)
all.wal.lin.pred <- forecast(all.wal.lin, h = 12, level = 0)

all.res.lin.ar1 <- Arima(all.wal.lin$residuals, order = c(1,0,0))
all.res.lin.ar1.pred <- forecast(all.res.lin.ar1, h = 12, level = 0)

all.res.lin.ar1.fitted <- all.wal.lin.pred$fitted + all.res.lin.ar1.pred$fitted

#Lin trend + MA
all.ma.trailing.res <- rollmean(all.wal.lin$residuals, k = 4, align = "right")
all.ma.trailing.res.pred <- forecast(all.ma.trailing.res, h = 12, level = 0)
all.ts.fitted <- all.wal.lin.pred$fitted + all.ma.trailing.res.pred$fitted

#Holt-winter AAN
all.wi.AAN <- ets(food_WI.ts, model = "AAN")
all.wi.AAN.pred <- forecast(all.wi.AAN, h = 12, level = 0)

#Holt-winter AAN+AR
all.res.wi.AAN.ar1 <- Arima(all.wi.AAN$residuals, order = c(1,0,0))
all.res.wi.AAN.ar1.pred <- forecast(all.res.wi.AAN.ar1, h = 12, level = 0)
all.res.wi.AAN.ar1.fitted <- all.wi.AAN.pred$fitted + all.res.wi.AAN.ar1.pred$fitted

#ARIMA (222)(001)
all.wal.arima.sea1<- Arima(food_WI.ts, order = c(2,2,2), seasonal = c(0,0,1))
all.wal.arima.sea1.pred <- forecast(all.wal.arima.sea1, h = 12, level = 0)

#ARIMA (222)(010)
all.wal.arima.sea2<- Arima(food_WI.ts, order = c(2,2,2), seasonal = c(0,1,0))
all.wal.arima.sea2.pred <- forecast(all.wal.arima.sea2, h = 12, level = 0)


round(accuracy(all.ma.trailing.7.pred$fitted, food_WI.ts), 3)
round(accuracy(all.res.lin.ar1.fitted, food_WI.ts), 3)
round(accuracy(all.ts.fitted, food_WI.ts), 3)
round(accuracy(all.wi.AAN.pred$fitted, food_WI.ts), 3)
round(accuracy(all.res.wi.AAN.ar1.fitted, food_WI.ts), 3)
round(accuracy(all.wal.arima.sea1.pred$fitted, food_WI.ts), 3)
round(accuracy(all.wal.arima.sea2.pred$fitted, food_WI.ts), 3)
round(accuracy((snaive(food_WI.ts))$fitted, food_WI.ts), 3)
round(accuracy((naive(food_WI.ts))$fitted, food_WI.ts), 3)

#-Wisconsin ----------Final model---------------------------
#Summary
summary(all.wal.arima.sea2)

#Plotting AR1
plot(food_WI.ts, 
     xlab = "Time", ylab = "Revenue($)", ylim = c(250000, 1200000), bty = "l",
     xaxt = "n", xlim = c(2011, 2018), lwd = 2,
     main = "ARIMA Model(222)(001) for Entire Dataset") 
axis(1, at = seq(2011, 2018, 1), labels = format(seq(2011, 2018, 1)))
lines(all.wal.arima.sea1$fitted, col = "blue", lwd = 2)
lines(all.wal.arima.sea1.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2011,1000000, legend = c("Revenue Series", 
                                "ARIMA Forecast", 
                                "ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2016.3,2016.3), c(0, 1200000))
text(2013.5, 1150000, "Training")
text(2016.75, 1150000, "Future")
arrows(2016.3, 1100000, 2011, 1100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.3, 1100000, 2017.3, 1100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Plotting AR2
plot(food_WI.ts, 
     xlab = "Time", ylab = "Revenue($)", ylim = c(250000, 1200000), bty = "l",
     xaxt = "n", xlim = c(2011, 2018), lwd = 2,
     main = "ARIMA Model(222)(010) for Entire Dataset") 
axis(1, at = seq(2011, 2018, 1), labels = format(seq(2011, 2018, 1)))
lines(all.wal.arima.sea2$fitted, col = "blue", lwd = 2)
lines(all.wal.arima.sea2.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2011,1000000, legend = c("Revenue Series", 
                                "ARIMA Forecast", 
                                "ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2016.3,2016.3), c(0, 1200000))
text(2013.5, 1150000, "Training")
text(2016.75, 1150000, "Future")
arrows(2016.3, 1100000, 2011, 1100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.3, 1100000, 2017.3, 1100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

