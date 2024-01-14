library(data.table)
library(dtplyr)
library(dplyr)
library(forecast)
library(ggplot2)
library(fpp2)
library(tseries)
library(DIMORA)
#load("genre_time_series.RData")
#load("variables.RData")
#load("max_genres.RData")
load("data_business_project.RData")


#fiction_genres <- c("Mystery", "Science Fiction", "Romance","Fantasy","Historical Fiction","Adventure","Thriller","Humor","Psychological")
#non_fiction_genres <- c("History", "Biography", "Finance", "Health","Business","Travel","Religion","Cooking","Autobiography")#"Self improvement",

ggplot(books_year, aes(x = PublicationYear, y = unique_title_count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Unique Title Count per Publication Year",
       x = "Publication Year",
       y = "Unique Title Count") +
  theme_minimal()


ggplot(books_checkout_year, aes(x = CheckoutYear, y = unique_title_count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Unique Title Count per Checkout year",
       x = "Publication Year",
       y = "Unique Title Count") +
  theme_minimal()


#BOOKS
#ts_titles_year <- ts(books_checkout_year$unique_title_count, start = books_checkout_year$CheckoutYear[1], frequency = 1)
plot(ts_titles_year,main="Unique titles by checkout year",xlab="year",ylab="frequency")

#detailed_ts_books <- ts(detailed_books_checkout$unique_title_count, start = c(detailed_books_checkout$CheckoutYear[1],detailed_books_checkout$CheckoutMonth[1]), frequency = 12)
plot(detailed_ts_books,main="Unique Book titles time series",xlab="year",ylab="frequency")

#print(detailed_books_checkout["2020-3":"2020-11"])
# Load the stats package (if not already loaded)
library(stats)

# Subset the time series during pandemic
pandemic_unique <- window(detailed_ts_books, start = c(2020,1), end = c(2020,11))
pandemic_checkouts<-window(total_time_series,start=c(2020,1),end=c(2020,11))
# Print the result
print(pandemic_unique)
print(pandemic_checkouts)


plot(total_time_series,main="Total Book Checkouts",xlab="year",ylab="frequency")
autoplot(total_time_series,series="BOOKS")+ labs(title = "Time Series of Total Checkouts per medium", y = "Checkouts")+autolayer(ts_ebooks_checkouts,series="EBOOKS")+autolayer(ts_audiobooks_checkouts,series="AUDIOBOOKS")


fit_total_l<-tslm(total_time_series~ trend+season+detailed_ts_books)
summary(fit_total_l)
Acf(total_time_series)
Pacf(total_time_series)
plot(residuals(fit_total_l))
Acf(residuals(fit_total_l))
lines(fitted(fit_total_l),col="red")
Pacf(residuals(fit_total_l))
res<-residuals(fit_total_l)
fit_res<-auto.arima(residuals(fit_total_l))
summary(fit_res)
fit_res2<-Arima(res,order=c(2,1,1))
summary(fit_res2)
fit_res<-Arima(res,order=c(1,1,1))#best

Total_fit<-Arima(total_time_series,order=c(1,1,1),xreg=fitted(fit_total_l))
plot(residuals(Total_fit))
summary(Total_fit)
Acf(residuals(Total_fit))

plot(total_time_series,main="Total Book Checkouts",xlab="year",ylab="frequency")
lines(fitted(Total_fit),col="red")
#fore_b<-forecast(Total_fit,h=12)


Acf(total_time_series)
Pacf(total_time_series)
fit_gbm<-GBM(total_time_series,nshock=1,shock="rett",prelimestimates = c(BM(total_time_series, display=FALSE)$Estimate[1,1],
                                                                                                  BM(total_time_series, display=FALSE)$Estimate[2,1],
                                                                       BM(total_time_series, display=FALSE)$Estimate[3,1],
                                                                           180,185,-0.1))


summary(fit_gbm)
plot(residuals(fit_gbm))   
res<-residuals(fit_gbm)
Acf(res)
Pacf(res)
fit_refinement<-auto.arima(res)
summary(fit_refinement)
fit2<-Arima(res,order=c(1,1,1))
summary(fit2)

fitted_gbm<-fitted(fit_gbm)
fitted_gbm<-make.instantaneous(fitted_gbm)
Total_fit<-Arima(total_time_series,order=c(1,1,1),xreg=fitted_gbm)
summary(Total_fit)
plot(total_time_series,main="Total Book Checkouts",xlab="year",ylab="frequency")
lines(fitted(Total_fit),col="red")
checkresiduals(Total_fit)
Pacf(residuals(Total_fit))
Acf(residuals(Total_fit))
fits<-Arima(total_time_series,order=c(1,1,1),seasonal=list(order=c(2,3,2),period=12),xreg=fitted_gbm)
summary(fits)
checkresiduals(fits)
Pacf(residuals(fits))

plot(total_time_series,main="Total Book Checkouts",xlab="year",ylab="frequency")
lines(fitted(fits),col="red")
legend('bottomleft', legend=c('Original Data', 'SARMAX Fit'), col=c('black', 'red'), lty=1)

xreg_gbm <- predict(fit_gbm, newx = c(1:(length(total_time_series)+24)))
xreg_gbm<-make.instantaneous(xreg_gbm)

fore_b<-forecast(fits,xreg=tail(xreg_gbm,24))





##
fit_alt<-auto.arima(total_time_series)
summary(fit_alt)
plot(total_time_series,main="Total Book Checkouts",xlab="year",ylab="frequency")
lines(fitted(fit_alt),col="red")
legend('bottomleft', legend=c('Original Data', 'Fit'), col=c('black', 'red'), lty=1)

Acf(residuals(fit_alt))
Pacf(residuals(fit_alt))
fit_o<-Arima(total_time_series,order=c(1,1,0),seasonal=list(order=c(2,3,1),period=12))
plot(total_time_series,main="Total Book Checkouts",xlab="year",ylab="frequency")
lines(fitted(fit_o),col="red")
summary(fit_o)
checkresiduals(fit_o)
Acf(residuals(fit_o))
Pacf(residuals(fit_o))

jung_box_test <- Box.test(residuals(fit_o), lag = 24, type = "Ljung-Box")
print(jung_box_test)

train_size <- length(total_time_series)-12
train_data <- head(total_time_series, train_size)
test_data <- tail(total_time_series, 12)
fit_train1<-Arima(train_data,order=c(1,1,1),seasonal=list(order=c(2,3,2),period=12),xreg=head(fitted_gbm,train_size))
fit_train2<-Arima(train_data,order=c(1,1,0),seasonal=list(order=c(2,3,1),period=12))

# Fit ARIMA model

arima_forecast1 <- forecast(fit_train1, h = length(test_data),xreg=tail(fitted_gbm,12))
arima_rmse <- sqrt(mean((arima_forecast1$mean - test_data)^2))
arima_rmse
arima_forecast2 <- forecast(fit_train2, h = length(test_data))
arima_rmse2 <- sqrt(mean((arima_forecast2$mean - test_data)^2))
arima_rmse2
plot(arima_forecast1,main="Forecast of best SARMAX(GBM) model on test set")
lines(total_time_series)
legend('bottomleft', legend=c('Original Data', 'Forecast'), col=c('black', 'blue'), lty=1)
lines(fitted(fit_train1),col="red")
checkresiduals(fit_train1)

plot(arima_forecast2,main="Forecast of best Arima model on test set")

# Summary of the ARIMA model
arima_summary <- summary(arima_model)



###
P_values <- c(0, 1, 2,3)
D_values <- c(0, 1)
Q_values <- c(0, 1, 2,3)

# Perform grid search
best_model <- list(order = c(0, 0, 0), seasonal = list(order = c(0, 0, 0)))
best_aic <- Inf

for (P in P_values) {
  for (D in D_values) {
    for (Q in Q_values) {
      current_order <- c(1, 1, 1)
      current_seasonal_order <- list(order = c(P, D, Q))
      
      # Fit ARIMA model
      current_model <- Arima(total_time_series, order = current_order, seasonal = current_seasonal_order,xreg=fitted_gbm)
      
      # Compare AIC
      current_aic <- AIC(current_model)
      
      # Update best model if current AIC is lower
      if (current_aic < best_aic) {
        best_aic <- current_aic
        best_model <- list(order = current_order, seasonal = current_seasonal_order)
      }
    }
  }
}

# Use the best model
best_arima_model <- Arima(ts_data, order = best_model$order, seasonal = best_model$seasonal)





#EBOOK
ts_ebooks_checkouts<-window(ts_ebooks_checkouts,start=2010)
plot(ts_ebooks_checkouts,main="Ebook checkouts",ylab="Checkouts")
fit_e<-tslm(ts_ebooks_checkouts~ trend)
fit_e<-tslm(ts_ebooks_checkouts~ trend+season)
Acf(ts_ebooks_checkouts)
summary(fit_e)
plot(ts_ebooks_checkouts,main="TSLM Model",ylab="checkouts")
lines(fitted(fit_e),col="red")
resfit<-residuals(fit_e)
plot(resfit)
dwtest(fit_e)
Acf(residuals(fit_e))
Pacf((residuals(fit_e)))

plot(forecast(fit_e, h=20))

refinement<-auto.arima(resfit)
summary(refinement)
ref_1<-Arima(resfit,order=c(1,1,1),seasonal=list(order=c(2,1,2),period=12))
summary(ref_1)
checkresiduals(ref_1)
plot(resfit)
lines(fitted(refinement),col="red")
plot(residuals(refinement))
checkresiduals(refinement)
plot(ts_ebooks_checkouts,main="TSLM Model with ARIMA refinement",ylab="checkouts")
lines(fitted(fit_e)+fitted(ref_1),col="red")
fit_tslm_ref<-Arima(ts_ebooks_checkouts,order=c(1,1,1),seasonal=list(order=c(2,1,2),period=12),xreg=fitted(fit_e))
fit_auto<-Arima(ts_ebooks_checkouts,order=c(1,0,1),seasonal=list(order=c(1,0,1),period=12),include.mean=TRUE,xreg=fitted(fit_e))
checkresiduals(fit_tslm_ref)
Pacf(residuals(fit_tslm_ref))
summary(fit_tslm_ref)
AIC(fit_e)



#
#forecast(fit_e,refinement)

fit_e_a<-auto.arima(ts_ebooks_checkouts)
plot(ts_ebooks_checkouts)
lines(fitted(fit_e_a))
fit_e_a<-Arima(ts_ebooks_checkouts,order=c(1,0,1),seasonal=c(1,1,2),include.drift=T,include.mean=T)
summary(fit_e_a)
checkresiduals(fit_e_a)
Pacf(residuals(fit_e_a))
plot(ts_ebooks_checkouts,main="Best ARIMA Model including drift",ylab="Checkouts")
lines(fitted(fit_e_a),col="red")
fore<-forecast(fit_e_a)
plot(fore)



train_size <- length(ts_ebooks_checkouts)-12
train_data <- head(ts_ebooks_checkouts, train_size)
test_data <- tail(ts_ebooks_checkouts, 12)
fit_train1<-Arima(train_data,order=c(1,1,1),seasonal=list(order=c(2,1,2),period=12),xreg=head(fitted(fit_e),train_size))
fit_train2<-Arima(train_data,order=c(1,0,1),seasonal=c(1,1,2),include.drift=T,include.mean=T)
train_tslm<-tslm(train_data~trend+season)
train_res<-Arima(residuals(train_tslm),order=c(1,1,1),seasonal=list(order=c(2,1,2),period=12))

# Fit ARIMA model

arima_forecast1 <- forecast(fit_train1, h = length(test_data),xreg=tail(fitted(fit_e),12))
arima_rmse <- sqrt(mean((arima_forecast1$mean - test_data)^2))
arima_rmse
arima_forecast2 <- forecast(fit_train2, h = length(test_data))
arima_rmse2 <- sqrt(mean((arima_forecast2$mean - test_data)^2))
arima_rmse2
tslm_forecast3 <- forecast(train_tslm,h=length(test_data))
arima_forecast3<-forecast(train_res, h = length(test_data))

arima_rmse3 <- sqrt(mean((tslm_forecast3$mean+arima_forecast3$mean - test_data)^2))
arima_rmse3
tslm_rmse4<-sqrt(mean(tslm_forecast3$mean-test_data)^2)
tslm_rmse4
plot(arima_forecast2)

lines(fitted(train_tslm)+fitted(refinement),col="red")
checkresiduals(fit_train2)




par(mfrow=c(1,1))

#audiobook
fit_audio <- auto.arima(ts_audiobooks_checkouts,lambda="auto")#ARIMA (1,1,1)(2,0,0)[2]
summary(fit_audio)
fit_audio_3<-Arima(ts_audiobooks_checkouts,order=c(2,1,1),seasonal=list(order=c(2,0,0),period=2),lambda="auto")
summary(fit_audio_3)
plot(ts_audiobooks_checkouts,main="Audiobook checkouts",ylab="Checkouts")
ts_log <- log(ts_audiobooks_checkouts)
plot(ts_log)
# Print the selected model
print(fit_audio)

plot(ts_audiobooks_checkouts,main="ARIMA with BOX-COX transformation fit",ylab="Checkouts")
lines(fitted(fit_audio),col="red")
fit_audio_2<-ets(ts_audiobooks_checkouts)
plot(ts_audiobooks_checkouts,main="ETS model fit",ylab="Checkouts")
lines(fitted(fit_audio_2),col="red")

checkresiduals(fit_audio)
Pacf(residuals(fit_audio))
Acf(residuals(fit_audio))

#testing models


train_size <- length(ts_audiobooks_checkouts)-12
train_data <- head(ts_audiobooks_checkouts, train_size)
test_data <- tail(ts_audiobooks_checkouts, 12)
fit_train1<-auto.arima(train_data,lambda="auto")
fit_train2<-ets(train_data)

arima_forecast1 <- forecast(fit_train1, h = length(test_data))
arima_rmse <- sqrt(mean((arima_forecast1$mean - test_data)^2))
arima_rmse
ets_forecast <- forecast(fit_train2, h = length(test_data))
ets_rmse2 <- sqrt(mean((ets_forecast$mean - test_data)^2))
ets_rmse2








predicted_values <- fitted(fit_audio)


plot(ts_audiobooks_checkouts, col = "black", main = "Original and Predicted Time Series", ylab = "Value")
lines(predicted_values, col = "red")
fore_audio<-forecast(fit_audio)
plot(fore_audio,ylab="Checkouts")

autoplot(total_time_series)+autolayer(ts_ebooks_checkouts,series="EBOOKS",col="blue")+autolayer(fore,series="EBOOKS",col="lightblue")+autolayer(ts_audiobooks_checkouts,series="AUDIOBOOks",alpha=0.5,col="red")+autolayer(fore_audio,col="pink",alpha=0.5)+  labs(title = "Forecasts of different mediums", y = "checkouts") +
  theme_minimal()

library(ggplot2)
autoplot(total_time_series, col = "green",series="BOOKS") +
  autolayer(fore_b,alpha=0.3,col="green")+
  autolayer(ts_ebooks_checkouts, series = "EBOOKS") +
  autolayer(fore, series = "EBOOKS") +
  autolayer(ts_audiobooks_checkouts, series = "AUDIOBOOKS") +
  autolayer(fore_audio, alpha = 0.3,col="pink") +
  labs(title = "Forecasts of different mediums", y = "checkouts") +
  theme_minimal()



##############

find_intersection_point <- function(ts1, ts2) {
  # Ensure the time series have the same time points
  #if (!identical(time(ts1), time(ts2))) {
  #  stop("Time series must have the same time points")
  #}
  
  # Find the index where the values are closest
  index <- which.min(abs(ts1 - ts2))
  
  # Return the corresponding time point
  return(time(ts1)[index])
}

intersection_point <- find_intersection_point(fore_b$mean, fore$mean)

# Print or use the intersection point as needed
print(intersection_point)

autoplot(total_time_series, col = "green", series = "BOOKS") +
  autolayer(fore_b, alpha = 0.3, col = "green", geom = "line") +
  autolayer(ts_ebooks_checkouts, series = "EBOOKS") +
  autolayer(fore, series = "EBOOKS", geom = "line") +
  autolayer(ts_audiobooks_checkouts, series = "AUDIOBOOKS") +
  autolayer(fore_audio, alpha = 0.3, col = "pink", geom = "line") +
  geom_vline(xintercept = intersection_point, linetype = "dashed", color = "blue") +
  labs(title = "Forecasts of different mediums", y = "checkouts") +
  theme_minimal()


autoplot(total_time_series, series = "BOOKS") +
  autolayer(fore_b$mean,series="BOOKS") +
  autolayer(ts_ebooks_checkouts, series = "EBOOKS") +
  autolayer(fore$mean, series = "EBOOKS") +
  autolayer(ts_audiobooks_checkouts, series = "AUDIOBOOKS") +
  autolayer(fore_audio$mean,series="AUDIOBOOKS") +
  geom_vline(xintercept = 2025, linetype = "dashed", color = "red") +
  labs(title = "Forecasts of different mediums", y = "checkouts") +
  theme_minimal()



  autoplot(fore_b$mean,series="BOOKS") +
  autolayer(fore$mean, series = "EBOOKS") +
    autolayer(fore_audio$mean,series="AUDIOBOOKS") +
  geom_vline(xintercept = 2025, linetype = "dashed", color = "red") +
  labs(title = "Forecasts of different mediums", y = "checkouts") +
  theme_minimal()








#forecast_ts1 <- forecast(ts1, h = 12)
forecast_books<-forecast(fits,h=12,xreg=tail(xreg_gbm,24))
forecast_ebooks <-forecast(fit_e,h=12)
forecast_audiobooks <- forecast(fit_audio, h = 12)

# Extract the last known values

last_values <- data.frame(
  series = rep(c("Ebooks", "Audiobooks","BOOKS"), each = 2),
  time = rep(c("Last Value", "Forecasted Value"), times = 3),
  value = c(tail(ts_ebooks_checkouts, 1), forecast_ebooks$mean[12], 
            tail(ts_audiobooks_checkouts, 1), forecast_audiobooks$mean[12],tail(total_time_series,1),forecast_books$mean[12]),
  lower = c(tail(ts_ebooks_checkouts, 1), forecast_ebooks$lower[12],
            tail(ts_audiobooks_checkouts, 1), forecast_audiobooks$lower[12],tail(total_time_series,1),forecast_books$lower[12]),
  upper = c(tail(ts_ebooks_checkouts, 1), forecast_ebooks$upper[12],
            tail(ts_audiobooks_checkouts, 1), forecast_audiobooks$upper[12],tail(total_time_series,1),forecast_books$upper[12])
)
last_values$time <- factor(last_values$time, levels = c("Last Value", "Forecasted Value"))

# Plot the bar graph with error bars
ggplot(last_values, aes(x = series, y = value, fill = time)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.9), width = 0.25) +
  labs(title = "Last Known and 12-Month Forecasted Values by medium",
       x = "Medium", y = "Checkouts") +
  scale_fill_manual(values = c("grey", "green"), labels = c("Last Value", "Forecasted Value")) +
  theme_minimal()

ucrcdEB<- UCRCD(total_time_series,ts_ebooks_checkouts, display=T)#Unbalanced Competition Regime Change Diachronic model
summary(ucrcdEB)

#Checkouts per unique books

#ts_checkouts_per_unique<-total_time_series/detailed_ts_books
plot(ts_checkouts_per_unique,main="Checkouts per unique book",xlab="time",ylab="frequency",col="red")
str(ts_checkouts_per_unique)
plot(ts_checkouts_per_unique,main="Checkouts per unique book",xlab="time",ylab="frequency",col="red",xlim=c(2005,2019),ylim=c(0,5))
#plot(ts_checkouts_per_unique,main="Checkouts per unique book",xlab="time",ylab="frequency",col="red",xlim=c(2020,2023),ylim=c(0,5))

#filtered10_unique_df<- full[Checkouts>10, .(unique_title_count = uniqueN(Title)), by = .(CheckoutYear,CheckoutMonth)][order(CheckoutYear,CheckoutMonth)]
#filtered10_total_checkouts<-full[Checkouts>10, .(TotalCheckouts = sum(Checkouts)), by = .(CheckoutYear,CheckoutMonth)][order(CheckoutYear,CheckoutMonth)]
#ts_filtered10_unique<-ts(filtered10_unique_df$unique_title_count,frequency=12, start = c(max_df$CheckoutYear[1],max_df$CheckoutMonth[1]))
#ts_filtered10_total_checkouts<-ts(filtered10_total_checkouts$TotalCheckouts,frequency=12, start = c(max_df$CheckoutYear[1],max_df$CheckoutMonth[1]))

#ts_filtered10_checkouts_per_unique<-ts_filtered10_total_checkouts/ts_filtered10_unique

hist(distribution_checkouts, 
     main = "Distribution of unique Book Checkouts -July 2015",
     xlab = "Checkouts",
     ylab = "Frequency",
     col = "lightblue",
     border = "black",breaks=250) 
abline(v=3,lty=2,lwd=2,col="red")
hist(distribution_checkouts, 
     main = "Distribution of unique Book Checkouts -July 2015",
     xlab = "Checkouts",
     ylab = "Frequency",
     col = "lightblue",
     border = "black",breaks=250,ylim=c(0,40)) 
abline(v=3,lty=2,lwd=2,col="red")
summary(distribution_checkouts)

#time series of checkouts per unique book, with min checkouts>3
plot(ts_filtered3_checkouts_per_unique,main="Checkouts per unique book",xlab="time",ylab="frequency",col="red")

#time series of checkouts per unique book, with min checkouts>10
plot(ts_filtered10_checkouts_per_unique,main="Checkouts per unique book",xlab="time",ylab="frequency",col="red")



plot(checkout_month,main="Total checkouts by Month",xlab="Month",ylab="frequency",pch=16,col="red",lwd=2)


#Max checkout of unique book per month


#max_df<-full[,.(max_checkout=max(Checkouts)),by=.(CheckoutYear,CheckoutMonth)][order(CheckoutYear,CheckoutMonth)]
#ts_max_df<-ts(max_df$max_checkout,frequency=12, start = c(max_df$CheckoutYear[1],max_df$CheckoutMonth[1]))

plot(ts_max_df,main="Max Checkouts per unique book",xlab="time",ylab="frequency",col="black")

Acf(ts_max_df)
Pacf(ts_max_df)
adf.test(ts_max_df)
ndiffs(ts_max_df)
fit_max<-auto.arima(ts_max_df,allowdrift=TRUE,allowmean = TRUE)
summary(fit_max)
plot(residuals(fit_max))
Acf(residuals(fit_max))
plot(ts_max_df,main="Max Checkouts per unique book",xlab="time",ylab="frequency",col="black")
lines(fitted(fit_max),col="red")
fit1<-Arima(ts_max_df,order=c(1,1,0))
summary(fit1)
lines(fitted(fit1),col="green")
forecast_max<-forecast(fit1,h=12)
plot(forecast_max,main="Forecast Of the Max checkouts of a book (Arima(1,1,0))")
Box.test(residuals(fit1),lag=12,type="Ljung")

tsdiag(fit1)
par(mfrow=c(1,1))

plot(fiction_time_series,main="Fiction- Checkouts Time series",ylab="checkouts",xlab="time")
plot(non_fiction_time_series,main="Non Fiction- Checkouts Time series",ylab="checkouts",xlab="time")
autoplot(combined_time_series)+ labs(title = "Time Series of Checkouts per category", y = "Checkouts")
plot(total_time_series,main="Total Checkouts Time series",ylab="checkouts",xlab="time")
autoplot(combined_time_series)+ labs(title = "Time Series of Total Checkouts per category", y = "Checkouts")+autolayer(total_time_series,series="Total")


#Fiction

# Plot the time series
base_plot <- autoplot(window(ts_f_list[[1]],end=2023), series = fiction_genres[1])
plot(window(ts_f_list[[1]],end=2023), main = fiction_genres[1], ylab = "Checkouts")
for (i in 2:length(ts_f_list)) {
  plot(window(ts_f_list[[i]],end=2023), main = fiction_genres[i], ylab = "Checkouts")
  base_plot <- base_plot + autolayer(window(ts_f_list[[i]],end=2023), series = fiction_genres[i])
}
#Click the left arrow key to see all the different time series plots

# Print the plot
print(base_plot + labs(title = "Time Series of Checkouts per Genre (Fiction)", y = "Checkouts"))


#Bar graph - Genre Last value (2023) checkouts
ts_f_list_2023 <- lapply(ts_f_list, window, end = 2023)
ts_nf_list_2023<-lapply(ts_nf_list, window, end = 2023)

last_values_genre_f_checkouts <- sapply(ts_f_list_2023, tail, n = 1)
last_values_genre_f_checkouts_data <- data.frame(
  time_series = fiction_genres,
  last_value = last_values_genre_f_checkouts
)
last_values_genre_nf_checkouts <- sapply(ts_nf_list_2023, tail, n = 1)
last_values_genre_nf_checkouts_data <- data.frame(
  time_series = non_fiction_genres,
  last_value = last_values_genre_nf_checkouts
)

ggplot(last_values_genre_f_checkouts_data, aes(x = time_series, y = last_value,fill=time_series)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Last Value of Fiction Time series(2023)",
       x = "Fiction genres",
       y = "Chekouts")

ggplot(last_values_genre_nf_checkouts_data, aes(x = time_series, y = last_value,fill=time_series)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Last Value of Non-Fiction Time series(2023)",
       x = "Non-Fiction genres",
       y = "Chekouts")



base_plot_u <- autoplot(window(ts_f_unique_list[[1]],end=2023), series = fiction_genres[1])
plot(window(ts_f_unique_list[[1]],end=2023), main = fiction_genres[1], ylab = "Unique title checkouts")
for (i in 2:length(ts_f_unique_list)) {
  plot(window(ts_f_unique_list[[i]],end=2023), main = fiction_genres[i], ylab = "Unique title checkouts")
  base_plot_u <- base_plot_u + autolayer(window(ts_f_unique_list[[i]],end=2023), series = fiction_genres[i])
}

print(base_plot_u + labs(title = "Time Series of unique title counts per Genre (Fiction)", y = "Checkouts"))

#checkouts per unique book

ts_f_cu_list<- Map("/", ts_f_list, ts_f_unique_list)


base_plot_cu <- autoplot(ts_f_cu_list[[1]], series = fiction_genres[1])

plot(ts_f_cu_list[[1]], main = fiction_genres[1], ylab = "checkouts per unique title")
for (i in 2:length(ts_f_cu_list)) {
  plot(ts_f_cu_list[[i]], main = fiction_genres[i], ylab = "checkouts per unique title")
  base_plot_cu <- base_plot_cu + autolayer(ts_f_cu_list[[i]], series = fiction_genres[i])
}

print(base_plot_cu + labs(title = "Time Series of checkouts per unique title counts per Genre (Fiction)", y = "Checkouts"))

#Max checkouts of a single book

base_plot <- autoplot(ts_f_max_list[[1]], series = fiction_genres[1])
plot(ts_f_max_list[[1]], main = fiction_genres[1], ylab = "Maximum checkouts")
for (i in 2:length(ts_f_max_list)) {
  plot(ts_f_max_list[[i]], main = fiction_genres[i], ylab = "Maximum title checkouts")
  base_plot <- base_plot + autolayer(ts_f_max_list[[i]], series = fiction_genres[i])
}

print(base_plot + labs(title = "Time Series of Maximum checkouts per Genre (Fiction)", y = "Checkouts"))


apply_ma_to_ts <- function(ts_object, window_size) {
  ma(ts_object, order = window_size)
}

# Define the window size for the moving average
window_size <- 12 

# Apply moving average to each time series in the list
ma_max_list <- lapply(ts_f_max_list, apply_ma_to_ts, window_size = window_size)

# Plot original time series and moving averages
par(mfrow = c(3, 3))
for (i in seq_along(ts_f_max_list)) {
  plot(ts_f_max_list[[i]], main = fiction_genres[i], col = "black",ylab="max checkouts", lwd = 2)
  lines(ma_max_list[[i]], col = "red", lwd = 2)
  abline(lm(as.numeric(ma_max_list[[i]]) ~ time(ma_max_list[[i]])), col = "green", lwd = 2)
  legend("topleft", legend = c("Original", "Moving Average","trend"), col = c("black", "red","green"), lwd = 2,cex=0.5)
}
par(mfrow = c(1, 1))

apply_ema_to_ts <- function(ts_object, alpha) {
  hw(ts_object, alpha = alpha)$fitted[, "fitted"]
}





# Apply exponential moving average to each time series in the list
#ema_result_list <- lapply(ts_f_max_list, apply_ema_to_ts, alpha = alpha)

# Plot original time series and exponential moving averages with legend
par(mfrow = c(3, 3))
for (i in seq_along(ts_f_max_list)) {
  fit1<-hw(window(ts_f_max_list[[i]],end=2023))
  fit2<-ets(window(ts_f_max_list[[i]],end=2023))
  fit3<-auto.arima(window(ts_f_max_list[[i]],end=2023))
  plot(ts_f_max_list[[i]], main = fiction_genres[i], col = "black", lwd = 2)
  #lines(fitted(fit1), col = "green", lwd = 2)
  #lines(fitted(fit2),col="yellow",lwd=2)
  lines(fitted(fit3),col="orange",lwd=2)
  print(paste("AIC ets:",fit2$aic," AIC hw: ",fit1$model$aic," , AIC Auto arima: ",fit3$aic))
  summary(fit1)
  # Add legend
  legend("topleft", legend = c("Original", "Holt winters fit","ets","auto.arima"), col = c("blue", "green","yellow","orange"), lwd = 2)
}

par(mfrow = c(3, 3))
for (i in seq_along(ts_f_max_list)) {
  arima_model<-auto.arima(window(ts_f_max_list[[i]],end=2023))
  
  forecast_values <- forecast(arima_model, h = 12)  
  
  # Plot the forecast
  plot(forecast_values, main = fiction_genres[i], xlab = "Time", ylab = "Values")
}

last_values <- data.frame()

# Loop through each time series
for (i in seq_along(ts_f_max_list)) {
  # Fit ARIMA model
  arima_model <- auto.arima(window(ts_f_max_list[[i]], end = 2023))
  
  # Generate 12-month forecast
  forecast_values <- forecast(arima_model, h = 12) 
  
  # Extract the last known value
  last_known_value <- tail(ts_f_max_list[[i]], 1)
  
  # Extract only the 12th element of the forecasted values
  forecast_12th <- forecast_values$mean[12]
  
  lower_bound <- forecast_values$lower[12]
  upper_bound <- forecast_values$upper[12]
  
  # Calculate the standard deviation based on the prediction interval
  forecast_sd <- (upper_bound - lower_bound) / (2 * qnorm(0.975))
  
  # Create a data frame for the current series
  series_data <- data.frame(
    series = rep(fiction_genres[i], 2),
    value = c(last_known_value, forecast_12th),
    time = c("Last Value", "Forecasted Value"),
    #lower = c(NA, forecast_values$lower[12]),
    lower=c(NA,forecast_12th-forecast_sd),
    #upper = c(NA, forecast_values$upper[12])
    upper=c(NA,forecast_12th+forecast_sd)
  )
  
  # Combine with the overall data frame
  last_values <- rbind(last_values, series_data)
}
last_values$time <- factor(last_values$time, levels = c("Last Value", "Forecasted Value"))
# Plot the bar graph with error bars
ggplot(last_values, aes(x = series, y = value, fill = time)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.9), width = 0.25) +
  labs(title = "Last Known(2023) and 12-Month Forecasted Values of the Maximum Checkouts by Genre(Fiction)",
       x = "Medium", y = "Values") +
  scale_fill_manual(values = c("grey", "green"), labels = c("Last Value", "Forecasted Value")) +
  theme_minimal()


par(mfrow = c(3, 6))
for (i in seq_along(ts_f_max_list)) {
  Acf(ts_f_max_list[[i]])
  Pacf(ts_f_max_list[[i]])
}
par(mfrow = c(1, 1))
summary(fit2)


######Non fiction



# Plot the time series
base_plot1 <- autoplot(ts_nf_list[[1]], series = non_fiction_genres[1])
plot(ts_nf_list[[1]], main = non_fiction_genres[1], ylab = "Checkouts")
for (i in 2:length(ts_nf_list)) {
  plot(ts_nf_list[[i]], main = non_fiction_genres[i], ylab = "Checkouts")
  base_plot1 <- base_plot1 + autolayer(ts_nf_list[[i]], series = non_fiction_genres[i])
}

# Print the plot
print(base_plot1 + labs(title = "Time Series of Checkouts per Genre (Non Fiction)", y = "Checkouts"))


base_plot1_u <- autoplot(ts_nf_unique_list[[1]], series = non_fiction_genres[1])
plot(ts_nf_unique_list[[1]], main = non_fiction_genres[1], ylab = "unique book checkouts")
for (i in 2:length(ts_nf_unique_list)) {
  plot(ts_nf_unique_list[[i]], main = non_fiction_genres[i], ylab = "Unique bookcheckouts")
  base_plot1_u <- base_plot1_u + autolayer(ts_nf_unique_list[[i]], series = non_fiction_genres[i])
}

# Print the plot
print(base_plot1_u + labs(title = "Time Series of Unique title counts per Genre (Non Fiction)", y = "Checkouts"))



#Checkouts per unique book 
#checkouts per unique book

ts_nf_cu_list<- Map("/", ts_nf_list, ts_nf_unique_list)


base_plot1_cu <- autoplot(ts_nf_cu_list[[1]], series = non_fiction_genres[1])

plot(ts_nf_cu_list[[1]], main = non_fiction_genres[1], ylab = "checkouts per unique title")
for (i in 2:length(ts_nf_cu_list)) {
  plot(ts_nf_cu_list[[i]], main = non_fiction_genres[i], ylab = "checkouts per unique title")
  base_plot1_cu <- base_plot1_cu + autolayer(ts_nf_cu_list[[i]], series = non_fiction_genres[i])
}

print(base_plot1_cu + labs(title = "Time Series of checkouts per unique title counts per Genre (Non Fiction)", y = "Checkouts"))

###Max checkouts of a book
base_plot <- autoplot(ts_nf_max_list[[1]], series = non_fiction_genres[1])
plot(ts_nf_max_list[[1]], main = non_fiction_genres[1], ylab = "Maximum checkouts")
for (i in 2:length(ts_nf_max_list)) {
  plot(ts_nf_max_list[[i]], main = non_fiction_genres[i], ylab = "Maximum title checkouts")
  base_plot <- base_plot + autolayer(ts_nf_max_list[[i]], series = non_fiction_genres[i])
}

print(base_plot + labs(title = "Time Series of Maximum checkouts per Genre (Non Fiction)", y = "Checkouts"))




par(mfrow = c(3, 3))
for (i in seq_along(ts_nf_max_list)) {
  fit1<-hw(window(ts_nf_max_list[[i]],end=2023))
  fit2<-ets(window(ts_nf_max_list[[i]],end=2023))
  fit3<-auto.arima(window(ts_nf_max_list[[i]],end=2023))
  plot(ts_nf_max_list[[i]], main = non_fiction_genres[i], col = "black", lwd = 2)
  #lines(fitted(fit1), col = "green", lwd = 2)
  #lines(fitted(fit2),col="yellow",lwd=2)
  lines(fitted(fit3),col="orange",lwd=2)
  print(paste("AIC ets:",fit2$aic," AIC hw: ",fit1$model$aic," , AIC Auto arima: ",fit3$aic))
  summary(fit1)
  # Add legend
  legend("topleft", legend = c("Original","auto.arima"), col = c("black","orange"), lwd = 2)
}

par(mfrow = c(3, 3))
for (i in seq_along(ts_nf_max_list)) {
  arima_model<-auto.arima(window(ts_nf_max_list[[i]],end=2023))
  
  forecast_values <- forecast(arima_model, h = 12)  
  
  # Plot the forecast
  plot(forecast_values, main = non_fiction_genres[i], xlab = "Time", ylab = "Values")
}

last_values <- data.frame()
par(mfrow=c(1,1))
# Loop through each time series
for (i in seq_along(ts_nf_max_list)) {
  # Fit ARIMA model
  arima_model <- auto.arima(window(ts_nf_max_list[[i]], end = 2023))
  
  # Generate 12-month forecast
  forecast_values <- forecast(arima_model, h = 12) 
  
  # Extract the last known value
  last_known_value <- tail(ts_nf_max_list[[i]], 1)
  
  # Extract only the 12th element of the forecasted values
  forecast_12th <- forecast_values$mean[12]
  
  lower_bound <- forecast_values$lower[12]
  upper_bound <- forecast_values$upper[12]
  
  # Calculate the standard deviation based on the prediction interval
  forecast_sd <- (upper_bound - lower_bound) / (2 * qnorm(0.975))
  
  # Create a data frame for the current series
  series_data <- data.frame(
    series = rep(non_fiction_genres[i], 2),
    value = c(last_known_value, forecast_12th),
    time = c("Last Value", "Forecasted Value"),
    #lower = c(NA, forecast_values$lower[12]),
    lower=c(NA,forecast_12th-forecast_sd),
    #upper = c(NA, forecast_values$upper[12])
    upper=c(NA,forecast_12th+forecast_sd)
  )
  
  # Combine with the overall data frame
  last_values <- rbind(last_values, series_data)
}
last_values$time <- factor(last_values$time, levels = c("Last Value", "Forecasted Value"))
# Plot the bar graph with error bars
ggplot(last_values, aes(x = series, y = value, fill = time)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.9), width = 0.25) +
  labs(title = "Last Known(2023) and 12-Month Forecasted Values of the Maximum Checkouts by Genre(Non Fiction)",
       x = "Medium", y = "Values") +
  scale_fill_manual(values = c("grey", "green"), labels = c("Last Value", "Forecasted Value")) +
  theme_minimal()



#
#fin_checkouts <- window(ts_nf_list[[3]], start = c(2019,1), end = c(2020,11))
#fin_unique<-window(ts_nf_unique_list[[3]],start=c(2019,1),end=c(2020,11))
# Print the result
#print(fin_unique)
#print(fin_checkouts)


#AUDIOBOOKS
plot(ts_audiobooks_checkouts,main="Audiobooks- Frequency of checkouts",xlab="year",ylab="frequency")
plot(audiobooks_month,main="Audiobooks - Frequency of checkout by Month",xlab="Month",ylab="frequency",pch=16,col="red",lwd=2)

#EBOOKS

plot(ts_ebooks_checkouts,main="Ebooks- Frequency of checkouts Time series",xlab="year",ylab="frequency")
plot(ebooks_month,main="Ebooks - Frequency of checkout by Month",xlab="Month",ylab="frequency",pch=16,col="red",lwd=2)

