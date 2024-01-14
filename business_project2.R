library(dplyr)
library(forecast)
library(ggplot2)
library(DIMORA)
library(data.table)
best_sellers<-read.delim("nyt_full.tsv")
str(best_sellers)
total_sellers<-read.delim("nyt_titles.tsv")
str(total_sellers)
top<-total_sellers[order(-total_sellers[["total_weeks"]]),]
head(top)
top10<-top[1:10,"title"]
top10
#peak<-top[top$best_rank==1,]
#head(peak,10)
top10_details<-head(top,10)
str(top10_details)
#plot(top10_details$year,top10_details$total_weeks,pch=16,col="red",xlab="year of release",ylab="Number of NYT best selling weeks",main="Ney York Times best selling Books",ylim=c(100,200),xlim=c(1930,2023))
plot(total_sellers$year,total_sellers$total_weeks,pch=16,col="blue",xlab="year of release",ylab="Number of NYT best selling weeks",main="Ney York Times best selling Books")
points(top10_details$year,top10_details$total_weeks,pch=16,col="red",xlab="year of release",ylab="Number of NYT best selling weeks",main="Ney York Times best selling Books",ylim=c(100,200),xlim=c(1930,2023))
text(top10_details$year,jitter(top10_details$total_weeks,amount=10),labels=top10_details$title,pos=2,cex=0.5)
#####

##
##



# Creating an empty list to store time series objects
ts_list <- list()

i<-0
# Looping through each file name
for (book in top10) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  
  # Creating a time series object
  ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
  
  # Adding the time series object to the list
  ts_list[[book]] <- ts_object
  plot(ts_object, main=book,ylab="Checkouts")  
  if (i==0){
    base_plot <- autoplot(ts_list[[book]], series = book)
  }else{
    base_plot <- base_plot + autolayer(ts_list[[book]], series = book)
    }
  i<-i+1
  
}
print(base_plot + labs(title = "Time Series of Top 10 NYT selling Books", y = "Checkouts"))


top2023<-total_sellers[(total_sellers$year==2022),]
top2023<-top2023[order(-top2023[["total_weeks"]]),]
head(top2023)

recent<-c("THE THREE BODY PROBLEM","IT ENDS WITH US","48 LAWS OF POWER","SAPIENS","ATOMIC HABITS","A MAN CALLED OVE","GAME OF THRONES","LESSONS IN CHEMISTRY")

#amazon_top1<-c("HARRY POTTER AND THE HALF-BLOOD PRINCE","GOOD TO GREAT","HARRY POTTER AND THE DEATHLY HOLLOWS","A NEW EARTH","THE LOST SYMBOL","THE LOST SYMBOL","THE GIRL WHO KICKED THE HORNETS NEST","STEVE JOBS","FIFTY SHADES OF GREY","LEAN IN","THE LIFE CHANGING MAGIC OF TIDYING UP","HARRY POTTER AND THE CURSED CHILD","WONDER","BECOMING","WHERE THE CRAWDADS SING","A PROMISED LAND","ATOMIC HABITS","IT ENDS WITH US")

ts_list <- list()

i<-0
# Looping through each file name
for (book in recent) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  
  # Creating a time series object
  ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
  #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
  # Adding the time series object to the list
  #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
  
  ts_list[[book]] <- ts_object
  
  plot(ts_object, main=book,ylab="Checkouts")
  if (i==0){
    base_plot <- autoplot(ts_list[[book]], series = book)
  }else{
    base_plot <- base_plot + autolayer(ts_list[[book]], series = book)
  }
  i<-i+1
  
}
print(base_plot + labs(title = "Time Series of recent top selling books", y = "Checkouts"))

##

amazon_top1<-c("HARRY POTTER AND THE HALF-BLOOD PRINCE","HARRY POTTER AND THE DEATHLY HALLOWS","A NEW EARTH","THE LOST SYMBOL","THE GIRL WHO KICKED THE HORNET’S NEST","STEVE JOBS","FIFTY SHADES OF GREY","LEAN IN","THE LIFE CHANGING MAGIC OF TIDYING UP","HARRY POTTER AND THE CURSED CHILD","WONDER","BECOMING","WHERE THE CRAWDADS SING","A PROMISED LAND","ATOMIC HABITS","IT ENDS WITH US")


ts_list <- list()

i<-0
# Looping through each file name
for (book in amazon_top1) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  
  # Creating a time series object
  ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
  #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
  # Adding the time series object to the list
  #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
  
  ts_list[[book]] <- ts_object
  
  plot(ts_object, main=book,ylab="Checkouts")
  if (i==0){
    base_plot <- autoplot(ts_list[[book]], series = book)
  }else{
    base_plot <- base_plot + autolayer(ts_list[[book]], series = book)
  }
  i<-i+1
  
}
print(base_plot + labs(title = "Time Series of the yearly Best selling Amazon books", y = "Checkouts"))





plot(total_sellers$year,total_sellers$total_weeks,pch=16,col="blue",xlab="year of release",ylab="Number of NYT best selling weeks",main="Ney York Times best selling Books")


####
plot(total_sellers$year,total_sellers$total_weeks,pch=16,col="blue",xlab="year of release",ylab="Number of NYT best selling weeks",main="Ney York Times best selling Books")


highlight<-total_sellers$year>2005 & total_sellers$total_weeks>40
abline(v=2005,lty=2,col="black",lwd=3)
abline(h=40,col="black",lty=2,lwd=3)
modern<-total_sellers[highlight,]
plot(total_sellers$year,total_sellers$total_weeks,pch=16,col=ifelse(highlight,"red","gray"),xlab="year of release",ylab="Number of NYT best selling weeks",main="Ney York Times best selling Books")
#plot <- ggplot(data, aes(x=total_sellers$year, y=total_sellers$total_weeks)) +
#  geom_point(aes(color = ifelse(highlight, "Highlighted", "Other"), alpha = ifelse(highlight, 1, 0.2))) +
#  scale_color_manual(values = c("Highlighted" = "red", "Other" = "black")) +
#  labs(title = "Scatter Plot with Highlighted Points") +
#  theme_minimal()
modern[["title"]]
#####

i<-0
# Looping through each file name
for (book in modern[["title"]][-13]) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  
  # Creating a time series object
  if (max(data_summary$Checkouts)>10){
    ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
  #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
  # Adding the time series object to the list
  #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
  
    ts_list[[book]] <- ts_object
  
    plot(ts_object, main=book,ylab="Checkouts")
    if (i==0){
      base_plot <- autoplot(ts_list[[book]], series = book)
    }else{
      base_plot <- base_plot + autolayer(ts_list[[book]], series = book)
    }
    i<-i+1
  }
  
}
print(base_plot + labs(title = "Time Series of New york times top selling books", y = "Checkouts"))


###Aply bass model to each modern time series

m<-numeric()
p<-numeric()
q<-numeric()
publication_year<-numeric()
BM_R2<-numeric()
mt<-numeric()
pt<-numeric()
qt<-numeric()
publication_yeart<-numeric()

i<-0
par(mfrow=c(3,5))
# Looping through each file name
for (book in modern[["title"]][-13]) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  #data<-data[, PublicationYear := as.integer(gsub("[^0-9]+", "", PublicationYear))]
  data$PublicationYear <- as.integer(gsub("[^0-9]+", "", data$PublicationYear))
  data$PublicationYear <- as.integer(substr(data$PublicationYear, 1, 4))
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))


  
  # Creating a time series object
  if (max(data_summary$Checkouts)>10){
    ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
    #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
    # Adding the time series object to the list
    #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")

    ts_list[[book]] <- ts_object
    
    BM_ts<-BM(ts_object,display=F)
    pred_bmts<- make.instantaneous(predict(BM_ts, newx=c(1:length(ts_object))))
    #pred.instcas<- make.instantaneous(pred_bmcas)
    BM_R2<-c(BM_R2,BM_ts$Rsquared)
    if(all(BM_ts$Estimate["p-value"]<=0.05)){
    plot(ts_object, main=book,ylab="Checkouts",col.main="green")
    pred_bmts_ts <- ts(pred_bmts, frequency = frequency(ts_object), start = start(ts_object))
    
    
    # Overlay the fitted values on the plot
    lines(pred_bmts_ts, lwd = 2, col = "red")
    
    # Extract relevant information from BM_ts object
    estimates <- BM_ts$Estimate$Estimate
    std_errors <- BM_ts$Estimate$Std.Error
    p_values <- BM_ts$Estimate[["p-value"]]
    # Format p-value to scientific notation
    #formatted_p_values <- format(p_values, scientific = TRUE)
    #formatted_p_values <- sprintf("%.2e", p_values)
    significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
    summary(BM_ts)
    #for (i in seq_along(estimates)) {
    #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
    #       labels = paste( rownames(BM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
    #                      " \u00B1 ", round(std_errors[i], 3),
    #                      "\nP-Value:", significance_levels[i]),
    #       pos = 3, col = "black", cex = 0.8)
    #}
    #summary(BM_ts)
    R_squared=BM_ts$Rsquared
    text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
    publication_year<-c(publication_year,data[1,"PublicationYear"])
    m<-c(m,BM_ts$coefficients[1])
    p<-c(p,BM_ts$coefficients[2])
    q<-c(q,BM_ts$coefficients[3])
    


    }else{
      plot(ts_object, main=book,ylab="Checkouts", col.main="red")
      pred_bmts_ts <- ts(pred_bmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_bmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- BM_ts$Estimate$Estimate
      std_errors <- BM_ts$Estimate$Std.Error
      p_values <- BM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(BM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(BM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      R_squared=BM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
    }

  }
}
par(mfrow=c(1,1))

mtb<-m
ptb<-p
qtb<-q
publication_yeartb<-publication_year


m_corr <- cor(publication_year,m )
p_corr <- cor(publication_year,p )
q_corr <- cor(publication_year,q )
cat("Correlation between m and publication year:", m_corr, "\n")
cat("Correlation between p and publication year:", p_corr, "\n")
cat("Correlation between q and publication year:", q_corr, "\n")
# Create linear regression model
model_m <- lm(m ~ publication_year)
model_p <- lm(p ~ publication_year)
model_q <- lm(q ~ publication_year)
# Print summary of the model
summary(model_m)
summary(model_p)
summary(model_q)
# Plot the data points
par(mfrow=c(2,3))

# Add the linear regression line to the plot
plot(publication_year,m, main="m vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_m, col = "green")
text(x = min(publication_year)+2, y = mean(m), labels = paste("r =", round(m_corr, 3)), pos = 3, col = "green",cex=1.5)
plot(publication_year,p, main="p vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_p, col = "red")
text(x = min(publication_year)+2, y = mean(p)+0.01, labels = paste("r =", round(p_corr, 3)), pos = 3, col = "red",cex=1.5)
plot(publication_year,q, main="q vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_q, col = "green")
text(x = min(publication_year)+2, y = mean(q), labels = paste("r =", round(q_corr, 3)), pos = 3, col = "green",cex=1.5)

pm <- lm(p ~m)
pq <- lm(p ~ q)
qm <- lm(q ~ m)
plot(p,q,main="p vs q - Bass Model",pch=16)
cor(q,p)
abline(pq, col = "green")
text(x = mean(q), y = mean(p)+0.01, labels = paste("r =", round(cor(q,p), 3)), pos = 3, col = "green",cex=1.5)
plot(m,p)
cor(m,p)
abline(pm, col = "yellow")
text(x = mean(m), y = mean(p)+0.01, labels = paste("r =", round(cor(m,p), 3)), pos = 3, col = "yellow",cex=1.5)
plot(m,q)
cor(m,q)
abline(qm, col = "green")
text(x = mean(m), y = mean(q)+0.01, labels = paste("r =", round(cor(m,q), 3)), pos = 3, col = "green",cex=1.5)
#### Applying GBM


m<-numeric()
p<-numeric()
q<-numeric()
c<-numeric()
a<-numeric()
b<-numeric()
publication_year<-numeric()
GBM_R2<-numeric()

k<-1
par(mfrow=c(3,5))
# Looping through each file name
for (book in modern[["title"]][-13]) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  #data<-data[, PublicationYear := as.integer(gsub("[^0-9]+", "", PublicationYear))]
  data$PublicationYear <- as.integer(gsub("[^0-9]+", "", data$PublicationYear))
  data$PublicationYear <- as.integer(substr(data$PublicationYear, 1, 4))
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  #publication_year<-c(publication_year,data[1,"PublicationYear"])
  
  
  # Creating a time series object
  if (max(data_summary$Checkouts)>10){
    ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
    #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
    # Adding the time series object to the list
    #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
    
    ts_list[[book]] <- ts_object
    #if( k!=12){
    GBM_ts<-GBM(ts_object,display=F,shock = "rett",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,12,1))
    #}else if(k==3){
    #  GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,-0.5,2))      
    #}else if(k==12){
    #  GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,0.5,-1))
    #}
    #GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,-0.5,1))
    #k<-k+1
    GBM_R2<-c(GBM_R2,GBM_ts$Rsquared)
    #GBMr1tw<- GBM(tw,shock = "rett",nshock = 1,prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 24,38,-0.1))
    pred_gbmts<- make.instantaneous(predict(GBM_ts, newx=c(1:length(ts_object))))
    #pred.instcas<- make.instantaneous(pred_bmcas)
    
    if(all(GBM_ts$Estimate["p-value"][1:3,]<=0.05)){
      plot(ts_object, main=book,ylab="Checkouts",col.main = "green")
      pred_gbmts_ts <- ts(pred_gbmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_gbmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GBM_ts$Estimate$Estimate
      std_errors <- GBM_ts$Estimate$Std.Error
      p_values <- GBM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GBM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GBM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      R_squared=GBM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
     
      publication_year<-c(publication_year,data[1,"PublicationYear"])
      m<-c(m,GBM_ts$coefficients[1])
      p<-c(p,GBM_ts$coefficients[2])
      q<-c(q,GBM_ts$coefficients[3])
      a<-c(a,GBM_ts$coefficients[4])
      b<-c(b,GBM_ts$coefficients[5])
      c<-c(c,GBM_ts$coefficients[6])
    }else{
      plot(ts_object, main=book,ylab="Checkouts",col.main = "red")
      pred_gbmts_ts <- ts(pred_gbmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_gbmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GBM_ts$Estimate$Estimate
      std_errors <- GBM_ts$Estimate$Std.Error
      p_values <- GBM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GBM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GBM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #" \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      #R_squared=GBM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
      
      
      #m<-c(m,GBM_ts$coefficients[1])
      #p<-c(p,GBM_ts$coefficients[2])
      #q<-c(q,GBM_ts$coefficients[3])      
    }
    
  }
}
par(mfrow=c(1,1))
mtgbm<-m
ptgbm<-p
qtgbm<-q
publication_yeartgbm<-publication_year
R2_tilde<-(GBM_R2-BM_R2)/(1-BM_R2)
R2_tilde


###Highlights significant GBM model

m<-numeric()
p<-numeric()
q<-numeric()
publication_year<-numeric()
GBM_R2<-numeric()
R2_tilde<-numeric()

k<-1
par(mfrow=c(3,5))
# Looping through each file name
for (book in modern[["title"]][-13]) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  #data<-data[, PublicationYear := as.integer(gsub("[^0-9]+", "", PublicationYear))]
  data$PublicationYear <- as.integer(gsub("[^0-9]+", "", data$PublicationYear))
  data$PublicationYear <- as.integer(substr(data$PublicationYear, 1, 4))
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  publication_year<-c(publication_year,data[1,"PublicationYear"])
  
  
  # Creating a time series object
  if (max(data_summary$Checkouts)>10){
    ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
    #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
    # Adding the time series object to the list
    #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
    
    ts_list[[book]] <- ts_object
    #if( k!=12){
    GBM_ts<-GBM(ts_object,display=F,shock = "rett",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,12,1))
    #}else if(k==3){
    #  GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,-0.5,2))      
    #}else if(k==12){
    #  GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,0.5,-1))
    #}
    #GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,-0.5,1))
    #k<-k+1
    GBM_R2<-c(GBM_R2,GBM_ts$Rsquared)
    #GBMr1tw<- GBM(tw,shock = "rett",nshock = 1,prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 24,38,-0.1))
    pred_gbmts<- make.instantaneous(predict(GBM_ts, newx=c(1:length(ts_object))))
    #pred.instcas<- make.instantaneous(pred_bmcas)
    R2_tilde<-c(R2_tilde,round((GBM_R2[k]-BM_R2[k])/(1-BM_R2[k]),1))
    if(R2_tilde[k]>=0.3){
      plot(ts_object, main=book,ylab="Checkouts",col.main = "green")
      pred_gbmts_ts <- ts(pred_gbmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_gbmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GBM_ts$Estimate$Estimate
      std_errors <- GBM_ts$Estimate$Std.Error
      p_values <- GBM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GBM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GBM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      R_squared=GBM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
      
      m<-c(m,GBM_ts$coefficients[1])
      p<-c(p,GBM_ts$coefficients[2])
      q<-c(q,GBM_ts$coefficients[3])  
      #m[k]<-GBM_ts$coefficients[1]
      #p[k]<-GBM_ts$coefficients[2]
      #q[k]<-GBM_ts$coefficients[3]
    }else{
      plot(ts_object, main=book,ylab="Checkouts",col.main = "red")
      pred_gbmts_ts <- ts(pred_gbmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_gbmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GBM_ts$Estimate$Estimate
      std_errors <- GBM_ts$Estimate$Std.Error
      p_values <- GBM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GBM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #labels = paste( rownames(GBM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      R_squared=GBM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
      
      
      m<-c(m,GBM_ts$coefficients[1])
      p<-c(p,GBM_ts$coefficients[2])
      q<-c(q,GBM_ts$coefficients[3])      
    }
    k<-k+1
  }
}
par(mfrow=c(1,1))

m_corr <- cor(publication_year,m )
p_corr <- cor(publication_year,p )
q_corr <- cor(publication_year,q )
c_corr<-cor(publication_year,c)

a_corr <- cor(publication_year,a )
b_corr<-cor(publication_year,b)

cat("Correlation between m and publication year:", m_corr, "\n")
cat("Correlation between p and publication year:", p_corr, "\n")
cat("Correlation between q and publication year:", q_corr, "\n")
cat("Correlation between c and publication year:", c_corr, "\n")
cat("Correlation between a and publication year:", a_corr, "\n")
cat("Correlation between b and publication year:", b_corr, "\n")
# Create linear regression model
model_m <- lm(m ~ publication_year)
model_p <- lm(p ~ publication_year)
model_q <- lm(q ~ publication_year)
model_c<-lm(c~ publication_year)
model_a<-lm(a~ publication_year)
model_b<-lm(b~ publication_year)
# Print summary of the model
summary(model_m)
summary(model_p)
summary(model_q)
summary(model_c)
# Plot the data points

par(mfrow=c(2,3))
# Add the linear regression line to the plot
plot(publication_year,m, main="m vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_m, col = "red")
text(x = min(publication_year)+2, y = mean(m), labels = paste("r =", round(m_corr, 3)), pos = 3, col = "red",cex=1.5)
plot(publication_year,p, main="p vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_p, col = "red")
text(x = min(publication_year)+2, y = mean(p)+0.01, labels = paste("r =", round(p_corr, 3)), pos = 3, col = "red",cex=1.5)
plot(publication_year,q, main="q vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_q, col = "red")
text(x = min(publication_year)+2, y = mean(q), labels = paste("r =", round(q_corr, 3)), pos = 3, col = "red",cex=1.5)
plot(publication_year,c, main="c vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_c, col = "yellow")
text(x = min(publication_year)+2, y = mean(c), labels = paste("r =", round(c_corr, 3)), pos = 3, col = "yellow",cex=1.5)
plot(publication_year,a, main="a vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_a, col = "red")
text(x = min(publication_year)+2, y = mean(a), labels = paste("r =", round(a_corr, 3)), pos = 3, col = "red",cex=1.5)
plot(publication_year,b, main="b vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_b, col = "yellow")
text(x = min(publication_year)+2, y = mean(b), labels = paste("r =", round(b_corr, 3)), pos = 3, col = "yellow",cex=1.5)





par(mfrow=c(2,2))
pm <- lm(p ~m)
pq <- lm(p ~ q)
qm <- lm(q ~ m)
bc<-lm(b~ c)
plot(q,p, main="q vs p -GBM model")
cor(q,p)
abline(pq, col = "green")
text(x = mean(q), y = mean(p)+0.01, labels = paste("r =", round(cor(q,p), 3)), pos = 3, col = "green",cex=1.5)
plot(m,p,main="m vs p -GBM model")
cor(m,p)
abline(pm, col = "red")
text(x = mean(m), y = mean(p)+0.01, labels = paste("r =", round(cor(m,p), 3)), pos = 3, col = "red",cex=1.5)
plot(m,q,main="m vs q -GBM model")
cor(m,q)
abline(qm, col = "yellow")
text(x = mean(m), y = mean(q)+0.01, labels = paste("r =", round(cor(m,q), 3)), pos = 3, col = "yellow",cex=1.5)

plot(c,b,main="b vs c -GBM model")
cor(c,b)
abline(bc, col = "yellow")
text(x = mean(c), y = mean(b)+0.01, labels = paste("r =", round(cor(c,b), 3)), pos = 3, col = "yellow",cex=1.5)


####GGM

#GGM_tw<- GGM(tw, prelimestimates=c(4.463368e+04, 0.001, 0.01, 1.923560e-03, 9.142022e-02))

K<-numeric()
pc<-numeric()
qc<-numeric()
ps<-numeric()
qs<-numeric()
publication_year<-numeric()
GGM_R2<-numeric()
R2_tilde1<-numeric()



k<-1
par(mfrow=c(3,5))
# Looping through each file name
for (book in modern[["title"]][-13]) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  #data<-data[, PublicationYear := as.integer(gsub("[^0-9]+", "", PublicationYear))]
  data$PublicationYear <- as.integer(gsub("[^0-9]+", "", data$PublicationYear))
  data$PublicationYear <- as.integer(substr(data$PublicationYear, 1, 4))
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  publication_year<-c(publication_year,data[1,"PublicationYear"])
  
  
  # Creating a time series object
  if (max(data_summary$Checkouts)>10){
    ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
    #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
    # Adding the time series object to the list
    #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
    
    ts_list[[book]] <- ts_object
    
    GGM_ts<-GGM(ts_object,display=F, prelimestimates=c(4.463368e+04, 0.001, 0.01, 1.923560e-03, 9.142022e-02))
    GGM_R2<-c(GGM_R2,GGM_ts$Rsquared)
    
    pred_ggmts<- make.instantaneous(predict(GGM_ts, newx=c(1:length(ts_object))))
    #pred.instcas<- make.instantaneous(pred_bmcas)
    R2_tilde1<-c(R2_tilde1,round((GGM_R2[k]-BM_R2[k])/(1-BM_R2[k]),1))
    if(R2_tilde1[k]>=0.3){
      plot(ts_object, main=book,ylab="Checkouts",col.main = "green")
      pred_ggmts_ts <- ts(pred_ggmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_ggmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GGM_ts$Estimate$Estimate
      std_errors <- GGM_ts$Estimate$Std.Error
      p_values <- GGM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GGM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GGM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      R_squared=GGM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
      
      
      K<-c(K,GGM_ts$coefficients[1])
      pc<-c(pc,GGM_ts$coefficients[2])
      qc<-c(qc,GGM_ts$coefficients[3])
      ps<-c(ps,GGM_ts$coefficients[4])
      qs<-c(qs,GGM_ts$coefficients[5])      
    }else{
      plot(ts_object, main=book,ylab="Checkouts",col.main = "red")
      pred_ggmts_ts <- ts(pred_ggmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_ggmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GGM_ts$Estimate$Estimate
      std_errors <- GGM_ts$Estimate$Std.Error
      p_values <- GGM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GGM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GGM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      R_squared=GGM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
      
      
      K<-c(K,GGM_ts$coefficients[1])
      pc<-c(pc,GGM_ts$coefficients[2])
      qc<-c(qc,GGM_ts$coefficients[3])
      ps<-c(ps,GGM_ts$coefficients[4])
      qs<-c(qs,GGM_ts$coefficients[5])      
    }
    k<k+1
  }
}
par(mfrow=c(1,1))
R2_tilde1<-((GGM_R2-BM_R2)/(1-BM_R2))
R2_tilde1


#######Highlighting fits with atleast 3 significant coefficients

K<-numeric()
pc<-numeric()
qc<-numeric()
ps<-numeric()
qs<-numeric()
publication_year<-numeric()
GGM_R2<-numeric()
R2_tilde1<-numeric()



k<-1
par(mfrow=c(3,5))
# Looping through each file name
for (book in modern[["title"]][-13]) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  #data<-data[, PublicationYear := as.integer(gsub("[^0-9]+", "", PublicationYear))]
  data$PublicationYear <- as.integer(gsub("[^0-9]+", "", data$PublicationYear))
  data$PublicationYear <- as.integer(substr(data$PublicationYear, 1, 4))
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  #publication_year<-c(publication_year,data[1,"PublicationYear"])
  
  
  # Creating a time series object
  if (max(data_summary$Checkouts)>10){
    ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
    #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
    # Adding the time series object to the list
    #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
    
    ts_list[[book]] <- ts_object
    
    GGM_ts<-GGM(ts_object,display=F, prelimestimates=c(4.463368e+04, 0.001, 0.01, 1.923560e-03, 9.142022e-02))
    GGM_R2<-c(GGM_R2,GGM_ts$Rsquared)
    
    pred_ggmts<- make.instantaneous(predict(GGM_ts, newx=c(1:length(ts_object))))
    #pred.instcas<- make.instantaneous(pred_bmcas)
    #R2_tilde1<-c(R2_tilde1,round((GGM_R2[k]-BM_R2[k])/(1-BM_R2[k]),1))
    if(all(GGM_ts$Estimate["p-value"][3:5,]<=0.05)){
      plot(ts_object, main=book,ylab="Checkouts",col.main = "green")
      pred_ggmts_ts <- ts(pred_ggmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_ggmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GGM_ts$Estimate$Estimate
      std_errors <- GGM_ts$Estimate$Std.Error
      p_values <- GGM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GGM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GGM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      R_squared=GGM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
      
      publication_year<-c(publication_year,data[1,"PublicationYear"])
      K<-c(K,GGM_ts$coefficients[1])
      pc<-c(pc,GGM_ts$coefficients[2])
      qc<-c(qc,GGM_ts$coefficients[3])
      ps<-c(ps,GGM_ts$coefficients[4])
      qs<-c(qs,GGM_ts$coefficients[5])      
    }else{
      plot(ts_object, main=book,ylab="Checkouts",col.main = "red")
      pred_ggmts_ts <- ts(pred_ggmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_ggmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GGM_ts$Estimate$Estimate
      std_errors <- GGM_ts$Estimate$Std.Error
      p_values <- GGM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GGM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GGM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      R_squared=GGM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
      
      
      #K<-c(K,GGM_ts$coefficients[1])
      #pc<-c(pc,GGM_ts$coefficients[2])
      #qc<-c(qc,GGM_ts$coefficients[3])
      #ps<-c(ps,GGM_ts$coefficients[4])
      #qs<-c(qs,GGM_ts$coefficients[5])      
    }
    k<k+1
  }
}
par(mfrow=c(1,1))
R2_tilde1<-((GGM_R2-BM_R2)/(1-BM_R2))
R2_tilde1










Ktggm<-K
pctggm<-pc
qctggm<-qc
pstggm<-ps
qstggm<-qs
publication_yeartggm<-publication_year


K_corr <- cor(publication_year,K)
pc_corr <- cor(publication_year,pc )
qc_corr <- cor(publication_year,qc )
ps_corr <- cor(publication_year,ps )
qs_corr <- cor(publication_year,qs )
cat("Correlation between K and publication year:", K_corr, "\n")
cat("Correlation between pc and publication year:", pc_corr, "\n")
cat("Correlation between qc and publication year:", qc_corr, "\n")
cat("Correlation between ps and publication year:", ps_corr, "\n")
cat("Correlation between qs and publication year:", qs_corr, "\n")
# Create linear regression model
model_K <- lm(K ~ publication_year)
model_pc <- lm(pc ~ publication_year)
model_qc <- lm(qc ~ publication_year)
model_ps <- lm(ps ~ publication_year)
model_qs <- lm(qs ~ publication_year)
# Print summary of the model
summary(model_K)
summary(model_pc)
summary(model_qc)
summary(model_ps)
summary(model_qs)
# Plot the data points


# Add the linear regression line to the plot
plot(publication_year,K, main="K vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_K, col = "red")
par(mfrow=c(2,2))
text(x = min(publication_year)+2, y = mean(K), labels = paste("r =", round(K_corr, 3)), pos = 3, col = "red",cex=1.5)
plot(publication_year,pc, main="pc vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_pc, col = "green")
text(x = min(publication_year)+2, y = mean(pc), labels = paste("r =", round(pc_corr, 3)), pos = 3, col = "green",cex=1.5)
plot(publication_year,qc, main="qc vs publication year",xlab="Publication year",pch=16,col="black",ylim=c(-1,0.5))
abline(model_qc, col = "green")
text(x = min(publication_year)+2, y = mean(qc)-0.5, labels = paste("r =", round(qc_corr, 3)), pos = 3, col = "green",cex=1.5)
plot(publication_year,ps, main="ps vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_ps, col = "green")
text(x = min(publication_year)+2, y = mean(ps), labels = paste("r =", round(ps_corr, 3)), pos = 3, col = "green",cex=1.5)
plot(publication_year,qs, main="qs vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_qs, col = "red")
text(x = min(publication_year)+2, y = mean(qs), labels = paste("r =", round(qs_corr, 3)), pos = 3, col = "red",cex=1.5)

#bm_cass<-BM(cassette,display = T)
#summary(bm_cass)

###prediction (out-of-sample)
#pred_bmcas<- predict(bm_cass, newx=c(1:50))
#pred.instcas<- make.instantaneous(pred_bmcas)

###plot of fitted model 
#plot(cassette, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)#xaxt="n" removes the x axis labels and ticks, cex=0.6 make the points and text on the plot smaller
#axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
#lines(pred.instcas, lwd=2, col=2)


#####Amazon Best selling books
amazon_top1<-c("HARRY POTTER AND THE HALF-BLOOD PRINCE","HARRY POTTER AND THE DEATHLY HALLOWS","A NEW EARTH","THE LOST SYMBOL","THE GIRL WHO KICKED THE HORNET’S NEST","STEVE JOBS","FIFTY SHADES OF GREY","LEAN IN","THE LIFE CHANGING MAGIC OF TIDYING UP","HARRY POTTER AND THE CURSED CHILD","WONDER","BECOMING","WHERE THE CRAWDADS SING","A PROMISED LAND","ATOMIC HABITS","IT ENDS WITH US")



i<-0
# Looping through each file name
for (book in amazon_top1) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  
  # Creating a time series object
  if (max(data_summary$Checkouts)>10){
    ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
    #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
    # Adding the time series object to the list
    #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
    
    ts_list[[book]] <- ts_object
    
    plot(ts_object, main=book,ylab="Checkouts")
    if (i==0){
      base_plot <- autoplot(ts_list[[book]], series = book)
    }else{
      base_plot <- base_plot + autolayer(ts_list[[book]], series = book)
    }
    i<-i+1
  }
  
}
print(base_plot + labs(title = "Time Series of Amazon best selling books", y = "Checkouts"))


###Aply bass model to each modern time series

m<-numeric()
p<-numeric()
q<-numeric()
publication_year<-numeric()
BM_R2<-numeric()

i<-0
par(mfrow=c(4,4))
# Looping through each file name
for (book in amazon_top1) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  #data<-data[, PublicationYear := as.integer(gsub("[^0-9]+", "", PublicationYear))]
  data$PublicationYear <- as.integer(gsub("[^0-9]+", "", data$PublicationYear))
  data$PublicationYear <- as.integer(substr(data$PublicationYear, 1, 4))
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  
  
  
  # Creating a time series object
  if (max(data_summary$Checkouts)>10){
    ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
    #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
    # Adding the time series object to the list
    #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
    
    ts_list[[book]] <- ts_object
    
    BM_ts<-BM(ts_object,display=F)
    pred_bmts<- make.instantaneous(predict(BM_ts, newx=c(1:length(ts_object))))
    #pred.instcas<- make.instantaneous(pred_bmcas)
    BM_R2<-c(BM_R2,BM_ts$Rsquared)
    if(all(BM_ts$Estimate["p-value"]<=0.05)){
      plot(ts_object, main=book,ylab="Checkouts",col.main="green")
      pred_bmts_ts <- ts(pred_bmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_bmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- BM_ts$Estimate$Estimate
      std_errors <- BM_ts$Estimate$Std.Error
      p_values <- BM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(BM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(BM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      #summary(BM_ts)
      R_squared=BM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")


      
      
    }else{
      plot(ts_object, main=book,ylab="Checkouts", col.main="red")
      pred_bmts_ts <- ts(pred_bmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_bmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- BM_ts$Estimate$Estimate
      std_errors <- BM_ts$Estimate$Std.Error
      p_values <- BM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(BM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
       #labels = paste( rownames(BM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
       #                      "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      R_squared=BM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
    }
    m<-c(m,BM_ts$coefficients[1])
    p<-c(p,BM_ts$coefficients[2])
    q<-c(q,BM_ts$coefficients[3])
    publication_year<-c(publication_year,data[1,"PublicationYear"])
    mtb<-c(mtb,BM_ts$coefficients[1])
    ptb<-c(ptb,BM_ts$coefficients[2])
    qtb<-c(qtb,BM_ts$coefficients[3])
    publication_yeartb<-c(publication_yeartb,data[1,"PublicationYear"])
  }
}
par(mfrow=c(1,1))


##AMAZON
m_corr <- cor(publication_year,m )
p_corr <- cor(publication_year,p )
q_corr <- cor(publication_year,q )
cat("Correlation between m and publication year:", m_corr, "\n")
cat("Correlation between p and publication year:", p_corr, "\n")
cat("Correlation between q and publication year:", q_corr, "\n")
# Create linear regression model
model_m <- lm(m ~ publication_year)
model_p <- lm(p ~ publication_year)
model_q <- lm(q ~ publication_year)
# Print summary of the model
summary(model_m)
summary(model_p)
summary(model_q)
# Plot the data points


# Add the linear regression line to the plot
plot(publication_year,m, main="m vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_m, col = "red")
text(x = min(publication_year)+2, y = mean(m), labels = paste("r =", round(m_corr, 3)), pos = 3, col = "red",cex=1.5)
plot(publication_year,p, main="p vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_p, col = "red")
text(x = min(publication_year)+2, y = mean(p), labels = paste("r =", round(p_corr, 3)), pos = 3, col = "red",cex=1.5)
plot(publication_year,q, main="q vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_q, col = "red")
text(x = min(publication_year)+2, y = mean(q), labels = paste("r =", round(q_corr, 3)), pos = 3, col = "red",cex=1.5)


#TOTAL


mtb_corr <- cor(publication_yeartb, mtb)
ptb_corr <- cor(publication_yeartb, ptb)
qtb_corr <- cor(publication_yeartb, qtb)

cat("Correlation between mtb and publication_yeartb:", mtb_corr, "\n")
cat("Correlation between ptb and publication_yeartb:", ptb_corr, "\n")
cat("Correlation between qtb and publication_yeartb:", qtb_corr, "\n")

# Create linear regression model
model_mtb <- lm(mtb ~ publication_yeartb)
model_ptb <- lm(ptb ~ publication_yeartb)
model_qtb <- lm(qtb ~ publication_yeartb)

# Print summary of the model
summary(model_mtb)
summary(model_ptb)
summary(model_qtb)

# Plot the data points
plot(publication_yeartb, mtb, main="mtb vs publication_yeartb", xlab="Publication year", pch=16, col="black")
abline(model_mtb, col = "red")
text(x = min(publication_yeartb) + 2, y = mean(mtb), labels = paste("r =", round(mtb_corr, 3)), pos = 3, col = "red", cex=1.5)

plot(publication_yeartb, ptb, main="ptb vs publication_yeartb", xlab="Publication year", pch=16, col="black")
abline(model_ptb, col = "red")
text(x = min(publication_yeartb) + 2, y = mean(ptb), labels = paste("r =", round(ptb_corr, 3)), pos = 3, col = "red", cex=1.5)

plot(publication_yeartb, qtb, main="qtb vs publication_yeartb", xlab="Publication year", pch=16, col="black")
abline(model_qtb, col = "yellow")
text(x = min(publication_yeartb) + 2, y = mean(qtb), labels = paste("r =", round(qtb_corr, 3)), pos = 3, col = "yellow", cex=1.5)



pm <- lm(p ~m)
pq <- lm(p ~ q)
qm <- lm(q ~ m)
plot(p,q)
cor(p,q)
abline(pq, col = "green")
text(x = mean(q), y = mean(p)+0.01, labels = paste("r =", round(cor(p,q), 3)), pos = 3, col = "green",cex=1.5)
plot(m,p)
cor(m,p)
abline(pm, col = "green")
text(x = mean(m), y = mean(p)+0.01, labels = paste("r =", round(cor(m,p), 3)), pos = 3, col = "green",cex=1.5)
plot(m,q)
cor(m,q)
abline(qm, col = "red")
text(x = mean(m), y = mean(q)+0.01, labels = paste("r =", round(cor(m,q), 3)), pos = 3, col = "red",cex=1.5)


#### Applying GBM


m<-numeric()
p<-numeric()
q<-numeric()
publication_year<-numeric()
GBM_R2<-numeric()

k<-1
par(mfrow=c(4,4))
# Looping through each file name
for (book in amazon_top1) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  #data<-data[, PublicationYear := as.integer(gsub("[^0-9]+", "", PublicationYear))]
  data$PublicationYear <- as.integer(gsub("[^0-9]+", "", data$PublicationYear))
  data$PublicationYear <- as.integer(substr(data$PublicationYear, 1, 4))
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  publication_year<-c(publication_year,data[1,"PublicationYear"])
  #publication_yeartgbm<-c(publication_yeartgbm,data[1,"PublicationYear"])
  
  # Creating a time series object
  if (max(data_summary$Checkouts)>10){
    ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
    #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
    # Adding the time series object to the list
    #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
    
    ts_list[[book]] <- ts_object
    #if( k!=12){
    GBM_ts<-GBM(ts_object,display=F,shock = "rett",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,12,1))
    #}else if(k==3){
    #  GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,-0.5,2))      
    #}else if(k==12){
    #  GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,0.5,-1))
    #}
    #GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,-0.5,1))
    #k<-k+1
    GBM_R2<-c(GBM_R2,GBM_ts$Rsquared)
    #GBMr1tw<- GBM(tw,shock = "rett",nshock = 1,prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 24,38,-0.1))
    pred_gbmts<- make.instantaneous(predict(GBM_ts, newx=c(1:length(ts_object))))
    #pred.instcas<- make.instantaneous(pred_bmcas)
    
    if(all(GBM_ts$Estimate["p-value"][1:3,]<=0.05)){
      plot(ts_object, main=book,ylab="Checkouts",col.main = "black")
      pred_gbmts_ts <- ts(pred_gbmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_gbmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GBM_ts$Estimate$Estimate
      std_errors <- GBM_ts$Estimate$Std.Error
      p_values <- GBM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GBM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GBM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
    #                         "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      R_squared=GBM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
      
      m<-c(m,GBM_ts$coefficients[1])
      p<-c(p,GBM_ts$coefficients[2])
      q<-c(q,GBM_ts$coefficients[3])
      #mtgbm<-c(mtgbm,GBM_ts$coefficients[1])
      #ptgbm<-c(ptgbm,GBM_ts$coefficients[2])
      #qtgbm<-c(qtgbm,GBM_ts$coefficients[3])
    }else{
      plot(ts_object, main=book,ylab="Checkouts",col.main = "black")
      pred_gbmts_ts <- ts(pred_gbmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_gbmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GBM_ts$Estimate$Estimate
      std_errors <- GBM_ts$Estimate$Std.Error
      p_values <- GBM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GBM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GBM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      R_squared=GBM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
      
      m<-c(m,GBM_ts$coefficients[1])
      p<-c(p,GBM_ts$coefficients[2])
      q<-c(q,GBM_ts$coefficients[3])
      #mtgbm<-c(mtgbm,GBM_ts$coefficients[1])
      #ptgbm<-c(ptgbm,GBM_ts$coefficients[2])
      #qtgbm<-c(qtgbm,GBM_ts$coefficients[3])
    }
    
  }
}
par(mfrow=c(1,1))


R2_tilde<-(GBM_R2-BM_R2)/(1-BM_R2)
R2_tilde


###Highlights significant GBM model

m<-numeric()
p<-numeric()
q<-numeric()
publication_year<-numeric()
GBM_R2<-numeric()
R2_tilde<-numeric()

k<-1
par(mfrow=c(4,4))
# Looping through each file name
for (book in amazon_top1) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  #data<-data[, PublicationYear := as.integer(gsub("[^0-9]+", "", PublicationYear))]
  data$PublicationYear <- as.integer(gsub("[^0-9]+", "", data$PublicationYear))
  data$PublicationYear <- as.integer(substr(data$PublicationYear, 1, 4))
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  publication_year<-c(publication_year,data[1,"PublicationYear"])
  publication_yeartgbm<-c(publication_yeartgbm,data[1,"PublicationYear"])
  
  # Creating a time series object
  if (max(data_summary$Checkouts)>10){
    ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
    #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
    # Adding the time series object to the list
    #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
    
    ts_list[[book]] <- ts_object
    if( k!=11){
    GBM_ts<-GBM(ts_object,display=F,shock = "rett",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,12,1))
    #}else if(k==3){
    #  GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,-0.5,2))      
    }else {
      GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1, prelimestimates = c(BM(ts_object, display=FALSE)$Estimate[1,1],
                                                                                     BM(ts_object, display=FALSE)$Estimate[2,1],
                                                                                     BM(ts_object, display=FALSE)$Estimate[3,1],
                                                                                     70,-0.5,10))
    }
    #GBM_ts<-GBM(ts_object,display=F,shock = "exp",nshock = 1,prelimestimates = c(10000, 0.01, 0.1, 1,-0.5,1))
    #k<-k+1
    GBM_R2<-c(GBM_R2,GBM_ts$Rsquared)
    #GBMr1tw<- GBM(tw,shock = "rett",nshock = 1,prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 24,38,-0.1))
    pred_gbmts<- make.instantaneous(predict(GBM_ts, newx=c(1:length(ts_object))))
    #pred.instcas<- make.instantaneous(pred_bmcas)
    R2_tilde<-c(R2_tilde,round((GBM_R2[k]-BM_R2[k])/(1-BM_R2[k]),1))
    if(R2_tilde[k]>=0.3){
      plot(ts_object, main=book,ylab="Checkouts",col.main = "green")
      pred_gbmts_ts <- ts(pred_gbmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_gbmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GBM_ts$Estimate$Estimate
      std_errors <- GBM_ts$Estimate$Std.Error
      p_values <- GBM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GBM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
       #      labels = paste( rownames(GBM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2", " =", round(GBM_ts$Rsquared,3)), pos = 4, cex = 0.8, col = "black")
      
      
      #m[k]<-GBM_ts$coefficients[1]
      #p[k]<-GBM_ts$coefficients[2]
      #q[k]<-GBM_ts$coefficients[3]
      m<-c(m,GBM_ts$coefficients[1])
      p<-c(p,GBM_ts$coefficients[2])
      q<-c(q,GBM_ts$coefficients[3])
      mtgbm<-c(mtgbm,GBM_ts$coefficients[1])
      ptgbm<-c(ptgbm,GBM_ts$coefficients[2])
      qtgbm<-c(qtgbm,GBM_ts$coefficients[3])
    }else{
      plot(ts_object, main=book,ylab="Checkouts",col.main = "red")
      pred_gbmts_ts <- ts(pred_gbmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_gbmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GBM_ts$Estimate$Estimate
      std_errors <- GBM_ts$Estimate$Std.Error
      p_values <- GBM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GBM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GBM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
        #}
      R_squared=GBM_ts$Rsquared
      text(x=median(time(ts_object)),y=2*max(ts_object)/3,paste("R^2 =", round(R_squared, 3)), pos = 4, cex = 0.8, col = "black")
          
      m<-c(m,GBM_ts$coefficients[1])
      p<-c(p,GBM_ts$coefficients[2])
      q<-c(q,GBM_ts$coefficients[3])    
      mtgbm<-c(mtgbm,GBM_ts$coefficients[1])
      ptgbm<-c(ptgbm,GBM_ts$coefficients[2])
      qtgbm<-c(qtgbm,GBM_ts$coefficients[3])
    }
    k<-k+1
  }
}
par(mfrow=c(1,1))

#Amazon
m_corr <- cor(publication_year,m )
p_corr <- cor(publication_year,p )
q_corr <- cor(publication_year,q )
cat("Correlation between m and publication year:", m_corr, "\n")
cat("Correlation between p and publication year:", p_corr, "\n")
cat("Correlation between q and publication year:", q_corr, "\n")
# Create linear regression model
model_m <- lm(m ~ publication_year)
model_p <- lm(p ~ publication_year)
model_q <- lm(q ~ publication_year)
# Print summary of the model
summary(model_m)
summary(model_p)
summary(model_q)
# Plot the data points

par(mfrow=c(3,1))
# Add the linear regression line to the plot
plot(publication_year,m, main="m vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_m, col = "red")
text(x = min(publication_year)+2, y = mean(m), labels = paste("r =", round(m_corr, 3)), pos = 3, col = "red",cex=1.5)
plot(publication_year,p, main="p vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_p, col = "red")
text(x = min(publication_year)+2, y = mean(p), labels = paste("r =", round(p_corr, 3)), pos = 3, col = "red",cex=1.5)
plot(publication_year,q, main="q vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_q, col = "green")
text(x = min(publication_year)+2, y = mean(q), labels = paste("r =", round(q_corr, 3)), pos = 3, col = "green",cex=1.5)
par(mfrow=c(1,1))
par(mfrow=c(3,1))
pm <- lm(p ~m)
pq <- lm(p ~ q)
qm <- lm(q ~ m)
plot(p,q)
cor(q,p)
abline(pq, col = "green")
text(x = mean(q), y = mean(p)+0.01, labels = paste("r =", round(cor(q,p), 3)), pos = 3, col = "green",cex=1.5)
plot(m,p)
cor(m,p)
abline(pm, col = "green")
text(x = mean(m), y = mean(p)+0.01, labels = paste("r =", round(cor(m,p), 3)), pos = 3, col = "green",cex=1.5)
plot(m,q)
cor(m,q)
abline(qm, col = "red")
text(x = mean(m), y = mean(q)+0.01, labels = paste("r =", round(cor(m,q), 3)), pos = 3, col = "red",cex=1.5)
par(mfrow=c(1,1))

#Total

mtgbm_corr <- cor(publication_yeartgbm, mtgbm)
ptgbm_corr <- cor(publication_yeartgbm, ptgbm)
qtgm_corr <- cor(publication_yeartgbm, qtgbm)

cat("Correlation between mtgbm and publication_yeartgbm:", mtgbm_corr, "\n")
cat("Correlation between ptgbm and publication_yeartgbm:", ptgbm_corr, "\n")
cat("Correlation between qtgm and publication_yeartgbm:", qtgm_corr, "\n")

# Create linear regression model
model_mtgbm <- lm(mtgbm ~ publication_yeartgbm)
model_ptgbm <- lm(ptgbm ~ publication_yeartgbm)
model_qtgbm <- lm(qtgbm ~ publication_yeartgbm)

# Print summary of the model
summary(model_mtgbm)
summary(model_ptgbm)
summary(model_qtgbm)

# Plot the data points
par(mfrow=c(3,1))
plot(publication_yeartgbm, mtgbm, main="m vs publication_year", xlab="Publication year", pch=16, col="black")
abline(model_mtgbm, col = "red")
text(x = min(publication_yeartgbm) + 2, y = mean(mtgbm), labels = paste("r =", round(mtgbm_corr, 3)), pos = 3, col = "red", cex=1.5)

plot(publication_yeartgbm, ptgbm, main="pt vs publication_year", xlab="Publication year", pch=16, col="black")
abline(model_ptgbm, col = "red")
text(x = min(publication_yeartgbm) + 2, y = mean(ptgbm), labels = paste("r =", round(ptgbm_corr, 3)), pos = 3, col = "red", cex=1.5)

plot(publication_yeartgbm, qtgbm, main="qt vs publication_year", xlab="Publication year", pch=16, col="black")
abline(model_qtgbm, col = "yellow")
text(x = min(publication_yeartgbm) + 2, y = mean(qtgbm), labels = paste("r =", round(qtgm_corr, 3)), pos = 3, col = "yellow", cex=1.5)
par(mfrow=c(1,1))





####GGM

#GGM_tw<- GGM(tw, prelimestimates=c(4.463368e+04, 0.001, 0.01, 1.923560e-03, 9.142022e-02))

K<-numeric()
pc<-numeric()
qc<-numeric()
ps<-numeric()
qs<-numeric()
publication_year<-numeric()
GGM_R2<-numeric()
R2_tilde1<-numeric()

k<-1
par(mfrow=c(4,4))
# Looping through each file name
for (book in amazon_top1) {
  # Reading the CSV file
  data <- read.csv(paste0(book, ".csv"))
  
  # Filtering for BOOKs
  data <- data[data$MaterialType == "BOOK", ]
  #data_e<- data[data$MaterialType == "EBOOK", ]
  #data<-data[, PublicationYear := as.integer(gsub("[^0-9]+", "", PublicationYear))]
  data$PublicationYear <- as.integer(gsub("[^0-9]+", "", data$PublicationYear))
  data$PublicationYear <- as.integer(substr(data$PublicationYear, 1, 4))
  # Summarizing data
  data_summary <- data %>%
    group_by(CheckoutYear, CheckoutMonth) %>%
    summarise(Checkouts = sum(Checkouts))
  #data_summary_e <- data_e %>%
  #  group_by(CheckoutYear, CheckoutMonth) %>%
  #  summarise(Checkouts = sum(Checkouts))
  publication_year<-c(publication_year,data[1,"PublicationYear"])
  publication_yeartggm<-c(publication_yeartggm,data[1,"PublicationYear"])
  
  
  # Creating a time series object
  if (max(data_summary$Checkouts)>10){
    ts_object <- ts(data_summary$Checkouts, frequency = 12, start = c(data_summary$CheckoutYear[1], data_summary$CheckoutMonth[1]))
    #ts_object_e <- ts(data_summary_e$Checkouts, frequency = 12, start = c(data_summary_e$CheckoutYear[1], data_summary_e$CheckoutMonth[1]))  
    # Adding the time series object to the list
    #autoplot(ts_object, series = "book")+autolayer(ts_object_e, series = "ebook")+labs(title = book, y = "Checkouts")
    
    ts_list[[book]] <- ts_object
    
    GGM_ts<-GGM(ts_object,display=F, prelimestimates=c(4.463368e+04, 0.001, 0.01, 1.923560e-03, 9.142022e-02))
    GGM_R2<-c(GGM_R2,GGM_ts$Rsquared)
    
    pred_ggmts<- make.instantaneous(predict(GGM_ts, newx=c(1:length(ts_object))))
    #pred.instcas<- make.instantaneous(pred_bmcas)
    R2_tilde1<-c(R2_tilde1,round((GGM_R2[k]-BM_R2[k])/(1-BM_R2[k]),1))
    if(R2_tilde1[k]>=0.3){
      plot(ts_object, main=book,ylab="Checkouts",col.main = "green")
      pred_ggmts_ts <- ts(pred_ggmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_ggmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GGM_ts$Estimate$Estimate
      std_errors <- GGM_ts$Estimate$Std.Error
      p_values <- GGM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GGM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GGM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      
      
      K<-c(K,GGM_ts$coefficients[1])
      pc<-c(pc,GGM_ts$coefficients[2])
      qc<-c(qc,GGM_ts$coefficients[3])
      ps<-c(ps,GGM_ts$coefficients[4])
      qs<-c(qs,GGM_ts$coefficients[5]) 
      Ktggm<-c(Ktggm,GGM_ts$coefficients[1])
      pctggm<-c(pctggm,GGM_ts$coefficients[2])
      qctggm<-c(qctggm,GGM_ts$coefficients[3])
      pstggm<-c(pstggm,GGM_ts$coefficients[4])
      qstggm<-c(qstggm,GGM_ts$coefficients[5])    
    }else{
      plot(ts_object, main=book,ylab="Checkouts",col.main = "black")
      pred_ggmts_ts <- ts(pred_ggmts, frequency = frequency(ts_object), start = start(ts_object))
      
      
      # Overlay the fitted values on the plot
      lines(pred_ggmts_ts, lwd = 2, col = "red")
      
      # Extract relevant information from BM_ts object
      estimates <- GGM_ts$Estimate$Estimate
      std_errors <- GGM_ts$Estimate$Std.Error
      p_values <- GGM_ts$Estimate[["p-value"]]
      # Format p-value to scientific notation
      #formatted_p_values <- format(p_values, scientific = TRUE)
      #formatted_p_values <- sprintf("%.2e", p_values)
      significance_levels <- ifelse(p_values <= 0.01, " <= 0.01", ifelse(p_values <= 0.05, "<= 0.05", "Not Significant"))
      summary(GGM_ts)
      #for (i in seq_along(estimates)) {
      #  text(x = median(time(ts_object)+2), y = max(ts_object) - i*max(ts_object)/3, 
      #       labels = paste( rownames(GGM_ts$Estimate)[i],"  Estimate :", round(estimates[i], 3),
      #                       " \u00B1 ", round(std_errors[i], 3),
      #                       "\nP-Value:", significance_levels[i]),
      #       pos = 3, col = "black", cex = 0.8)
      #}
      
      
      K<-c(K,GGM_ts$coefficients[1])
      pc<-c(pc,GGM_ts$coefficients[2])
      qc<-c(qc,GGM_ts$coefficients[3])
      ps<-c(ps,GGM_ts$coefficients[4])
      qs<-c(qs,GGM_ts$coefficients[5])
      Ktggm<-c(Ktggm,GGM_ts$coefficients[1])
      pctggm<-c(pctggm,GGM_ts$coefficients[2])
      qctggm<-c(qctggm,GGM_ts$coefficients[3])
      pstggm<-c(pstggm,GGM_ts$coefficients[4])
      qstggm<-c(qstggm,GGM_ts$coefficients[5]) 
    }
    k<k+1
  }
}
par(mfrow=c(1,1))
R2_tilde1<-((GGM_R2-BM_R2)/(1-BM_R2))
R2_tilde1




K_corr <- cor(publication_year,K)
pc_corr <- cor(publication_year,pc )
qc_corr <- cor(publication_year,qc )
ps_corr <- cor(publication_year,ps )
qs_corr <- cor(publication_year,qs )
cat("Correlation between K and publication year:", K_corr, "\n")
cat("Correlation between pc and publication year:", pc_corr, "\n")
cat("Correlation between qc and publication year:", qc_corr, "\n")
cat("Correlation between ps and publication year:", ps_corr, "\n")
cat("Correlation between qs and publication year:", qs_corr, "\n")
# Create linear regression model
model_K <- lm(K ~ publication_year)
model_pc <- lm(pc ~ publication_year)
model_qc <- lm(qc ~ publication_year)
model_ps <- lm(ps ~ publication_year)
model_qs <- lm(qs ~ publication_year)
# Print summary of the model
summary(model_K)
summary(model_pc)
summary(model_qc)
summary(model_ps)
summary(model_qs)
# Plot the data points


# Add the linear regression line to the plot
plot(publication_year,K, main="K vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_K, col = "yellow")
text(x = min(publication_year)+2, y = mean(K), labels = paste("r =", round(K_corr, 3)), pos = 3, col = "yellow",cex=1.5)
plot(publication_year,pc, main="pc vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_pc, col = "red")
text(x = min(publication_year)+2, y = mean(pc), labels = paste("r =", round(pc_corr, 3)), pos = 3, col = "red",cex=1.5)
plot(publication_year,qc, main="qc vs publication year",xlab="Publication year",pch=16,col="black",ylim=c(-1,0.5))
abline(model_qc, col = "green")
text(x = min(publication_year)+2, y = mean(qc)-0.5, labels = paste("r =", round(qc_corr, 3)), pos = 3, col = "green",cex=1.5)
plot(publication_year,ps, main="ps vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_ps, col = "yellow")
text(x = min(publication_year)+2, y = mean(ps), labels = paste("r =", round(ps_corr, 3)), pos = 3, col = "yellow",cex=1.5)
plot(publication_year,qs, main="qs vs publication year",xlab="Publication year",pch=16,col="black")
abline(model_qs, col = "red")
text(x = min(publication_year)+2, y = mean(qs), labels = paste("r =", round(qs_corr, 3)), pos = 3, col = "red",cex=1.5)

#total
Ktggm_corr <- cor(publication_yeartggm, Ktggm)
pctggm_corr <- cor(publication_yeartggm, pctggm)
qctggm_corr <- cor(publication_yeartggm, qctggm)
pstggm_corr <- cor(publication_yeartggm, pstggm)
qstggm_corr <- cor(publication_yeartggm, qstggm)

cat("Correlation between Ktggm and publication_yeartggm:", Ktggm_corr, "\n")
cat("Correlation between pctggm and publication_yeartggm:", pctggm_corr, "\n")
cat("Correlation between qctggm and publication_yeartggm:", qctggm_corr, "\n")
cat("Correlation between pstggm and publication_yeartggm:", pstggm_corr, "\n")
cat("Correlation between qstggm and publication_yeartggm:", qstggm_corr, "\n")

# Create linear regression model
model_Ktggm <- lm(Ktggm ~ publication_yeartggm)
model_pctggm <- lm(pctggm ~ publication_yeartggm)
model_qctggm <- lm(qctggm ~ publication_yeartggm)
model_pstggm <- lm(pstggm ~ publication_yeartggm)
model_qstggm <- lm(qstggm ~ publication_yeartggm)

# Print summary of the model
summary(model_Ktggm)
summary(model_pctggm)
summary(model_qctggm)
summary(model_pstggm)
summary(model_qstggm)

# Plot the data points
plot(publication_yeartggm, Ktggm, main="Kt vs publication year", xlab="Publication year", pch=16, col="black")
abline(model_Ktggm, col = "red")
text(x = min(publication_yeartggm) + 2, y = mean(Ktggm), labels = paste("r =", round(Ktggm_corr, 3)), pos = 3, col = "red", cex=1.5)

plot(publication_yeartggm, pctggm, main="pc vs publication year", xlab="Publication year", pch=16, col="black")
abline(model_pctggm, col = "red")
text(x = min(publication_yeartggm) + 2, y = mean(pctggm), labels = paste("r =", round(pctggm_corr, 3)), pos = 3, col = "red", cex=1.5)

plot(publication_yeartggm, qctggm, main="qc vs publication year", xlab="Publication year", pch=16, col="black", ylim=c(-1, 0.5))
abline(model_qctggm, col = "green")
text(x = min(publication_yeartggm) + 2, y = mean(qctggm) - 0.5, labels = paste("r =", round(qctggm_corr, 3)), pos = 3, col = "green", cex=1.5)

plot(publication_yeartggm, pstggm, main="ps vs publication year", xlab="Publication year", pch=16, col="black")
abline(model_pstggm, col = "red")
text(x = min(publication_yeartggm) + 2, y = mean(pstggm), labels = paste("r =", round(pstggm_corr, 3)), pos = 3, col = "red", cex=1.5)

plot(publication_yeartggm, qstggm, main="qs vs publication year", xlab="Publication year", pch=16, col="black")
abline(model_qstggm, col = "red")
text(x = min(publication_yeartggm) + 2, y = mean(qstggm), labels = paste("r =", round(qstggm_corr, 3)), pos = 3, col = "red", cex=1.5)





##
lessons<-read.csv("LESSONS IN CHEMISTRY.csv")
str(lessons)
lessons<-lessons[lessons$MaterialType=="BOOK",]
#oh_books<-aggregate(Checkouts ~ CheckoutMonth+CheckoutYear, data = oh_books, sum)
lessons <- lessons %>%
  group_by(CheckoutYear,CheckoutMonth) %>%
  summarise(Checkouts = sum(Checkouts))
ts_lessons<-ts(lessons$Checkouts,frequency=12,start=c(lessons$CheckoutYear[1],lessons$CheckoutMonth[1]))
str(ts_lessons)
plot(ts_lessons, main="Time series: LESSONS IN CHEMISTRY ",ylab="Checkouts")
autoplot(ts_lessons)+labs(title = "Time Series:LESSONS IN CHEMISTRY", y = "Checkouts")
#autoplot(ts_oh_book,series="OH, THE PLACES YOU'LL GO!")+autolayer(ts_celest,series="THE CELESTINE PROPHECY")+autolayer(ts_davinci,series="THE DA VINCI CODE")+labs(title = "Time Series of Top 10 NYT selling Books", y = "Checkouts")


sapiens<-read.csv("SAPIENS.csv")
str(sapiens)

#
sapiense<-sapiens[sapiens$MaterialType=="EBOOK",]
sapiens<-sapiens[sapiens$MaterialType=="BOOK",]


#oh_books<-aggregate(Checkouts ~ CheckoutMonth+CheckoutYear, data = oh_books, sum)
sapiens <- sapiens %>%
  group_by(CheckoutYear,CheckoutMonth) %>%
  summarise(Checkouts = sum(Checkouts))
ts_sapiens<-ts(sapiens$Checkouts,frequency=12,start=c(sapiens$CheckoutYear[1],sapiens$CheckoutMonth[1]))
str(ts_sapiens)
sapiense <- sapiense %>%
  group_by(CheckoutYear,CheckoutMonth) %>%
  summarise(Checkouts = sum(Checkouts))
ts_sapiense<-ts(sapiense$Checkouts,frequency=12,start=c(sapiense$CheckoutYear[1],sapiense$CheckoutMonth[1]))
plot(ts_sapiens, main="Time series:SAPIENS ",ylab="Checkouts")
autoplot(ts_sapiens)+labs(title = "Time Series:sapiens", y = "Checkouts")+autolayer(ts_sapiense,series="EBOOK")

max_index <- which.max(ts_sapiens)

# Get the timestamp at the maximum value
time_at_max <- time(ts_sapiens)[max_index]

# Print the result
cat("Time of maximum value:", time_at_max, "\n")
#specific_date <- as.date("2022-10-13")
decimal_year<-time_at_max
year <- floor(decimal_year)
decimal_part <- decimal_year - year

# Convert the decimal part to months
months <- round(decimal_part * 12)+1

# Display the result
cat("Year:", year, "\n")
cat("Months:", months, "\n")
##

max_index <- which.max(sapiens$Checkouts)

# Get the timestamp at the maximum value
time_at_max <- sapiens$CheckoutMonth[max_index]

# Print the result
cat("Time of maximum value:", time_at_max, "\n")
head(sapiens)
for (i in seq_len(nrow(sapiens))) {
  cat("Year:", sapiens$CheckoutYear[i], "Month:", sapiens$CheckoutMonth[i], "  ", time(ts_sapiens)[i],"\n")
}

###############################################################################################################################
#########################################################################
############################################################






library(data.table)
library(dtplyr)

?fread
# Show the plot
#print(plot)
full<-fread("Checkouts_By_Title.csv",header=TRUE)[MaterialType=="BOOK",c(4,5,6,7,10,12)]



#######Checkouts by year
#checkout_year<-full[,.(count=.N),by=CheckoutYear]
#checkout_year<-checkout_year[order(CheckoutYear),]
#checkout_year<-as.data.frame(checkout_year)
#total_checkouts<-full[,.]
books_checkout_year <- full[, .(unique_title_count = uniqueN(Title)), by = .(CheckoutYear)][order(CheckoutYear)]

detailed_books_checkout<-full[,.(unique_title_count = uniqueN(Title)), by = .(CheckoutYear,CheckoutMonth)][order(CheckoutYear,CheckoutMonth)]
#ts_year <- ts(checkout_year$count, start = checkout_year$CheckoutYear[1], frequency = 1)

# Print the resulting time series
#print(ts_year)
#plot(ts_year,main="Frequency of checkout by year",xlab="year",ylab="frequency")

####checkouts by month
checkout_month<-full[,.(count=sum(Checkouts)),by=CheckoutMonth][order(CheckoutMonth)]
#checkout_month<-checkout_month[order(CheckoutMonth),]
plot(checkout_month,main="Total checkouts by Month",xlab="Month",ylab="checkouts",pch=16,col="red",lwd=2)
#
checkout_month_mean<-full[,.(mean=mean(Checkouts)),by=CheckoutMonth][order(CheckoutMonth)]
plot(checkout_month_mean,main="Mean checkouts by Month",xlab="Month",ylab="checkouts",pch=16,col="red",lwd=2)

####Books per publication year
full[, PublicationYear := as.integer(gsub("[^0-9]+", "", PublicationYear))]

books_year <- full[, .(unique_title_count = uniqueN(Title)), by = .(PublicationYear)]

books_year<-books_year[order(PublicationYear),]
books_year<-books_year[PublicationYear>1500 & PublicationYear<2023]
#books_year <- full %>%
#  group_by(PublicationYear) %>%
#  summarise(unique_title_count = n_distinct(title))
#checkout_year<-as.data.frame(checkout_year)
str(books_year)
#ts_pubyear <- ts(books_year$unique_title_count, start = books_year$PublicationYear[1], frequency = 1)
ggplot(books_year, aes(x = PublicationYear, y = unique_title_count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Unique Title Count per Publication Year",
       x = "Publication Year",
       y = "Unique Title Count") +
  theme_minimal()


#Total checkouts per genre


# Create a new column indicating whether the book is fiction or non-fiction
#checkouts_data[, CheckoutDate := as.Date(CheckoutDate)]

###### Categorize books as "Fiction" or "Non-Fiction" based on the presence of "fiction" in the 'Genre' column

full[, Category := ifelse(grepl("fiction", tolower(Subjects)), "Fiction", "Non-Fiction")]

# Group by Category, CheckoutDate, and sum the CheckoutCount
category <- full[, .(TotalCheckouts = sum(Checkouts)), by = .( CheckoutYear,CheckoutMonth,Category)][order(CheckoutYear, CheckoutMonth)]
head(category)
#ts_category<-ts(category,frequency=12,start=c(category$CheckoutYear[1],category$CheckoutMonth[1]))


# Create a time series plot for fiction
fiction_data <- subset(category, Category == "Fiction")
non_fiction_data <- subset(category, Category == "Non-Fiction")

# Create time series for Fiction and Non-Fiction
#fiction_time_series <- ts(fiction_data$TotalCheckouts, start = c(min(category$CheckoutYear), min(category$CheckoutMonth)), frequency = 12)
#non_fiction_time_series <- ts(non_fiction_data$TotalCheckouts, start = c(min(category$CheckoutYear), min(category$CheckoutMonth)), frequency = 12)
fiction_time_series <- ts(fiction_data$TotalCheckouts, start = c(category$CheckoutYear[1], category$CheckoutMonth[1]), frequency = 12)
non_fiction_time_series <- ts(non_fiction_data$TotalCheckouts, start = c(category$CheckoutYear[1], category$CheckoutMonth[1]), frequency = 12)

total_time_series<-fiction_time_series+non_fiction_time_series

# Combine both time series into a single ts object
combined_time_series <- ts.union(fiction = fiction_time_series, non_fiction = non_fiction_time_series)
autoplot(combined_time_series)+ labs(title = "Time Series of Checkouts per category", y = "Checkouts")




fiction_genres <- c("Mystery", "Science Fiction", "Romance","Fantasy","Historical Fiction","Adventure","Thriller","Humor","Psychological")
non_fiction_genres <- c("History", "Biography", "Finance", "Health","Business","Travel","Religion","Cooking","Autobiography")#"Self improvement",

######Checkouts per genre
#ts_gen_list<-list()
#i<-0
#fiction genres
#for (gen in fiction_genres){
  #sub<-subset(full,grepl(gen,tolower(full$Subjects)))
#  gen_checkouts <- full[grepl(tolower(gen),tolower(Subjects)), .(TotalCheckouts = sum(Checkouts)), by = .( CheckoutYear,CheckoutMonth)][order(CheckoutYear, CheckoutMonth)]
#  ts_object <- ts(gen_checkouts$TotalCheckouts, frequency = 12, start = c(gen_checkouts$CheckoutYear[1], gen_checkouts$CheckoutMonth[1]))
#  ts_gen_list[[gen]] <- ts_object
  
  #plot(ts_object, main=gen,ylab="Checkouts")
#  if (i==0){
#    base_plot <- autoplot(ts_gen_list[[gen]], series = gen)
#  }else{
#    base_plot <- base_plot + autolayer(ts_gen_list[[gen]], series = gen)
#  }
#  i<-i+1
#}
#print(base_plot + labs(title = "Time Series of Checkouts per Genre (Fiction)", y = "Checkouts"))


###parallel computatation of the above for loop to speedup according to chatgpt
library(future)
library(doFuture)
library(doParallel)
library(future.apply)

plan(multicore)

# Function to create time series for a genre
create_genre_time_series <- function(genre, data) {
  gen_checkouts <- data[grepl(tolower(genre), tolower(Subjects)),
                        .(TotalCheckouts = sum(Checkouts)),
                        by = .(CheckoutYear, CheckoutMonth)][order(CheckoutYear, CheckoutMonth)]
  
  ts_object <- ts(gen_checkouts$TotalCheckouts, frequency = 12,
                  start = c(gen_checkouts$CheckoutYear[1], gen_checkouts$CheckoutMonth[1]))
  
  return(ts_object)
}

# Apply the function in parallel
ts_gen_list <- future.apply::future_lapply(fiction_genres, FUN = create_genre_time_series, data = full)

# Plot the time series
base_plot <- autoplot(ts_gen_list[[1]], series = fiction_genres[1])
plot(ts_gen_list[[1]],main=fiction_genres[1],ylab="Checkouts")
for (i in 2:length(ts_gen_list)) {
  plot(ts_gen_list[[i]],main=fiction_genres[i],ylab="Checkouts")
  base_plot <- base_plot + autolayer(ts_gen_list[[i]], series = fiction_genres[i])
}

# Print the plot
print(base_plot + labs(title = "Time Series of Checkouts per Genre (Fiction)", y = "Checkouts"))

###
setDT(full)

df_f_unique_list <- future.apply::future_lapply(fiction_genres, FUN = function(genre) {
  gen_unique <- full[grepl(tolower(genre), tolower(Subjects)), .(unique_title_count = uniqueN(Title)), by = .(CheckoutYear, CheckoutMonth)][order(CheckoutYear, CheckoutMonth)]
  return(gen_unique)
}, future.seed = TRUE)

df_nf_unique_list <- future.apply::future_lapply(non_fiction_genres, FUN = function(genre) {
  gen_unique <- full[grepl(tolower(genre), tolower(Subjects)), .(unique_title_count = uniqueN(Title)), by = .(CheckoutYear, CheckoutMonth)][order(CheckoutYear, CheckoutMonth)]
  return(gen_unique)
}, future.seed = TRUE)

##########


#trying out splitting the data in chunks

library(future)
library(doFuture)
library(doParallel)
library(future.apply)

options(future.globals.maxSize = 5 * 1024^3)

# Set the number of cores
num_cores <- detectCores()
plan(multicore, workers = num_cores)

# Function to create time series for a genre
create_dataframe <- function(genre, data_chunk) {
  gen_checkouts <- data_chunk[grepl(tolower(genre), tolower(Subjects)),
                              .(TotalCheckouts = sum(Checkouts)),
                              by = .(CheckoutYear, CheckoutMonth)]#[order(CheckoutYear, CheckoutMonth)]
  #gen_unique <- data_chunk[grepl(tolower(genre), tolower(Subjects)),
  #                            .(unique_title_count = uniqueN(Title)),
  #                            by = .(CheckoutYear, CheckoutMonth)]  
  #ts_object <- ts(gen_checkouts$TotalCheckouts, frequency = 12,
  #                start = c(gen_checkouts$CheckoutYear[1], gen_checkouts$CheckoutMonth[1]))
  
  return(gen_checkouts)
}

create_dataframe_unique <- function(genre, data_chunk) {
  #gen_checkouts <- data_chunk[grepl(tolower(genre), tolower(Subjects)),
  #                            .(TotalCheckouts = sum(Checkouts)),
  #                            by = .(CheckoutYear, CheckoutMonth)]#[order(CheckoutYear, CheckoutMonth)]
  gen_unique <- data_chunk[grepl(tolower(genre), tolower(Subjects)),
                           .(unique_title_count = uniqueN(Title)),
                           by = .(CheckoutYear, CheckoutMonth)]  
  #ts_object <- ts(gen_checkouts$TotalCheckouts, frequency = 12,
  #                start = c(gen_checkouts$CheckoutYear[1], gen_checkouts$CheckoutMonth[1]))
  
  return(gen_unique)
}

create_dataframe_max <- function(genre, data_chunk) {
  #gen_checkouts <- data_chunk[grepl(tolower(genre), tolower(Subjects)),
  #                            .(TotalCheckouts = sum(Checkouts)),
  #                            by = .(CheckoutYear, CheckoutMonth)]#[order(CheckoutYear, CheckoutMonth)]
  gen_max <- data_chunk[grepl(tolower(genre), tolower(Subjects)),
                           .(max_checkout = max(Checkouts)),
                           by = .(CheckoutYear, CheckoutMonth)]  
  #ts_object <- ts(gen_checkouts$TotalCheckouts, frequency = 12,
  #                start = c(gen_checkouts$CheckoutYear[1], gen_checkouts$CheckoutMonth[1]))
  
  return(gen_max)
}


# Split data into chunks
chunk_size <- ceiling(nrow(full) / num_cores)
data_chunks <- split(full, rep(1:num_cores, each = chunk_size, length.out = nrow(full)))

fiction_genres <- c("Mystery", "Science Fiction", "Romance","Fantasy","Historical Fiction","Adventure","Thriller","Humor","Psychological")
non_fiction_genres <- c("History", "Biography", "Finance", "Health","Business","Travel","Religion","Cooking","Autobiography")#"Self improvement",


######FICTION


# Apply the function in parallel
df_f_list <- future.apply::future_lapply(fiction_genres, FUN = function(genre) {
  future.apply::future_lapply(data_chunks, FUN = function(.) create_dataframe(genre, .), future.seed = TRUE)
}, future.seed = TRUE)


# Combine the results
df_f_list <- lapply(df_f_list, function(df_list) {
  do.call(rbind, df_list)
})

df_f_list <- lapply(df_f_list, function(df) {
  df[order(df$CheckoutYear, df$CheckoutMonth),.(TotalCheckouts = sum(TotalCheckouts)),by = .(CheckoutYear, CheckoutMonth) ]
})

# Convert data frames to time series objects
ts_f_list <- lapply(df_f_list, function(df) {
  ts(df$TotalCheckouts, frequency = 12,
     start = c(df$CheckoutYear[1], df$CheckoutMonth[1]))
})
# Plot the time series
base_plot <- autoplot(ts_f_list[[1]], series = fiction_genres[1])
plot(ts_f_list[[1]], main = fiction_genres[1], ylab = "Checkouts")
for (i in 2:length(ts_f_list)) {
  plot(ts_f_list[[i]], main = fiction_genres[i], ylab = "Checkouts")
  base_plot <- base_plot + autolayer(ts_f_list[[i]], series = fiction_genres[i])
}

# Print the plot
print(base_plot + labs(title = "Time Series of Checkouts per Genre (Fiction)", y = "Checkouts"))

####unique

df_f_unique_list <- future.apply::future_lapply(fiction_genres, FUN = function(genre) {
  future.apply::future_lapply(data_chunks, FUN = function(.) create_dataframe_unique(genre, .), future.seed = TRUE)
}, future.seed = TRUE)


df_f_unique_list <- lapply(df_f_unique_list, function(df_list) {
  do.call(rbind, df_list)
})

df_f_unique_list <- lapply(df_f_unique_list, function(df) {
  df[order(df$CheckoutYear, df$CheckoutMonth), ]
})


ts_f_unique_list <- lapply(df_f_unique_list, function(df) {
  ts(df$unique_title_count, frequency = 12,
     start = c(df$CheckoutYear[1], df$CheckoutMonth[1]))
})
# Plot the time series
base_plot <- autoplot(ts_f_unique_list[[1]], series = fiction_genres[1])
plot(ts_f_unique_list[[1]], main = fiction_genres[1], ylab = "Unique title checkouts")
for (i in 2:length(ts_f_unique_list)) {
  plot(ts_f_unique_list[[i]], main = fiction_genres[i], ylab = "Unique title checkouts")
  base_plot <- base_plot + autolayer(ts_f_unique_list[[i]], series = fiction_genres[i])
}

print(base_plot + labs(title = "Time Series of unique title counts per Genre (Fiction)", y = "Checkouts"))

######MAX

df_f_max_list <- future.apply::future_lapply(fiction_genres, FUN = function(genre) {
  future.apply::future_lapply(data_chunks, FUN = function(.) create_dataframe_max(genre, .), future.seed = TRUE)
}, future.seed = TRUE)


df_f_max_list <- lapply(df_f_max_list, function(df_list) {
  do.call(rbind, df_list)
})

df_f_max_list <- lapply(df_f_max_list, function(df) {
  df[order(df$CheckoutYear, df$CheckoutMonth), .(max_checkout = max(max_checkout)),by = .(CheckoutYear, CheckoutMonth)]
})


ts_f_max_list <- lapply(df_f_max_list, function(df) {
  ts(df$max_checkout, frequency = 12,
     start = c(df$CheckoutYear[1], df$CheckoutMonth[1]))
})
# Plot the time series
base_plot <- autoplot(ts_f_max_list[[1]], series = fiction_genres[1])
plot(ts_f_max_list[[1]], main = fiction_genres[1], ylab = "Maximum checkouts")
for (i in 2:length(ts_f_max_list)) {
  plot(ts_f_max_list[[i]], main = fiction_genres[i], ylab = "Maximum title checkouts")
  base_plot <- base_plot + autolayer(ts_f_max_list[[i]], series = fiction_genres[i])
}

print(base_plot + labs(title = "Time Series of Maximum checkouts per Genre (Fiction)", y = "Checkouts"))



######Non fiction


df_nf_list <- future.apply::future_lapply(non_fiction_genres, FUN = function(genre) {
  future.apply::future_lapply(data_chunks, FUN = function(.) create_dataframe(genre, .), future.seed = TRUE)
}, future.seed = TRUE)

# Combine the results
df_nf_list <- lapply(df_nf_list, function(df_list) {
  do.call(rbind, df_list)
})

df_nf_list <- lapply(df_nf_list, function(df) {
  df[order(df$CheckoutYear, df$CheckoutMonth), .(TotalCheckouts = sum(TotalCheckouts)),by = .(CheckoutYear, CheckoutMonth)]
})
#df_nf_list<-df_nf_list[-c(4,9)]

# Convert data frames to time series objects
ts_nf_list <- lapply(df_nf_list, function(df) {
  ts(df$TotalCheckouts, frequency = 12,
     start = c(df$CheckoutYear[1], df$CheckoutMonth[1]))
})
# Plot the time series
base_plot1 <- autoplot(ts_nf_list[[1]], series = non_fiction_genres[1])
plot(ts_nf_list[[1]], main = non_fiction_genres[1], ylab = "Checkouts")
for (i in 2:length(ts_nf_list)) {
  plot(ts_nf_list[[i]], main = non_fiction_genres[i], ylab = "Checkouts")
  base_plot1 <- base_plot1 + autolayer(ts_nf_list[[i]], series = non_fiction_genres[i])
}

# Print the plot
print(base_plot1 + labs(title = "Time Series of Checkouts per Genre (Non Fiction)", y = "Checkouts"))


####unique


df_nf_unique_list <- future.apply::future_lapply(non_fiction_genres, FUN = function(genre) {
  future.apply::future_lapply(data_chunks, FUN = function(.) create_dataframe_unique(genre, .), future.seed = TRUE)
}, future.seed = TRUE)


# Combine the results
df_nf_unique_list <- lapply(df_nf_unique_list, function(df_list) {
  do.call(rbind, df_list)
})

df_nf_unique_list <- lapply(df_nf_unique_list, function(df) {
  df[order(df$CheckoutYear, df$CheckoutMonth),.(unique_title_count=sum(unique_title_count)),by = .(CheckoutYear, CheckoutMonth)  ]
})
#df_nf_list<-df_nf_list[-c(4,9)]

# Convert data frames to time series objects
ts_nf_unique_list <- lapply(df_nf_unique_list, function(df) {
  ts(df$unique_title_count, frequency = 12,
     start = c(df$CheckoutYear[1], df$CheckoutMonth[1]))
})
# Plot the time series
base_plot1 <- autoplot(ts_nf_unique_list[[1]], series = non_fiction_genres[1])
plot(ts_nf_unique_list[[1]], main = non_fiction_genres[1], ylab = "Checkouts")
for (i in 2:length(ts_nf_unique_list)) {
  plot(ts_nf_unique_list[[i]], main = non_fiction_genres[i], ylab = "Checkouts")
  base_plot1 <- base_plot1 + autolayer(ts_nf_unique_list[[i]], series = non_fiction_genres[i])
}

# Print the plot
print(base_plot1 + labs(title = "Time Series of Unique title counts per Genre (Non Fiction)", y = "Checkouts"))

######MAX

######MAX

df_nf_max_list <- future.apply::future_lapply(non_fiction_genres, FUN = function(genre) {
  future.apply::future_lapply(data_chunks, FUN = function(.) create_dataframe_max(genre, .), future.seed = TRUE)
}, future.seed = TRUE)


df_nf_max_list <- lapply(df_nf_max_list, function(df_list) {
  do.call(rbind, df_list)
})

df_nf_max_list <- lapply(df_nf_max_list, function(df) {
  df[order(df$CheckoutYear, df$CheckoutMonth),.(max_checkout = max(max_checkout)),by = .(CheckoutYear, CheckoutMonth) ]
})


ts_nf_max_list <- lapply(df_nf_max_list, function(df) {
  ts(df$max_checkout, frequency = 12,
     start = c(df$CheckoutYear[1], df$CheckoutMonth[1]))
})
# Plot the time series
base_plot <- autoplot(ts_nf_max_list[[1]], series = non_fiction_genres[1])
plot(ts_nf_max_list[[1]], main = non_fiction_genres[1], ylab = "Maximum checkouts")
for (i in 2:length(ts_nf_max_list)) {
  plot(ts_nf_max_list[[i]], main = non_fiction_genres[i], ylab = "Maximum title checkouts")
  base_plot <- base_plot + autolayer(ts_nf_max_list[[i]], series = non_fiction_genres[i])
}

print(base_plot + labs(title = "Time Series of Maximum checkouts per Genre (Non Fiction)", y = "Checkouts"))



###

audiobooks<-fread("Checkouts_By_Title.csv",header=TRUE)[MaterialType=="AUDIOBOOK",c(4,5,6,7,10,12)]

audiobooks_checkouts<-audiobooks[,.(count=sum(Checkouts)),by=.(CheckoutYear,CheckoutMonth)][order(CheckoutYear,CheckoutMonth)]
#audiobooks_year<-audiobooks_year[order(CheckoutYear),]
#checkout_year<-as.data.frame(checkout_year)

ts_audiobooks_checkouts <- ts(audiobooks_checkouts$count, start = c(audiobooks_checkouts$CheckoutYear[1], audiobooks_checkouts$CheckoutMonth[1]), frequency = 12)

# Print the resulting time series
#print(ts_audiobooks_checkouts)
plot(ts_audiobooks_checkouts,main="Audiobooks- Frequency of checkout by year",xlab="year",ylab="frequency")

####checkouts by month
audiobooks_month<-audiobooks[,.(count=sum(Checkouts)),by=CheckoutMonth]
audiobooks_month<-audiobooks_month[order(CheckoutMonth),]
plot(audiobooks_month,main="Audiobooks - Frequency of checkout by Month",xlab="Month",ylab="frequency",pch=16,col="red",lwd=2)

ebooks<-fread("Checkouts_By_Title.csv",header=TRUE)[MaterialType=="EBOOK",c(4,5,6,7,10,12)]

#ebooks_year<-ebooks[,.(count=.N),by=CheckoutYear]
#ebooks_year<-ebooks_year[order(CheckoutYear),]
#checkout_year<-as.data.frame(checkout_year)

#ts_ebooks_year <- ts(ebooks_year$count, start = c(ebooks_year$CheckoutYear[1],ebooks_year$CheckoutMonth[1]), frequency = 1)

ebooks_checkouts<-ebooks[,.(count=sum(Checkouts)),by=.(CheckoutYear,CheckoutMonth)][order(CheckoutYear,CheckoutMonth)]
#ebooks_year<-ebooks_year[order(CheckoutYear,CheckoutMonth),]
#checkout_year<-as.data.frame(checkout_year)

ts_ebooks_checkouts <- ts(ebooks_checkouts$count,frequency=12, start = c(ebooks_checkouts$CheckoutYear[1],ebooks_checkouts$CheckoutMonth[1]))


# Print the resulting time series
#print(ts_ebooks_checkouts)
plot(ts_ebooks_checkouts,main="Ebooks- Frequency of checkout by year",xlab="year",ylab="frequency")

####checkouts by month
ebooks_month<-ebooks[,.(count=sum(Checkouts)),by=CheckoutMonth]
ebooks_month<-ebooks_month[order(CheckoutMonth),]
plot(ebooks_month,main="Ebooks - Frequency of checkout by Month",xlab="Month",ylab="frequency",pch=16,col="red",lwd=2)
seasonplot(ts_ebooks_year,ylab="checkouts", xlab="Month", main="Seasonal plot: Ebook Checkouts", year.labels=T, year.labels.left=T, col=1:20, pch=19)


#
# Save the entire environment to a file
save(list = ls(), file = "data_business_project.RData")

# Load the entire environment back into R
#load("my_workspace.RData")

##########################


#Maximum checkout of unique title 
max_df<-full[,.(max_checkout=max(Checkouts)),by=.(CheckoutYear,CheckoutMonth)][order(CheckoutYear,CheckoutMonth)]
head(max_df)
ts_max_df<-ts(max_df$max_checkout,frequency=12, start = c(max_df$CheckoutYear[1],max_df$CheckoutMonth[1]))


filtered10_unique_df<- full[Checkouts>10, .(unique_title_count = uniqueN(Title)), by = .(CheckoutYear,CheckoutMonth)][order(CheckoutYear,CheckoutMonth)]
filtered10_total_checkouts<-full[Checkouts>10, .(TotalCheckouts = sum(Checkouts)), by = .(CheckoutYear,CheckoutMonth)][order(CheckoutYear,CheckoutMonth)]
ts_filtered10_unique<-ts(filtered10_unique_df$unique_title_count,frequency=12, start = c(max_df$CheckoutYear[1],max_df$CheckoutMonth[1]))
ts_filtered10_total_checkouts<-ts(filtered10_total_checkouts$TotalCheckouts,frequency=12, start = c(max_df$CheckoutYear[1],max_df$CheckoutMonth[1]))
ts_filtered10_checkouts_per_unique<-ts_filtered10_total_checkouts/ts_filtered10_unique

filtered3_unique_df<- full[Checkouts>3, .(unique_title_count = uniqueN(Title)), by = .(CheckoutYear,CheckoutMonth)][order(CheckoutYear,CheckoutMonth)]
filtered3_total_checkouts<-full[Checkouts>3, .(TotalCheckouts = sum(Checkouts)), by = .(CheckoutYear,CheckoutMonth)][order(CheckoutYear,CheckoutMonth)]
ts_filtered3_unique<-ts(filtered3_unique_df$unique_title_count,frequency=12, start = c(max_df$CheckoutYear[1],max_df$CheckoutMonth[1]))
ts_filtered3_total_checkouts<-ts(filtered3_total_checkouts$TotalCheckouts,frequency=12, start = c(max_df$CheckoutYear[1],max_df$CheckoutMonth[1]))
ts_filtered3_checkouts_per_unique<-ts_filtered3_total_checkouts/ts_filtered3_unique

distribution_checkouts<-full[(CheckoutMonth==7 & CheckoutYear==2015),Checkouts]

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
quantile(distribution_checkouts,0.95)
quantile(distribution_checkouts,0.99)
confint(distribution_checkouts)
head(distribution_checkouts)










##


for (i in seq_along(length(df_nf_max_list))){
  cat(paste(" Checkout year: ", df_f_unique_list[[1]]$CheckoutYear,"  Checkout month:  ",df_f_unique_list[[1]]$CheckoutMonth," ,value: ",df_f_unique_list[[1]]$unique_title_count,"\n"))
}
