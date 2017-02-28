library(data.table)
library(lubridate)
library(ggplot2)
library(forecast)
library(gridExtra)

dairy <- read.csv("cadairydata.csv")
str(dairy)
summary(dairy)

#make month names uniform by choosing only the first 3 characters
dairy$Month <- substr(dairy$Month, 1, 3)

#paste year, month and date together in a new column Date
dairy$Date <- with(dairy, paste(as.character(Year), "-",
                                as.character(Month), "-",
                                "01", sep = ""))

#Convert new column into Date object
dairy$Date <- ymd(dairy$Date)

#Remove unnecessary columns
dairy <- subset(dairy, select = -c(1:4))

#Inspect date object
dairy$Date[1:20]
dairy[dairy$Date > "2012-12-01",]

#create time series plot
dairy.plot <- function(df, col = 'Milk.Prod'){
  require(ggplot2)
  ggplot(df, aes_string('Date', col)) +
    geom_line() +
    xlab('Time in years')
}

dairy.plot(dairy, dairy$Milk.Prod)

#creat acf and pacf plots 
dairy.acf <- function(df, col = 'remainder', is.df = TRUE){
  if(is.df) temp <- df[,col]
  else temp <- df
  temp <- ts(temp, start = 1995, frequency = 12)
  par(mfrow = c(2,1))
  acf(temp, main = paste("ACF of", col))
  pacf(temp, main = paste("PACF of", col))
  par(mfrow = c(1,1))
}  

dairy.acf(dairy, col="Milk.Prod")

#create a histogram
hist.ts <- function(df, col = "Milk.Prod", bins = 40){
  temp <- df[,col]
  breaks = seq(min(temp), max(temp), length.out = 41)
  hist(temp, breaks = breaks, main = paste("Distribution of", col), xlab = col)
}  

hist.ts(dairy)

#detrend the time series using moving average
dairy.ma <- function(df, col = 'Milk.Prod', order = 12){
  temp = df[, col]
  end = length(temp) - 1
  out = rep(0, length(temp))
  out[1] = temp[1]
  for(i in 1:end){
    if(i - order <= 1) j = 1 
    else j = j + 1
    out[i + 1] = sum(temp[j:i])/(i - j + 1)
  }
  out
}

#compute the season component using lm
dairy.seasons <- function(df, col = "Milk.Prod"){
  df$y <- df[,col]
  fit = lm(y ~ 0 + Month, data=df)
  predict(fit, newdata=df)
}
seasonal <- dairy.seasons(dairy, col = "Milk.Prod")

#break down the time series into 3 components

decomp.dairy <- function(df,  col = 'Milk.Prod', multiplicative = TRUE, order = 12){
  if(multiplicative) {
    temp = log(df[, col])
    df[, col] = temp
  } else { 
    temp = df[, col] 
  }
  trend = dairy.ma(df, col = col, order = order)
  temp = temp - trend
  df[, col] = temp
  seasonal = dairy.seasons(df, col = col)
  remainder = temp - seasonal
  data.frame(trend = trend, seasonal = seasonal, remainder = remainder)
}

decomp <- decomp.dairy(dairy, order = 12)
head(decomp)


#Create a plot with all 3 trends
decomp.plot <- function(df){
  require(ggplot2)
  require(gridExtra)
  df$x = 1:nrow(df)
  ycols = c('trend', 'seasonal', 'remainder')
  p <- lapply(ycols, function(y){
    ggplot(df, aes_string('x', y)) + 
      geom_line() +
      ylab(y)
  })
  grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 3)
}

decomp.plot(decomp)
dairy.acf(decomp)

#Exploring the multiplicative model with lowess
dairy.decomp <- function(df, col = "Milk.Prod", span = 0.5, Mult = TRUE){
  if(Mult){temp <- ts(log(df[,col]), frequency = 12, start =1)
  } else {temp <- ts(df[,col], frequency = 24, start=1)}
  span = span * length(temp)
  dairyFit = stl(temp, s.window = "periodic", t.window = span)
  plot(dairyFit, main = "Decomposition of Time Series")
  cbind(df, as.data.frame(dairyFit$time.series))
}

dairyMult <- dairy.decomp(dairy, col = "Milk.Prod", span = 0.2)

#checking the remainder of the decomposed series to check for dependencies
dairy.acf(dairyMult)

#checking the remainder for normal approximations
hist.ts(dairyMult, col = "remainder")

#checking the remainder to see if the IQ range overlaps
dairy.box <- function(df, col = "remainder"){
  require(ggplot2)
  p <- ggplot(df, aes_string('Month', col)) +
    geom_boxplot() +
    ggtitle('Variation of remainder component of dairy production by month')
  print(p)
}  

dairy.box(dairyMult)

# BUILD MA model for the remainder
model.dairy <- function(df, col = "remainder", order = c(0,0,1)){
  ts <- ts(df[,col], frequency = 12, start = 1995)
  dairy.mod = arima(ts, order=order, include.mean = FALSE)
  print(dairy.mod)
  dairy.mod
}
ma1 <- model.dairy(dairyMult, order = c(0,0,4))

#reduce the order since coefficient of ma4 < SE of ma4
ma2 <- model.dairy(dairyMult, order = c(0,0,3))

#plot the ACF of the residuals of the MA(3) model
dairy.acf(ma2$resid[-1], is.df = FALSE)

# FIT AN AR MODEL
ar1 <- model.dairy(dairyMult, order = c(2,0,0))
ar1 <- model.dairy(dairyMult, order = c(1,0,0))

#plot the ACF and PACF of remainder of AR model
dairy.acf(ar1$resid[-1], is.df = FALSE)

#test an ARMA model
arma1 <- model.dairy(dairyMult, order = c(1,0,3))
arma1 <- model.dairy(dairyMult, order = c(1,0,1))
dairy.acf(arma1$resid[-1], is.df=FALSE)

#Create a differencing series to remove trend
dairy.diff <- function(df, col = "Milk.Prod", out = "Milk.diff"){
  ln <- nrow(df)
  temp <- ts(df[,col], frequency = 12, start = 1995)
  df[2:ln, out] <- diff(temp)
  df <- df[2:ln,]
  df
}

dairydiff <- dairy.diff(dairy)

#take the stl of the differenced series
dairyDiff <- dairy.decomp(dairydiff, col = "Milk.diff", span = 0.5, Mult = FALSE)

#acf of remainder
dairy.acf(dairyDiff)
hist.ts(dairyDiff, col = "remainder")
hist.ts(dairyMult, col = "remainder")
dairy.box(dairyDiff)

#arima model
arima <- model.dairy(dairyMult, order = c(1,1,1))
dairy.acf(arima$resid[-1], is.df=FALSE)

# Auto build an ARIMA using FORECAST package
temp <- ts(log(dairy[,'Milk.Prod']), frequency = 12, start=1995)
fit.dairy <- auto.arima(temp, max.p = 3, max.q = 3,
                        max.P = 2, max.Q = 2, max.order = 5, max.d = 2, max.D = 1,
                        start.p = 0, start.q = 0, start.P = 0, start.Q = 0)
summary(fit.dairy)

#FORECAST NEXT 12 MONTH'S PRODUCTION
dairy.fit <- forecast(fit.dairy, h=12)
summary(dairy.fit)
plot(dairy.fit)

#forecast next 12 month's prediction for ice cream production
temp <- ts(log(dairy[, 'Icecream.Prod']), frequency = 12, start = 1995)
fit.icecream <- auto.arima(temp, max.p = 1, max.q = 3, max.P = 2, max.Q = 2,
                           max.order = 5, max.d = 2, max.D = 1, start.p = 0, 
                           start.q = 0, start.P = 0, start.Q = 0)
summary(fit.icecream)

icecream.fit <- forecast(fit.icecream, h=12)
plot(icecream.fit)
