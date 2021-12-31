setwd("D:/3. RESEARCH/Inflation 2019 conference bisaaa/Data")
ifl<-read.csv("InflationData.csv")

#data structure
str(ifl)
summary(ifl)

#plot data source
plot(ifl$Data1,col="black",lwd=2,type="l",las=1,ylab="Indonesia Inflation (Percentage)",xlab="Year")
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
plot(ifl$Data2,col="black",lwd=2,type="l",las=1,ylab=" Malaysia Inflation (Percentage)",xlab="Year")
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
plot(ifl$Data3,col="black",lwd=2,type="l",las=1,ylab="Philippines Inflation (Percentage)",xlab="Year")
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
plot(ifl$Data4,col="black",lwd=2,type="l",las=1,ylab="Singapore Inflation (Percentage)",xlab="Year")
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
plot(ifl$Data5,col="black",lwd=2,type="l",las=1,ylab="Thailand Inflation (Percentage)",xlab="Year")
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")

#split data (75%:25%)
training1<-ifl[c(1:43),1]
training2<-ifl[c(1:43),2]
training3<-ifl[c(1:43),3]
training4<-ifl[c(1:43),4]
training5<-ifl[c(1:43),5]
summary(training1)

#differencing data
diff1<-diff(training1)
diff2<-diff(training2)
diff3<-diff(training3)
diff4<-diff(training4)
diff5<-diff(training5)

summary(diff1)
summary(diff2)
summary(diff3)
summary(diff4)
summary(diff5)

acf(diff1, las=1)
acf(diff2, las=1)
acf(diff3, las=1)
acf(diff4, las=1)
acf(diff5, las=1)

#plotting stationary data
windows()
plot(diff1,col="black",lwd=2,type="l",las=1,ylab="Indonesia Stationary Data",xlab="Year")
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
plot(diff2,col="black",lwd=2,type="l",las=1,ylab="Malaysia Stationary Data",xlab="Year")
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
plot(diff3,col="black",lwd=2,type="l",las=1,ylab="Philippines Stationary Data",xlab="Year")
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
plot(diff4,col="black",lwd=2,type="l",las=1,ylab="Singapore Stationary Data",xlab="Year")
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
plot(diff5,col="black",lwd=2,type="l",las=1,ylab="Thailand Stationary Data",xlab="Year")
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")

#exponential smoothing
library(forecast)
fit1<- tbats(ts(diff1)) # generalizing Holt-Winters
exp1<-forecast(fit1, 14)
ind1<- data.frame(exp1)[,1]+ifl[c(43:56),1]

#plot(forecast(fit1, 14))
#accuracy(forecast(fit1, 14))
#write.csv(exp1, file="ES1", row.names=FALSE)
getwd()

fit2<- tbats(ts(diff2)) # generalizing Holt-Winters
exp2<-forecast(fit2, 14)
malay1<- data.frame(exp2)[,1]+ifl[c(43:56),1]

#plot(forecast(fit2, 14))
#accuracy(forecast(fit2, 14))
#write.csv(exp2, file="ES2", row.names=FALSE)
getwd()

fit3<- tbats(ts(diff3)) # generalizing Holt-Winters
exp3<-forecast(fit3, 14)
phil1<- data.frame(exp3)[,1]+ifl[c(43:56),1]

#plot(forecast(fit3, 14))
#accuracy(forecast(fit3, 14))
#write.csv(exp3, file="ES3", row.names=FALSE)
getwd()

fit4<- tbats(ts(diff4)) # generalizing Holt-Winters
exp4<-forecast(fit4, 14)
sing1<- data.frame(exp4)[,1]+ifl[c(43:56),1]

#plot(forecast(fit4, 14))
#accuracy(forecast(fit4, 14))
#write.csv(exp4, file="ES4", row.names=FALSE)
getwd()

fit5<- tbats(ts(diff5)) # generalizing Holt-Winters
exp5<-forecast(fit5, 14)
thai1<- data.frame(exp5)[,1]+ifl[c(43:56),1]

#plot(forecast(fit5, 14))
#accuracy(forecast(fit5, 14))
#write.csv(exp5, file="ES5", row.names=FALSE)
getwd()


#ARIMA
library(forecast)
auto.arima(diff1)
arimafit1<-Arima(diff1,order=c(0,0,2))
arimamod1<-forecast(arimafit1,h=14)
ind2<- data.frame(arimamod1)[,1]+ifl[c(43:56),1]

#plot(forecast(arimafit1,h=14))
#accuracy(forecast(arimafit1,h=14))
#write.csv(arimamod1, file="arima1", row.names=FALSE)
getwd()

#check the forecast error of ARIMA are normally distributed with mean zero and constant variance             
source("D:/3. RESEARCH/Inflation 2019 conference bisaaa/Code/plotForecastErrors.R")

acf(forecast(arimafit1,h=14)$residuals,lag.max=20)
Box.test(forecast(arimafit1,h=14)$residual, lag=20, type="Ljung-Box")
plot.ts(forecast(arimafit1,h=14)$residual)

plotForecastErrors <- function(forecasterrors)
{  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins, xlab="Forecast errors")
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(forecast(arimafit1,h=14)$residual)
mean(forecast(arimafit1,h=14)$residual)

auto.arima(diff2)
arimafit2<-Arima(diff2,order=c(3,0,0))
arimamod2<-forecast(arimafit2,h=14)
malay2<- data.frame(arimamod2)[,1]+ifl[c(43:56),1]
#check the forecast error of ARIMA are normally distributed with mean zero and constant variance             
acf(forecast(arimafit2,h=14)$residuals,lag.max=20)
Box.test(forecast(arimafit2,h=14)$residual, lag=20, type="Ljung-Box")
plot.ts(forecast(arimafit2,h=14)$residual)
plotForecastErrors(forecast(arimafit2,h=14)$residual)
mean(forecast(arimafit2,h=14)$residual)

auto.arima(diff3)
arimafit3<-Arima(diff3,order=c(0,0,1))
arimamod3<-forecast(arimafit3,h=14)
phil2<- data.frame(arimamod3)[,1]+ifl[c(43:56),1]
#check the forecast error of ARIMA are normally distributed with mean zero and constant variance             
acf(forecast(arimafit3,h=14)$residuals,lag.max=20)
Box.test(forecast(arimafit3,h=14)$residual, lag=20, type="Ljung-Box")
plot.ts(forecast(arimafit3,h=14)$residual)
plotForecastErrors(forecast(arimafit3,h=14)$residual)
mean(forecast(arimafit3,h=14)$residual)


auto.arima(diff4)
arimafit4<-Arima(diff4,order=c(0,0,0))
arimamod4<-forecast(arimafit4,h=14)
sing2<- data.frame(arimamod4)[,1]+ifl[c(43:56),1]
#check the forecast error of ARIMA are normally distributed with mean zero and constant variance             
acf(forecast(arimafit4,h=14)$residuals,lag.max=20)
Box.test(forecast(arimafit4,h=14)$residual, lag=20, type="Ljung-Box")
plot.ts(forecast(arimafit4,h=14)$residual)
plotForecastErrors(forecast(arimafit4,h=14)$residual)
mean(forecast(arimafit4,h=14)$residual)

auto.arima(diff5)
arimafit5<-Arima(diff5,order=c(0,0,0))
arimamod5<-forecast(arimafit5,h=14)
thai2<- data.frame(arimamod5)[,1]+ifl[c(43:56),1]
#check the forecast error of ARIMA are normally distributed with mean zero and constant variance             
acf(forecast(arimafit5,h=14)$residuals,lag.max=20)
Box.test(forecast(arimafit5,h=14)$residual, lag=20, type="Ljung-Box")
plot.ts(forecast(arimafit5,h=14)$residual)
plotForecastErrors(forecast(arimafit5,h=14)$residual)
mean(forecast(arimafit5,h=14)$residual)

#Fuzzy time series 
library(AnalyzeTS)
library(forecast)
model1<-fuzzy.ts1(ts(diff1),n=7,type="Chen",plot=TRUE,grid=TRUE)
model2<-fuzzy.ts1(ts(diff1),n=7,type="Singh",plot=TRUE,grid=TRUE)
fuzzy1<-forecast(model1,14)
fuzzy2<-forecast(model2,14)
ind3<- data.frame(fuzzy1)[,1]+ifl[c(43:56),1]
ind4<- data.frame(fuzzy2)[,1]+ifl[c(43:56),1]

#plot(forecast(model1,150))
#plot(forecast(model2,150))
#accuracy(forecast(model1,150))
#accuracy(forecast(model2,150))
#write.csv(fuzzy5, file="CHEN1", row.names=FALSE)
#write.csv(fuzzy6, file="SINGH1", row.names=FALSE)

model3<-fuzzy.ts1(ts(diff2),n=7,type="Chen",plot=TRUE,grid=TRUE)
model4<-fuzzy.ts1(ts(diff2),n=7,type="Singh",plot=TRUE,grid=TRUE)
fuzzy3<-forecast(model3,14)
fuzzy4<-forecast(model4,14)
malay3<- data.frame(fuzzy3)[,1]+ifl[c(43:56),1]
malay4<- data.frame(fuzzy4)[,1]+ifl[c(43:56),1]

model5<-fuzzy.ts1(ts(diff3),n=7,type="Chen",plot=TRUE,grid=TRUE)
model6<-fuzzy.ts1(ts(diff3),n=7,type="Singh",plot=TRUE,grid=TRUE)
fuzzy5<-forecast(model5,14)
fuzzy6<-forecast(model6,14)
phil3<- data.frame(fuzzy5)[,1]+ifl[c(43:56),1]
phil4<- data.frame(fuzzy6)[,1]+ifl[c(43:56),1]

model7<-fuzzy.ts1(ts(diff4),n=7,type="Chen",plot=TRUE,grid=TRUE)
model8<-fuzzy.ts1(ts(diff4),n=7,type="Singh",plot=TRUE,grid=TRUE)
fuzzy7<-forecast(model7,14)
fuzzy8<-forecast(model8,14)
sing3<- data.frame(fuzzy7)[,1]+ifl[c(43:56),1]
sing4<- data.frame(fuzzy8)[,1]+ifl[c(43:56),1]

model9<-fuzzy.ts1(ts(diff5),n=7,type="Chen",plot=TRUE,grid=TRUE)
model10<-fuzzy.ts1(ts(diff5),n=7,type="Singh",plot=TRUE,grid=TRUE)
fuzzy9<-forecast(model9,14)
fuzzy10<-forecast(model10,14)
thai3<- data.frame(fuzzy9)[,1]+ifl[c(43:56),1]
thai4<- data.frame(fuzzy10)[,1]+ifl[c(43:56),1]

#compiling
frcst <- data.frame(cbind(ind1,ind2,ind3,ind4,malay1,malay2,malay3,malay4,phil1,phil2,phil3,phil4,sing1,sing2,sing3,sing4,thai1,thai2,thai3,thai4))
write.csv(frcst, file="fullset.csv", row.names=FALSE)
View(frcst)

#accuracy RMSE and MSE
rmse <- NULL
for (i in c(1:20)){
  p <- sqrt(sum(((ifl[c(44:57),1]-(frcst[,i]))^2)/nrow(frcst)))
  #print(p) 
  rmse[i] <- p 
}
write.csv(rmse, file="rmse.csv", row.names=FALSE)

#plotting
windows(16,9)
ifl[c(44:57),6] <- ind1
ifl[c(44:57),7] <- ind2
ifl[c(44:57),8] <- ind3
ifl[c(44:57),9] <- ind4
ifl[c(44:57),10] <- malay1
ifl[c(44:57),11] <- malay2
ifl[c(44:57),12] <- malay3
ifl[c(44:57),13] <- malay4
ifl[c(44:57),14] <- phil1
ifl[c(44:57),15] <- phil2
ifl[c(44:57),16] <- phil3
ifl[c(44:57),17] <- phil4
ifl[c(44:57),18] <- sing1
ifl[c(44:57),19] <- sing2
ifl[c(44:57),20] <- sing3
ifl[c(44:57),21] <- sing4
ifl[c(44:57),22] <- thai1
ifl[c(44:57),23] <- thai2
ifl[c(44:57),24] <- thai3
ifl[c(44:57),25] <- thai4

windows(16,9) 
#indonesia
plot(ifl$Data1,col="black",lwd=2,type="l",las=1,ylab="Indonesian Inflation (Percentage)",xlab="Year",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,6],col="red",lwd=2,type="l",las=1,xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,7],col="green",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,8],col="blue",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,9],col="cyan",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
legend(45,200,col=c("black","red","green","blue","cyan"),
       lty=c(1,1,1,1,1), lwd = c(2,2,2,2,2),bty = "o", 
       paste(c("Actual         ",
               "ES              ",
               "ARIMA       ",
               "FTS Chen  ",
               "FTS Singh "),round(c(0,rmse[c(1:4)]),digit=2)))
box()

windows(16,9)
#malaysia
plot(ifl$Data2,col="black",lwd=2,type="l",las=1,ylab="Malaysian Inflation (Percentage)",xlab="Year",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,10],col="red",lwd=2,type="l",las=1,xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,11],col="green",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,12],col="blue",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,13],col="cyan",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
legend(45,200,col=c("black","red","green","blue","cyan"),
       lty=c(1,1,1,1,1), lwd = c(2,2,2,2,2),bty = "o", 
       paste(c("Actual         ",
               "ES              ",
               "ARIMA       ",
               "FTS Chen  ",
               "FTS Singh "),round(c(0,rmse[c(5:8)]),digit=2)))
box()

windows(16,9)
#philipine
plot(ifl$Data3,col="black",lwd=2,type="l",las=1,ylab="Philippines Inflation (Percentage)",xlab="Year",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,14],col="red",lwd=2,type="l",las=1,xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,15],col="green",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,16],col="blue",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,17],col="cyan",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
legend(45,200,col=c("black","red","green","blue","cyan"),
       lty=c(1,1,1,1,1), lwd = c(2,2,2,2,2),bty = "o", 
       paste(c("Actual         ",
               "ES              ",
               "ARIMA       ",
               "FTS Chen  ",
               "FTS Singh "),round(c(0,rmse[c(9:12)]),digit=2)))
box()

windows(16,9)
#singapura
plot(ifl$Data4,col="black",lwd=2,type="l",las=1,ylab="Singapore Inflation (Percentage)",xlab="Year",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,18],col="red",lwd=2,type="l",las=1,xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,19],col="green",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,20],col="blue",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,21],col="cyan",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
legend(45,200,col=c("black","red","green","blue","cyan"),
       lty=c(1,1,1,1,1), lwd = c(2,2,2,2,2),bty = "o", 
       paste(c("Actual         ",
               "ES              ",
               "ARIMA       ",
               "FTS Chen  ",
               "FTS Singh "),round(c(0,rmse[c(13:16)]),digit=2)))
box()

windows(16,9)
#thailand
plot(ifl$Data5,col="black",lwd=2,type="l",las=1,ylab="Thailand Inflation (Percentage)",xlab="Year",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,22],col="red",lwd=2,type="l",las=1,xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,23],col="green",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,24],col="blue",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
points(ifl[,25],col="cyan",lwd=2,type="l",xlab="Year",ylab="Inflation (Percentage)",xlim=c(0,57),ylim=c(-100,230))
grid(nx =9, ny = 9,col = "lightgray", lty = "dotted")
legend(45,200,col=c("black","red","green","blue","cyan"),
       lty=c(1,1,1,1,1), lwd = c(2,2,2,2,2),bty = "o", 
       paste(c("Actual         ",
               "ES              ",
               "ARIMA       ",
               "FTS Chen  ",
               "FTS Singh "),round(c(0,rmse[c(17:20)]),digit=2)))
box()