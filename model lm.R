library(lmtest)
library(ExcelFunctionsR)
library(car)
library(dplyr)
library(tseries)
library(lmtest)
library(ivreg)
library(car)
library(stats)
rm(list = ls())

#importing file and assigning to 'player' names of players in order that is same as is assigned in 'listcsv' 
setwd("/Users/franciszekkaczmarek/Desktop/Praca licencjacka/Algorytm/1 Podejście/Dane/Danenaszybko/")
listcsv <- dir(pattern = "*.csv")
listcsv
player <-c("Aslanov, Sergei", "Carlsen, Magnus", "Ding, Liren", "Dubov, Daniil", "Erigaisi, Arjun",
           "Firouzja, Alireza", "Marzolo, Cyril","Giri, Anish",  "Nakamura, Hikaru",
           "Nepomniachtchi, Ian", "Nigalidze, Gaioz")

#creating empty data frame
daneall <-data.frame()
#assigning boolean values: white=1 black=0
for (k in 1:length(listcsv)) {
  dane1 <- read.csv(listcsv[k], header = TRUE)
  
  kolor <- rep(NA,nrow(dane1))
  for(i in 1:nrow(dane1)) {
    if (dane1$player.White[i]==player[k]) {
      kolor[i] <- 1
    } else {
      kolor[i] <- 0
    }
  }

  ACPL <- dane1$PlayerToAnalyseACPL
  ELO <- dane1$PlayerToAnalyseElo
  OACPL <- dane1$OpponentACPL
  OELO <- dane1$OpponentElo
  dane1 <- cbind(ELO, ACPL, OELO, OACPL,kolor)
  dane1 <- as.data.frame(dane1)
  dane1 <- dane1[complete.cases(dane1[,c(3,4)]),]
  #removing outliers 
  outliers <- boxplot(dane1$ACPL, plot = FALSE)$out
  dane1 <- dane1[ ! dane1$ACPL %in% outliers, ]
  daneall <- rbind.data.frame(daneall, dane1)
}

outliers <- boxplot(daneall$ACPL, plot = FALSE)$out
daneall <-daneall[ ! daneall$ACPL %in% outliers, ]
daneall<- arrange(daneall, daneall$ELO)

#dependent and independent values of linear regression model
ACPL <- daneall$ACPL
ELO <-daneall$ELO
OACPL <- daneall$OACPL
OELO <- daneall$OELO
kolor <- daneall$kolor
hist(ACPL)
#graphs 
par(mfrow=c(2,2))
plot(ELO,ACPL)
plot(OACPL,ACPL)
plot(OELO, ACPL)
dev.off()

#model##############################################################
model <- lm(ACPL ~ ELO + OELO + OACPL)
summary(model)
par(mfrow=c(2,2))
plot(model)

dev.off()
#tests
##reset
resettest(model) #null hypothesis is that model is correctly specified 



#colinearity
vif(model)

#error term
##Jarque-Bera test #null hypothesis is that residuals are normally distributed 
jarque.bera.test(model$residuals)
par(mfrow=c(2,2))
hist(model$residuals, main = "Histogram reszt modelu", xlab = "Reszty modelu", ylab = "częstotliwość")

##expected errors = 0
dwtest(model)
residualPlot(model)
plot(model, which = 1, main = "reszty vs wartości teoretyczne", ylab(""))
acf(model$residuals, type = "correlation")
dev.off()
residualPlots(model, quadratic = FALSE, fitted = FALSE, tests = FALSE)



#Breuschan-Pagan test # null hypothesis is that the the error variances are all equal
bptest(model)
plot(fitted(model), resid(model), xlab='Wartości teoretyczne', ylab='Reszty')

#add a horizontal line at 0 
abline(0,0)


#using logarithms 
par(mfrow=c(2,2))
plot(log(ELO),ACPL)
plot(log(OACPL),ACPL)
plot(log(OELO), ACPL)
dev.off()
#model###################################################################################

logmodel <- lm(ACPL ~ ELO + OELO + log(OACPL))
summary(logmodel)
par(mfrow=c(2,2))
plot(logmodel)
#tests
##reset
resettest(logmodel) #null hypothesis is that model is corectly specified 


#colinearity
vif(logmodel)

#error term
##Jarque-Bera test #null hypothesis is that residuals are normally distributed 
jarque.bera.test(logmodel$residuals)


par(mfrow=c(2,2))
hist(logmodel$residuals, main = "Histogram reszt modelu", xlab = "Reszty modelu ze zmienną zlogatymowaną", ylab = "Częstotliwość")

##expected errors = 0
dwtest(logmodel)
residualPlot(logmodel)
plot(logmodel, which = 1, main = "reszty vs wartości teoretyczne", ylab(""))
acf(logmodel$residuals, type = "correlation")
dev.off()
residualPlots(logmodel, quadratic = FALSE, fitted = FALSE, tests = FALSE)



#Breuschan-Pagan test # null hypothesis is that the the error variances are all equal
bptest(logmodel)

plot(fitted(logmodel), resid(model), xlab='Wartości teoretyczne', ylab='Reszty')

#add a horizontal line at 0 
abline(0,0)
#model###################################################################################
#define weights to use
wt <- 1 / lm(abs(logmodel$residuals) ~ logmodel$fitted.values)$fitted.values^2

#perform weighted least squares regression

wls_model <- lm(ACPL ~ ELO + OELO + log(OACPL), weights=wt)

#view summary of model
summary(wls_model)
par(mfrow=c(2,2))
plot(wls_model)





#tests
##reset
resettest(wls_model) #null hypothesis is that model is corectly specified 


#colinearity
vif(wls_model)

#error terms
##Jarque-Bera test #null hypothesis is that residuals are normally distributed 
jarque.bera.test(wls_model$residuals)
par(mfrow=c(2,2))
hist(wls_model$residuals, main = "Histogram reszt modelu", xlab = "Reszty modelu ważonej regresji liniowej", ylab = "Częstotliwość")

##expected errors = 0
dwtest(wls_model)
residualPlot(wls_model)
acf(model$residuals, type = "correlation")
plot(wls_model, which = 1, main = "reszty vs wartości teoretyczne", ylab(""))
dev.off()
residualPlots(wls_model, quadratic = FALSE, fitted = FALSE, tests = FALSE)



#Breuschan-Pagan test # null hypothesis is that the the error variances are all equal
bptest(wls_model)
plot(fitted(wls_model), resid(model), xlab='Wartości teoretyczne', ylab='Reszty')

#add a horizontal line at 0 
abline(0,0)
par(mfrow=c(1,2))
plot(fitted(model), resid(logmodel), xlab='logmodel Fitted Values', ylab='Residuals')
plot(fitted(wls_model), resid(wls_model), xlab='wls_mode Fitted Values', ylab='Residuals')
dev.off()









#average vlues of ACPL, sd(ACPL) for players 
##ELO i dane wszystkich graczy
ELO1 <- c(2200,2300,2400,2500,2600,2700,2800)
AE <- rep(NA,length(ELO1))


#avg ACPL i sd(ACPL) dla 1 i 7 rekordu
x <- filter(daneall, daneall$ELO < ELO1[1])

AE[1]<-AVERAGE(x$ACPL)

x <- filter(daneall, daneall$ELO > ELO1[7])
AE[7]<- AVERAGE(x$ACPL)


STD <- rep(NA,length(ELO1))
x <- filter(daneall, daneall$ELO < ELO1[1])
STD[1]<-sd(x$ACPL)

x <- filter(daneall, daneall$ELO > ELO1[7])
STD[7]<-sd(x$ACPL)



#avg ACPL i sd(ACPL) for records between
for (i in 2:(length(ELO1)-1)) {
  x <- filter(daneall, daneall$ELO > ELO1[(i-1)], daneall$ELO < ELO1[i])
  AE[i]<-AVERAGE(x$ACPL)
  STD[i] <- sd(x$ACPL)
}

plot(ELO1,AE, xlab = "ELO", ylab = "ACPL")
df <- data.frame(ELO1,AE,STD)
df



df <- data.frame(matrix(ncol = 3, nrow = 7))
x <- c("ELO","ACPL uzyskane przeciwko graczom z niższym ELO", "ACPL uzyskane przeciwko graczom z wyższym ELO")
colnames(df) <- x
df[,1]<- c("2175:2225", "2275:2325", "2375:2425", "2475:2525","2575:2625", "2675:2725", "2775:2825")
xx<- data.frame(matrix(ncol = 2, nrow = 7))
xx[,1]<- c(2175, 2275, 2375, 2475,2575, 2675, 2775)
xx[,2]<- c(2225, 2325, 2425, 2525, 2625, 2725, 2825)

for (i in 1:nrow(xx)){
  x<- filter(daneall, daneall$ELO > xx[i,1], daneall$ELO < xx[i,2], daneall$OELO<xx[i,1])
  df[i,2]<- AVERAGE(x$ACPL)
  x<- filter(daneall, daneall$ELO > xx[i,1], daneall$ELO < xx[i,2], daneall$OELO>xx[i,2])
  df[i,3]<- AVERAGE(x$ACPL)
}



plot(1:7 ,df$`ACPL uzyskane przeciwko graczom z niższym ELO`,type="l",col="red", xlab = "ELO", ylab = "ACPL",yaxt="none",xaxt="none", lwd=2)
lines(1:7,df$`ACPL uzyskane przeciwko graczom z wyższym ELO`,col="green", lwd=2)
legend(2, 95, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

df <- data.frame(matrix(ncol = 2, nrow = 19))
df[,1] <- seq(5,95,5)

x<- filter(daneall, daneall$OACPL<5)
df[1,2] <-AVERAGE(x$ACPL)


x<- filter(daneall, daneall$OACPL<95)
df[20,2] <-AVERAGE(x$ACPL)

for (i in 2:nrow(df)) {
  x<- filter(daneall, daneall$OACPL<df[i,1], daneall$OACPL>df[(i-1),1])
  df[i,2] <-AVERAGE(x$ACPL)
}

plot(df$X1,df$X2, xlab = "OACPL", ylab = "ACPL", yaxt="none",xaxt="none")


