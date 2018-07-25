################
library(quantmod)
library(tseries)
library(urca)
library("PairTrading")
symbols <- c("JPM", "XLF")


start_date <- "2012-01-01"
end_date <- "2018-04-30"

getSymbols(Symbols = symbols, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = TRUE)

JPM<- JPM[,6]
XLF<- XLF[,6]

formStart<-"2012-01-01"
formEnd<-"2015-01-01"
formPeriod<-paste(formStart,"::",formEnd,sep ="")

JPMf<-JPM[formPeriod]
XLFf<-XLF[formPeriod]
length(JPMf)

#####################################################
SSD<-function(x,y){ 
  retx<-ROC(x,type="discrete")[-1] 
  rety<-ROC(y,type="discrete")[-1]   
  standardX<-cumprod(1+retx) 
  standardY<-cumprod(1+rety) 
  SSD<-sum((standardX-standardY)^2) 
  return(SSD) 
}
JXdistance<-SSD(JPMf,XLFf)
JXdistance


#######################################
#unit root test
JPMflog<-log(JPMf)
JPMflogdf<- ur.df(coredata(JPMflog),type='none',lags=1)
summary(JPMflogdf)

XLFflog<-log(XLFf)
XLFflogdf<- ur.df(coredata(XLFflog),type='none',lags=1)
summary(XLFflogdf)

#First order difference
retJPMf<-diff(JPMflog)[-1]
retJPMfdf<- ur.df(coredata(retJPMf),type='none',lags=1)
summary(retJPMfdf)

retXLFf<-diff(XLFflog)[-1]
retXLFfdf<- ur.df(coredata(retXLFf),type='none',lags=1)
summary(retXLFfdf)

plot(XLFflog,ylim=c(2,5),type="l",main="log price")
points(JPMflog,col="red",pch=20)
legend("bottomleft",legend=c("JMP","XLFf"),
       col=c("red","black"),pch=c(20,NA_integer_),lty=c(0,1))

plot.zoo(cbind(retJPMf,retXLFf),col=c("red","black"),lty=c(2,1),
         main="Log price first difference (return)")

legend("topright",legend=c("JPMf","XLFf"),
       col=c("red","black"),lty=c(2,1),cex=0.7)

#regression
reg<-lm(JPMflog~XLFflog)
reg
summary(reg)

alpha<-coef(reg)[1]
alpha
beta<-coef(reg)[2]
beta

spread<-JPMflog-beta*XLFflog-alpha
names(spread)<-"spread"
head(spread)
plot(spread,type="l",main="Spread = JPM - beta* XLF - alpha")

UnitRoot<-ur.df(spread,type="none")
summary(UnitRoot)

#####################################################
standardJPMf<-cumprod(1+ROC(JPMf,type="discrete")[-1])
standardXLFf<-cumprod(1+ROC(XLFf,type="discrete")[-1])

SSD_JX<-standardJPMf-standardXLFf
head(SSD_JX)

meanSSD_JX<- as.numeric(mean(SSD_JX,na.rm=TRUE))
sdSSD_JX<- as.numeric(sd(SSD_JX,na.rm=TRUE))

thresholdUp<-meanSSD_JX + 2 * sdSSD_JX
thresholdDown<-meanSSD_JX - 2 * sdSSD_JX

plot.zoo(SSD_JX,ylim=c(-0.2,0.2) ,
         main = "JPMf & XLFf standardized price difference")
abline(h = meanSSD_JX,col="black",lty=1,lwd =1)
abline(h = c(thresholdUp,thresholdDown),col = "green", lty=3,lwd=2.5)


tradStart<-"2015-01-01"
tradEnd<-"2018-04-30"
tradPeriod<-paste(tradStart,"::",tradEnd,sep ="")
JPMf.Trade<-JPM[tradPeriod]
XLFf.Trade<-XLF[tradPeriod]


spreadCal<-function(x,y){
  retx<-ROC(x,type="discrete")[-1] 
  rety<-ROC(y,type="discrete")[-1] 
  standardX<-cumprod(1+retx)
  standardY<-cumprod(1+rety)
  spread<-standardX-standardY
  return(spread)
}

TradSpread<-spreadCal(JPMf.Trade,XLFf.Trade)
summary(TradSpread)


plot.zoo(TradSpread,ylim=c(-0.2,0.2),main = "Trading period Standard Price spread")
abline(h = meanSSD_JX,col="black",lty=1,lwd =1)
abline(h = c(thresholdUp,thresholdDown),col = "green", lty=3,lwd=2.5)

###################################
library(tseries)
library("PairTrading")
library(PerformanceAnalytics)

prcdata<-merge(JPMf.Trade,XLFf.Trade)

JXreg<-EstimateParameters(prcdata, method = lm)
str(JXreg)  
plot(JXreg$spread,main="Pair price difference")

BTreg<-EstimateParametersHistorically(prcdata,period=60, method = lm) 
str(BTreg)
head(na.omit(BTreg$hedge.ratio))
mean((na.omit(BTreg$hedge.ratio)))

IsStationary(JXreg$spread, 0.05)
spread<-na.omit(BTreg$spread)
IsStationary(spread, 0.05) 

signal<-Simple(spread, spread.entry=0.05)
head(signal)

barplot(signal,col="green",space = 0, border = "green",
        xaxt="n",yaxt="n",xlab="",ylab="") 
par(new=TRUE)
plot.zoo(spread,xlab = "signal and spread")

pairReturn<-Return(prcdata,lag(signal,1),lag(na.omit(BTreg$hedge.ratio),1))
names(pairReturn)<-"pairReturn" 
head(pairReturn) 
library(PerformanceAnalytics)
charts.PerformanceSummary(pairReturn,main="JPMf and XLFf performance",geometric=FALSE) 
plot(cumsum(pairReturn),main="strategy performance")
Return.annualized(pairReturn)
SharpeRatio.annualized(pairReturn)


write.zoo(pairReturn,sep = " ",file = "pairReturn.csv")
