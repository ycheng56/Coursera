################
library(quantmod)
library(urca) 
library(quantmod)
library(PerformanceAnalytics)

symbols <- c("JPM", "XLF")
start_date <- "2012-01-01"
end_date <- "2018-04-30"

getSymbols(Symbols = symbols, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = TRUE)
JPM <- JPM[,6]
XLF <- XLF[,6]

formStart<-"2012-01-01"
formEnd<-"2015-01-01"

formPeriod<-paste(formStart,"::",formEnd,sep ="")
XLFf<-XLF[formPeriod]
JPMf<-JPM[formPeriod]

log_JPMf<-log(JPMf)
summary(ur.df(log_JPMf))
summary(ur.df(diff(log_JPMf)[-1]))
log_XLFf<-log(XLFf)
summary(ur.df(log_XLFf))
summary(ur.df(diff(log_XLFf)[-1]))


regreJX<-lm(log_JPMf~log_XLFf)
summary(regreJX)
alpha<-coef(regreJX)[1]
beta<-coef(regreJX)[2]
spreadf<-log_JPMf-beta*log_XLFf-alpha
UnitRootf<-ur.df(spreadf,type="none")
summary(UnitRootf)
mu<- mean(spreadf) 
sd<-sd(spreadf)


JPMT<-JPM["2015-01-01/2018-04-30"]
XLFT<-XLF["2015-01-01/2018-04-30"]
CoSpreadT<-log(JPMT)-beta*log(XLFT)-alpha
names(CoSpreadT)<-"CoSpreadT"
summary(CoSpreadT)


plot.zoo(CoSpreadT,ylim=c(-0.20,0.20) ,main = "price difference")
abline(h = mu,col="black",lwd =1)
abline(h = c(mu+0.2*sd,mu-0.2*sd),col = "blue",lty=6,lwd =2)
abline(h = c(mu+1.5*sd,mu-1*sd), col = "green",lty=2,lwd =2.5)
abline(h = c(mu+2.5*sd,mu-2.5*sd),col = "red",lty=3,lwd =3)
##############################################
level<-c(mu-2.5*sd,mu-1.5*sd,mu-0.2*sd,
         mu+0.2*sd, mu+1.5*sd,mu+2.5*sd)

interval<-function(x,level){
  prcLevel<-cut(x,breaks=c(-Inf,level,Inf))
  prcLevel<-as.numeric(prcLevel)-4
}

prcLevel<-interval(CoSpreadT,level)
head(prcLevel)

TradeSig<-function(prcLevel){
  n<-length(prcLevel)
  signal<-rep(0,n)
  for (i in (2:n)){
    if(prcLevel[i-1]==1 & prcLevel[i]==2 )
      signal[i]<-(-2)
    
    if(prcLevel[i-1]==1 & prcLevel[i]== 0)
      signal[i]<-2
    
    if(prcLevel[i-1]==2 & prcLevel[i]==3)
      signal[i]<-3
    
    if(prcLevel[i-1]==(-1)& prcLevel[i]==(-2))
      signal[i]<-1
    
    if(prcLevel[i-1]==(-1)& prcLevel[i]==0)
      signal[i]<-(-1)
    
    if(prcLevel[i-1]==(-2)& prcLevel[i]==(-3))
      signal[i]<-(-3)
  }
  return(signal)
}

signal<-TradeSig(prcLevel)
position<-rep(0,length(signal))
position[1]<-signal[1]
ns<-length(signal)
for (i in 2:ns){
  position[i]<-position[i-1]
  if (signal[i]==1)
    position[i]=1
  if (signal[i]==(-2))
    position[i]=(-1)
  if (position[i-1]==1 &signal[i]==(-1))
    position[i]=0
  if (position[i-1]==(-1) &signal[i]==(2))
    position[i]=0
  if (signal[i]==3) break
  if (signal[i]==-3) break
}

position<-xts(position,order.by=index(CoSpreadT))
tail(position)

#####

TradeSim<-function(PriceA,PriceB,Position){
  n<-length(Position)
  priceA<-as.numeric(PriceA)
  priceB<-as.numeric(PriceB)
  position<-as.numeric(Position)
  size<-100
  shareA<-size*position
  shareB<-c()
  shareB[1]<-(-beta)*shareA[1]*priceA[1]/priceB[1]
  cash<-c()
  cash[1]<-10000
  for (i in 2:n){
    shareB[i]<-shareB[i-1]
    cash[i]<- cash[i-1]
    if(position[i-1]==0 & position[i]==1){
      shareB[i]<-(-beta)*shareA[i]*priceA[i]/priceB[i]
      cash[i]<-cash[i-1]-(shareA[i]*priceA[i]+shareB[i]*priceB[i])
    }
    if(position[i-1]==0&position[i]==(-1)){
      shareB[i]<-(-beta)*shareA[i]*priceA[i]/priceB[i]
      cash[i]<-cash[i-1]-(shareA[i]*priceA[i]+shareB[i]*priceB[i])
    }
    if(position[i-1]==1& position[i]==0){
      shareB[i]<-0
      cash[i]<-cash[i-1]+(shareA[i-1]*priceA[i]+shareB[i-1]*priceB[i])
    }
    if((position[i-1]==(-1))&(position[i]==0)){
      shareB[i]<-0
      cash[i]<-cash[i-1]+(shareA[i-1]*priceA[i]+shareB[i-1]*priceB[i])
    }
    
  }
  cash<-xts(cash,order.by=index(Position))
  shareA<-xts(shareA,order.by=index(Position))
  shareB<-xts(shareB,order.by=index(Position))
  asset<-cash+shareA*PriceA+shareB*PriceB
  account<-merge(Position,shareA,shareB,cash,asset)
  colnames(account)<-c("Position","shareA","shareB","cash","asset")
  return(account)
}

account<-TradeSim(JPMT,XLFT,position)
tail(account)

plot.zoo(account[,c(1,4,5)],col=c("black","blue","red"), main="pair trade")
asset<- account[,5]
rets.pair<- dailyReturn(asset)
charts.PerformanceSummary(rets.pair,main="JPM & XLF pair trade performance",geometric=FALSE) 
Return.annualized(rets.pair)
SharpeRatio.annualized(rets.pair)
PAIR<-asset

#idx=intersect(as.Date(index(rets.pair)),as.Date(index(ff3)))
#fit=lm(rets.pair[as.Date(idx)]~ff3[as.Date(idx)][,1:3])  
#factorCoefs=rbind(factorCoefs,data.frame(Symbol=PAIR,Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
#colnames(rets.pair)="PAIR"
#rets=cbind(rets,rets.pair)

