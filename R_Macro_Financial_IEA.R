###############################################
# Using R for Macroeconomic and Financial Data
# Scott W. Hegerty
# Illinois Economics Association Workshop
# October 25, 2024
# Please see www.github.com/hegerty/IEA2024
###############################################


#### 1. Loading Data into R 
### Quarterly data for Poland
data<-read.csv("https://raw.githubusercontent.com/hegerty/IEA2024/refs/heads/main/MacroDataIEA24.csv")
head(data);tail(data) # check first and last observations


#### 2. Setting a time series
ts<-ts(data[,-1],start=c(1997,1),freq=4) # note (year, quarter); I also cut the first column
head(ts) # I just call it "ts" for "time series"

#### 3. Plotting a time series
plot(ts) # all at once
plot(ts,main="Polish Macreconomic Data",lwd=3,col="dark grey") #customized
plot(ts[,2]) # just one (can customize further)

#### 4. Stationarity
PP.test(ts[,1]) # Phillips-Perron (akin to ADF)
PP.test(ts[,6])
PP.test(ts[,5])

# can do a loop for all! Save p-values but can do statistic too
for(i in 1:ncol(ts)){
  pp1<-PP.test(ts[,i])
  print(noquote(c(colnames(ts)[i],round(pp1$p.value,3))))
}

#### 5. Lagging/Differencing Variables
lts<-log(ts) # take natural log 
head(lts) # this is the set of logged values, note you can't log a negative number

lts[,4]<-ts[,4] # Keep the one with zeros (replace it back in)--not logged
dlts<-100*diff(lts) # difference one period (default--can choose others)
head(dlts) # this is the set of log-differenced ts

plot(lts,col="#FF9999",lwd=3) # i picked totally random colors
dlts[,5]<-ts[-1,5] # put the stationary one back in (but still take REER returns)
plot(dlts, col="#009944",lwd=3)

dltslag<-lag(dlts,-1) # one quarter lag
## You can also lead with positive values--make sure you get it right!
head(cbind(dlts[,1],dltslag[,1])) # compare the two

# re-run the stationarity test
for(i in 1:ncol(dlts)){
  pp1<-PP.test(dlts[,i])
  print(noquote(c(colnames(dlts)[i],round(pp1$p.value,3))))
}

#### 6. Moving Standard Deviations: Time-varying volatility
#install.packages("zoo") # time-series package
require(zoo)
rollsd<-rollapply(dlts,FUN="sd",4) #volatility series
plot(rollsd) #all six
plot(rollsd[,1],xlab="",ylab="Volatility, Credit Growth",lwd=3) #customized



#### 7. Seasonality/Trend/Cycle
credits<-stl(lts[,1],11) # decompose into trend, seasonal, error
plot(credits)
#install.packages("mFilter")
require(mFilter) # Filter to get cycle
creditcyc<-hpfilter(credits$time.series[,2]) #Hodrick-Prescott
plot(creditcyc)

gdps<-stl(lts[,2],11) # the window is necessary, should be odd, but sort of arbitrary here
gdpcyc<-hpfilter(gdps$time.series[,2]) # takes second column, which is cycle
plot(gdpcyc)

ccf<-ccf(creditcyc$cycle,gdpcyc$cycle,8) #Cross-correlation function
# Plot S-Curve over 8 quarters, label, line graph with width 3
xaxis<--8:8
plot(xaxis,ccf$acf,type="l",ylab="Cross-correlation",xlab="Lag/Lead",lwd=3)
abline(h=0,lty=3)

#### 8. Autocorrelation, Partial Correlation, and ARIMA
crg<-dlts[,1] # credit growth
acf(crg)
pacf(crg)
arima(crg,order = c(2,0,2)) # Just try a bunch!
arima(crg,order = c(3,0,3))
arima(crg,order = c(1,0,1))
arima(crg,order = c(2,0,0))
arima(crg,order = c(1,0,0))
#######################################################
# I wrote this to search over all combinations
arimalist<-NULL # double loop
for(p in 0:6){
  for(q in 0:6){
    arima0<-arima(crg,order = c(p,0,q)) #run the ARIMA
    se<-sqrt(diag(arima0$var.coef)) # calculate standard errors
    tstat<-arima0$coef[-length(se)]/se[-length(se)] #look for significant coefficients
    sig<-round(sum(abs(tstat)>2)/length(tstat),2) # % of total p, q significant
    arimalist<-rbind(arimalist,c(p,q,arima0$aic,sig))
  }
}
arimalist[order(arimalist[,3],decreasing = F),] # AIC minimizing
arimalist[order(arimalist[,4],decreasing = T),] # % coefficient maximizing
# Can rank these, etc.
arima(crg,order = c(2,0,2)) # May be a good option?


#####################################
# One more! Here's a VAR
#install.packages("vars")
require(vars)
colnames(dlts) # should order in order of increasing endogeneity

# Make a VAR, minimize Schwarz Criterion
var1<-VAR(dlts,type = c("const"),lag.max = 4,ic="SC")

plot(irf(var1)) #Impulse-Response Functions (I usually plot these manually, though)
fevd((var1))
