#Part I: my notes of "Advanced Time series Analysis" at PITT.

#####################################################################
##Time series characteristics
#######################################################

#Some of the common kinds of time series data
#qustions that might ask

#Example 1.1 Johnson and Johnson
library(astsa)
tsplot(jj,type="o")
tsplot(log(jj))
#characteristics: increasing underlying trend
#regular ocillation superimposed on the trend seems to repeat over quaters
#Method of analysis: regression

#Exmaple 1.2 Global Warming
tsplot(globtemp,type="o")
#Charateristics: there's upward trend in the latter part
#the trend is not linear

#Example 1.3 Dow Jones Industrial Average (DJIA returns)
library(xts)
djiar = diff(log(djia$Close))[-1]
ts.plot(djiar)
#The mean of the series appears to be stable with an average of return approximate 0
#The volatile periods tend to be clustered together.
#Qustion: to forecast the volatility of future returns (Chapter5)

#Example 1.4 EI Nino and Fish Population - analyszing several time series at once
par(mfrow=c(2,1))
tsplot(soi)
tsplot(rec)
#Charateristics: there's two basic oscillations types: annual circle (hot in sumer and cold 
# in winter), and four year repeatation. The two series are related.
#Method to study circles and strenghts: Chapter4

#Example1.5 fMRI Imaging
par(mfrow=c(2,1),mar=c(3,2,1,0)+.5,mgp=c(1.6,0.6,0))
ts.plot(fmri1[,2:5],col=1:4)
ts.plot(fmri1[,6:9],col=1:4)

##############################################################################
#Time series data preprocessing models
##############
#1. White noise (Portions of Example 1.3: DJIA returns)

#2. Two methods of introducing serial correlation and more smoothness
#1) Moving Average
#filtered series
w = rnorm(500,0,1)
v = filter(w,sides=2,filter=rep(1/3,3)) #filter as moving average
par(mfrow=c(2,1))
ts.plot(w,main="white noise")
ts.plot(v,main="Moving Average")
#2) Autoregressive
w = rnorm(550,0,1)
x <- filter(w,filter = c(1,-0.9),method="recursive")[-(1:50)] #avoid startup problem
ts.plot(x,main="autoregression")

#3.Random Walk with drift - analyzing trend
set.seed(154)
w = rnorm(200)
x = cumsum(w)
wd = w + .2
xd = cumsum(wd)
ts.plot(xd,ylim=c(-5,55),main="random walk",ylab='') #random walk with drift
abline(a=0,b=0.2,lty=2) #trend
lines(x,col=4) #radom walk,no trend
abline(h=0,col=4,lty=2)

#4.Signal in Noise
cs = 2*cos(2*pi*1:500/50 + .6*pi)
w = rnorm(500)
par(mfrow=c(3,1))
ts.plot(cs)
ts.plot(cs+w)
ts.plot(cs+5*w)

#5. Prediction using cross-correlation
x= rnorm(100)
y = lag(x,-5)+rnorm(100)
ccf(y,x,ylab="CCovF",type="covariance")

#6. Smaple ACF
r = round(acf(soi,6,plot = FALSE)$acf[-1],3)
r
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.6,0.6,0))
plot(lag(soi,-1),soi) #lag 
plot(lag(soi,-6),soi)


#A Simulated Time Series with small sample 10 and large sample 100
set.seed(101011)
x1 = sample(c(-2,2),11,replace = TRUE) #Toss a coin with head 2 tail -2
x2 = sample(c(-2,2),101,replace = TRUE)
y1 = 5 + filter(x1, sides=1,filter = c(1,-.5)[-1])
y2 = 5 + filter(x1, sides=1,filter = c(1,-.5)[-1])
tsplot(y1)
acf(y1,lag.max = 4)
acf(y2,lag.max = 4)

#CCF
par(mfrow=c(1,3))
acf1(soi,48)
acf1(rec,48)
ccf2(soi,rec,48)

#Prewhitening - remove signal by running a regression on cos2pit and sin2pit
se.seed(1492)
num=120
t = 1:num
X = ts(2*cos(2*pi*t/12))
Y = ts(2*cos(2*pi*(t+5)/12))
Yw = resid(lm(Y~cos(2*pi*t/12) + sin(2*pi*t/12),na.action = NULL)) #using linear regression and take residual
tsplot(X) 
tsplot(Y)
acf1(X,48)
acf1(Y,48)
ccf2(X,Y,24)
ccf2(X,Yw,24)

#####################################################################
#Time series regression
#############################################
#estimating a linear trend (Chicken price data)
fit <- lm(chicken~time(chicken),na.action = NULL)
summary(fit)

#mortality data (find the mode the the best fit with fewest paramethers)
par(mfrow=c(3,1))
tsplot(cmort)
tsplot(tempr)
tsplot(part) #particulate
dev.new()
ts.plot(cmort,tempr,part,col=1:3)

pairs(cbind(Mortlity=cmort,tempreture=tempr,Particulates=part))
temp=tempr-mean(tempr)
temp2=temp^2
trend=time(cmort)
fit <- lm(cmort~trend+temp+temp2+part,na.action = NULL)
summary(fit)
summary(aov(fit))
summary(aov(lm(cmort~cbind(trend+temp+temp2+part))))
num=length(cmort)
AIC(fit)/num - log(2*pi)
BIC(fit)/num-log(2*pi)
AICc = log(sum(resid(fit)^2/num))+(num+5)/(num-5-2)
AICc

#regression with lagged variables(SOI data)
fish <- ts.intersect(rec,soiL6=lag(soi,-6))   #used in R to stick with lag data
summary(fit1 <- lm(rec~soiL6,data=fish,na.action = NULL))
#or use package dynlm
library(dynlm)
summary(fit2 <- lm(rec ~ L(soi,6)))

############################################################################
###EDA - Exploratory Data Analysis : Regression,stationarity
########################3
# To study the stationary properties of the series data

#type1: trend stationary, has stationary behavior around a trend
#method: remove the trend first: detrending

par(mfrow=c(2,1))
#Detrending chicken prices
fit <- lm(chicken~time(chicken),na.action=NULL)
tsplot(fit$residuals)

#type2:random walk with a drift:
#method2:differencing
tsplot(diff(chicken))

par(mfrow=c(3,1))
acf1(chicken)
acf1(resid(fit))
acf1(diff(chicken))

#differencing global tempreture
par(mfrow=c(2,1))
tsplot(diff(globtemp))
mean(diff(globtemp))
acf1(diff(gtemp),48)

#Paleoclimatic Glacial Varve data
par(mfrow=c(2,1))
tsplot(varve)
tsplot(log(varve))

#Use ACF to explore linear relations and use scatterplot to explore nonlinear relations
lag1.plot(soi,12)
lag2.plot(soi,rec,8)

#Mark:regression with laged variables
dummy = ifelse(soi<0,0,1)
fish=ts.intersect(rec,soiL6=lag(soi,-6),dL6=lag(dummy,-6),dframe = TRUE)
summary(fit <- lm(rec~soiL6*dL6,data=fish,na.action = NULL))
attach(fish)
plot(soiL6,rec)
points(resid(fit),col=2,pch="+")
lines(lowess(soiL6,rec),col=4,lwd=2)
tsplot(resid(fit))
detach(fish)

#using regression to discover a signal in Noise
set.seed(90210)
x = 2*cos(2*pi*1:500/50 + .6*pi) + rnorm(500,0,5)
Z1 = cos(2*pi*1:500/50)
Z2 = sin(2*pi*1:500/50)
summary(fit <- lm(x~0+Z1+Z2))
par(mfrow=c(2,1))
tsplot(x)
tsplot(x,col=8)
lines(fitted(fit))

#Smoothing time series
#1 . Moving Average
wgts=c(.5,rep(1,11),.5)/12 #monthly smooth  
#if quarterly smooth wgts = c(1/8,1/4,1/4,1/4,1/8)
soif=filter(soi,sides=2,filter=wgts) #trendpattern
tsplot(soi)
lines(soif,col=2)
seasonal = soi-soif
plot (seasonal, type = "b", main = "Seasonal pattern")

#2 . Kernel Smoothing
tsplot(soi)
lines(ksmooth(time(soi),soi,"normal",bandwidth = 1),col=4)
SOI = ts(soi,freq=1)
tsplot(SOI)
lines(ksmooth(time(SOI),SOI,"normal",bandwidth=12),col=2)

#3 . Lowess
tsplot(soi)
lines(lowess(soi,f=.05),col=4)
lines(lowess(soi),col=2)

#4. Smoothing one series as a Function of Another
plot(tempr,cmort)
lines(lowess(tempr,cmort))

#5. Classicla Structural Modeling
x = window(hor,start=2002)
plot(decompose(x))
plot(stl(x,s.window = 'per'))  #lowess smooth
plot(stl(x,s.window = 15))

##########################################################################
#ARMA

#sample path of AR(1)model
par(mfrow=c(2,1))
tsplot(arima.sim(list(order=c(1,0,0),ar=.9),n=100)) #smooth
tsplot(arima.sim(list(order=c(1,0,0),ar=-.9),n=100)) #choppy

#sample path of MA(1)model
par(mfrow=c(2,1))
tsplot(arima.sim(list(order=c(0,0,1),ma=.9),n=100)) #smooth
tsplot(arima.sim(list(order=c(0,0,1),ma=-.9),n=100)) #choppy

#parameter reduandancy in ARMA model
set.seed(8675309)
x = rnorm(150,mean=5)
arima(x,order = c(1,0,1))

#to find the caulity form-10 psi-weights
ARMAtoMA(ar=.9,ma=.5,10)

#to find the invertible form-10 pi-weights
ARMAtoAR(ar=.9,ma=.5,10)

#calculate polynomial roots and solve for arg
z = c(1,-1.5,0.75) #coefficients of the polynomial
a= polyroot(z)[1]
polyroot(z)
arg = Arg(a)/2*pi #arg in circles
1/arg # the pseudo circle

#Figure of the model
set.seed(8675309)
ar2 = arima.sim(list(order=c(2,0,0),ar=c(1.5,-.75)),n=144)
plot(ar2)
axis(2)
axis(1,at=seq(0,144,by=12))
box()
abline(v=seq(0,144,by=12),lty=2)

#ACF of the model
ACF = ARMAacf(ar=c(1.5,-0.75),ma=0,50)
plot(ACF,type="h")
abline(h=0)

ACF = ARMAacf(ar=c(1.5,-0.75),ma=0,24)[-1]
PACF = ARMAacf(ar=c(1.5,-0.75),ma=0,24,pacf=TRUE)
par(mfrow=c(1,2))
tsplot(ACF,type="h")
tsplot(PACF,type="h")

#recruiment series
acf2(rec,48) #with both ACF and PACF
regr = ar.ols(rec,order=2,demean = FALSE,intersept = TRUE)
regr$asy.se.coef

#estimation of coefficients
#1. method of moments estimators(essentially least squares) - optimal estimators for AR models
#for AR(2)
rec.yw = ar.yw(rec,order=2)
rec.yw$x.mean
rec.yw$ar #parameter estimates
sqrt(diag(rec.yw$asy.var.coef)) #their standard errors
rec.yw$var.pred #error variance estimate

#2.Gauss-Newton algorithm - use moments as initial, for MA and ARMA models
#for MA(1)
#Glacial Varve Data - cita(0)
x = diff(log(varve))
r = acf(x,lag=1,plot=FALSE)$acf[-1] #acf(1)
c(0) -> w -> z #initialize
c() ->Sc ->Sz -> Szw ->SS ->para
num = length(x)
para[1] = (1-sqrt(1-4*(r^2)))/(2*r)
niter = 12
for (p in 1:niter) {
  for (i in 2:num) {
    w[i] = x[i] - para[p]*w[i-1]
    z[i] = w[i-1] - para[p]*z[i-1]
  }
  Sc[p] = sum(w^2)
  Sz[p] = sum(z^2)
  Szw[p] = sum(z*w)
  para[p+1] = para[p] + Szw[p]/Sz[p]
}
#results
round(cbind(iteration=0:(niter-1),thetahat = para[1:niter],Sc,Sz),3)

#plot cond SS
th = seq(-.3,-.94,-.01)
for (p in 1:length(th)) {
  for (i in 2:num) {w[i] = x[i] - th[p]*w[i-1]}
  SS[p] = sum(w^2)
}
plot(th,SS,type="l")
abline(c=para[1:12],lty=2)
points(para[1:12],Sc[1:12],pch=16)

#Forecast
#AR(2)
sarima(rec,2,0,0)
sarima.for(rec,24,2,0,0)

#summary :
# Steps of building ARIMA model
#1.plotting the data
#2.possibly transforming the data
#3.identifying the dependence order of the model
#4.parameter estimation
#5.diagnositics and model choice

#example:
tsplot(gnp)
acf2(gnp,50)
#transform
#linear trend: first-order difference, quadratic trend:2nd order difference,rarely go beyond 2 
gnpgr = diff(log(gnp)) #growth rate
tsplot(gnpgr)
acf2(gnpgr,24)
#AR(1)
lag1.plot(gnpgr)
sarima(gnpgr,1,0,0)
#MA(2)
sarima(gnpgr,0,0,2) #chice of models using AIC and BIC

#autocorrelated residual in linear regression model
trend = time(cmort)
temp = tempr-mean(tempr) #center data in case of collinearity
tem2 = temp^2
fit = lm(cmort~trend+temp+tem2+part,na.action=NULL)
acf2(resid(fit),50) #residual is autocorrelated
#AR(2)for the residuals
sarima(cmort,2,0,0,xreg=cbind(trend,temp,temp2,part))

##########################################################################
#seasonal ARIMA model:SARIMA model
#
x = AirPassengers
lx = log(x)
dlx=diff(lx) #there's a trend then difference to make it to be stationary (constant mean, autovariance is only related with time span)
ddlx=diff(dlx,12)
plot.ts(cbind(x,lx,dlx,ddlx))
#evidence of seasonal persistence
monthplot(dlx)
monthplot(ddlx)
acf2(ddlx,50)
#SARIMA
sarima(lx,1,1,1,0,1,1,12) #non-seasonal(p,d,q) * seasonal(P,D,Q)
sarima.for(lx,12,1,1,1,0,1,1,12)


#########################################################################
#refer to https://newonlinecourses.science.psu.edu/stat510/node/33/ 
######################################################################
#What to consider first with time series data?
#
#1. trend over time 2.seasonality (seasonal, weekly, monthly or quarter repeating pattern)
#3. outliers 4.long-raun circle 5.constant variance over time
#6. abrupt change

#Decomposing data : Additive: series = trend+seasonal+random (used when seasonal variation is constant)
#Multiplicative: series = trend * seasonal * random (used when seasonal variation increases over time)

#Decompose process: 1. estimate the trend; 2.detrend; 3. estimate seasonal factors; 4.determine random part

#decompose(name of series, type ="additive").
#decompose(name of series, type ="multiplicative").

########################################################################
#Intervention Analysis
###
#By intervention, we mean a change to a procedure, or law, or policy, etc. 
#that is intended to change the values of the series
#Intervention analysis in time series refers to the analysis of how the mean level of a series
#changes after an intervention, when it is assumed that the same ARIMA structure for the series 
# xt holds both before and after the intervention.

#There are several possible patterns for how an intervention may affect the values of a series for t ??? T
#1. Permanent constant change to the mean level
#2. Brief constant change to the mean level:  There may be a temporary change for one or more periods, 
# after which there is no effect of the intervention.
#3.  Gradual increase or decrease to a new mean level
# 4.  Initial change followed by gradual return to the no change

#Estimation
#PartI: ARIMA model + Part II: intervention effect
#Steps:
#1. Use the data before the intervention point to determine the ARIMA model for the series.
# 2. Use that ARIMA model to forecast values for the period after the intervention.
# 3. Calculate the differences between actual values after the intervention and the forecasted values.
# 4. Examine the differences in step 3 to determine a model for the intervention effect.

