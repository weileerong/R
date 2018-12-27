#Data: bith data
library(ggplot2)
library(forecast)
library(astsa)
library(lmtest)
library(fUnitRoots)
library(FitARMA)
library(strucchange)
library(reshape)
library(Rmisc)
library(fBasics)

#EDA
url <- "https://www.openintro.org/stat/data/arbuthnot.csv"
ab<- read.csv(url, header=TRUE)
nrow(ab)
head(ab)
ab_rs <- melt(ab, id = c("year")) #expand data to prepare for ts
head(ab_rs)
tail(ab_rs) #from year of 1629 to 1710

ggplot(data = ab_rs, aes(x = year)) + geom_line(aes(y = value, colour = variable)) +
  scale_colour_manual(values = c("blue", "red"))
#It seems boy's births geater than girl's
#can use t-test to compare
t.test(value ~ variable, data = ab_rs) #results show that there's no significant difference
#summary for boy's and girl's bith
basicStats(ab[-1]) #from package of fBasics

p1 <- ggplot(data = ab_rs, aes(x = variable, y = value)) + geom_boxplot()
p2 <- ggplot(data = ab, aes(boys)) + geom_density()
p3 <- ggplot(data = ab, aes(girls)) + geom_density()
plot(p1)
plot(p2)
plot(p3)

#take fraction of boys and girls as time series: 
excess_frac <- (ab$boys - ab$girls)/ab$girls
excess_ts <- ts(excess_frac, frequency = 1, start = ab$year[1])
tsplot(excess_ts)
basicStats(excess_frac)
#Boys births were at least 1% higher than girls ones,
#reaching a top percentage excess equal to 15%.
par(mfrow=c(1,2))
acf2(excess_ts)
#We observe the auto-correlation spike at lag = 10 beyond confidence region. 
#That suggests the presence of a seasonal component with period = 10.

#smooth
tsplot(excess_ts)
lines(lowess(excess_ts,f=.05),col=4)
lines(lowess(excess_ts),col=2)

####################################################################
#Investigate structural changes(whether there is break point and where is it?)
#verify if regression against a constant is significative for our time series
summary(lm(excess_ts ~ 1))
#YES!
# if there are any structural breaks.
break_point <- breakpoints(excess_ts ~ 1)
break_point
plot(break_point)
summary(break_point)
#The BIC minimum value is reached when m = 1, hence just one break point is determined corresponding to year 1670

#the time series against its structural break and its confidence interval.
plot(excess_ts)
lines(fitted(break_point, breaks = 1), col = 4) #break line shows the break point
lines(confint(break_point, breaks = 1)) 

print(paste("Sex raitios changes from ", fitted(break_point)[1], "to ", fitted(break_point)[length(excess_ts)] ))

#t-test: the difference in mean is significative across the two time windows identified by the breakpoint date, year 1670
break_date <- breakdates(break_point)  #strcturechange package
win_1 <- window(excess_ts, end = break_date)
win_2 <- window(excess_ts, start = break_date + 1)
t.test(win_1, win_2)

#ARIMA models and comparisons
#1. non seasonal (1,1,1), as determined by auto.arima() within forecast package
#2. seasonal (1,0,0)(0,0,1)[10]
#3. seasonal (1,0,0)(1,0,0)[10]
#4. seasonal (0,0,0)(0,0,1)[10] with level shift regressor as intervention variable
#5. seasonal (1,0,0)(0,0,1)[10] with level shift regressor as intervention variable
#6. non seasonal (1,0,0) with level shift regressor as intervention variable

#Model1:stepwise indicates in-depth search of potential models,trace show traced models
model_1 <- auto.arima(excess_ts, stepwise = FALSE, trace = TRUE) #in forecast package
coeftest(model_1)
#The model is: xt-0.222x(t-1) = wt-0.926w(t-1)

#Model2:
acf2(excess_ts) #could be AR(1) or MA(1) maybe ARMA(1,1) can't really tell
#AR(1) and seasonal MA(1)
model_2 <- Arima(excess_ts, order = c(1,0,0), 
                 seasonal = list(order = c(0,0,1), period = 10), 
                 include.mean = TRUE)
summary(model_2)
coeftest(model_2)

#Model3:
model_3 <- Arima(excess_ts, order = c(1,0,0), 
                 seasonal = list(order = c(1,0,0), period = 10), 
                 include.mean = TRUE)
summary(model_3)
coeftest(model1_3)

#Model4:
model_4 <- Arima(excess_ts,order=c(0,1,1),
                 seasonal = list(order = c(1,0,0), period = 10), 
                 include.mean = TRUE)
summary(model_4)

#Model5:
#take into acount of level shift at break-point
#level as intervention varibale to xreg
#assume same ARIMA model for both levels
level <- c(rep(0, break_point$breakpoints), 
           rep(1, length(excess_ts) - break_point$breakpoints))

model_5 <- Arima(excess_ts, order = c(0,0,0), 
                 seasonal = list(order = c(0,0,1), period = 10), 
                 xreg = level, include.mean = TRUE)
summary(model_5)
coeftest(model_5)

#Model6:
#AR(1)model with intervention
model_6 <- Arima(excess_ts, order = c(1,0,0), 
                 seasonal = list(order = c(0,0,1), period=10), 
                 xreg = level, include.mean = TRUE)
summary(model_6) #shows ar(1) is not significant
coeftest(model_6)

#Model7: there's no seasonal
model_7 <- Arima(excess_ts, order = c(1,0,0), xreg = level)
summary(model_7)
coeftest(model_7)

########################################################
#model residual analysis
#ARIMA(1,1,1)
checkresiduals(model_1)
plot(LjungBoxTest(residuals(model_1), k = 2, lag.max = 20)) #acf at some point spike

sarima(excess_ts, p = 1, d = 1, q = 1)

#Model2 SARIMA:(1,0,0)*(0,0,1)
sarima(excess_ts, p = 1, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 10)

#Model3 :white noise 
sarima(excess_ts, p = 1, d = 0, q = 0, P = 1, D = 0, Q = 0, S = 10)

#Model4:
sarima(excess_ts, p = 1, d = 0, q = 1, P = 0, D = 0, Q = 1, S = 10, xreg = level)

#Model5:
sarima(excess_ts, p = 0, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 10, xreg = level)

#Model6:
sarima(excess_ts, p = 1, d = 0, q = 0, xreg = level)

#Model comparisons
df <- data.frame(col_1_res = c(model_1$aic, model_2$aic, model_3$aic, model_5$aic, model_6$aic),
                 col_2_res = c(model_1$aicc, model_2$aicc, model_3$aicc, model_5$aicc, model_6$aicc),
                 col_3_res = c(model_1$bic, model_2$bic, model_3$bic, model_5$bic, model_6$bic))

colnames(df) <- c("AIC", "AICc", "BIC")
rownames(df) <- c("ARIMA(1,1,1)", 
                  "ARIMA(1,0,0)(0,0,1)[10]", 
                  "ARIMA(1,0,0)(1,0,0)[10]", 
                  "ARIMA(0,0,0)(0,0,1)[10] with level shift", 
                  "ARIMA(1,0,0) with level shift")
df

#Use best model5 to forecast
#xreg = rep(1,20)
levelf=rep(1,20)
sarima.for(excess_ts,n.ahead=20,p = 0, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 10, xreg = level,newxreg = levelf)


#My comment about the process:
#1, EDA: plot, diff, log? smooth, seasonal?
#2, breakpoint, test (breakpoint known or not known)
#3, acf2, models
#4. model diagnois
#4, model comparisons
#5, select model, forecast

###########################################################################
#Intervention Analysis

#Still using the data of bith 
excess_frac <- (ab$boys - ab$girls)/ab$girls
excess_ts <- ts(excess_frac, frequency = 1, start = ab$year[1])
ts.plot(excess_ts)

#consider:1-trend,2-seasonality,3-outlier,4-long-run circle,5-constant variance,6-abrupt change
#1-trend,3-outlier
#smooth:lowess
tsplot(excess_ts)
lines(lowess(excess_ts),f=.02,col=4)
#detrend (can also use linear regression and then investigate residuals)
tsplot(diff(excess_ts))
dexcess_ts=diff(excess_ts)
#2-seasonality,4-long-run circle
ts.plot(dexcess_ts)
#no clear
acf2(dexcess_ts) #may be at 10 from ACF
#5-constant cariance
tsplot(log(excess_ts))
#6-abrupt change
#structural change analysis
lev_fit <- lm(excess_ts ~ 1)
summary(lev_fit)
#Yes
plot(excess_ts)
lines(ts(fitted(lev_fit), start = 1629, frequency = 1), col = 4)
#search for structural change (whether intercept(level) or slope different(trend))
break_excess <- breakpoints(excess_ts ~ 1, h = 0.1) #1 for level change, tt for trend change
summary(break_excess)
plot(break_excess)
break_excess
#one break at 1670
plot(excess_ts)
lines(fitted(break_excess, breaks = 1), col = 4)
lines(confint(break_excess, breaks = 1))
breakpoint = breakdates(break_excess,breaks=1)
breakpoint
#trend change
tt = 1:length(excess_ts)
trend_fit <- lm(excess_ts ~ tt)
summary(trend_fit)
plot(excess_ts)
lines(ts(fitted(trend_fit), start=1629, frequency = 1), col = 4)
break_trend <- breakpoints(excess_ts ~ tt, h = 0.1)
summary(break_trend)
plot(break_trend)
break_trend
#no trend break

#Use level change break before the break use the ARIMA
before= window(excess_ts,end=1670)
after=window(excess_ts,start=1671)
ts.plot(before)
acf2(excess_ts)
model1 = sarima(before,1,0,0,0,0,1,S=10)
model2 = sarima(before,0,0,1,0,0,1,S=10)
model3 = sarima(before,1,1,1,0,0,1,S=10)
model1
model2
model3
#select model2
for_after = sarima.for(before,40,0,0,1,0,0,1,S=10)
for_after
#compare forecasted and real
ts.plot(for_after$pred,after,col=1:2)
t.test(for_after$pred,after)
#significantly different

#Can fit arima with intervention on full data
#assumption:arima models for both region are the same
level <- c(rep(0, break_point$breakpoints), 
           rep(1, length(excess_ts) - break_point$breakpoints))
model4 =  sarima(excess_ts,0,0,1,0,0,1,S=10,xreg=level)
model4

