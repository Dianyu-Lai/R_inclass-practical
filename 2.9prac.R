library(datasets)
library(ggplot2)
install.packages("trend")
library(forecast)
library(trend)

head(ldeaths)
summary(ldeaths)
str(ldeaths)
window(ldeaths)
plot(ldeaths)
abline(a=intercept,b=slope)

head(time(ldeaths))

ld_model=lm(ldeaths~time(ldeaths))
intercept=as.numeric(ld_model$coefficients[1])
slope=as.numeric(ld_model$coefficients[2])
summary(ld_model)

plot(ld_model)
plot(ld_model,1)
plot(ld_model,2)
plot(ld_model,3)
plot(ld_model,4)
plot(ld_model,5)

#deal with outliers
cd=cooks.distance(ld_model)
thre=4/length(ldeaths)
ldeaths[which(cd>thre)]
outlier_indices=which(cd>thre)
ldeaths_new=ldeaths
ldeaths_new[outlier_indices]=NA

#decompose the ts if it has periods (seasonality)
lung_dec <- decompose(ldeaths)
lung_dec
plot(lung_dec)
summary(lung_dec$trend)#the moving average

#calculate the diff to see if there is any trend
#!!! 判断是否有趋势的几种方法：
#cortest 对时间和值计算相关性，如果有相关就有trend
#mk.test 对ts直接分析相关性
#diff + ttest
#lm 用线性模型分析看斜率（如果本身不满足线性模型假设要慎用，特别是残差自相关）

#！！！判断数据是否随机：
#acf 考察残差是否自相关
#run test 也就是random walk test，看数据的表现（正负...）

#！！！判断季节性：
#seasonal MK test 对每年固定时间用mktest
#多线性回归 lm(x~factor(season))对每个季节的数据单独进行线性回归，如果有固定pattern斜率会0
diff=diff(lung_dec$trend,lag=1)
summary(diff)
t.test(diff)
plot(lung_dec$x)
lines(lung_dec$trend)

#calculate CI

t1 <- 1:length(ldeaths)
model <- lm(ldeaths ~ t1)
summary(model)
#利用predict函数计算CI，不需要自己手动计算 library(trend)
pred <- predict(model, interval = "confidence")

plot(t1,ldeaths,type="l")
lines(x=t1,y=pred[,1],col="red")
lines(x=t1,y=pred[,2])
lines(t1,pred[,3])

resid_sd=sd(lung_dec$random,na.rm = TRUE)
t_val=qt(0.975,df=11)
upper=lung_dec$trend+t_val*resid_sd/sqrt(12)
lower=lung_dec$trend-t_val*resid_sd/sqrt(12)
lines(upper,col="red",lty=2)
lines(lower,col="red",lty=2)
lines(lung_dec$trend+lung_dec$seasonal,col="blue")

#how to calculate ma season random
#ma can be calculated using ma(centered=TRUE) library(forecast)
#season: mean(x-ma) for every season
#random=x-ma-season
monthly_variance <- tapply(ldeaths, cycle(ldeaths), var)
time=1:12
boxplot(monthly_variance~time)
boxplot(ldeaths ~ cycle(ldeaths))

plot(lung_dec$random)
t.test(diff(lung_dec$random))# additive model is ok

next_2_year=(length(ldeaths)+1):(length(ldeaths)+24)
mean_ma=mean(lung_dec$trend,na.rm = TRUE)
seasonal_2_year=rep(lung_dec$figure,2)
estimate_death=mean_ma+seasonal_2_year
estimate_ts <- ts(estimate_death, start = c(1980, 1), frequency = 12)
plot(lung_dec$x,xlim=c(1974,1982))
lines(estimate_ts,col="red")

plot(mdeaths)
plot(fdeaths)
summary(mdeaths)
summary(fdeaths)
window(mdeaths)
window(fdeaths)
#比较两个ts的相关性
ccf(mdeaths,fdeaths)
ccf(diff(mdeaths), diff(fdeaths))

lm_mdeaths=lm(mdeaths~time(mdeaths))
summary(lm_mdeaths)
m_lung_dec=decompose(mdeaths)
f_lung_dec=decompose(fdeaths)


##uspop
plot(uspop)
hist(uspop,breaks=12)
window(uspop)
log_uspop=log(uspop)
plot(log_uspop)
lm_uspop=lm(uspop~time(uspop))
cof=lm_uspop$coefficients
plot(uspop)
abline(a=as.numeric(cof[1]),b=as.numeric(cof[2]),col="red")

lm_log_uspop=lm(log_uspop~time(log_uspop))
cof_log=lm_log_uspop$coefficients
plot(log_uspop)
abline(a=cof_log[1],b=cof_log[2],col="red")
plot(lm_log_uspop,1)
diff_resid=diff(lm_log_uspop$residuals)
plot(diff(lm_log_uspop$residuals))
plot(diff(diff_resid))

acf(diff(log_uspop,difference = 2))#white noise
acf(diff(log_uspop,differences = 1))
plot(log_uspop)
plot(diff(log_uspop))
plot(diff(log_uspop,difference=2))

arima=arima(log_uspop, order=c(0,2,0))
acf(residuals(arima))
arima(log_uspop, order=c(0,2,1))

#white noise
set.seed(42)
wt=ts(rnorm(100,0,10))
hist(wt)
plot(wt)
abline(h=mean_wt,col="red")
lines(MA,col="blue")

mean_wt=mean(wt)
MA=ma(wt,order=3)
t=1:100
wt_model=lm(wt~t)
summary(wt_model)
acf(wt)


#random walk
walk=c(0)
for (i in t){
  walk=c(walk,walk[i]+wt[i])
}

#用函数不用写循环
#walk=ts(cumsum(wt))

walk_ts=ts(walk)
plot(walk_ts)
lines(walk_ma,col="red")
hist(walk_ts,breaks=10)
walk_ma=ma(walk_ts,order=10)
sd(walk_ts)
acf(walk_ts)
plot(diff(walk_ts))
mean(diff(walk_ts))
sd(diff(walk_ts))
mean(walk_ts)

##time as regressor
t3=c(0)
b0=1
b1=0.05
for (i in t){
t3=c(t3,b0+i*b1+wt[i])
}
t3=ts(t3)
plot(t3)
t3_ma=ma(t3,order=5)
lines(t3_ma,col="red")
acf(t3)
#如果随时间增长的趋势很小（b1=0.05）acf检测不出来自回归性

for(i in seq(from = 0.1, to = 0.7, by = 0.2)){
  lm_ts <- i * (1:100) + wn
  lm_ts <- ts(lm_ts)
}
mk.test(t3)
mk.test(walk_ts)
mk.test(ldeaths)
cor.test(1:101,t3)
