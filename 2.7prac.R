setwd("D:/Downloads")
Mice_WT=read.csv("WT.csv")
head(Mice_WT)

#assumptions fo linear regression:• Independence of observations;
#                                 • Linear relationship between variables;
#                                 • Homoscedacity of residuals;
#                                 • Normal distribution of residuals.

#age~weight
model1=lm(Weight~Age,Mice_WT)
summary(model1)
str(model1)
class(model1)
typeof(model1)
coef=coef(model1)

plot(model1,1)#check for homoscedacity 如果不满足方差齐性，看outlier，尝试添加二次项/log变换
plot(model1,2)#residual normal distribution
hist(resid(model1))
shapiro.test(resid(model1))

plot(model1,3)#残差平方根图，水平->方差齐性
plot(model1,4)#cook's distance plot
plot(model1,5)#残差-杠杆图 进入cook等高线的是outlier

cooks_d=cooks.distance(model1)
n <- nrow(Mice_WT)
outliers <- which(cooks_d > (4/n))
outliers
Mice_WT[outliers, ]
Mice_clean=Mice_WT[-outliers,]

model1_clean=lm(Weight~Age,Mice_clean)
summary(model1_clean)

#predict new data
new_data <- data.frame(Age = 20)
predict(model1, newdata = new_data)


library(ggplot2)
p=ggplot(Mice_WT)+
  geom_point(aes(x=Age,y=Weight))+
  theme_classic()+
  geom_smooth(aes(x=Age,y=Weight),method = "lm")

p_clean=ggplot(Mice_clean)+
  geom_point(aes(x=Age,y=Weight))+
  theme_classic()+
  geom_smooth(aes(x=Age,y=Weight),method = "lm")
p_clean

plot(model1_clean,1)
plot(model1_clean,5)

cor(Mice_clean$Age,Mice_clean$Weight,method="pearson")
cor.test(Mice_clean$Age,Mice_clean$Weight,method="pearson")
cor.test(Mice_clean$Age,Mice_clean$Weight,method="spearman")
cor.test(Mice_clean$Age,Mice_clean$Weight,method="kendall")

#median 靠近0说明正态性比较好
KO=read.csv("KO.csv")
head(KO)
ggplot(KO)+
  geom_point(aes(x=Age,y=Weight))+
  theme_classic()+
  geom_smooth(aes(x=Age,y=Weight),method="lm")

model2=lm(Weight~Age,KO)
summary(model2)
plot(model2,1)#只要没有随着fitted values改变就行
plot(model2,3)#homoscedacity!!!
plot(model2,5)
plot(model2,4)
cooks_d_1=cooks.distance(model2)
KO_row=nrow(KO)
which(cooks_d_1>4/KO_row)#no outliers
shapiro.test(resid(model2))#residual normally distributed

full_data=data.frame(rbind(KO,Mice_WT))
full_data$Genotype=c(rep("KO",20),rep("WT",20))
model_mixed=lm(Weight~Age+Genotype,full_data)
summary(model_mixed)

#Residual standard error: 3.548 on 37 degrees of freedom
#Multiple R-squared:  0.6855,	Adjusted R-squared:  0.6685 
#F-statistic: 40.32 on 2 and 37 DF,  p-value: 5.084e-10 like ANOVA, 说的是Model Explainable Variance和Residual Variance (残差变异)之间的差异

