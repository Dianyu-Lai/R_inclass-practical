#普通最小二乘回归的 4 个核心假设：残差呈正态分布且同方差（homoscedastic）；
#观测值相互独立；
#误差项相互独立；
#自变量与因变量呈线性关系。

#假设不满足的解决办法：对数据做变换（对数、平方根、秩变换等）；
#移除异常值；cooks_distance
#增加解释变量；
#采用非参数统计或非线性模型。spearman/non linear model
data("cars")
head(cars,9)
class(cars)
library(ggplot2)
library()
p=ggplot(cars,aes(x=speed,y=dist))+
  geom_point()+
  theme_classic()
p
p1=ggplot(cars)+
  geom_density(aes(x=speed),color="red")+
  geom_density(aes(x=dist),color="blue")+
  labs(
    x = "Value",
    y = "Density",
    title = "Density Distribution: Speed (red) vs Distance (blue)"
  ) +
  theme_classic()
p1

cor(cars$speed,cars$dist,method="pearson")
cor(cars$speed,cars$dist,method="kendall")
cor(cars$speed,cars$dist,method="spearman")
cor.test(cars$speed,cars$dist,method="pearson")

cov_speed_dist=0
for (i in 1:length(cars$speed)){
  cov_speed_dist=cov_speed_dist+(cars$speed[i]-mean(cars$speed))*(cars$dist[i]-mean(cars$dist))/(length(cars$speed)-1)
}
cov_speed_dist
r=cov_speed_dist/sqrt(var(cars$speed)*var(cars$dist))
r

model1=lm(cars$speed~cars$dist)
str(model1)
model1

summary(model1)

model2=lm(dist~speed,cars)
summary(model2)

cooks_d=cooks.distance(model1)
threshold=4/length(cars$speed)
influential_points=which(cooks_d>threshold)
# 绘制 Cook's 距离图
plot(cooks_d, 
     type = "h", 
     main = "Cook's Distance for cars Model",
     xlab = "Observation", 
     ylab = "Cook's Distance")

# 添加阈值线
abline(h = threshold, col = "red", lty = 2, lwd = 2)

# 标记影响点
points(influential_points, cooks_d[influential_points], 
       col = "red", pch = 19)
