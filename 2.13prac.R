library(ggplot2)
setwd("D:/Downloads")
guest=read.csv("guests.csv")

head(guest)
plot(x=guest$age_norm,y=guest$hours_norm)
data=guest[,-1]
head(data)
a=kmeans(data,4,iter.max = 10)
a$cluster
a$tot.withinss

p=ggplot(data)+
  geom_point(aes(x=age_norm,y=hours_norm),shape=3,size=2,color="red")

p+geom_point(clu_res)

clust=function(data,num_clust,max_iter){
  matrix_data=as.matrix(data)
  num_center=num_clust
  num_data=nrow(matrix_data)
  assignment=numeric(num_data)
  
  initial=sample(1:num_data,num_center,replace=FALSE)
  center=matrix_data[initial,]
  
  for (iter in 1:max_iter){
    previous_center=center
    for (i in 1:num_data) {
      current=matrix_data[i,]
      
      distance=apply(center,1,function(c){
    sqrt(sum((current-c)^2))
        })
      
      assignment[i]=which.min(distance)
    }
    
    for (j in 1: num_clust){
      points=matrix_data[assignment==j,,drop=FALSE]
      if (nrow(points)>0){
        center[j,]=colMeans(points)
      }
    }
    if (identical(center,previous_center)) break
  }
  grouped_clusters_data <- lapply(1:num_clust, function(k) {
    matrix_data[assignment == k, , drop = FALSE]
  })
  
  clusters=data.frame()
  cat("iteration =", iter, "\n")
  return(list(centers=center,
              cluster_assignment=assignment,
              cluster_data = grouped_clusters_data))
}

clu_res=clust(data,4,10)


#hierachical clust
distance=dist(data,method="euclidean",diag=FALSE)
hc_res=hclust(distance,method="ward.D2")
plot(hc_res)
hc_res$labels=guest$names

table_2=cutree(hc_res,k=2)
table_3=cutree(hc_res,k=3)
table_9=cutree(hc_res,k=9)

seating_plan_3 <- data.frame(
  Guest = guest$names, 
  Table_Number = table_3
)
print("--- 方案：分 3 桌的宾客名单 ---")
print(seating_plan_3[-1])

guests_order=guest[hc_res$order,1]
guests_order

# 设定随机种子以保证结果可复现
set.seed(123)

best_wcss_4 <- Inf         # 初始化一个无穷大的 WCSS 作为基准
best_kmeans_4 <- NULL      # 用于存放最优的模型结果

# 运行 100 次 4-means 聚类，寻找 WCSS 最小的那一次
for (i in 1:100) {
  # nstart = 1 表示每次循环只使用当前的一个随机初始中心
  current_km <- kmeans(data, centers = 4, nstart = 1)
  
  # 提取当前的 WCSS
  current_wcss <- current_km$tot.withinss
  
  # 如果当前的 WCSS 比记录的最佳值还要小，则“刷新”最佳记录
  if (current_wcss < best_wcss_4) {
    best_wcss_4 <- current_wcss
    best_kmeans_4 <- current_km
  }
}

cat("经过 100 次随机初始化，找到的最佳 4-means 聚类的 WCSS 为:", best_wcss_4, "\n")
cat("各个簇的人数分配为:", best_kmeans_4$size, "\n")

# 其实在 R 语言中，你不需要自己写这个 for 循环。
# 原生的 kmeans 函数自带了 nstart 参数，就是为了干这个的：
# official_best_km <- kmeans(data, centers = 4, nstart = 100)
# 这行代码的底层逻辑和我们上面的循环完全一模一样，但执行速度更快。

k_values <- 1:10
wcss_values <- numeric(length(k_values))

set.seed(456)
for (k in k_values) {
  # 这里我们直接使用 nstart = 25 这个快捷参数，确保每次对于给定的 K 都能找到较优的 WCSS
  km <- kmeans(data, centers = k, nstart = 25)
  wcss_values[k] <- km$tot.withinss
}

# 绘制标准的肘部图 (Elbow Plot)
plot(k_values, wcss_values, 
     type = "b",          # "b" 表示同时画出点和线
     pch = 19,            # 实心圆点
     col = "blue",        # 蓝色
     lwd = 2,             # 线条加粗
     main = "K-means 肘部法则 (Elbow Method)",
     xlab = "聚类数量 (Number of Clusters K)", 
     ylab = "总簇内误差平方和 (Total WCSS)")