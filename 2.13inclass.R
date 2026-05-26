data(iris)
mydata=iris[,-5]
normalmydata=scale(mydata)
class(normalmydata)
class(iris)
set.seed(42)

custom_kmeans=function(dataset,num_cluster,max_iter=100){
  data_matrix=as.matrix(dataset)
  num_rows=nrow(data_matrix)
  
  #initial assignment
  initial_sample=sample(1:num_rows,num_cluster)
  center=data_matrix[initial_sample,,drop=FALSE]
  
  cluster_assignment=numeric(num_rows)
  
  for (iteration in 1:max_iter){
    previous_center=center
    for (i in 1:num_rows){
      current=data_matrix[i,]
      distance=apply(center,1, function(center){
        sqrt(sum((current-center)^2))
      })
      
      cluster_assignment[i]=which.min(distance)
    }
    
    #update
    for (j in 1:num_cluster){
      points_in_cluster=data_matrix[cluster_assignment==j,,drop=FALSE]
      
      if(nrow(points_in_cluster)>0){
        center[j,]=colMeans(points_in_cluster)
      }
    }
    
    if(identical(center,previous_center))break
  }
  return(list(
    cluster=cluster_assignment,
    center=center,
    iteration=iteration
  ))
}

custom_kmeans(normalmydata,num_cluster = 3)#可以再加上不同的距离计算/初始随机设置多个initial再自动比较

data("USArrests")
arrest=scale(USArrests)
str(arrest)
head(arrest)

distance_matrix=dist(arrest,method = "euclidean")

hc_res=hclust(distance_matrix,method = "ward.D2")
# 5. 可视化：绘制树状图 (Dendrogram)
# 层次聚类最迷人的地方就是这张树状图
plot(hc_res, 
     cex = 0.6,          # 缩小标签字体，避免重叠
     hang = -1,          # 让所有叶子节点对齐到同一水平线
     main = "criminal rate of US",
     xlab = "States", 
     ylab = "Height",
     sub = "")           # 去掉底部的默认副标题

# 6. “砍树”：决定最终的聚类数量
# 假设我们通过观察树状图，决定将所有州分为 4 类 (k = 4)
# rect.hclust() 可以在原有的树状图上画红框，直观展示分类结果
rect.hclust(hc_res, k = 4, border = c("red", "blue", "green", "purple"))

# 7. 提取分类标签
# 使用 cutree() 函数，根据设定的类别数 k 提取每个州对应的类标号
cluster_labels <- cutree(hc_res, k = 4)

