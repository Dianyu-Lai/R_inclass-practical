pchisq(4,3,lower.tail = FALSE)
pchisq(4,3,lower.tail = TRUE)
pool_season=c(40,30,18,28)
chisq.test(pool_season,correct = FALSE,p=rep(0.25,4))
Severe <- data.frame(Spring = 5, Summer = 1, Fall =1, Winter=9)
Mild <- data.frame(Spring = 8, Summer= 5, Fall =2 , Winter = 5)
Sporadic <- data.frame(Spring = 9, Summer = 8, Fall =3 , Winter = 9)
Never <- data.frame(Spring = 18, Summer = 16, Fall =12 , Winter = 5)
seasonal_data=rbind(Severe,Mild,Sporadic,Never)
chisq.test(seasonal_data)
file.path(Sys.getenv("HOME"), ".Rprofile")
fisher.test(matrix(c(1513,74932,342,76103),ncol=2))

# 1. 构造三维数据的二维矩阵（行：基因+性别组合，列：生存状态）
three_way_data <- matrix(
  c(34, 7,  # WT-雌：存活35，死亡15
    40, 9,  # WT-雄：存活30，死亡20
    25, 20,  # KO-雌：存活15，死亡35
    20, 15), # KO-雄：存活20，死亡30
  nrow = 4,  # 4行（4种基因+性别组合）
  ncol = 2,  # 2列（存活/死亡）
  byrow = TRUE,
  # 给行列命名，方便解读
  dimnames = list(
    "gene+gender" = c("WT-female", "WT-male", "KO-female", "KO-male"),
    "status" = c("alive", "dead")
  )
)

# 查看整理后的数据
print(three_way_data)
chisq.test(three_way_data)

wilcox.test()
kruskal.test()

library(DESeq2)
