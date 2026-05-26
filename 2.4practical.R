# assumptions of chi square test: No expected cell frequencies < 1
# No more than 20% of cells are less than 5

library(ggplot2)

Poll_seasons <- data.frame(
  Spring = 40,
  Summer = 30,
  Autum = 18,
  Winter = 28
)
set.seed(123)
n_sample=sum(Poll_seasons)
expected <- sum(Poll_seasons) * 0.25

#打乱的思路实际上是假定有一个满足H0假设的无穷大总体，在总体中进行相同的抽样（上面的observation同样视为一次抽样），
#之后再多次重复（n=1000）这个过程，比较观测值在所有抽样结果中的分位数，如果满足>95%就可以认为H0不成立

n_sim=1:1000
chi_sim=numeric(1000)
for (i in n_sim) {
result_sim=sample(1:4,n_sample,replace = TRUE,prob=rep(0.25,4))
sample_count=c(as.numeric(table(result_sim)))
chi_sim[i]=sum((sample_count-expected)^2/expected)
}
chi_sim
plot(density(chi_sim))
df=data.frame(n_sim,chi_sim,row.names = n_sim)
ggplot(df,aes(chi_sim))+
  geom_density()+
  geom_vline(xintercept=a$statistic,color="red")

chi_res=chisq.test(Poll_seasons)
chi_p=chi_res$p.value
p_sim=sum(chi_sim>chi_res$statistics)/length(chi_sim)

#part2
chi_value=c()
df_vc=c()
for (i in 1:9){
chi_value=c(chi_value,rchisq(10000,df=i))
df=c(df,rep(i,10000))
}

df_chi=data.frame(chi_value=chi_value,
                  df=factor(df))

ggplot(df_chi, aes(x = chi_value, color = df)) +
  geom_density(alpha = 0.2,linewidth = 0.65) +
  coord_cartesian(xlim = c(0, 30)) +
  labs(title = "Chi-Squared Distribution",
       x = expression(chi^2), y = "Density",
       fill = "df", color = "df") +
  theme_classic()

#part3
allergy_data <- data.frame(
  Season = rep(c("Spring", "Summer", "Fall", "Winter"), 4),
  Allergy_Type = rep(c("Severe allergies", "Mild allergies", "Sporadic allergies", "Never allergic"), each = 4),
  Count = c(5, 1, 1, 9,
            8, 5, 2, 5,
            9, 8, 3, 9,
            18, 16, 12, 5)
)

# 1. Bar plot 
ggplot(allergy_data, aes(x = Season, y = Count, fill = Allergy_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Plot: Season vs Allergy Type",
       x = "Season", y = "Count", fill = "Allergy Type") +
  theme_classic()

# 2. Balloon plot 
ggplot(allergy_data, aes(x = Season, y = Allergy_Type, size = Count, color = Count)) +
  geom_point(alpha = 0.7) +
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "lightblue", high = "darkred") +
  labs(title = "Balloon Plot: Season vs Allergy Type",
       x = "Season", y = "Allergy Type", size = "Count") +
  theme_classic()

# 3. Mosaic plot
allergy_data$Season <- factor(allergy_data$Season, 
                              levels = c("Spring", "Summer", "Fall", "Winter"))
allergy_data$Allergy_Type <- factor(allergy_data$Allergy_Type, 
                                    levels = c("Severe allergies", "Mild allergies", 
                                               "Sporadic allergies", "Never allergic"))
allergy_matrix <- xtabs(Count~Allergy_Type+Season, data = allergy_data)
mosaicplot(allergy_matrix,
           main  = "Mosaic Plot: Season vs Allergy Type",
           color = c("red","green","pink","blue"))

chisq.test(allergy_matrix)

# part4
df_mice <- data.frame(
  Genotype = rep(c("WT", "KO"), each = 2),
  Status = rep(c("Alive", "Dead"), 2),
  Count = c(7, 3, 2, 7)
)
df_mice$Genotype=factor(df_mice$Genotype,levels=c("WT","KO"))
df_mice$Status=factor(df_mice$Status,levels=c("Alive","Dead"))
mice_matrix=xtabs(Count~Genotype+Status,df_mice)
chisq.test(mice_matrix)
fisher.test(mice_matrix)
