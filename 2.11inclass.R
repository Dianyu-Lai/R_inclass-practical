library(dplyr)
library(tidyr)
library(tidyverse)
#Bayesian theorium
P_testcorrect_preg=0.95
P_testcorrect_nonpreg=0.8
P_preg=0.2222
#P_preg_testposi=P_preg_testposi/P_testposi
#=P_testposi_preg*P_preg/P_testposi
#=0.95*0.2222/P_posi
#=/P_posipreg+P_posinonpreg
#=/0.95*0.2222+0.2*0.7778
P_preg_posi=0.95*0.2222/(0.95*0.2222+0.2*0.7778)

#what if ++
#P_preg increase to 0.5757
#=P_posi2_preg*P_preg/P_posi2
#=P_posiposi_preg*0.2222/P_posi2_posi1*P_posi1
#=P_
P_preg_posi2 = 0.95*0.5757/(0.95*0.5757+0.2*0.4243)

pbinom(q=17,size=53,prob=0.247,lower.tail = FALSE)

#model based on observations
model=seq(from=0,to=0.6,by=0.02)
a=dbinom(x=18,size=53,prob=model)
barplot(a,names.arg=model,
        xlab="Probability of success",
        ylab="Likelihood of model based on observation")

#model based on prior hypothesis
b=dbinom(x=18,size=53,prob=0.247)
library(ggplot2)
n=53
p=0.247
alpha=p*n
beta=(1-p)*n
df=data.frame(prob_of_success=seq(0,1,0.02))
df$likelihood=dbeta(df$prob_of_success,shape1=alpha,shape2 = beta)
ggplot(df,aes(x=prob_of_success,y=likelihood))+
  geom_col()+
  xlim(0,0.6)+
  theme_minimal()

Pd1=df %>% 
  mutate(prob_of_success = round(prob_of_success, 2)) %>% 
  filter(prob_of_success == 0.35)
d=dbinom(x=18,size=53,prob=0.247)
d
0.05*7==0.35

library(ggplot2)
library(tidyr)

# 参数设置
n <- 53
k <- 18
p_h0 <- 0.247

# 定义 x 轴 (成功概率 p)
p_grid <- seq(0, 1, length.out = 1000)

# 1. Prior (先验): 假设使用弱信息的 Beta 分布，均值为 0.247
# 我们给先验赋予较小的权重（例如 n_prior = 10），这样它比较宽，容易被数据修正
n_prior <- 10
alpha_prior <- n_prior * p_h0
beta_prior <- n_prior * (1 - p_h0)
prior <- dbeta(p_grid, alpha_prior, beta_prior)

# 2. Likelihood (似然): 数据给出的证据
# 注意：似然函数通常需要归一化，以便与概率分布在同一尺度对比
likelihood <- dbinom(k, n, p_grid) 
likelihood_scaled <- likelihood / (sum(likelihood) * (p_grid[2]-p_grid[1])) # 归一化处理

# 3. Posterior (后验): 结合先验和数据
# Beta 分布的后验参数计算公式：alpha_post = alpha_prior + k; beta_post = beta_prior + (n-k)
alpha_post <- alpha_prior + k
beta_post <- beta_prior + (n - k)
posterior <- dbeta(p_grid, alpha_post, beta_post)

# 整合数据框
df_all <- data.frame(
  p = p_grid,
  Prior = prior,
  Likelihood = likelihood_scaled,
  Posterior = posterior
)

df_long <- df_all %>% pivot_longer(cols = -p, names_to = "Type", values_to = "Density")

ggplot(df_long, aes(x = p, y = Density, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Prior" = "gray60", "Likelihood" = "darkgreen", "Posterior" = "steelblue")) +
  scale_linetype_manual(values = c("Prior" = "dashed", "Likelihood" = "dotted", "Posterior" = "solid")) +
  xlim(0, 0.7) +
  labs(
    title = "贝叶斯更新过程: 从先验到后验",
    subtitle = "先验中心在 0.247，数据观测值为 18/53 (约 0.34)",
    x = "成功概率 (Prob of Success)",
    y = "密度 / 似然 (Density/Likelihood)"
  ) +
  theme_minimal()

ggplot(df_long, aes(x = p, y = Density, fill = Type)) +
  geom_area(alpha = 0.5) +
  facet_wrap(~Type, scales = "free_y") + # 分成三格
  xlim(0, 0.7) +
  theme_minimal() +
  labs(title = "贝叶斯推断的三个阶段")

#task for practice
p <- c(0, 0.1, 0.25, 0.35, 0.5)

# 数据（可以改）
n <- 53
x <- 13   # 比如成功人数

# likelihood
likelihood <- dbinom(x, size = n, prob = p)

prior <- rep(1/length(p), length(p))

# posterior（先占位）
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

posterior

prior <- c(0.05, 0.1, 0.2, 0.35, 0.3)
prior <- prior / sum(prior)

# 让均值 ~0.25，例如：
alpha <- 2.5
beta <- 7.5

prior <- dbeta(p, alpha, beta)
prior <- prior / sum(prior)

n <- 80
x <- 20   # 比例差不多
