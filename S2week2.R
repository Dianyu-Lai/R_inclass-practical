library(ggplot2)
library(tidyverse)
library(tidyr)
library(car)
library(lme4)

getwd()
setwd("D:/Downloads")
drug=read.csv("drug_trial.csv")
head(drug,5)
summary(drug)
table(drug$treatment)
drug$treatment=factor(drug$treatment)

ggplot(drug,aes(x=treatment,y=pain,fill = treatment))+
  geom_boxplot(outliers = T)+
  theme_bw()

#3 assumptions
#1. random and independent
#cause we can't change the data, we have to assume it is true

#2. residual normality
model=aov(pain~treatment,drug)
residual=model$residuals
shapiro.test(residual)

#3. variance homogeneity
leveneTest(pain~treatment,drug)

#run anova
summary(model)
TukeyHSD(model)
pairwise.t.test(drug$pain,drug$treatment,p.adjust.method="holm")
pairwise.t.test(drug$pain,drug$treatment,p.adjust.method ="bonf")

mouse=read.csv("mouse_experiment.csv")
mouse$genotype=factor(mouse$genotype)
mouse$diet=factor(mouse$diet)
str(mouse)
anyNA(mouse)

ggplot(mouse,aes(x=genotype,y=weight_gain,fill=diet))+
  geom_boxplot()+
  theme_bw()

summary_stats <- mouse %>%
  group_by(genotype, diet) %>%
  summarise(
    mean_gain = mean(weight_gain, na.rm = TRUE),
    se_gain = sd(weight_gain, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  )

ggplot(summary_stats, aes(x = diet, y = mean_gain, color = genotype, group = genotype)) +
  geom_point(size = 3) +                
  geom_line(linewidth = 1) +            
  geom_errorbar(aes(ymin = mean_gain - se_gain, ymax = mean_gain + se_gain), width = 0.1) +
  labs(
    x = "Diet Condition",
    y = "Weight Gain (g)",
    title = "Weight Gain by Genotype and Diet in Mice",
    color = "Genotype"
  ) +
  theme_bw()

model1=aov(data=mouse,weight_gain~genotype*diet)
shapiro.test(model1$residuals)
leveneTest(data=mouse,weight_gain~genotype*diet)

summary(model1)
TukeyHSD(model1)
pairwise.t.test(mouse$weight_gain,interaction(mouse$genotype,mouse$diet),p.adjust.method="holm")


#pro set
set.seed(123)
n=seq(10,100,by=10)
lapply(n, function(n){
label=c(rep("A",n/2),rep("B",n/2))
digit=rnorm(n,0,1)
inte=runif(n,min=0,max=100)
data=digit+inte
df=data.frame(label=label,score=data)
modeldf=aov(data=df,score~label)
qqnorm(modeldf$residuals,main = paste("sample size = ",n))
qqline(modeldf$residuals)})

ds1=runif(50,0,100)
