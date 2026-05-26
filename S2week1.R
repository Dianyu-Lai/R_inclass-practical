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

#H0: those groups of painkillers are the same.
#HA: those groups of painkillers are they different

#newH0: the difference between groups are the same as difference within gorups
#newHA: the difference between groups are the different from difference within gorups

sample_index=sample(1:nrow(drug),2)
drug[sample_index,]

within=c()
inter=c()
n=1:1e3
for(i in n){sample_index=sample(1:nrow(drug),2)
          if(drug[sample_index[1],1]==drug[sample_index[2],1]){
            within=c(within,abs(drug[sample_index[1],2]-drug[sample_index[2],2]))
          }else{
              inter=c(inter,abs(drug[sample_index[1],2]-drug[sample_index[2],2]))
              }
}

mean(inter)
mean(within)

df=data.frame(group=c(rep("within",343),rep("inter",657)),difference=c(within,inter))
ggplot(df,aes(x=group,y=difference,fill=group))+
  geom_boxplot()+
  theme_bw()

#randomization test should be used
#shapiro.test(within)
#shapiro.test(inter)
t.test(within,inter,alternative = "two.sided")#we cannot use this
wilcox.test(within,inter)
wilcox.test(within,inter,alternative = "less")
#the distribution of the inter group is not the same as within group.
#the median of within group is less than the inter group

result=mean(replicate(1e5,sample(within,replace = T)>sample(inter,replace = T)))
1-result
p=0.711144


druga=drug%>%filter(treatment=="drugA")%>%select(pain)
drugb=drug%>%filter(treatment=="drugB")%>%select(pain)
placebo=drug%>%filter(treatment=="placebo")%>%select(pain)

#shapiro.test(druga$pain)
#shapiro.test(drugb$pain)不是看raw data是否正态，是residual
#shapiro.test(placebo$pain)

t.test(druga$pain,drugb$pain)
wilcox.test(druga$pain,placebo$pain)
wilcox.test(drugb$pain,placebo$pain)
#a b, placebo b different, a placebo the same

df1=df
mean1=c()
for (i in n){
df1$newgroup=sample(df1$group)
pain1=df1%>%filter(newgroup=="within")%>%select(difference)
pain2=df1%>%filter(newgroup=="inter")%>%select(difference)
mean1=c(mean1,abs(mean(pain1$difference)-mean(pain2$difference)))
}

m1=df1%>%filter(group=="within")%>%select(difference)
mean(m1$difference)
m2=df1%>%filter(group=="inter")%>%select(difference)
mean(m2$difference)
m=abs(mean(m1$difference)-mean(m2$difference))
m

hist(mean1,breaks=15,probability=T,xlim = range(0,0.6))
x95 <- quantile(mean1, 0.95, na.rm = TRUE)
abline(v = x95, lwd = 2, lty = 2)
abline(v=m,lwd=2,lty=2,col="red")

mean(m>mean1)

res=aov(data=drug,pain~treatment)
res
resd=residuals(res)
shapiro.test(resd)
summary(res)
tapply(drug$pain, drug$treatment, mean)
leveneTest(data=drug,pain~treatment)
TukeyHSD(res)
#TukeyHSD honestly significant difference



#pro set
jlyb=read.csv("jellybeans.csv")
ggplot(jlyb,aes(x=colour,y=score,fill=colour))+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#20 tests,false positive rate
1-(0.95)^20#at least 1 false positive
beige=jlyb%>%filter(colour=="beige")%>%select(score)
shapiro.test(beige$score)
ctrl=jlyb%>%filter(colour=="control")%>%select(score)
shapiro.test(ctrl$score)
t.test(ctrl,beige)
mauve=jlyb%>%filter(colour=="mauve")%>%select(score)
t.test(ctrl,mauve)

set.seed(123)
n=1:1e5
within1=c()
between1=c()
for (i in n) {
  idx=sample(1:nrow(jlyb),2)
  if(jlyb[idx[1],1]==jlyb[idx[2],1]){
    within1=c(within1,abs(jlyb[idx[1],2]-jlyb[idx[2],2]))
  }else{
    between1=c(between1,abs(jlyb[idx[1],2]-jlyb[idx[2],2]))
  }
}

df2=data.frame(group=c(rep("within",4503),rep("between",95497)),difference=c(within1,between1))

ggplot(df2,aes(x=group,y=difference,fill=group))+
  geom_boxplot()+
  theme_bw()

mean2=c()
for (i in 1:1e3) {
  df2$newgroup=sample(df2$group)
  score1=df2%>%filter(newgroup=="within")%>%select(difference)
  score2=df2%>%filter(newgroup=="between")%>%select(difference)
  mean2=c(mean2,abs(mean(score1$difference)-mean(score2$difference)))
}

n1=df2%>%filter(group=="within")%>%select(difference)
n2=df2%>%filter(group=="between")%>%select(difference)
n=abs(mean(n1$difference)-mean(n2$difference))

hist(mean2,probability=T,breaks=15,xlim=range(0,0.2))
score95=quantile(mean2,0.95,na.rm = T)
abline(v=score95,lwd=2,lty=2,col="red")
abline(v=n,lwd=2,lty=2,col="blue")

jlyb$colour=factor(jlyb$colour)
res=aov(data=jlyb,score~colour)
summary(res)
tukey=TukeyHSD(res)
df_tukey=as.data.frame(tukey$colour)
which(df_tukey$`p adj`<0.05)
shapiro.test(residuals(res))

#2 ways controlling the a level
#1. bonferroni adjustment
#2. holm adjustment
pairwise.t.test(jlyb$score, jlyb$colour, p.adjust.method = "bonferroni")
pairwise.t.test(jlyb$score, jlyb$colour, p.adjust.method = "holm")

