##guiness factory
#PH1=0.5
#PH2=0.5
PDATAH1=0.7
PDATAH2=0.4

#1.1
BF1=PDATAH1/PDATAH2
BF1
#PH1DATA=0.35/0.55=0.6363
#PH2DATA=0.2/0.55=0.3636

#1.2
#PDATA = 0.55
#PDATAH1=PH1DATA*PDATA/PH1=2PH1DATA*PDATA
#PDATAH2=2PH2DATA*PDATA
#PH2DATA+PH1DATA=1
#So PDATAH1*PH2DATA=PDATAH2*(1-PH2DATA)

PH1=seq(0,1,0.1)
PH2=seq(0,1,0.1)
grid=expand.grid(PH1=PH1,PH2=PH2)
grid$BF=with(grid,
             ifelse(PH1*PH2==0,NA,PH1/PH2))
ggplot(grid,aes(x=PH1,y=PH2,fill=BF))+
  geom_tile()+
  theme_minimal()

#1.3
#PH1=0.2
#PH2=0.8
#PDATA=0.46
#PH1DATA=0.7*0.2/0.46=0.304
#PH2DATA=0.4*0.8/0.46=0.696
#4PDATAH1*PH2DATA=PDATAH2*(1-PH2DATA)
#PH2DATA (posterior) decrease
#BF不变
#more likely for H0 to be false 

#dice game
pbinom(7,20,prob=1/6,lower.tail = FALSE)
pbinom(7,20,prob=5/6,lower.tail = TRUE)
#first hypothesis
#H0: 1 six (probably not true)
#...
#H4: 5 sixes (probably not true)
#so here we examine 2-4 sixes, 3 hypothesis
#for each hypothesis, calculate the likelihood
P_DATA_H1=dbinom(7,20,prob=1/6)
P_DATA_H2=dbinom(7,20,prob=2/6)
P_DATA_H3=dbinom(7,20,prob=3/6)
P_DATA_H4=dbinom(7,20,prob=4/6)
P_DATA_H5=dbinom(7,20,prob=5/6)
seq=seq(1/6,5/6,1/6)
prob1=dbinom(7,20,prob = seq)
prob=c(P_DATA_H1,P_DATA_H2,P_DATA_H3,P_DATA_H4,P_DATA_H5)
barplot(prob, names.arg = six_seq,
        xlab = "Hypothesis (p = k/6)",
        ylab = "P(Data | Hi)",
        main = "Likelihood under Different Hypotheses")
#how to compare
#just pick the bigest one to compare with others
BF21=P_DATA_H2/P_DATA_H1
BF21 #strongly support H2
BF23=P_DATA_H2/P_DATA_H3
BF23 #slightly support H2

#we dont know PDATA cause dont know P_Hi
#P_hi depends on prior hypothesis
#BF可以用posterior ratio和prior ratio计算
#也可以用likelihood计算 本质是数学变形
#likelihood->在假设条件下事件发生的概率
#一般可以用dbinom计算似然