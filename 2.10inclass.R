num_posi=c()

for (i in 100:200){
  num_staff=i
  num_steal=0.1*num_staff
  num_good=0.9*num_staff
  num_posi=c(num_posi,num_steal*0.8+num_good*0.2)
}
head(num_posi)

which(num_posi>=50)
tail(num_posi,10)
193*0.08
#so about 193 of total staff and about 20 of them are thieves.

#using Bayes' theory
P_posi_thief=0.8
P_posi_good=0.2
N_posi=50

#if we know the P_posi, we can calculate the total staff num
#P_posi=P_posithief+P_posigood
#=Pthief*P_posi_thief+Pgood*P_posi_good
#=0.1*0.8+0.9*0.2
#=0.26
#P_thief_posi=P_thiefposi/P_posi
#=P_posi_thief*P_thief/P_posi
#=0.8*0.1/0.26
#=0.3076923
50*0.3076923

#so total staff num
TSN=N_posi/0.26
thief=0.1*TSN
thief
round(thief)
#so about 19-20 staffs are thieves

#in the following Markov chain question, i name H as 1 T as 0
#so HTTH -> 1001
hyperrecord=c()
res=replicate(3000,{
seq=c()
coin=c(0,1)
target=c(1,0,0,1)
record=c()
for (i in 1:200){
  seq=c(seq,sample(coin,1,replace=TRUE))
}
for (a in 1:197){
  window=seq[a:(a+3)]
  if (all(window==target)){
    record=c(record,a)
  }
}
if (length(record) > 0) {
  hyperrecord<<-c(hyperrecord,record[1]) }else NA
})
mean(hyperrecord)+3
length(hyperrecord)


set.seed(1)
result = replicate(10000, {
  i = 0
  state = 0
  repeat {
    flip = sample(c(0,1), 1)
    i = i + 1
    if      (state == 0) state = ifelse(flip==1, 1, 0)
    else if (state == 1) state = ifelse(flip==0, 2, 1)
    else if (state == 2) state = ifelse(flip==0, 3, 1)  
    else if (state == 3) { if(flip==1) break else state = 0 }
  }
  i
})
mean(result)

Q = matrix(c(1/2, 1/2,   0,   0,
             0, 1/2, 1/2,   0,
             0, 1/2,   0, 1/2,
             1/2,   0,   0,   0),
           nrow=4, byrow=TRUE)

I = diag(4)
N = solve(I - Q)        # 基本矩阵
t = N %*% rep(1, 4)     # 每行求和
cat("从S0出发的期望步数：", t[1], "\n")


#proset
#P_posi_out=2*P_posi_home
#P_out_posi=1.8*P_home_posi
#P_posiout/P_out=2*P_posihome/P_home
#1.8/P_out=2/P_home
#P_posiout/P_posi=1.8*P_posihome/P_posi
#P_posiout=1.8*P_posihome
#so P_out=0.9*P_home
#and P_out+P_home=1
#so 1.9*P_home=1
1-1/1.9
#P_out=0.4736842
#P_home=0.5263158

m=matrix(c(0,0.5,0,
         0.5,0,0.5,
         0,0.5,0),
         nrow=3,byrow=TRUE)
m
I=diag(3)
n=solve(I-m)
E=n%*%rep(1,3)
E[1]
