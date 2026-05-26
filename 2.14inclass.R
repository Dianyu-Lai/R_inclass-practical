#lecture outline
#1.R function sample()
#2.bootstrapping for hypothesis testing
#3.bootstrapping to generate confidence intervals
#4.reflection

#1.
# attributes of sample(): dataset size replace prob
#how to generate random numbers: rnorm(size,mean,sd);runif(size,min,max)

#2.自助法
#We use the data itself as the basis for our randomisation. 
#We create a bootstrap sample by sampling (with replacement) from the data, and repeat this procedure many times.

#example
a=rnorm(100,3,1)
b=rnorm(80,4,1.5)
diff=median(b)-median(a)
lena=length(a)
lenb=length(b)
record=c()
pool=c(a,b)
#H0 no difference
for (i in 1:1000) {
temp=sample(pool,lena+lenb,replace=TRUE)
a1=temp[1:lena]
b1=temp[(lena+1):(lena+lenb)]
record=c(record,median(b1)-median(a1))
}
hist(record)
quantile(record,probs=c(0.025,0.975))
mean(diff>record)


#confidence intervals
c=rnorm(100,0,1)
meanc=c()
for (i in 1:200){
  c1=sample(c,100,replace=TRUE)
  meanc=c(meanc,mean(c1))
}

hist(meanc)
quantile(meanc,probs = c(0.025,0.975))
#c mean CI is -0.20,0.23