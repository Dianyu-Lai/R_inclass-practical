#1. 
#ave -> enhancer activatity
#epigenetic status -> binary of genestatus active/suppressed
#Transcription_status whether each enhancer shows a transcription mark or not

setwd("D:/Downloads")
assay=read.table("Reporter_assay_4-1-15.txt",header=TRUE)

library(tidyr)
library(ggplot2)
class(assay)
ggplot(assay)+
  geom_boxplot(aes(x=Category,y=ave))

#bootstrap to compare median
a1=assay[assay$Transcription_status=="Active",3]
a2=assay[assay$Transcription_status=="None",3]
shapiro.test(a1)
shapiro.test(a2)

pool1=c(a1,a2)
diff=c()

for (i in 1:1000){
temp=sample(pool1,length(pool1),replace=TRUE)
temp1=temp[1:length(a1)]
temp2=temp[(length(a1)+1):length(pool1)]
diff=c(diff,(median(temp1)-median(temp2)))
}
diff
quantile(diff,probs=c(0.025,0.975))
plot(density(diff))
real1=assay[assay$Transcription_status=="Active",3]
real2=assay[assay$Transcription_status=="None",3]
median(real1)-median(real2)


#bootstrap to compare median
b1=assay[assay$Epigenetic_status=="Active",3]
b2=assay[assay$Epigenetic_status=="Repressed",3]
pool2=c(b1,b2)
diff2=c()

for (i in 1:1000){
  temp=sample(pool2,length(pool2),replace=TRUE)
  temp1=temp[1:length(b1)]
  temp2=temp[(length(b1)+1):length(pool2)]
  diff2=c(diff2,(median(temp1)-median(temp2)))
}
diff2
quantile(diff2,probs=c(0.025,0.975))
plot(density(diff2))
real3=assay[assay$Epigenetic_status=="Active",3]
real4=assay[assay$Epigenetic_status=="Repressed",3]
median(real3)-median(real4)


#2.
movie=read.table("movie_data.txt",header=TRUE)
View(movie)
class(movie)
ggplot(movie)+
  geom_col(aes(x=genre,y=students,fill=genre))+
  theme_minimal()

vector=c(rep(1,73),rep(0,267-73))
vote=c()
#bootstrapping
for (i in 1:1000){
  a=sample(vector,size=length(vector),replace=TRUE)
  vote=c(vote,sum(a))  
}
quantile(vote,probs=c(0.025,0.975))

calCI=function(n){
  vector=c(rep(1,n),rep(0,267-n))
  vote=c()
  #bootstrapping
  for (i in 1:1000){
    a=sample(vector,size=length(vector),replace=TRUE)
    vote=c(vote,sum(a))  
  }
  quantile(vote,probs=c(0.025,0.975))
}
calCI(movie[movie$genre=="war",2])


#proset1
league=read.csv2("quidditch_league.csv",sep=",",header = TRUE)

#H0: no difference
ggplot(league)+
  geom_boxplot(aes(x=gender,y=points))+
  theme_minimal()
shapiro.test(league[league$gender=="F",4])
shapiro.test(league[league$gender=="M",4])
t.test(league[league$gender=="F",4],league[league$gender=="M",4])

#bootstrap
diff=c()
pool=league$points
for (i in 1:1000){
  a=sample(pool,size=length(pool),replace=TRUE)
  female=a[1:4]
  male=a[5:9]
  diff=c(mean(female)-mean(male),diff)
}
quantile(diff,probs=c(0.025,0.975))
plot(density(diff))
abline(v=real_mean,col="red")
real_mean=mean(league[league$gender=="F",4])-mean(league[league$gender=="M",4])

choose_gender = function(probs) {
  genders = c()
  for (prob in probs) {
    # Added size = 1
    gender = sample(c("F", "M"), size = 1, prob = c((1 - prob), prob), replace = TRUE)
    genders = c(genders, gender)
  }
  # Moved return() outside the loop
  return(genders) 
}

diff3 = numeric(1000) # Pre-allocating is much faster than c()
probs_clean = as.numeric(league$prob_male) # Do this once outside the loop

for (i in 1:1000) {
  # Assign the output of the function to the 'genders' variable
  genders = choose_gender(probs_clean)
  num_female = sum(genders == "F", na.rm = TRUE)
  num_male = sum(genders == "M", na.rm = TRUE)
  
  temp = sample(pool, length(pool), replace = TRUE)
  # Protect against instances where one group might be 0
  if (num_female > 0 && num_male > 0) {
    female1 = temp[1:num_female]
    male1 = temp[(num_female + 1):(num_female + num_male)]
    
    diff3[i] = mean(female1) - mean(male1)
  } else {
    diff3[i] = NA # Handle edge cases smoothly
  }
}
quantile(diff3, probs = c(0.025, 0.975), na.rm = TRUE)

library(readxl)
read_excel()