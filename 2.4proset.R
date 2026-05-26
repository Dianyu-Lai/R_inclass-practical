#part1
icecream_flavor=matrix(c(40,32,48,57,23),nrow = 1,
                       dimnames = list(Count="counts",
                                       Flavor=c("Mint","Vanilla", "Chocolate", "Lemon", "Orange")
                                       )
)
# H0: there is no preference for the customers towards these different ice cream flavor

#using simulations
num=sum(icecream_flavor)
expected=40
set.seed(342)
sim_chi=numeric(1000)
for (i in 1:1000){
  sim=sample(1:5,num,replace=TRUE)
  sim_count=table(sim)
  sim_chi[i]=sum((sim_count-expected)^2)/expected
}
sim_df=data.frame(num_sim=1:1000,chi_value=sim_chi)

sim_p=mean(chi_res$statistic<sim_chi)
sim_p

observed_chi=sum((icecream_flavor[1,]-expected)^2)/expected
observed_chi
ggplot(sim_df,aes(chi_value))+
  geom_density(lwd=1)+
  theme_classic()+
  geom_vline(xintercept =chi_res$statistic,color="red",lwd=1)

chi_res=chisq.test(icecream_flavor)
chi_res$p.value
chi_res$p.value<0.05
#for chi square test the p value is smaller than 0.05,
#so there is strong evidence to reject the null hypothesis

#1.2
#df=4, 
rchi_val=rchisq(1000,df=4)
plot(density(rchi_val))
#yes, it matches the curve of 1.1

#part2
matrix(c(40,34,9,7),nrow = 2,
       dimnames = list(c("WT","KO")))

mice_array <- array(c(40,9,34,7,20,15,25,20), dim=c(2,2,2),
                      dimnames = list(Status = c("Alive", "Dead"),
                                      Sex    = c("Male", "Female"),
                                      Genotype=c("WT","KO")))
ftable(mice_array, row.vars = "Status", col.vars = c("Genotype", "Sex"))
# method.1 better
mice_table=as.table(mice_array)
summary(mice_table)
# method.2: dedimension but change the df from 4 to 3
chisq.test(as.matrix(ftable(mice_array)))

df_mice=data.frame(
  status=rep(c("Alive","Dead"),4),
  sex=rep(c("Male","Male","Female","Female"),2),
  genotype=c(rep("WT",4),rep("KO",4)),
  count=c(40,9,34,7,20,15,25,20)
)
df_mice

ggplot(df_mice,aes(color=status,shape=sex,y=count,x=genotype))+
  geom_point(size=4)+
  theme_classic()

# extra bonus
extra_matrix=matrix(c(40,9,34,7,20,15,25,20),nrow=2,
                    dimnames = list(status=c("Alive","Dead"),
                                    Genotype_Sex=c("Male+WT","Female+WT","Male+KO","Female+KO")
                    ))
chisq.test(extra_matrix)
