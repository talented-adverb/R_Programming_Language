#Q1 (a)
chest<-c(rep('gold',20),rep('silver',30),rep('bronze',50))
sample(x=chest,size=10)
#Q1 (b)
sample(c('succ','fail'),10,replace=T,prob=c(0.9,0.1))

#Q2
bday<-function(m){
  p=1-(choose(365,m)*factorial(m))/((365)^m)
}

for(i in 1:366){
  if(bday(i)>0.5){
    print(i)
    break
  }
}
#Q3
bayes<-function(pA,pB,pAB){
 pBA=(pB*pAB)/pA
 print(pBA)
}
bayes(0.4,0.2,0.85)
#Q4
dat<-iris
head(dat)
str(dat$Sepal.Length)
mean(dat$Sepal.Length)
median(dat$Sepal.Length)
quantile(dat$Sepal.Length,0.25)
quantile(dat$Sepal.Length,0.75)
IQR(dat$Sepal.Length)
lapply(dat[,1:4],sd)


