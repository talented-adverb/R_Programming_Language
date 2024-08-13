#Q1(a)
x<-c(rep("Gold",20),rep("Silver",30),rep("Bronze",50))
sample(x,10)
#Q1(b)
y<-c("succ","fail")
sample(y,10,replace=TRUE,prob=c(0.9,0.1))
#Q3
conditional_p<-function(p_c_given_r,p_r,p_c){
  p_r_given_c<-(p_c_given_r*p_r)/p_c
  return (p_r_given_c)
}
p_c<-0.4
p_r<-0.2
p_c_given_r<-0.85
p_r_given_c<-condtional_p(p_c_given_r,p_r,p_c)
p_r_given_c