#Q1
why<-c(5,10,15,20,25,30)
print(max(why))
#Q2
findfactorial <- function(n){
  factorial <- 1
  
  if(n<0){
    print("Enter a positive integer")
  }
  else if ((n==0)||(n==1))
    factorial <- 1
  else{
    for(i in 1:n)
      factorial <- factorial * i
  }
  return (factorial)
}
n=as.integer(readline(prompt="Enter a number"))
print(findfactorial(n))
#Q3
num=as.integer(readline(prompt="Enter a number"))
n1=0
n2=1
count=2
if(num<=0){
  print("please enter a positive number")
}else{
  if(num==1){
  print(n1)
  }
  else{
    print("Fibonacci Series")
    print(n1)
    print(n2)
    while(count<num){
      n=n1+n2
      n1=n2
      n2=n
      print(n)
      count=count+1
    }
  }
}
#Q4
add<-function(x,y){
  return (x+y)
}
subtract<-function(x,y){
  return (x-y)
}
multiply<-function(x,y){
  return (x*y)
}
divide<-function(x,y){
  return (x/y)
}
print('Select an operation')
print('1 add')
print('2 subtract')
print('3 multiply')
print('4 divide')
choice=as.integer(readline(prompt="Choose an operation[1/2/3/4:]"))
num1<-as.integer(readline(prompt="Enter number 1"))
num2<-as.integer(readline(prompt="Enter number 2"))
operator<-switch(choice,'+','-','/','*')
result<-switch(choice,add(num1,num2),subtract(num1,num2),multiply(num1,num2),divide(num1,num2))
print(paste(num1,operator,num2,'=',result))
#Q5
cat<-c('a','b')
values<-c(10,20)
barplot(
  values,
  name.org=cat,
  col='red',
  main='Bar Plot',
  xlab='Categories',
  ylab='Values'
)
x<-1:10
y<-x^2

plot(
  x,
  y,
  type="b",
  col="blue",
  pch=19,
  xlab='X Axis',
  ylab='Y Axis'
)

slices<-c(10,20)
lables<-c('a','b')

pie(
  slices,
  labels,
  col=rainbow(length(slices)),
  main='pie'
)



