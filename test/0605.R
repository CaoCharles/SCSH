# 小考
set.seed(2018)
x_old = 0
x = c()
x_new = 0
y=0
for(i in 1:5000){
  u1 = runif(1,0,1)
  if(x_old == 0){
    y = ifelse(u1 < 0.5, 0, 1)
  }else{
      y = ifelse(u1 < 0.5, x_old-1, x_old+1)
      }
  u = runif(1,0,1)
  alpha = min((5^(y-x_old))/(factorial(y)/factorial(x_old)),1)
  x_new = ifelse(u < alpha, y, x_old)
  x = cbind(x,x_new)
  x_old = x_new
}
k=1*((x[-1]-x[-5000])!=0)
acc_rate = sum(k)/5000
acc_rate

x <- as.matrix(x)
plot.ts(t(x))
mean(t(x));var(t(x))

Y <- x[3001:5000]
mean(Y);var(Y)
