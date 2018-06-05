# 小考
set.seed(2018)
x_old = 5
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

# (2)
x <- as.matrix(x)
plot.ts(t(x))
mean(t(x));var(t(x))

# (3)
Y <- x[2001:5000]
mean(Y);var(Y)

# (4)
library(dplyr)
options(digits = 4)
p <- NULL
for (i in 0:10) {
  px <- 1*(x[2001:5000]==i) %>% sum()
  px <- px/3000
  p <- c(p, px)
}
p
