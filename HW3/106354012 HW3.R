# HW3
# HM algorithm for N(0,1)

set.seed(2018)
x = x_old = 0; x_new = -10; s = 0.01; k = 1000;
for (i in 1:k){
  y = x_old + s*rnorm(1,0,1);         #  x*
  u = runif(1,0,1);                   # 亂數生成機率     
  alpha = exp(-(y^2-(x_old)^2)/2);    # 條件機率
  x_new = ifelse(u < alpha, y, x_old)
  x = cbind(x,x_new);
  x_old = x_new;
}

hist(x)

accept <- sum(x[-1]-x[-k]!=0)
acc_rate = 1-accept/k
acc_rate

# (x0 = -10, sigma = 0.1) -----------------------------------------------------------------------
# 我的想法(x0 = -10, sigma = 0.1)
x = x_old = -10; x_new = 0;k = 1000;

for (i in 1:k){
  y = rnorm(1,x_old,0.1);             # 生成一個x*
  u = runif(1,0,1);                   # 亂數生成機率     
  alpha = exp(-(y^2-(x_old)^2)/2);    # 條件機率
  x_new = ifelse(u < alpha, y, x_old)
  x = cbind(x,x_new);
  x_old = x_new;
}
hist(x)
x <- as.matrix(x)
plot.ts(t(x))
acf(ts(t(x)))
accept <- sum(x[-1]-x[-k]!=0)
acc_rate1 = accept/k
acc_rate1

# (x0 = 0, sigma = 0.1) -----------------------------------------------------------------------
# 我的想法(x0 = 0, sigma = 0.1)
x = x_old = 0; x_new = 0;k = 1000;

for (i in 1:k){
  y = rnorm(1,x_old,0.1);             # 生成一個x*
  u = runif(1,0,1);                   # 亂數生成機率     
  alpha = exp(-(y^2-(x_old)^2)/2);    # 條件機率
  x_new = ifelse(u < alpha, y, x_old)
  x = cbind(x,x_new);
  x_old = x_new;
}
hist(x)
x <- as.matrix(x)
plot.ts(t(x))
acf(ts(t(x)))
accept <- sum(x[-1]-x[-k]!=0)
acc_rate2 = accept/k
acc_rate2

# (x0 = -10, sigma = 0.5) -----------------------------------------------------------------------
# 我的想法
x = x_old = -10; x_new = 0;k = 1000;

for (i in 1:k){
  y = rnorm(1,x_old,0.5);             # 生成一個x*
  u = runif(1,0,1);                   # 亂數生成機率     
  alpha = exp(-(y^2-(x_old)^2)/2);    # 條件機率
  x_new = ifelse(u < alpha, y, x_old)
  x = cbind(x,x_new);
  x_old = x_new;
}
hist(x)
x <- as.matrix(x)
plot.ts(t(x))
acf(ts(t(x)))
accept <- sum(x[-1]-x[-k]!=0)
acc_rate3 = accept/k
acc_rate3

# (x0 = 0, sigma = 0.5) -----------------------------------------------------------------------
# 我的想法
x = x_old = 0; x_new = 0;k = 1000;

for (i in 1:k){
  y = rnorm(1,x_old,0.5);             # 生成一個x*
  u = runif(1,0,1);                   # 亂數生成機率     
  alpha = exp(-(y^2-(x_old)^2)/2);    # 條件機率
  x_new = ifelse(u < alpha, y, x_old)
  x = cbind(x,x_new);
  x_old = x_new;
}
hist(x)
x <- as.matrix(x)
plot.ts(t(x))
acf(ts(t(x)))
accept <- sum(x[-1]-x[-k]!=0)
acc_rate4 = accept/k
acc_rate4

# (x0 = -10, sigma = 10) --------------------------------------------------
# 我的想法
x = x_old = -10; x_new = 0;k = 1000;

for (i in 1:k){
  y = rnorm(1,x_old,10);             # 生成一個x*
  u = runif(1,0,1);                   # 亂數生成機率     
  alpha = exp(-(y^2-(x_old)^2)/2);    # 條件機率
  x_new = ifelse(u < alpha, y, x_old)
  x = cbind(x,x_new);
  x_old = x_new;
}
hist(x)
x <- as.matrix(x)
plot.ts(t(x))
acf(ts(t(x)))
accept <- sum(x[-1]-x[-k]!=0)
acc_rate5 = accept/k
acc_rate5

# (x0 = 0, sigma = 10) -----------------------------------------------------------------------
# 我的想法
x = x_old = 0; x_new = 0;k = 1000;

for (i in 1:k){
  y = rnorm(1,x_old,10);             # 生成一個x*
  u = runif(1,0,1);                   # 亂數生成機率     
  alpha = exp(-(y^2-(x_old)^2)/2);    # 條件機率
  x_new = ifelse(u < alpha, y, x_old)
  x = cbind(x,x_new);
  x_old = x_new;
}
hist(x)
x <- as.matrix(x)
plot.ts(t(x))
acf(ts(t(x)))
accept <- sum(x[-1]-x[-k]!=0)
acc_rate6 = accept/k
acc_rate6
c(acc_rate1,acc_rate2,acc_rate3,acc_rate4,acc_rate5,acc_rate6)

# (x0 = 0, sigma = 3) -----------------------------------------------------
x = x_old = 0; x_new = 0;k = 1000;

for (i in 1:k){
  y = rnorm(1,x_old,2);               # 生成一個x*
  u = runif(1,0,1);                   # 亂數生成機率     
  alpha = exp(-(y^2-(x_old)^2)/2);    # 條件機率
  x_new = ifelse(u < alpha, y, x_old)
  x = cbind(x,x_new);
  x_old = x_new;
}
hist(x)
x <- as.matrix(x)
plot.ts(t(x))
acf(ts(t(x)))
accept <- sum(x[-1]-x[-k]!=0)
acc_rate7 = accept/k
acc_rate7
x1 <- seq(min(x),max(x),length.out = 1000)
y1 <- dnorm(x1,0,1)
library(dplyr)
data <- cbind(x1,x[-1],y1) %>% as.data.frame()
library(ggplot2)
ggplot(data)+
  labs(title = "Normal distribution of N(0,1)", x = "x", y = "density")+
  geom_histogram(aes(x=V2,y=..density..),bins = 10)+
  geom_line(aes(x=x1,y=y1),col="red",lwd=1)+
  theme(title = element_text(face="bold",size=20))+
  theme(panel.background = element_rect(colour="black",size = 2))
  
# 抽取gamma樣本 -----------------------------------------------------------------------
x_old=1
x = c()

for (i in 1:k){
  y = rexp(1,5);                            # 生成一個x*
  u = runif(1,0,1);                          # 亂數生成機率     
  alpha = (y*exp(-y))/(x_old*exp(-x_old));    # 條件機率
  x_new = ifelse(u < alpha, y, x_old)
  x = cbind(x,x_new);
  x_old = x_new;
}
hist(x)
