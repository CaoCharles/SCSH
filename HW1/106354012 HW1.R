# 統計與模擬 HW1

# Question 01
n = 1000
z <- c();z1 <- c();z2 <- c()
for(i in 1:5000){
  y = runif(n,-20,20)
  x = 40*exp(-abs(y))
  z <- c(z,mean(x))
  
  y1 = rnorm(n,0,1)
  x1 = 40*exp(-abs(y1))*dunif(y1,-20,20)/dnorm(y1,0,1)
  z1 <- c(z1,mean(x1))
  
  y2 = rnorm(n,0,5)
  x2 = 40*exp(-abs(y2))*dunif(y2,-20,20)/dnorm(y2,0,5)
  z2 <- c(z2,mean(x2))
}
mean(z);var(z)
mean(z1);var(z1)
mean(z2);var(z2)

g <- function(x){
  y = exp(-abs(x))
  return(y)
}
f <- function(x){
  y = 0.025
  return(y)
}
par(pch=22, col="black") # plotting symbol and color
# set up the plot 

x=seq(-20,20,by=0.05)
plot(x,xlim = c(-20,20),ylim=c(0,1),type ="n", xlab="x", ylab="Probability density value") 
lines(x,dnorm(x,0,sd=1), col="blue", lwd=2, lty= 1,ylim=c(0,1))
curve(dnorm(x, sd=5), -20, 20, col="red", lwd=2, lty= 2, add=T)
curve(dunif(x, -20, 20),col="green", lwd=3, lty= 3, add=T)
lines(x,g(x),col="#AABBCC",lwd=3)
legend("topleft", c("N(0,1)", "N(0,5)","U(-20,20)"),
       col=c("blue","red",1), lwd=c(2,2,2), lty=c(1,2,1))
library(ggplot2)
library(dplyr)
x = seq(-20,20,by=0.05)
unif = f(x)
exp = g(x)
norm1 = dnorm(x,0,1)
norm2 = dnorm(x,0,5)
data <- cbind(x,unif,exp,norm1,norm2) %>% as.data.frame()
library(tidyverse)
data2 <- gather(data,key = "type",value = "value",2:5)
colnames(data) <- c("x","unif","exp(-|x|)","norm(0,1)","norm(0,5)")
ggplot(data2,aes(x=x))+ labs(title = "Probability density curve")+
  scale_y_continuous(limits = c(0,1))+
  geom_line(mapping = aes(x=x,y=value,color=type,group=type),size=1.2)+
  theme(legend.title = element_text(colour="royalblue", size=20, face="bold"))+
  theme(plot.title = element_text(size = 40, face = "bold"))+
  theme(legend.title=element_text(size=24))+
  theme(legend.text=element_text(size=20))+
  theme(legend.position = c(0.87,0.6))+
  theme(panel.background = element_rect(colour="black",size = 2))+ 
  scale_colour_discrete(name = "Function")

# plot2
x = seq(-20,20,by=0.05)
g_unif = g(x)/f(x)
g_norm1 = g(x)/dnorm(x,0,1)
g_norm2 = g(x)/dnorm(x,0,5)
data <- cbind(x,g_unif,g_norm1,g_norm2) %>% as.data.frame()
library(tidyverse)
data <- gather(data,key = "type",value = "value",2:4)
ggplot(data,aes(x=x))+ labs(title = "Ratios of g/f")+
  ylim(0,15)+
  geom_line(mapping = aes(x=x,y=value,color=type,group=type),size=1.2)+
  theme(legend.title = element_text(colour="royalblue", size=20, face="bold"))+
  theme(plot.title = element_text(size = 40, face = "bold"))+
  theme(legend.title=element_text(size=24))+
  theme(legend.text=element_text(size=20))+
  theme(legend.position = c(0.87,0.6))+
  theme(panel.background = element_rect(colour="black",size = 2))+ 
  scale_colour_discrete(name = "Function")
  
# Cao
x = seq(-20,20,by=0.05)
y = f(x)
y1 = g(x)
y2 = dnorm(x,0,1)
y3 = dnorm(x,0,5)
data <- cbind(x,y,y1,y2,y3) %>% as.data.frame()
ggplot(data,aes(x=x)) + labs(title = "Probability density curve") + 
  scale_y_continuous(limits = c(0,1))+
  geom_line(aes(y=y),lwd=1,col="green")+
  geom_line(aes(y=y1),lwd=1)+
  geom_line(aes(y=y2),lwd=1,col="red")+
  geom_line(aes(y=y3),lwd=1,col="blue")+
  theme(legend.position="topright")+
  theme(legend.title = element_text(colour="royalblue", size=20, face="bold"))+
  theme(plot.title = element_text(size = 40, face = "bold"))+
  theme(legend.title=element_text(size=24))+
  theme(legend.text=element_text(size=20))+
  theme(legend.position = c(0.87,0.6))+
  theme(panel.background = element_rect(colour="black",size = 2))+ 
  scale_colour_discrete(name = "Function")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Experimental\nCondition",
                    breaks=c("yl", "y2", "y3"),
                    labels=c("Control", "Treatment 1", "Treatment 2"))
  
# Question 02

g<-function(x){(1/((2*pi)^(1/2)))*exp((-1/2)*(x^2))}

n = 1000
z <- c();z0 <- c();z1 <- c();z2 <- c();z3 <- c()
for(i in 1:5000){
  y1 = rt(n,df = 3)
  x1 = y1*g(y1)/dt(y1,df = 3)
  z1 <- c(z1,mean(x1))
  x2 = (y1)^2*g(y1)/dt(y1,df=3)
  z2 <- c(z2,mean(x2))
}
mean(z1);var(z1)
mean(z2);var(z2)
var_z <- mean(z2)-(mean(z1))^2;var_z


f <- function(t){
    y =(gamma(2)/(sqrt(3*pi)*gamma(3/2))*((1+(t^2)/3))^(-2))
    return(y)
}


n = 1000
z <- c();z0 <- c();z1 <- c();z2 <- c();z3 <- c()
for(i in 1:5000){
  y1 = rnorm(n,0,1)
  x1 = y1*dt(y1,df = 3)/dnorm(y1,0,1)
  z1 <- c(z1,mean(x1))
  x2 = (y1)^2*dt(y1,df = 3)/dnorm(y1,0,1)
  z2 <- c(z2,mean(x2))
}
mean(z1);var(z1)
mean(z2);var(z2)
var_z <- mean(z2)-(mean(z1))^2;var_z

# plot
x = seq(-3,3,by=0.05)
norm = dnorm(x,0,1)
t3 = dt(x,df = 3)
data3 <- cbind(x,norm,t3) %>% as.data.frame()
options(digits = 6)
library(tidyverse)
data3 <- gather(data3,key = "Distribution",value = "Probability",2:3)
ggplot(data3,aes(x=x)) + labs(title = "Probability density curve") + 
  scale_y_continuous(limits = c(0,0.5))+
  geom_line(mapping = aes(y=Probability,color=Distribution,group=Distribution),size=1.2)+
  theme(legend.title = element_text(colour="royalblue", size=20, face="bold"))+
  theme(plot.title = element_text(size = 40, face = "bold"))+
  theme(legend.title=element_text(size=24))+
  theme(legend.text=element_text(size=20))+
  theme(legend.position = c(0.87,0.6))+
  theme(panel.background = element_rect(colour="black",size = 2))

# plot
x = seq(-3,3,by=0.05)
norm_t3 = dnorm(x,0,1)/ dt(x,df = 3)
data3 <- cbind(x,norm_t3) %>% as.data.frame()
library(tidyverse)
data3 <- gather(data3,key = "Distribution",value = "Probability",2)
ggplot(data3,aes(x=x)) + labs(title = "Ratios of norm/t3") + 
  scale_y_continuous(limits = c(0,1.5))+
  geom_line(mapping = aes(y=Probability,color=Distribution,group=Distribution),size=1.2)+
  theme(legend.title = element_text(colour="royalblue", size=20, face="bold"))+
  theme(plot.title = element_text(size = 40, face = "bold"))+
  theme(legend.title=element_text(size=24))+
  theme(legend.text=element_text(size=20))+
  theme(legend.position = c(0.87,0.9))+
  theme(panel.background = element_rect(colour="black",size = 2))

# plot
x = seq(-3,3,by=0.05)
t3_norm = dt(x,df = 3)/dnorm(x,0,1) 
data3 <- cbind(x,t3_norm) %>% as.data.frame()
library(tidyverse)
data3 <- gather(data3,key = "Distribution",value = "Probability",2)
ggplot(data3,aes(x=x)) + labs(title = "Ratios of t3/norm") + 
  scale_y_continuous(limits = c(0,5))+
  geom_line(mapping = aes(y=Probability,color=Distribution,group=Distribution),size=1.2)+
  theme(legend.title = element_text(colour="royalblue", size=20, face="bold"))+
  theme(plot.title = element_text(size = 40, face = "bold"))+
  theme(legend.title=element_text(size=24))+
  theme(legend.text=element_text(size=20))+
  theme(legend.position = c(0.87,0.9))+
  theme(panel.background = element_rect(colour="black",size = 2))
