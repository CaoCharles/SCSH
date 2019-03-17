# HW02
?faithful
library(ggplot2)
str(faithful)
plot(faithful[,-3], 
     xlab = "Eruption time (min)", 
     ylab = "Time to next eruption (min)",
     main = "faithful data : Eruptions of Old Faithful")

#load library for multivariate normal
library(mvtnorm)

#load Old Faithful data frame
data(faithful)
mean(faithful[,1]);mean(faithful[,2])
var(faithful[,1]);var(faithful[,2])
cov(faithful[,1],faithful[,2])

# initial parameter estimates (chosen to be deliberately bad)
# 初始參數估計
theta <- list(wei=c(0.5,0.5),                      # 權重
              mu1=c(2,60),                         # 二元常態分配(一)
              sigma1=matrix(c(1,7,7,100),ncol=2),
              mu2=c(4,80),                         # 二元常態分配(二)
              sigma2=matrix(c(20,0,0,100),ncol=2))

# E-step: calculates conditional probabilities for latent variables (update y)
# 將樣本帶入計算隱藏變數的條件機率
E.step <- function(theta){
  x <- cbind(theta$wei[1] * dmvnorm(faithful,mean=theta$mu1,sigma=theta$sigma1),
             theta$wei[2] * dmvnorm(faithful,mean=theta$mu2,sigma=theta$sigma2))
  t(apply(x,1,function(x){x/sum(x)}))
}

# M-step: calculates the parameter estimates which maximise Q
# 將E.step所計算出的機率帶入估計新的參數(MLE)
M.step <- function(T){
  list(wei= apply(T,2,mean),
       mu1= apply(faithful,2,weighted.mean,T[,1]),
       mu2= apply(faithful,2,weighted.mean,T[,2]),
       sigma1= cov.wt(faithful,T[,1])$cov,
       sigma2= cov.wt(faithful,T[,2])$cov)
}

# function to plot current data
# 畫圖
# setup grid for plotting
xpts <- seq(from=1,to=6,length.out=272)
ypts <- seq(from=40,to=100,length.out=272)
plot.em <- function(theta){
  mixture.contour <- outer(xpts,ypts,function(x,y) {
    theta$wei[1]*dmvnorm(cbind(x,y),mean=theta$mu1,sigma=theta$sigma1) + theta$wei[2]*dmvnorm(cbind(x,y),mean=theta$mu2,sigma=theta$sigma2)
  })
  contour(xpts,ypts,mixture.contour,nlevels=8,drawlabel=FALSE,col="red",xlab="Eruption time (mins)",ylab="Waiting time (mins)",main="Waiting time vs Eruption time of the Old Faithful geyser")
  points(faithful)
}
data1 <- c(faithful)
for (i in 2:272) {
  data1 <- rbind(data1,faithful)
}
plot_em <- function(theta){
r1 <- rmvnorm(73984,mean=theta$mu1,sigma=theta$sigma1) 
colnames(r1) <- c("D1x","D1y")
r2 <- rmvnorm(73984,mean=theta$mu2,sigma=theta$sigma2)
colnames(r2) <- c("D2x","D2y")
r <-rbind(r1,r2) 
data1 <- rbind(data1,data1)
data <- cbind(data1,r)
A <- ggplot(data)+
      labs(title = " Waiting time vs Eruption time of old faithful geyser", x = "Eruptions time (min)", y = "Time to next eruption (min)")+
      xlim(0,6)+ ylim(40,100)+
      geom_point(aes(x = eruptions, y = waiting))+
      stat_density2d(aes(x = D1x,y = D1y),col = "red")+
      theme(title = element_text(face="bold"))+
      scale_fill_gradientn(colours=rainbow(9))+
      theme(panel.background = element_rect(colour="black",size = 2))
return(A)
}  
  
# plot initial contours
# 繪製初始輪廓
#iter <- 1
#plot_em(theta)
#ggsave(filename=paste("EM",formatC(iter,width=4,flag="0"),".png",sep=""))
#dev.off()

# run EM and plot
for (iter in 2:30){
  T <- E.step(theta)
  theta <- M.step(T)
  #plot_em(theta)
  #ggsave(filename=paste("EM",formatC(iter,width=4,flag="0"),".png",sep=""))
}
theta

# ggplot2
library(ggplot2)
ggplot(faithful)+
  labs(title = "faithful data : Eruptions of old faithful", x = "Eruption time (min)", y = "Time to next eruption (min)")+
  theme(title = element_text(size=20, face="bold"))+
  theme(panel.background = element_rect(colour="black",size = 2))+
  geom_point(aes(x=eruptions,y=waiting),color = "red")
  
  
# bivariate normal distribution plot
# 繪製二元常態分配圖
library(MASS)
# Dis.1
bivnorm <- mvrnorm(n=1000, mu = theta$mu1 ,Sigma = theta$sigma1)
bivnorm.kde <- kde2d(bivnorm[,1], bivnorm[,2], n=50)
persp(bivnorm.kde, phi=45, theta=40, shade=0.1, main="bivariate normal distribution plot")
image(bivnorm.kde, main="contour plot for Dis.1")
contour(bivnorm.kde, add=TRUE)
# Dis.2
bivnorm2 <- mvrnorm(n=1000, mu = theta$mu2 ,Sigma = theta$sigma2)
bivnorm.kde2 <- kde2d(bivnorm2[,1], bivnorm2[,2], n=50)
image(bivnorm.kde2, main="contour plot for Dis.2")
contour(bivnorm.kde2, add=TRUE)
# end