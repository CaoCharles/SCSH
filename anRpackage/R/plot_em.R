plot_em <-
function(theta){
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
