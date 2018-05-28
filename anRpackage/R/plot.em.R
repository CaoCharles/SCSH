plot.em <-
function(theta){
  mixture.contour <- outer(xpts,ypts,function(x,y) {
    theta$wei[1]*dmvnorm(cbind(x,y),mean=theta$mu1,sigma=theta$sigma1) + theta$wei[2]*dmvnorm(cbind(x,y),mean=theta$mu2,sigma=theta$sigma2)
  })
  contour(xpts,ypts,mixture.contour,nlevels=8,drawlabel=FALSE,col="red",xlab="Eruption time (mins)",ylab="Waiting time (mins)",main="Waiting time vs Eruption time of the Old Faithful geyser")
  points(faithful)
}
