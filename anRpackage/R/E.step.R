E.step <-
function(theta){
  x <- cbind(theta$wei[1] * dmvnorm(faithful,mean=theta$mu1,sigma=theta$sigma1),
             theta$wei[2] * dmvnorm(faithful,mean=theta$mu2,sigma=theta$sigma2))
  t(apply(x,1,function(x){x/sum(x)}))
}
