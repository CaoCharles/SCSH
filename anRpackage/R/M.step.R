M.step <-
function(T){
  list(wei= apply(T,2,mean),
       mu1= apply(faithful,2,weighted.mean,T[,1]),
       mu2= apply(faithful,2,weighted.mean,T[,2]),
       sigma1= cov.wt(faithful,T[,1])$cov,
       sigma2= cov.wt(faithful,T[,2])$cov)
}
