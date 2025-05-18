
cucuq <- function(x,nivel){ 
  
  #C0 <- 0.09506
  #x <- t(std.resids)[,n]
  
  A <- 0.948 # CONSTANTE PARA RECTAS DEL 95% EN LA PRUEBA CUSUM 
  # (independiente del tama?o de muestra ?)
  N <- length(x)
  K <- 0
  T <- 1:N
  
  
  cu <- cumsum(x)/sd(x)
  LS <- A*sqrt(N-K-1)+2*A*(T-K-1)/sqrt(N-K-1)
  LI <- -LS
  
  cu2 <- cumsum(x*x)/sum(x*x)
  LS2 <- nivel+(T-K-1)/(N-K-1)
  LI2 <- -nivel+(T-K-1)/(N-K-1)
  
  y <- cbind(cu,LS,LI)
  y2 <- cbind(cu2,LS2,LI2)
  
  par(mfrow=c(1,2))
  par(las=1)
  matplot(x=(1:N),y, xlab="tiempo", ylab= "CUSUM", type ="l" , col=c(1,2,2),lwd = 1,lend=2, lty=c(1,2,2))
  
  par(las=1)
  matplot(x=(1:N),y2, xlab="tiempo", ylab= "CUSUMQ", type ="l" , col=c(1,2,2),lwd = 1,lend=2, lty=c(1,2,2))
  
  #durbin.watson(x)
  #shapiro.test(x)
  #levene.test(x)
}
