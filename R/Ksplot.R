Ksplot <-
function(UseData, Type = "numeric", Method){
  Par    <- data.frame(k = rep(seq(0.01, 1, 0.01), 100), plateau = rep(seq(0.01, 1, 0.01), rep(100, 100)))
  Object <- rep(NA, 10000)
  for(i in 1:10000){
    k         <- Par[i, 1]
    plateau   <- Par[i, 2]
    x         <- UseData[, 1]
    y         <- UseData[, 2]
    if(Type == "numeric") yhat <- (1   - plateau)*exp(-k*x) + plateau
    if(Type == "binary")  yhat <- (0.5 - plateau)*exp(-k*x) + plateau
    Object[i] <- sum((y - yhat)**2)
  }
  EstPar <- Par[Object == min(Object), ][1, ]
  
  if(Type == "numeric") plot(UseData, xlim = c(0, max(UseData[, 1])), lwd = 2, ylim=c(0, 1), xlab="K-Sample", ylab="1 - Explained Var", main = paste("Method = ", Method, sep = ""))
  if(Type == "binary")  plot(UseData, xlim = c(0, max(UseData[, 1])), lwd = 2, ylim=c(0, 0.5), xlab="K-Sample", ylab="1 - AUC", main = paste("Method = ", Method, sep = ""))
  k       <- as.numeric(EstPar[1])
  plateau <- as.numeric(EstPar[2])
  XLim    <- max(UseData[, 1])
  if(Type == "numeric") Ks.Est <- (1   - plateau)*exp(-k*c(1:XLim)) + plateau
  if(Type == "binary")  Ks.Est <- (0.5 - plateau)*exp(-k*c(1:XLim)) + plateau
  lines(c(1:XLim), Ks.Est, lwd = 2)
  if(Type == "numeric"){ 
    text(XLim*0.7, 0.8, paste("k (slope):",       round(EstPar[1], digits=2)), adj=0)
    text(XLim*0.7, 0.7, paste("plateau  :", round(EstPar[2], digits=2)), adj=0)
  }
  if(Type == "binary"){ 
    text(XLim*0.7, 0.45, paste("k (slope):",       round(EstPar[1], digits=2)), adj=0)
    text(XLim*0.7, 0.4 , paste("plateau  :", round(EstPar[2], digits=2)), adj=0)
  }
  EstPar
}

