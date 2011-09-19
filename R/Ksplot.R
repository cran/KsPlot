Ksplot <- function(UseDataTest, UseDataTrain, Type = "numeric", Method) {
  
  EstParFun <- function(UseData, OptimPar1) {
    # estimate plateau line
    # 
    # Arg:
    #   UseData: test or train
    #   OptimPar: 1 or 0
    
    Par <- data.frame(k = rep(seq(0.01, 1, 0.01), 100), 
                      plateau = rep(seq(0.01, 1, 0.01), rep(100, 100)))
    Object <- rep(NA, 10000)
    for (i in 1:10000) {
      k         <- Par[i, 1]
      plateau   <- Par[i, 2]
      x         <- UseData[, 1]
      y         <- UseData[, 2]
      if (Type == "numeric") {
        yhat <- (OptimPar1     - plateau) * exp(-k * x) + plateau
      }
      if (Type == "binary") {
        yhat <- (OptimPar1 / 2 - plateau) * exp(-k * x) + plateau
      }
      Object[i] <- sum((y - yhat) ^ 2)
    }
    Par[Object == min(Object), ][1, ]
  }
  EstParTest  <- EstParFun(UseDataTest,  1)
  EstParTrain <- EstParFun(UseDataTrain, 0)
  
  # plot
  if (Type == "numeric") {
    plot(UseDataTest, xlim=c(0, max(UseDataTest[, 1])), lwd=2, ylim=c(0, 1), 
         xlab="K-Sample", ylab="1 - Explained Var", 
         main = paste("Method = ", Method, sep = ""))
    points(UseDataTrain, pch=0, lwd=2)
  }
  if (Type == "binary") {
    plot(UseDataTest, xlim=c(0, max(UseDataTest[, 1])), lwd=2, ylim=c(0, 0.5), 
         xlab="K-Sample", ylab="1 - AUC", 
         main = paste("Method = ", Method, sep = ""))
    points(UseDataTrain, pch=0, lwd=2)
  }
  
  # curve
  k       <- as.numeric(EstParTest[1])
  plateau <- as.numeric(EstParTest[2])
  
  k2       <- as.numeric(EstParTrain[1])
  plateau2 <- as.numeric(EstParTrain[2])
  
  XLim    <- max(UseDataTest[, 1])
  if (Type == "numeric") {
    Ks.Est  <- (1 - plateau ) * exp(-k  * c(1:XLim)) + plateau 
    Ks.Est2 <- (0 - plateau2) * exp(-k2 * c(1:XLim)) + plateau2
  }
  if (Type == "binary") {
    Ks.Est  <- (0.5 - plateau ) * exp(-k  * c(1:XLim)) + plateau 
    Ks.Est2 <- (0   - plateau2) * exp(-k2 * c(1:XLim)) + plateau2
  }
  lines(c(1:XLim), Ks.Est , lwd=2)
  lines(c(1:XLim), Ks.Est2, lwd=2, lty=2)
  
  # legend
  if (Type == "numeric") {
    text(XLim * 0.7, 0.8, paste("Test  plateau  :", 
                                round(EstParTest[2], digits=2)), adj=0)
    text(XLim * 0.7, 0.76,  paste("Test  k (slope):", 
                                round(EstParTest[1], digits=2)), adj=0)
    text(XLim * 0.7, 0.7, paste("Train plateau  :", 
                                round(EstParTrain[2], digits=2)), adj=0)
    text(XLim * 0.7, 0.66,  paste("Train k (slope):", 
                                round(EstParTrain[1], digits=2)), adj=0)
    legend("topright", c("Test", "Train"), pch=c(1, 0), lty=1:2, lwd=2)
  }
  if (Type == "binary") {
    text(XLim * 0.7, 0.44 , paste("Test  plateau  :", 
                                 round(EstParTest[2], digits=2)), adj=0)
    text(XLim * 0.7, 0.42, paste("Test  k (slope):", 
                                 round(EstParTest[1], digits=2)), adj=0)
    text(XLim * 0.7, 0.39, paste("Train plateau  :", 
                                round(EstParTrain[2], digits=2)), adj=0)
    text(XLim * 0.7, 0.37,  paste("Train k (slope):", 
                                round(EstParTrain[1], digits=2)), adj=0)
    legend("topright", c("Test", "Train"), pch=c(1, 0), lty=1:2, lwd=2)
  }
  
  # output
  list(EstParTest, EstParTrain)
}



