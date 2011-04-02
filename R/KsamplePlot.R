KsamplePlot <-
function(X, y, Ksample = c(seq(40,100,10), 150, seq(200,1000,100)), 
			Method = "lm", Caret = "No", size = 5, Type = "numeric"){
  library(caTools)
  
  Rsq <- rep(NA, length(Ksample)*3)
  for(j in 1:3){
    for(i in 1:length(Ksample)){
      ksample  <- Ksample[i]
      Sample   <- sample(1:nrow(X), ksample)
      KSamplex <- X[Sample, ]
      KSampley <- y[Sample]
      
      
      #---Half CV
      Train     <- sample(1:ksample, ksample / 2)
      Trainx    <- KSamplex[Train, ]
      Trainy    <- KSampley[Train]
      TrainData <- data.frame(Trainx, y = Trainy)
      
      Varidx    <- KSamplex[-Train, ]
      Varidy    <- KSampley[-Train]
      VaridData <- data.frame(Varidx, y = Varidy)
      
      
      #------numeric
      if(Type == "numeric"){
        #---develop model
        if(Caret == "No"){
          if(Method == "lm")     Model <- lm(y~., data = TrainData); Varidx <- data.frame(Varidx)
          if(Method == "svm")    Model <- e1071::svm(y~., data = TrainData)
          if(Method == "nn")     Model <- nnet::nnet(y~., data = TrainData, size = size, linout = T)
          if(Method == "rf")     Model <- randomForest::randomForest(y~., data = TrainData)
          if(Method == "mars")   Model <- mda::mars(Trainx, Trainy)
          if(Method == "cart")   Model <- mvpart::rpart(y~., data = TrainData)
          if(Method == "lasso"){
            cvob1    <- glmnet::cv.glmnet(do.call(cbind, Trainx), Trainy)
            CvLambda <- cvob1$lambda[cvob1$cvm == min(cvob1$cvm)]
            Model    <- glmnet::glmnet(do.call(cbind, Trainx), Trainy, lambda = CvLambda)
            Varidx   <- do.call(cbind, Varidx)
          }
        }
        if(Caret == "Yes"){
          modeltxt <- parse(text=paste("caret::train(y~., data=TrainData, method= Method)", sep=""))
          Model    <- eval(modeltxt)
        }
        Err    <- as.numeric(Varidy - predict(Model, Varidx))
        MSE    <- Err%*%Err
        Var    <- (Varidy-mean(Varidy))%*%(Varidy-mean(Varidy))
        Rsq[i + length(Ksample)*(j-1)] <- (Var - MSE)/Var
      }
      #------class
      if(Type == "binary"){
        #---develop model
        if(Caret == "No"){
          if(Method == "lm"){
            Model <- MASS::lda(y~., data = TrainData)
            Post  <- predict(Model, VaridData)$posterior[, 2]
            ROC   <- caTools::colAUC(Post, Varidy)
            Rsq[i + length(Ksample)*(j-1)] <- as.numeric(ROC)
          }
          if(Method == "svm"){
            Model <- e1071::svm(y ~ ., data = TrainData, kernel="polynomial", degree=3, probability = T)
            Post  <- predict(Model, VaridData, probability = T)
            ROC   <- caTools::colAUC(Post, Varidy)
            Rsq[i + length(Ksample)*(j-1)] <- as.numeric(ROC)
          }
        }
        if(Caret == "Yes"){
        }
      }
    print(paste("Ksample", j, " ", ksample, sep = ""))
    }
  }
  Ks     <- cbind(Ksample, 1 - Rsq)
  Ks     <- Ks[(0 <= Ks[, 2] & Ks[, 2] <= 1), ]
  EstPar <- Ksplot(Ks, Method = Method, Type = Type)
  list(OneExpPar = EstPar, Ksample = cbind(Ksample, 1 - Rsq))
}

