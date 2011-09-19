KsamplePlot <- function(X, y, Ksample = c(seq(40,100,10), 150, 
                        seq(200,1000,100)), Method="lm", Caret="No", 
                        size=5, Type="numeric", NCV=5) {
  # calculate Error and draw k-sample plot
  # 
  # Args: 
  #   NCV: perform number of harf cross-validation
  
  
  library(caTools)
  
  RsqTest  <- rep(NA, length(Ksample) * NCV)
  RsqTrain <- rep(NA, length(Ksample) * NCV)
  
  
  for (j in 1:NCV) {
    for (i in 1:length(Ksample)) {
      ksample  <- Ksample[i]
      Sample   <- sample(1:nrow(X), ksample)
      KSamplex <- X[Sample, ]
      KSampley <- y[Sample]
      
      
      # Half CV
      Train     <- sample(1:ksample, ksample / 2)
      Trainx    <- KSamplex[Train, ]
      Trainy    <- KSampley[Train]
      TrainData <- data.frame(Trainx, y=Trainy)
      
      Varidx    <- KSamplex[-Train, ]
      Varidy    <- KSampley[-Train]
      VaridData <- data.frame(Varidx, y=Varidy)
      
      
      # numeric
      if (Type == "numeric") {
      
        # develop model
        if (Caret == "No") {
          if(Method == "lm") {
            Model  <- lm(y~., data = TrainData)
            Varidx <- data.frame(Varidx)
          }
          if (Method == "svm") {
            Model <- e1071::svm(y~., data = TrainData)
          }
          if (Method == "nn") {
            Model <- nnet::nnet(y~., data = TrainData, size = size, linout = T)
          }
          if (Method == "rf") {
            Model <- randomForest::randomForest(y~., data = TrainData)
          }
          if (Method == "mars") {
            Model <- mda::mars(Trainx, Trainy)
          }
          if (Method == "cart") {
            Model <- mvpart::rpart(y~., data = TrainData)
          }
          if (Method == "lasso") {
            cvob1    <- glmnet::cv.glmnet(do.call(cbind, Trainx), Trainy)
            CvLambda <- cvob1$lambda[cvob1$cvm == min(cvob1$cvm)]
            Model    <- glmnet::glmnet(do.call(cbind, Trainx), 
                                       Trainy, lambda = CvLambda)
            Varidx   <- do.call(cbind, Varidx)
          }
        }
        if (Caret == "Yes") {
          modeltxt <- parse(text=paste("caret::train(y~., data=TrainData, 
                                                     method= Method)", sep=""))
          Model    <- eval(modeltxt)
        }
        Err    <- as.numeric(Varidy - predict(Model, Varidx))
        MSE    <- Err%*%Err
        Var    <- (Varidy-mean(Varidy))%*%(Varidy-mean(Varidy))
        RsqTest[i + length(Ksample) * (j - 1)] <- (Var - MSE) / Var
        
        Err1   <- as.numeric(Trainy - predict(Model, Trainx))
        MSE1   <- Err1%*%Err1
        Var1   <- (Trainy-mean(Trainy))%*%(Trainy-mean(Trainy))
        RsqTrain[i + length(Ksample) * (j - 1)] <- (Var1 - MSE1) / Var1
      }
      
      
      
      # class
      if (Type == "binary") {
        # develop model
        if (Caret == "No") {
          if (Method == "lm") {
            Model <- MASS::lda(y~., data = TrainData)
            
            Post  <- predict(Model, VaridData)$posterior[, 2]
            ROC   <- caTools::colAUC(Post, Varidy)
            RsqTest[i + length(Ksample) * (j - 1)] <- as.numeric(ROC)
            
            Post1 <- predict(Model, TrainData)$posterior[, 2]
            ROC1  <- caTools::colAUC(Post1, Trainy)
            RsqTrain[i + length(Ksample) * (j - 1)] <- as.numeric(ROC1)
          }
          if (Method == "svm") {
            Model <- e1071::svm(y ~ ., data = TrainData, kernel="polynomial", 
                                degree=3, probability=T)
            Post  <- predict(Model, VaridData, probability=T)
            ROC   <- caTools::colAUC(Post, Varidy)
            RsqTest[i + length(Ksample) * (j - 1)] <- as.numeric(ROC)
            
            Post1 <- predict(Model, TrainData, probability=T)
            ROC1  <- caTools::colAUC(Post1, Trainy)
            RsqTrain[i + length(Ksample) * (j - 1)] <- as.numeric(ROC1)
          }
        }
        if (Caret == "Yes") {
        }
      }
    print(paste("Ksample", j, " ", ksample, sep = ""))
    }
  }
  KsTest  <- cbind(Ksample, 1 - RsqTest)
  KsTest  <- KsTest[(0 <= KsTest[, 2] & KsTest[, 2] <= 1), ]
  
  KsTrain <- cbind(Ksample, 1 - RsqTrain)
  KsTrain <- KsTrain[(0 <= KsTrain[, 2] & KsTrain[, 2] <= 1), ]
  
  EstPar <- Ksplot(KsTest, KsTrain, Method=Method, Type=Type)
  list(OneExpPar = EstPar, Ksample = cbind(Ksample, 1 - RsqTest, 1 - RsqTrain))
}

