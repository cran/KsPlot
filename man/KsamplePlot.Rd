\name{KsamplePlot}
\alias{KsamplePlot}

\title{
Check the power of a statistical model.
}
\description{
Check the power of a statistical model moving sample size k.
}
\usage{
KsamplePlot(X, y, Ksample = c(seq(40, 100, 10), 150, seq(200, 1000, 100)), 
	Method = "lm", Caret = "No", size = 5, Type = "numeric", NCV = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Explanatory variables
}
  \item{y}{
Dependent variable
}
  \item{Ksample}{
Numbers of sample size
}
  \item{Method}{
Method of how to predict the dependent variable. When Type="numeric", "lm" is linear regression (default), "svm" is Support Vector Machine, "nn" is Newral Network, "rf" is Random Forest, "mars" is Multipe Adaptive Regression Splines, "cart" is Classification and Regression Tree and "lasso" is LASSO. When Type="binary", "lm" is discriminant analysis (default), "logistic" is logistic regression, "cart" is Classification and Regression Tree, "svm" is Support Vector Machine, "nn" is Newral Network, "rf" is Random Forest.
}
  \item{Caret}{
If Caret == "Yes", you can specify the Method caret form.
}
  \item{size}{
NN's node size
}
  \item{Type}{
Type = "numeric" or Type = "binary" when the dependent variable is continuous or binary.
}
  \item{NCV}{
Performation number of harlf cross-validation (harlf sample is training data and the other is test data).
}
}

\value{
  A list containing entries:
  \item{Par}{
    Parameters estemated in one-phase exponential decay model of Ksplot() function.
}
  \item{Result}{
    Results of 1 - exprained variance (Type="numeric") or 1 - AUC (Type="binary").
}
  \item{Pred}{
    Raw predicted values of each Cross-Vadridation.
}
}

\references{
Kurahashi, I. (2011) Model Exploration using K-sample Plot in Big Data. http://isseing333.blogspot.com/2011/10/model-exploration-using-k-sample-plot.html
}

\examples{
library(KsPlot)

set.seed(1)
x1   <- rnorm(1000000)
set.seed(2)
x2   <- rnorm(1000000)
set.seed(3)
y    <- 2*x1 + x2**2 + rnorm(1000000)

X1      <- data.frame(x1 = x1, x2 = x2)
X2      <- data.frame(x1 = x1, x2 = x2, x3 = x2**2)
y       <- y


# numeric outcome
set.seed(1)
KsResult1 <- KsamplePlot(X1, y)
set.seed(1)
KsResult1 <- KsamplePlot(X1, y, NCV=10)
set.seed(1)
KsResult2 <- KsamplePlot(X1, y, Method="svm")
set.seed(1)
KsResult3 <- KsamplePlot(X2, y)


# binary outcome
y1 <- ifelse(0 < y, 1, 0)

set.seed(1)
KsResult4 <- KsamplePlot(X2, y1, Type="binary")
KsResult4$Pred[1:100, ]

# random forest is overfitting to training sample but alse have good response to test sample.
set.seed(1)
KsResult5 <- KsamplePlot(X2, y1, Type="binary", Method="rf")


}
