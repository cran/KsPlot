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
	Method = "lm", Caret = "No", size = 5, Type = "numeric")
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
Method of how to predict the dependent variable. "lm" is linear regression (default), 
"svm" is Support Vector Machine, "nn" is Newral Network, "rf" is Random Forest, 
"mars" is Multipe Adaptive Regression Splines, "cart" is Classification and Regression Tree, 
"lasso" is LASSO.
}
  \item{Caret}{
If Caret == "Yes", you can specify the Method caret form.
}
  \item{size}{
NN's node size
}
  \item{Type}{
Type = "numeric" and Type = "binary" when the dependent variable is continuous and binary.
}
}

\examples{
set.seed(1)
x1   <- rnorm(1000000)
set.seed(2)
x2   <- rnorm(1000000)
set.seed(3)
y    <- 2*x1 + x2**2 + rnorm(1000000)


X1      <- data.frame(x1 = x1, x2 = x2)
X2      <- data.frame(x1 = x1, x2 = x2, x3 = x2**2)
y       <- y

KsResult1 <- KsamplePlot(X1, y)
KsResult2 <- KsamplePlot(X2, y)

y1 <- ifelse(0 < y, 1, 0)
KsResult3 <- KsamplePlot(X2, y1, Type = "binary")

}
