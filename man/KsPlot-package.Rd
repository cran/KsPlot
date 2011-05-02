\name{KsPlot-package}
\alias{KsPlot-package}
\alias{KsPlot}
\docType{package}
\title{
Draw K-sample plot (K's plot).
}
\description{
Check the power of a statistical model moving sample size k.
}
\details{
\tabular{ll}{
Package: \tab KsPlot\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-04-02\cr
License: \tab GNU General Public License Version 2\cr
LazyLoad: \tab yes\cr
}

}
\author{
Issei Kurahashi
Maintainer: Issei Kurahashi <i.kurahashi@gmail.com>
}

\keyword{ statistical model, machine learning }

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
