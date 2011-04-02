\name{KsamplePlot}
\alias{KsamplePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check the power of a statistical model.
}
\description{
Check the power of a statistical model moving sample size k.
}
\usage{
KsamplePlot(X, y, Ksample = c(seq(40, 100, 10), 150, seq(200, 1000, 100)), Method = "lm", Caret = "No", size = 5, Type = "numeric")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{Ksample}{
%%     ~~Describe \code{Ksample} here~~
}
  \item{Method}{
%%     ~~Describe \code{Method} here~~
}
  \item{Caret}{
%%     ~~Describe \code{Caret} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{Type}{
%%     ~~Describe \code{Type} here~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
