\name{fit.trend}
\alias{fit.trend}
\title{ Fit polynomial trend functions }
\description{
  Fits a polynomial trend function to a \code{\link{point}} object.
  Similar to functions in B. Ripleys spatial library.
}
\usage{
fit.trend(point.obj, at, np=2, plot.it=TRUE)
}
\arguments{
  \item{point.obj}{ \code{\link{point}} object }
  \item{at}{ name of dependent variable in \code{point.obj} }
  \item{np}{ degree of polynom to be fitted }
  \item{plot.it}{ switches generation of a contour plot }
}
\value{
  \item{beta}{estimated parameters}
  ...
}
\references{ 
http://www.gis.iastate.edu/SGeoStat/homepage.html
}

\keyword{spatial}
