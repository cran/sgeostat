\name{in.chull}
\alias{in.chull}
\title{ Convex hull test }
\description{
  Checks if points are in the interior of a convex hull.
}
\usage{
in.chull(x0, y0, x, y)
}
\arguments{
  \item{x0}{ coordinates of points to check }
  \item{y0}{ see \code{x0} }
  \item{x}{ coordinates defining the convex hull }
  \item{y}{  see \code{x}  }
}
\details{
  Uses a simple points-in-polygon check combined with the \code{\link{chull}}
  function.
}
\value{
  \item{comp1 }{Description of `comp1'}
  \item{comp2 }{Description of `comp2'}
}
\references{ Follows an idea from algorithm 112 from CACM
     (available at http://www.netlib.org/tomspdf/112.pdf) }
\author{ Albrecht Gebhardt <agebhard@uni-klu.ac.at> }

\seealso{ \code{\link{in.convex.hull}}, \code{\link{chull}} }

\examples{
in.chull(c(0,1),c(0,1),c(0,1,0,-1),c(-1,0,1,0))
# should give: TRUE FALSE
}
\keyword{ sgeostat }%-- one or more ...
