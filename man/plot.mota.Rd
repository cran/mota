\name{plot.mota}
\alias{plot.mota}
\title{ Plot Aceplots Of Mota-Objects}
\description{This method produces plots for objects of class \code{mota} which support
		the investiagtion of the output provided by \code{mota} }
\usage{
plot.mota(x,...)
}
\arguments{
  \item{x}{object of class mota}
  \item{...}{option}
}
\details{
Following options are currently implemented

"numeric":  	the numeric indicates the index of functional relation that shell be plotted in form of ACE-plots

"cv":	      	plots (1) boxplots of all centred parameters and (2) the coefficient of variation in form of semi-logarithmic plots.
			The amount of explained variance is encoded in form of green circles in (2) which mark those parameters the variance
			of which can be explained above a certain threshold "r2-threshold". The default is
			"r2-threshold=0.95", but it may also be given as optional argument.

"pairs":		calls the function \code{pairs} with \code{x} as argument

"scatter":		This options is untested in the current version of \code{mota}. It plots 3D scatter plots of three columns of the input matrix x.
	            The columns are specified either by an additional argument "col=vector", where vector is vector of length
			three, or by an additional argument "ix=numeric", where the index "ix" corresponds to the identified
			functional relation, see \code{\link{summary.mota}}. Example: plot(motaOut,"scatter",ix=1).
			Another optional argument is "angle=degree" which specifies the angle in degrees for the perspective
			of the scatter plot. The default value is "angle=40".

"scatter.movie":	This options is untested
			in the current version of \code{mota}. It shows a rotating coordinate system in which the scatter
			plot of three parameters, specified either by "ix" oder "col", is plotted.
	}

\value{
  figure
}
\author{Stefan Hengl}
\seealso{\code{\link{mota}}, \code{\link{summary.mota}}}
\keyword{methods}
\keyword{utilities }

