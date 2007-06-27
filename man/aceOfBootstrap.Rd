\name{aceOfBootstrap}
\alias{aceOfBootstrap}
\title{Calculate Mean Optimal Transformations with Use of ACE}
\description{
  This function calculates the mean optimal transformations with use of ACE ((A)lternating (C)onditional (E)xpectation
	algorithm).  This is performed by bootstraping the input matrix \code{numOfBootSamp} times with a sample size
	\code{sampleSize}. 
}
\usage{
aceOfBootstrap(x, numOfBootSamp, sampleSize)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ \code{N X P}-matrix.}
  \item{numOfBootSamp}{The number of bootstrap samples drawn from x}
  \item{sampleSize}{The sample size of the bootstrap samples}
}

\value{
  A \code{(sampleSize X P)}-matrix representing estimated mean optimal transformations.  
}
\author{ Stefan Hengl}
\seealso{ \code{\link{mota}}}
\keyword{ methods}
\keyword{ utilities}
