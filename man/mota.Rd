\name{mota}
\alias{mota}
\title{ Mean Optimal Transformation Approach}
\description{Identifies arbitrary functional relations among columns of an input matrix. This is done with use of 
optimal transformations and a special test function designed to quantify the information contained in them. See [1] for
further information about the test function.}
\usage{
mota(x, numOfBootSamp = 35, sampleSize = floor(length(x[,1])/2), threshold1 = 0.01, threshold2 = 0.07, threshold3 = 0.08)
}
\arguments{
  \item{x}{(N X P) matrix, with \code{N} replicates for \code{P} parameters (see details)}
  \item{numOfBootSamp}{The number of bootstrap samples drawn from \code{x} to estimate the mean optimal transformation}
  \item{sampleSize}{ Size of the bootstrap samples }
  \item{threshold1}{ Lower threshold. The higher, the more conservative is the algorithm. \code{threshold1} determines
				the value of the test function beneath which \code{mota} definitely assigns no functional relation.
				For values of the test function between \code{threshold1} and \code{threshodl2}, \code{mota} adds
				additional parameters as predictors to explain more variance of the response in order to
				finally exceed \code{threshold2}.}
  \item{threshold2}{ Upper threshold. The higher, the more conservative gets the algorithm. \code{threshold2} should not
				exceed \code{1/12}. \code{threshold2} determines the value of the mean test function above which
				a functional relation is assigned to be strong.}
  \item{threshold3}{ Additional threshold to increase sensitivity.}
}
\details{
The matrix \code{x} is the only input required. \code{mota} investigates \code{x} for functional relations among
the columns. Note, these relations may in general be non-linear, which renders this approach superior to 
algorithms based simply on the investigation of correlations. Roughly, \code{mota} takes column \code{i} as 
response and tries to find the optimal combination of the other columns \code{j!=i} to explain the variance
of the response. \code{Optimal transformations} are employed to map potential nonlinear relations on a linear
model. The test function calculates the variance of the mean optimal transformation for each column, see references
for details. Predictors (columns) not associated with the response have low values of the test function. The mean of the
optimal transformation is estimated by drawing bootstrap samples of size \code{sampleSize} from \code{x}. 

The \code{sampleSize} does not equal the number of rows to ensure sufficient independency of the drawn bootstrap
samples. \code{threshold1} and \code{threshold2} may be determined analytically [1]. \code{threshold3} needs to be larger than \code{threshold2}. It
improves under certain circumstances the sensitivity of the algorithm, however, it is not required for the functionallity
of the algortithm. The number of bootstrap samples is a good trade off between computational speed and specificity,
see, again, [1].

\code{mota} was designed for the identifiability analysis of nonlinear dynamical models. The generality of the concept,
however, suggests applications in various fields. Whenever there is a need of detecting arbitrary functional relations
between subgroups of parameters, \code{mota} may be superior especially for non-linear relations.}


\value{
  An object of class \code{mota} with following attributes:
  \item{S}{A \code{(P X P)} matrix with one indicating functional relations}
  \item{x }{The input matrix}
  \item{Hout}{Value of the testfunction for the functional relation found for each paramater}
  }
\references{\item{}{[1] S. Hengl, C. Kreutz, J. Timmer, and T. Maiwald, Data-based Identifiability Analysis of Nonlinear Dynamical Models,
		Bioinformatics, 2007}
		\item{}{[2] L. Breiman and J.H. Friedman, Estimating Optimal Transformations for Multiple Regression and Correlation,
		  	Journal of the American Statistical Associtation, 1985, 80(391), 580-598}
		}
\author{ Stefan Hengl }
\note{Use 'summary' and 'plot' to investigate the output. The attributes may be accessed with use of 'attr'. So, in order
	to get the matrix \code{S} one has to type \code{S<-attr(motaOutput,"S")} in the command line}

\seealso{ \code{ace} from the package \code{acepack}, \code{\link{summary.mota}}, \code{\link{plot.mota}}, \code{\link{attr}} }

\examples{
x<-array(data=0,dim=c(200,4))
#
# generate uniformly distributed data
x[,4]<-runif(200,1,5)
x[,3]<-runif(200,1,5)
x[,2]<-runif(200,1,5)
#
# introduce functional relation
x[,1]<-x[,2]^2+5*sin(x[,3])
#
# test with mota
motaOut<-mota(x)
#
# investigate result
summary(motaOut)
#
# plot aceplots of all parameter
# related with the parameter three
plot(motaOut,3)
#
# inspect deviances of x and include
# information about explained variance
get(getOption("device"))()
plot(motaOut,"dev")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
\keyword{ utilities }
