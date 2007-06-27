\name{summary.mota}
\alias{summary.mota}
\title{ Summarize Mota Output }
\description{This method provides a nice print out to the command line for object of class mota
}
\usage{
summary.mota(object,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ Object of class \code{mota}}
  \item{...}{not yet implemented}
}

\value{
a nice list
}
\note{The rows of table represent indices which may be used as optional input for \code{\link{plot.mota}}.
	The content of the table changes if \code{\link{pvalue.mota}} is called in advance.}
\author{ Stefan Hengl}

\seealso{ \code{\link{mota}}, \code{\link{pvalue.mota}}}

\keyword{ methods }
\keyword{ utilities }