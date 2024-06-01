\name{summary.closer_grid_archetypal}
\alias{summary.closer_grid_archetypal}
\title{
Summary of an Object of the Class closer_grid_archetypal
}
\description{
It gives a summary for the output of \code{\link{closer_grid_archetypal}}
}
\usage{
\method{summary}{closer_grid_archetypal}(object, ...)
}
\arguments{
  \item{object}{
An object of the class closer_grid_archetypal
}
  \item{\dots}{
Other arguments (ignored)
}
}
\details{
Given an object of class closer_grid_archetypal the summary of the archetypal analysis output is given. 
}
\value{No return value, called for side effects}
\examples{
	# Load package
  library(GeomArchetypal)  
	# Create random data
	set.seed(20140519)
	df=matrix(runif(90) , nrow = 30, ncol=3) 
	colnames(df)=c("x","y","z")
	# Closer Grid Archetypal
	cga=closer_grid_archetypal(df, niter = 70, verbose = FALSE)	
	# Summary of the class "closer_grid_archetypal":
	summary(cga)  
}

