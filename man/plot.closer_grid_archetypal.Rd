\name{plot.closer_grid_archetypal}
\alias{plot.closer_grid_archetypal}
\title{
Plot an Object of the Class closer_grid_archetypal
}
\description{
It plots the output of \code{\link{closer_grid_archetypal}}
}
\usage{
\method{plot}{closer_grid_archetypal}(x, ...)
}
\arguments{
  \item{x}{
An object of the class closer_grid_archetypal
}
  \item{\dots}{
Other arguments (ignored)
}
}
\details{
Given an object of class closer_grid_archetypal the archetypal analysis result is plotted. 
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
# Plot the class "closer_grid_archetypal":
plot(cga)	
}

