\name{plot.grid_archetypal}
\alias{plot.grid_archetypal}
\title{
Plot an Object of the class grid_archetypal
}
\description{
It plots the output of \code{\link{grid_archetypal}}
}
\usage{
\method{plot}{grid_archetypal}(x, ...)
}
\arguments{
  \item{x}{
An object of the class grid_archetypal
}
  \item{\dots}{
Other arguments (ignored)
}
}
\details{
Given an object of class grid_archetypal the archetypal analysis result is plotted. \cr
Remark: the first 2^d rows of the input data frame has Grid Archetypes (d is the dimension of the data points).
}
\value{No return value, called for side effects}
\examples{
# Load package
library(GeomArchetypal)  
# Create random data:
set.seed(20140519)
df=matrix(runif(90) , nrow = 30, ncol=3) 
colnames(df)=c("x","y","z")
# Grid Archetypal:
gaa=grid_archetypal(df, niter = 70, verbose = FALSE)
# Plot the class "archetypal":
plot(gaa)    
}

