\name{summary.grid_archetypal}
\alias{summary.grid_archetypal}
\title{
Summary of an Object of the Class grid_archetypal
}
\description{
It gives a summary for the output of \code{\link{grid_archetypal}}
}
\usage{
\method{summary}{grid_archetypal}(object, ...)
}
\arguments{
  \item{object}{
An object of the class grid_archetypal
}
  \item{\dots}{
Other arguments (ignored)
}
}
\details{
Given an object of class grid_archetypal the summary of the archetypal analysis output is given. \cr
Remark: the first 2^d rows of the input data frame are the Grid Archetypes (d is the dimension of the data points).
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
# Summary of the class "grid_archetypal":
summary(gaa)
}

