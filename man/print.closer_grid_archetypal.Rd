\name{print.closer_grid_archetypal}
\alias{print.closer_grid_archetypal}
\title{
Print an Object of the closer_grid_archetypal
}
\description{
It prints the output of \code{\link{closer_grid_archetypal}}
}
\usage{
\method{print}{closer_grid_archetypal}(x, ...)
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
Given an object of class closer_grid_archetypal all the results are printed in explanatory form.
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
# Print the class "closer_grid_archetypal"
print(cga)	
}

