\name{grid_archetypal}
\alias{grid_archetypal}
\title{
Performs the Archetypal Analysis of a Data Frame by using as Archetypes the Archetypal Grid
}
\description{
The archetypal grid is the expand grid of all intervals (X.imin,X.imax), i=1,...,d for a d-dimensional data frame. \cr
That grid is used as archetypes and then only the A-update part of PCHA algorithm is used for computing the compositions of all data points. \cr
The number of archetypes is always kappas = 2^d.
}
\usage{
grid_archetypal(dg,  
                diag_less = 1e-2,
                niter = 30, 
                use_seed = NULL, 
                verbose = TRUE)
}
\arguments{
  \item{dg}{
The data frame with dimensions n x d
}
  \item{diag_less}{
The expected mean distance from 1 for the diagonal elements of submatrix A[1:kappas,:]
}
  \item{niter}{
The number of times that the A-update process should be done
}
  \item{use_seed}{
If it is not NULL, then is used at the set.seed() for reproducibility reasons
}
  \item{verbose}{
If it is set to TRUE, then both initialization and iteration details are printed out
}
}
\details{
The archetypal grid defines a hyper-volume which contains the 100 \% of all data points, if we take those grid points as the Convex Hull of all points. Although the archetypal grid points do not necessarily lie inside the data frame, here we do not care about that property: we only seek for the matrix of convex combinations (or composition matrix) A.
}
\value{
An object of class grid_archetypal which is a list with members:
\enumerate{
\item \code{grid}, the archetypal grid
\item \code{aa}, an object of class 'archetypal' which includes the archetypal grid as the first 2^d rows
\item \code{A}, the A-matrix with dimensions n x d that defines the compositions of all data points
\item \code{Y}, the matrix of initial data points
}
}
\seealso{
\code{\link{closer_grid_archetypal}}
}
\examples{  
# Load package  
library(GeomArchetypal)
# Create random data:
set.seed(20140519)
df=matrix(runif(90) , nrow = 30, ncol=3) 
colnames(df)=c("x","y","z")
# Grid Archetypal:
gaa=grid_archetypal(df, diag_less = 1e-6, 
                    niter = 70, verbose = FALSE)
# Print class "grid_archetypal":
gaa
# Summary class "grid_archetypal":
summary(gaa)
# Plot class "grid_archetypal":
plot(gaa)
# Observe the Grid Archetypes at the 8 corners of the cube ..
}
