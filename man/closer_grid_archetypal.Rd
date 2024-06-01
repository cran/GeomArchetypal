\name{closer_grid_archetypal}
\alias{closer_grid_archetypal}
\title{
Performs the Archetypal Analysis of a Data Frame by using as Archetypes the Closer to The Archetypal Grid Data Points
}
\description{
the closer points to the archetypal grid are used as archetypes and then every data point is being expressed as a convex combination of 
those by using a modified PCHA method.
}
\usage{
closer_grid_archetypal(dg, 
                       diag_less = 1e-2,
                       niter=30, 
                       use_seed = NULL,
                       verbose = TRUE)                       
}
\arguments{
  \item{dg}{
The data frame with dimensions n x d
}
  \item{diag_less}{
The expected mean distance from 1 for the diagonal elements of submatrix A[irows,:],
where \code{irows} are the closer to the \code{Grid Archetypal} data rows.
}
  \item{niter}{
The number of iterations that the A-update step should be done.
}
  \item{use_seed}{
If it is not NULL, then is used at the set.seed() for reproducibility reasons
}
  \item{verbose}{
If it is set to TRUE, then both initialization and iteration details are printed out
}
}
\details{
The archetypal grid is being computed by taking the expand grid of the [Ximin,Ximax], i=1,...,d of all available variables. 
Then distances of all data points from that grid are calculated and the closer set of vectors is chosen. 
}
\value{
An object of class closer_grid_archetypal which is a list with members:
\enumerate{
\item \code{grid}, the archetypal grid
\item \code{grid_rows}, the rows of the data frame that formed the archetypal grid
\item \code{aa}, an object of class archetypal
}
}
\seealso{
\code{\link{grid_archetypal}}
}
\examples{    
# Load package
library(GeomArchetypal)  
# Create random data
set.seed(20140519)
df=matrix(runif(90) , nrow = 30, ncol=3) 
colnames(df)=c("x","y","z")
# Closer Grid Archetypal
cga=closer_grid_archetypal(df, 
             diag_less = 1e-2, 
             niter = 150, 
             verbose = FALSE)
# Print the class "closer_grid_archetypal":
print(cga)
# Summary of the class "closer_grid_archetypal":
summary(cga)
# Plot the class "closer_grid_archetypal":
plot(cga)
# Observe the Closer Grid Archetypes near the 8 corners of the cube ...
}