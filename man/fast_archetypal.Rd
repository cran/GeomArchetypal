\name{fast_archetypal}
\alias{fast_archetypal}
\title{
Performs the Archetypal Analysis of a Data Frame by using a Given Set of Archetypes
}
\description{
Performs the archetypal analysis of a data frame by using a known set of archetypes as rows of the data matrix.
}
\usage{
fast_archetypal(df,                 
                irows, 
                diag_less = 1e-2,
                niter = 30, 
                verbose = TRUE, 
                data_tables = TRUE,
                use_seed = NULL)
}
\arguments{
  \item{df}{
The data frame with dimensions n x d
} 
  \item{irows}{
The rows from data frame that represent the archetypes
}
  \item{diag_less}{
The expected mean distance from 1 for the diagonal elements of submatrix A[irows,:]
}
  \item{niter}{
The number of times that the A-update process should be done
}
  \item{verbose}{
If it is set to TRUE, then both initialization and iteration details are printed out
}
  \item{data_tables}{
If it set to TRUE, then a data table for the initial data points will be computed
}
  \item{use_seed}{
If it is not NULL, then is used at the set.seed() for reproducibility reasons
}
}
\details{
If we know the archetypes, then we can bypass the half part of PCHA and perform only the A-update part, that of computing the convex combinations for each data point. 
Then archetypal analysis is a fast procedure, since we need only to compute one matrix.
}
\value{
An object of class 'archetypal' is returned.
}
\seealso{
\code{\link{grid_archetypal}}, \code{\link{closer_grid_archetypal}}
}
\examples{
# Load package
library(GeomArchetypal)  
# Create random data
set.seed(20140519)
df=matrix(runif(90) , nrow = 30, ncol=3) 
colnames(df)=c("x","y","z")
# Closer Grid Archetypal
cga=closer_grid_archetypal(df, diag_less = 1e-3, 
                           niter = 250, verbose = FALSE)
# The closer to the Grid Archetypes points - rows are:
crows = cga$grid_rows
print(crows)
# Now we call the fast_archetypal() with those rows as argument:
fa=fast_archetypal(df, irows = crows, diag_less = 1e-3, 
                   niter = 250, verbose = FALSE)
# Print:
print(fa)
# Summary:
summary(fa)
# Plot:
plot(fa)
# Results are identical to the closer_grid_archetypal() ones:
all.equal(cga$aa$BY,fa$BY)
}
  
