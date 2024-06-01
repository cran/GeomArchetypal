\name{points_inside_convex_hull}
\alias{points_inside_convex_hull}
\title{
Computes the Percentage of Points that Lie Inside the Convex Hull which is Created by a Set of Vectors
}
\description{
Given a set of k d-dimensional vectors which creates a Convex Hull (CH) we want to find the percentage of 
the n points of the n x d data frame df that lie inside that CH.
}
\usage{
points_inside_convex_hull(df, dp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
The n x d data frame of all available data points
}
  \item{dp}{
The k x d data frame of the given set of points that creates the Convex Hull
}
}
\details{
In order for a really Convex Hull creation it must hold that: k >= d + 1, otherwise the problem is not well stated.
}
\value{
A numeric output with percentage in two decimal digits
}
\author{
Demetris T. Christopoulos
}
\note{
Keep in mind that working with dimension greater than 6 will practical lead to extreme time executions.
It highly suggested to work only for spaces with d<=6.
}
\examples{
# Load package
library(GeomArchetypal)  
# Create random data:
set.seed(20140519)
df=matrix(runif(90) , nrow = 30, ncol=3) 
colnames(df)=c("x","y","z")
# Grid Archetypal:
gaa=grid_archetypal(df, niter = 70, verbose = FALSE)
pc1=points_inside_convex_hull(df,gaa$grid)
print(pc1)
# [1] 100
# Closer Grid Archetypal:
cga=closer_grid_archetypal(df, niter = 70, verbose = FALSE)
pc2=points_inside_convex_hull(df,cga$aa$BY)
print(pc2)
# [1] 59 
}
