\name{GeomArchetypal-package}
\alias{GeomArchetypal-package}
\alias{GeomArchetypal}
\docType{package}
\title{
Finds the Geometrical Archetypal Analysis of a Data Frame
}
\description{
Performs Geometrical Archetypal Analysis after creating Grid Archetypes which are the Cartesian Product of all minimum, maximum
   variable values. Since the archetypes are fixed now, we have the ability to compute the convex composition coefficients for all our
   available data points much faster by using the half part of PCHA method. Additionally we can decide to keep as archetypes the closer to 
   the Grid Archetypes ones. Finally the number of archetypes is always 2 to the power of the dimension of our data points 
   if we consider them as a vector space.
}
\details{
Given a data frame \code{df} which is a matrix of n observations (rows) for the d variables (columns) we compute for 
all variables Xj the (Xj.min , Xj.max), j=1,2,..., n. \cr \cr
By taking the Cartesian Product of all those sets we form the vector set of Grid Archetypes which are 2 to the power of d and 
all other points lie inside their Convex Hull. \cr \cr
For example if we take the case of d=2 and our variables are named X,Y, then the Cartesian Product gives next points: \cr \cr
(Xmin,Ymin),(Xmax,Ymin),(Xmin,Ymax),(Xmax,Ymax) \cr \cr
Now the problem of seeking for the best number of archetypes is solved and kappas is 2 to the power of d. \cr
The main task is to express all inner data points as convex combination of the Grid Archetypes. \cr
For that reason we drop the half part of the PCHA algorithm of [1], [2] and keep only the desired one, that of the A-matrix computation. \cr
This is the task for the \code{grid_archetypal()} function.\cr \cr
If we want to seek for the closer to the Grid Archetypes points and set them as archetypes and proceed by the same way, then we use 
the \code{closer_grid_archetypal()} function. \cr \cr
All the two above functions use the generic function \code{fast_archetypal()} which computes the A-matrix for a given set rows of archetypes for 
our data frame of interest. \cr \cr
Finally we introduce the function \code{points_inside_convex_hull()} which for a given data frame \code{df} and a given set of at least \code{d + 1} 
archetypes or points in general \code{dp},  computes the percentage of data points that lie inside the \code{Convex Hull} which is created by \code{dp}. \cr
A pdf version of a detailed description can be found in [3].
}
\author{
Demetris Christopoulos \cr
https://orcid.org/0009-0008-6436-095X
}
\references{
[1] M Morup and LK Hansen, "Archetypal analysis for machine learning and data mining", Neurocomputing (Elsevier, 2012).
 https://doi.org/10.1016/j.neucom.2011.06.033. \cr
[2] Source: https://mortenmorup.dk/?page_id=2 , last accessed 2024-03-09 \cr
[3] Christopoulos, DT. (2024) https://doi.org/10.13140/RG.2.2.14030.88642
}
\keyword{grid}
\seealso{
\code{\link{grid_archetypal}}, \code{\link{closer_grid_archetypal}}, \code{\link{fast_archetypal}}
}
\examples{ 
# Load package
library(GeomArchetypal)  
# Create random data
vseed = 20140519
set.seed(vseed)
df=matrix(runif(90) , nrow = 30, ncol=3) 
# Grid Archetypal
gaa=grid_archetypal(df, diag_less = 1e-6, 
                    niter = 50, use_seed = vseed)
# Print
print(gaa)
# Summary
summary(gaa)
plot(gaa)
# Closer Grid Archetypal
cga=closer_grid_archetypal(df, diag_less = 1e-3, 
                           niter = 200, use_seed = vseed)
# Print
print(cga)
# Summary
summary(cga)
# Plot
plot(cga)
# Fast Archetypal: 
# we use as archetyupal rows the closer to the Grid Archetypes
# as they were foind by closer_grid_archetypal() function
fa=fast_archetypal(df, irows = cga$grid_rows, diag_less = 1e-3, 
                    niter = 200, use_seed = vseed)
# Print
print(fa)
# Summary
summary(fa)
# Plot
plot(fa)
}