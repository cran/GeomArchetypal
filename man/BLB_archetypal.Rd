\name{BLB_archetypal}
\alias{BLB_archetypal}
\title{
Archetypal Analysis using the Bag of Little Bootstraps
}
\description{
Archetypal analysis using the bag of little bootstraps as the resampling approach, following [1]
}
\usage{
BLB_archetypal(df = NULL, group_var = NULL, 
				aa_var = NULL, use_seed = NULL, 
				b = 0.6, n = 20, r = 100, n_core = 1, 
				n_iter = 30, ci_sigma = 2, 
				n_tails = 10, max_cor = 0.3, 
				verbose = TRUE, diag_less = 1e-2)
}
\arguments{
  \item{df}{
The data frame with the original sample to be processed
}
  \item{group_var}{
Draw the subsample equally from groups (integer or character)
}
  \item{aa_var}{
Character vector of the variable names that will be used
}
  \item{use_seed}{
Integer, if not NULL, used as set.seed() for reproducibility 
}
  \item{b}{
Numeric, set size of subsample i.e. = nrow(df)^b (default 0.6) 
}
  \item{n}{
Integer, number of subsamples to generate (default 20)
}
  \item{r}{
Integer, number of bootstraps of each subsample (default 100)
}
  \item{n_core}{
Integer, number of cores used for archetypal analysis of bootstraps
}
  \item{n_iter}{
Integer, number of iterations for fast_archetypal
}
  \item{ci_sigma}{
Integer, for empirical confidence intervals
}
  \item{n_tails}{
Integer, minimum number of bootstrap estimates required in tails 
for robust interval estimates (default 10 each tail)
}
  \item{max_cor}{
Default = 0.20, numeric for warning on orthogonality
}
  \item{verbose}{
Logical, reports progress of each subsample and batch of bootstraps
}
  \item{diag_less}{
The expected mean distance from 1 for the diagonal elements of submatrix A[1:kappas,:]
}
}
\details{
  Note 1. \cr 
  Without the weighted analysis idea of Kleiner et al
  which is inappropriate for geometrically-based archetypal analysis \cr
  Note 2. \cr
  The archetypes are defined from the minimums and maximums
  of the data to provide a fixed frame of reference for resampling.
  Resampling variation is thus simplified and only concerns compositions. \cr
  Note 3. \cr
  Assumes grouped data but allows user to supply
  a group variable with only one value 
}
\value{
An object of class "BLB_archetypal" which is a list with next members:
\enumerate{
\item \code{arches}, the Grid Archetypes
\item \code{aa_tests}, the run statistics for all subsamples, batches and replications (bootstraps)
\item \code{pop_compos}, the population estimates of compositions (by group or without grouping)
\item \code{lower_ci}, the lower confidence interval at the ci_sigma sigma level
\item \code{upper_ci}, the upper confidence interval at the ci_sigma sigma level
\item \code{ci_sigma}, the ci_sigma level for confidence intervals 
}
}
\references{
[1] Ariel Kleiner, Ameet Talwalkar, Purnamrita Sarkar, Michael I. Jordan, \doi{10.1111/rssb.12050} 
}
\author{
David. F. Midgley
}
\seealso{
\code{\link{closer_grid_archetypal}}, \code{\link{grid_archetypal}},
\code{\link{fast_archetypal}}
}
\examples{
{\donttest{
# Load package
library(GeomArchetypal)
# Load data
data("gallupGPS6")
# draw a small sample
set.seed(2024)
df <- gallupGPS6[sample(1:nrow(gallupGPS6),35000,replace = FALSE),]
# invent a grouping variable
df$grp <- cut(df$risktaking, breaks = 2)
test <- BLB_archetypal(df = df, 
                        group_var = "grp",
                        aa_var = c("patience","risktaking","trust"), 
                        n = 1, r = 2, n_core = 1,
                        diag_less = 1e-2)
# will generate a warning because number of bootstraps is too small to
# estimate default confidence intervals 
# Print results of the "BLB_archetypal" class object:
print(test)
# Summarize the "BLB_archetypal" class object:
summary(test)
}
}
}