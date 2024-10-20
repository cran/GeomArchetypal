\name{BLB_archetypal}
\alias{BLB_archetypal}
\title{
Archetypal Analysis using the Bag of Little Bootstraps
}
\description{
Archetypal analysis using the bag of little bootstraps as the resampling approach, following [1]
}
\usage{
BLB_archetypal(df = NULL, ss_size = NULL, bs_size = NULL,
			arches = NULL, use_seed = NULL, n = 20,
			r = 100, n_core = 1, n_iter = 100, ci_sigma = 2.5757,
			n_tails = 10, diag_less = 0.01)
}
\arguments{
  \item{df}{
The data frame with the original sample to be processed.
}
  \item{ss_size}{
The size of the subsample to be drawn from df without replacement.
}
  \item{bs_size}{
The size of the bootstrap replications to be drawn with replacement from each subsample, typically nrow(df), but see Details.
}
  \item{arches}{
A data frame of the archetype profiles, whose column names correspond to variables found in df. See Details.
}
  \item{use_seed}{
Integer, if not NULL, used as set.seed() for reproducibility.
}
  \item{n}{
Integer, the number of subsamples to draw from df (default 20).
}
  \item{r}{
Integer, the number of bootstrap replications of each subsample (default 100). r must be an integer multiple of n_core. See Details.
}
  \item{n_core}{
Integer, the number of cores available for batch processing of bootstrap replications. 
}
  \item{n_iter}{
Integer, number of iterations for fast_archetypal.
}
  \item{ci_sigma}{
Numeric, for empirical confidence intervals (default 2.5757 for 99% intervals). See Details.
}
  \item{n_tails}{
Integer, minimum number of bootstrap estimates required in the tails for robust confidence intervals (default 10 each tail).
}
  \item{diag_less}{
Test of convergence for the archetypal row weights. The expected mean distance from 1 for the weights of the archetypes themselves (default 0.01). See Details.
}
}
\details{
Usage. BLB_archetypal is designed to be used with large samples (> 1k cases) and to minimize runtimes by employing parallel processing with multiple cores. \cr
Archetypes. BLB_archetypal is also designed to be used with fixed archetypes which the user has determined a priori. Resampling variability then solely concerns the row weights based on these archetypes, considerably reducing runtimes and simplifying analysis because all bootstraps have the same frame of reference. There are several ways of determining these archetypes, one recommendation that facilitates interpretation is to base them on all combinations of the maximums and minimums of the variables on which the archetypes are to be based. See GeomArchetypal.\cr
Computational process. n subsamples of size ss_size are drawn without replacement from df, r replications of size bs_size are then generated from each subsample with replacement. The fast_archetypal function is then applied to each of the resulting n * r bootstraps to estimate the row weights for the user supplied arches.  The mean weights from the n * r bootstraps provide the population estimate for the row weights, and the ci_sigma confidence intervals for this estimate are determined directly from the distribution of bootstrap estimates. The confidence intervals are thus not bias adjusted, but this is less necessary with large samples and the direct method reduces runtimes.\cr
Convergence. In fast_archetypal, the set of archetypes are appended to the data frame to be analyzed. At convergence the row weights for these archetypes should be a target matrix with 1s on the diagonal and 0s off diagonal. diag_less provides the test of this convergence, with the default 0.01 implying the mean of the distances between the set of weights and the target matrix must be <= 0.01. Users may wish to consider stricter tests, such as 0.001 or 1e-6 but these will considerably increase the number of iterations needed before the algorithm converges and hence runtimes. Note if one or more bootstraps do not converge, the function will generate an error as otherwise incorrect estimates would be included in the subsequent calculations. In this case, the user should increase n_iter before rerunning.\cr
Confidence intervals. The default settings of n = 20 subsamples each with r = 100 replications generate 2000 bootstraps and ci_sigma = 2.5757 or ~ 99% confidence intervals, where at least n_tails = 10 estimates are found in each tail outside the interval, providing a degree of robustness to the interval estimates. A warning will be issued if the total number of bootstraps is inconsistent with the desired level of confidence, given the n_tails setting.\cr
Parallel processing. To reduce run times parallel processing can be used, with n_core processing cores. In this case bootstraps are processed in n_core batches and r must be an integral multiple of n_core or an error will occur. From the first batch, the function generates a message with an approximate estimate of the total run time, allowing the user to assess when completion is likely.\cr
Bootstrap size. Typically, this is the size of the original sample. However, if the original sample is very large (> 100k), the user may wish to consider taking a large subsample of the original externally to the function, to avoid excessive runtimes.\cr
}
\value{
An object of class "BLB_archetypal" which is a list with next members:
\enumerate{
\item \code{arches}, the user supplied archetypes on which the results are based.
\item \code{pop_compos}, the population estimates of the compositions (archetypal case weights) for each row of df. 
\item \code{lower_ci}, the lower confidence interval for these estimates at the ci_sigma level.
\item \code{upper_ci}, the upper confidence interval for these estimates at the ci_sigma level.
\item \code{ci_sigma}, the ci_sigma level for confidence intervals.
\item \code{N}, the original sample size, nrow(df).
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
library(GeomArchetypal)
library(mirai)
library(parallel)
data("gallupGPS6")
aa_var <- c("patience","risktaking","trust") # variables for archetypal analysis
# define the 2^3 archetypes from minimums and maximums of the data
min_var <- apply(gallupGPS6[, aa_var], 2, min, na.rm = TRUE)
max_var <- apply(gallupGPS6[, aa_var], 2, max, na.rm = TRUE)
temp <- as.data.frame.matrix(rbind(min_var, max_var))
colnames(temp) <- aa_var
list_minmax <- apply(temp, 2, as.list)
rm(temp, min_var, max_var)
arches <- data.matrix(do.call(expand.grid, list_minmax))
arches <- as.data.frame(arches)
rm(list_minmax)
# apply BLB archetypal for a minimal example
test <- BLB_archetypal(df = gallupGPS6, 
                       ss_size = 50, 
                       bs_size = nrow(gallupGPS6),
                       arches = arches, 
                       use_seed = 2024,
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