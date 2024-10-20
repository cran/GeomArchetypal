\name{summary.BLB_archetypal}
\alias{summary.BLB_archetypal}
\title{
Summarize an Object of the Class BLB_archetypal
}
\description{
It summarizes the output of \code{\link{BLB_archetypal}}
}
\usage{
\method{summary}{BLB_archetypal}(object, ...)
}
\arguments{
  \item{object}{
An object of the class BLB_archetypal
}
  \item{\dots}{
Other arguments (ignored)
}
}
\details{
Given an object of class BLB_archetypal all the results are being summarized in explanatory form.
}
\value{No return value, called for side effects}
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
# Summarize the results of the "BLB_archetypal" class object:
summary(test)
}
}
}

