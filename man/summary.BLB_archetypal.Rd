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
# Summarize the results of the "BLB_archetypal" class object:
summary(test)
}
}
}

