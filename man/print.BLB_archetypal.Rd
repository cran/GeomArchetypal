\name{print.BLB_archetypal}
\alias{print.BLB_archetypal}
\title{
Print an Object of the Class BLB_archetypal
}
\description{
It prints the output of \code{\link{BLB_archetypal}}
}
\usage{
\method{print}{BLB_archetypal}(x, ...)
}
\arguments{
  \item{x}{
An object of the class BLB_archetypal
}
  \item{\dots}{
Other arguments (ignored)
}
}
\details{
Given an object of class BLB_archetypal all the results are printed in explanatory form.
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
# Print the results of the class "BLB_archetypal" object:
print(test)
}
}
}

