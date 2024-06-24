Overview
--------

GeomArchetypal is a package that performs Geometrical Archetypal Analysis after creating 
Grid Archetypes which are the Cartesian Product of all minimum, maximum variable values. 
Since the archetypes are fixed now, we have the ability to compute the convex composition 
coefficients for all our available data points much faster by using the half part of 
Principal Convex Hull Archetypal method. Additionally we can decide to keep as archetypes 
the closer to the Grid Archetypes ones. Finally the number of archetypes is always 
2 to the power of the dimension of our data points if we consider them as a vector space.

Basic functions are:

-   `grid_archetypal()` performs Geometrical Archetypal Analysis (GAA) or Grid Archetypal in short.
-   `closer_grid_archetypal()` finds the closer to the Grid Archetypes points and proceed to GAA.
-   `fast_archetypal()` the generic function for computing the A-matrix given the rows of archetypes.
-   `points_inside_convex_hull()` computes the percentage of points that lie inside the Convex Hull 
     which is created by a set of vectors (archetypes in general).
-   `BLB_archetypal()` performs GAA after using the bag of little bootstraps as the resampling approach.

Install the archetypal package and then read
`vignette("GeomArchetypal", package = "GeomArchetypal")`.

Installation
------------

``` r
# Install with dependencies:
install.packages("GeomArchetypal",dependencies=TRUE)
```

Usage
-----

``` r
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
# we use as archetypal rows the closer to the Grid Archetypes
# as they were find by closer_grid_archetypal() function
fa=fast_archetypal(df, irows = cga$grid_rows, diag_less = 1e-3, 
                    niter = 200, use_seed = vseed)
# Print
print(fa)
# Summary
summary(fa)
# Plot
plot(fa)
```

Contact
-------
Issues:

https://github.com/dchristop/GeomArchetypal/issues

Please send comments and suggestions to
<dchristop@econ.uoa.gr> or <dem.christop@gmail.com>