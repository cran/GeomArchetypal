## ----setup, include=FALSE-----------------------------------------------------
oldpar <- par(no.readonly = TRUE) 
oldoptions <- options()
library(GeomArchetypal)
library(knitr)
library(rmarkdown)
knitr::opts_chunk$set(echo = TRUE)
options(max.width = 1000)
options(max.print = 100000)
write_matex2 <- function(x) {
  begin <- "\\begin{matrix}"
  end <- "\\end{matrix}"
  X <-
    apply(x, 1, function(x) {
      paste(
        paste(x, collapse = "&"),
        "\\\\"
      )
    })
  paste(c(begin, X, end), collapse = "")
}
m3=expand.grid(c('Y1,min','Y1,max'),c('Y2,min','Y2,max'),c('Y3,min','Y3,max'))

## ----gaa1, echo=TRUE----------------------------------------------------------
library(GeomArchetypal)
# Create random data
set.seed(20140519)
df=matrix(runif(300) , nrow = 100, ncol=3) 
colnames(df)=c("x","y","z")
# Grid Archetypal
gaa=grid_archetypal(df, diag_less = 1e-6, niter = 50, verbose = FALSE)
summary(gaa)

## ----gaa2, echo=FALSE,out.width='75%', fig.align='center',fig.width=10, fig.height=6, echo=TRUE----
plot(gaa)
points_inside_convex_hull(df, gaa$grid)

## ----cga1, echo=TRUE----------------------------------------------------------
# Closer Grid Archetypal
cga=closer_grid_archetypal(df,diag_less = 1e-3, niter = 70, verbose = FALSE)
summary(cga)
plot(cga)

## ----cga2, echo=TRUE----------------------------------------------------------
pc=points_inside_convex_hull(df,cga$aa$BY)
print(pc)

## ----fa1, echo=TRUE-----------------------------------------------------------
# Fast Archetypal
fa=fast_archetypal(df, irows = cga$grid_rows, diag_less = 1e-6,
                   niter = 100, verbose = FALSE)
summary(fa)
plot(fa)
par(oldpar)
options(oldoptions)

