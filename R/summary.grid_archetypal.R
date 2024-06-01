summary.grid_archetypal=function(object,...){
  grid=object$grid
  aa=object$aa
  #
  A=aa$A
  B=aa$B
  BY=aa$BY
  cat(paste0("Number of observations / data rows n = ",dim(A)[1]),"\n")
  cat(paste0("(The first ",nrow(grid)," rows are the Grid Archetypes)"),"\n")
  cat(paste0("Dimension of data variables d = ",dim(BY)[2]),"\n")
  cat(paste0("Number of Grid Archetypes k = ",dim(BY)[1]),"\n")
  cat("\n") 
  cat("Grid Archetypes:","\n")
  cat("\n") 
  print(data.frame(BY))
  basics=data.frame("SSE"=aa$SSE, "VarianceExplained"=aa$varexpl, "Convergence"= aa$converges, "Iterations"=aa$iterations, "ElapsedTime"=aa$time)
  cat("\n") 
  cat("Run details:","\n")
  cat("\n") 
  print(basics)
  cat("\n")   
  return(invisible(NULL))
}