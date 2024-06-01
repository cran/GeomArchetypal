summary.closer_grid_archetypal=function(object,...){
  grid_rows=object$grid_rows
  aa=object$aa
  #
  A=aa$A
  B=aa$B
  BY=aa$BY
  #
  cat(paste0("Number of observations / data rows n = ",dim(A)[1]),"\n")
  cat(paste0("Dimension of data variables d = ",dim(BY)[2]),"\n")
  cat(paste0("Number of Closer Grid Archetypes k = ",dim(BY)[1]),"\n")
  paste0(aa$grid_rows,collapse = ",")
  cat("The rows that formed the Closer Grid Archetypes are:","\n")
  cat(paste0(grid_rows,collapse = ","),"\n")
  cat("\n") 
  cat("The Closer Grid Archetypes are:","\n")
  cat("\n") 
  print(data.frame(BY))
  basics=data.frame("SSE"=aa$SSE, "VarianceExplained"=aa$varexpl, "Convergence"= aa$converges, "Iterations"=aa$iterations, "ElapsedTime"=aa$time)
  cat("\n") 
  cat("The Run details are:","\n")
  cat("\n") 
  print(basics)
  cat("\n")   
  return(invisible(NULL))
}