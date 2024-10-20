summary.BLB_archetypal=function(object,...){	
	#
	# Check class
	#
	if(!inherits(object,"BLB_archetypal")){stop("Error, input must be an object of class 'BLB_archetypal'")} #RE SET!
	#		
  arches = object$arches
  pop_compos = object$pop_compos
  lower_ci = object$lower_ci
  upper_ci = object$upper_ci
  ci_sigma = object$ci_sigma
  N = object$N
  #
  cat("The Grid Archetypal is:","\n")
  cat("\n") 
  print(arches)
  cat("\n") 
  #
  cat("Population estimates of compositions (by group or without grouping): ","\n")
  cat("A i pm: Archetype i composition mean value","\n") 
  print(pop_compos)
  cat("\n")
  #
  cat(paste0("Lower confidence interval at ",ci_sigma," sigma: "),"\n")
  cat("A i l: Archetype i composition lower CI value","\n") 
  print(lower_ci)
  cat("\n")
  #
  cat(paste0("Upper confidence interval at ",ci_sigma," sigma: "),"\n")
  cat("A i u: Archetype i composition upper CI value","\n") 
  print(lower_ci)
  cat("\n")
  #
  cat(paste0("The original sample size N: "),"\n")
  cat(N,"\n")
  cat("\n")
  #
	return(invisible(NULL))	
}
