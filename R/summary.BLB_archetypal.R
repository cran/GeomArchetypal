summary.BLB_archetypal=function(object,...){	
	#
	# Check class
	#
	if(!inherits(object,"BLB_archetypal")){stop("Error, input must be an object of class 'BLB_archetypal'")} #RE SET!
	#	
	arches = object$arches
	aa_tests = object$aa_tests
	pop_compos = object$pop_compos
	lower_ci = object$lower_ci
	upper_ci = object$upper_ci
	ci_sigma = object$ci_sigma
	#
	cat("The Grid Archetypal is:","\n")
	cat("\n") 
	print(arches)
	cat("\n") 
	#
	cat("For the tests we have the next short results: ","\n")
	cat("It converged as in next Table:","\n")
	cat("\n") 
	print(table(aa_tests$converges))
	cat("\n") 
	cat("The Variance Explained:","\n")
	cat("\n") 
	print(summary(aa_tests$varexpl))
	cat("\n") 
	cat("The maximum absolute correlations:","\n")
	cat("\n") 
	print(summary(aa_tests$max_abs_cors))
	cat("The orthogonality test was true under next Table:","\n")
	cat("\n") 
	print(table(aa_tests$ortho))
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
	return(invisible(NULL))	
}
