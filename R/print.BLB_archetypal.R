print.BLB_archetypal=function(x,...){	
	#
	# Check class
	#
	if(!inherits(x,"BLB_archetypal")){stop("Error, input must be an object of class 'BLB_archetypal'")} #RE SET!
	#	
	arches = x$arches
	aa_tests = x$aa_tests
	pop_compos = x$pop_compos
	lower_ci = x$lower_ci
	upper_ci = x$upper_ci
	ci_sigma = x$ci_sigma
	#
	cat("The Grid Archetypal is:","\n")
	cat("\n") 
	print(arches)
	cat("\n") 
	#
	cat("Run Statistics: ","\n")
	cat("s i b j r k: subsample i, batch i, replication (bootstrap) k","\n")
	cat("\n") 
	print(aa_tests)
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
