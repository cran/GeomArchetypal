plot.closer_grid_archetypal=function(x,...){
	#
	# Check class
	#
	if(!inherits(x,"closer_grid_archetypal")){stop("Error, input must be an object of class 'closer_grid_archetypal'")} #RE SET!
	#	 
	aa=x$aa	
	# Plot the closer_grid_archetypal:
	plot(aa)
	return(invisible(NULL))
}

