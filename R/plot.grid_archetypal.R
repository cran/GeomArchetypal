plot.grid_archetypal=function(x,...){
	#
	# Check class
	#
	if(!inherits(x,"grid_archetypal")){stop("Error, input must be an object of class 'grid_archetypal'")} #RE SET!
	#	 
	aa=x$aa	
	# Plot the grid_archetypal:
	plot(aa)
	return(invisible(NULL))
}

