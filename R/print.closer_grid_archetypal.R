print.closer_grid_archetypal=function(x,...){
	#
	# Check class
	#
	if(!inherits(x,"closer_grid_archetypal")){stop("Error, input must be an object of class 'closer_grid_archetypal'")} #RE SET!
	#	 
	grid=x$grid
	#
	aa=x$aa	
	#
	grid_rows=x$grid_rows
	#
	# Print the Grid
	cat("The Grid Archetypes are:","\n")
	cat("\n") 
	print(data.frame(grid))
	cat("\n")
	# Print the Grid Rows
	cat("The rows that formed the Closer Grid Archetypes are:","\n")
	cat("\n")
	cat(paste0(grid_rows,collapse = ","),"\n")
	cat("\n")
	# Print the AA
	cat("The Closer Grid Archetypal Analysis results are:","\n")
	cat("\n")
	print(aa)
	cat("\n")	
	return(invisible(NULL))
}

