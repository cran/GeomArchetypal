print.grid_archetypal=function(x,...){
	#
	# Check class
	#
	if(!inherits(x,"grid_archetypal")){stop("Error, input must be an object of class 'grid_archetypal'")} #RE SET!
	#	 
  oldoptions = options()
  #
	grid=x$grid
	k=nrow(grid)
	aa=x$aa	
	A=x$A
	Y=x$Y
	# Print the Grid
	cat("The Grid Archetypes are:","\n")
	cat("\n") 
	print(data.frame(grid))
	cat("\n")
	# Print the AA
	cat("The Grid Archetypal Analysis after setting the grid archetypes","\n")
	cat(paste0("as the first ",k," rows of the data set is:"),"\n")
	cat("\n")
	print(aa)
	cat("\n")
	# print the A matrix of compositions
	cat("The A matrix of compsistions for the initial data points is:","\n")
	cat("\n")
	#  
	nr=dim(A)[1]
	nc=dim(A)[2]
	nda=6
	if(nr<=20){
	A0=A
	A0=data.frame(round(A,digits = nda))
	vlines=matrix("|",nrow=dim(A0)[1],ncol=1)
	rsums=round(rowSums(A0),digits=2)
	APF=cbind(A0,vlines,rsums)
	colnames(APF)=c(1:nc,"|","Sums")
	if(nr>10){nda=3}	
	}else {
	A0=data.frame(round(A[1:5,],digits = nda))
	colnames(A0)=1:nc
	A1=data.frame(round(A[(nr-4):nr,],digits = nda))
	colnames(A1)=1:nc
	hdots=data.frame(matrix(rep(".",3*nc),nrow = 3,ncol = nc))
	colnames(hdots)=1:nc
	AP=data.frame(rbind(A0,hdots,A1))
	rownames(AP)=c(1:5," ","  ","   ",(nr-4):nr)
	colnames(AP)=1:nc
	vlines=matrix("|",nrow=dim(AP)[1],ncol=1)
	rsums=c(round(rowSums(A1),digits=2),rep(".",3),round(rowSums(A1),digits = 2))
	APF=cbind(AP,vlines,rsums)
	colnames(APF)=c(1:nc,"|","Sums")
	}
	#	
	print(APF)
	cat("\n")  
	# Print the initial data set
	cat("The initial data frame was:","\n") 
	cat("\n")
	YDF=data.frame(Y)
	if(nr<=20){
	  print(round(YDF,digits=nda))}else{
	          print(round(YDF[1:5,],digits=nda))
	          cat("...","\n")
    			  cat("...","\n")
    			  cat("...","\n")
    			  print(round(YDF[(nrow(YDF)-5):NROW(YDF),],digits=nda))
    			  }
	cat("\n") 	
  #
  options(oldoptions)
  #
	return(invisible(NULL))
}

