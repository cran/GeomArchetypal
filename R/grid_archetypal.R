grid_archetypal <- function(x) UseMethod("grid_archetypal")
#
grid_archetypal=function(dg, 
                         diag_less = 1e-2,
                         niter=30,
                         use_seed = NULL,
                         verbose = TRUE){
  #
  # Set seed if it is given ..
  #
  if(!is.null(use_seed)){
    set.seed(use_seed)
  }
  #
  # CREATE GRID
  list_minmax=apply(apply(dg,2,range),2,as.list)
  grids=data.matrix(do.call(expand.grid,list_minmax))
  # MERGE GRID WITH DATA FRAME
  dm=data.matrix(rbind(grids,dg))
  # PERFORM AA
  aa2 = fast_archetypal(dm,                        
                        irows = 1:nrow(grids),
                        diag_less = diag_less,
                        niter = niter,
                        use_seed = use_seed,
                        verbose = verbose)
  A1=aa2$A[(nrow(grids)+1):nrow(aa2$A),]
  # B1=aa2$B[,(nrow(grids)+1):ncol(aa2$B)]
  Y1=aa2$Y[(nrow(grids)+1):nrow(aa2$Y),]
  out=list("grid"=grids,"aa"=aa2,"A"=A1,"Y"=Y1)
  #
  class(out)="grid_archetypal"
  #
  return(out)
}
