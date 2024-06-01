#
closer_grid_archetypal <- function(x) UseMethod("closer_grid_archetypal")
#
closer_grid_archetypal=function(dg, 
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
  # FIND THE CLOSER TO THE GRID POINTS AS ARCHETYPES
  dm=as.matrix(rbind(grids,dg))
  ds=distances::distances(dm)
  gc()
  # ds=as.matrix(dist(dm))
  dsn=ds[1:nrow(grids),(nrow(grids)+1):ncol(ds)]
  minrows=apply(dsn,1,which.min)
  jrows=minrows
  # RUN FAST AA WITH THOSE ARCHETYPES
  aa22 = fast_archetypal(dg,                         
                         irows = jrows,
                         diag_less = diag_less,
                         niter = niter,
                         use_seed = use_seed,
                         verbose = verbose)
  # RETURN
  out=list("grid"=grids,"grid_rows"=jrows,"aa"=aa22)
  #
  class(out)="closer_grid_archetypal"
  #
  return(out)
}


