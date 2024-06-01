points_inside_convex_hull=function(df, dp) {
  # function which computes the percentage of points from df 
  # that lie inside the convex hull which is created by dp
  # df = the data frame of all the n available d-dimensional points: n x d
  # dp = a data frame with rows the k points that define a convex hull: k x d
  # number of points must at least equal to the dimension plus one: k >= d+1
  d=ncol(df)
  k=nrow(dp)
  if(k<d+1){
    stop(paste0("The number of points that create the convex hull must be at least equal to the dimension plus one: k >= ",d+1))
  }
  if(ncol(dp)!=d){
    stop(paste0("The vector points that form the convex hull must have the same dimension with the data points vector space: d=",d))
  }
  #
  chp = convhulln(as.matrix(dp), 'Fx')
  pins_all = inhulln(chp, as.matrix(df))
  100 * sum(pins_all) / length(pins_all)
}