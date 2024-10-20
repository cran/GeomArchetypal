BLB_archetypal <- function(x) UseMethod("BLB_archetypal")
#
BLB_archetypal <- function (df = NULL, ss_size = NULL, bs_size = NULL,
          arches = NULL, use_seed = NULL, n = 20, r = 100, n_core = 1, 
          n_iter = 100, ci_sigma = 2.5757, n_tails = 10, diag_less = 0.01) 
{
 # Version 7
 #--------------------------------------------------------------------  
    aa_batch <- function(subs = NULL, reps_pb = NULL, n_core = NULL,
                       bs_size = NULL, arches = NULL, diag_less = NULL, 
                       n_iter = NULL) {
    reps <- list()
    for (rr in 1:reps_pb) reps[[rr]] <- subs[sample(1:nrow(subs), 
                                            size = bs_size, replace = TRUE), ]
    irows <- 1:nrow(arches)
    reps <- lapply(reps, function(x) {
      rbind(arches,x)
    })
    cl <- make_cluster(n_core)
    
    aa_lst <- parLapply(cl = cl, X = reps, fun = fast_archetypal, 
                        irows = irows, diag_less = diag_less, niter = n_iter, 
                        verbose = FALSE, data_tables = FALSE,
                        use_seed = as.integer(.Random.seed[1]))
    stop_cluster(cl)
    conv <- unlist(lapply(aa_lst, "[", "converges"))
    not_conv <- reps_pb - sum(conv)
    if(not_conv > 0) stop(paste(not_conv,"of",reps_pb,"did not converge, increase n_iter?"))
    # stop: any non-converged would make next calculations erroneous
    #
    compos <- lapply(aa_lst, "[[", "A")
    rm(aa_lst)
    compos <- lapply(compos, function(x) {
      x <- x[-irows,]
      x
    })
    # estimates = mean of row compositions
    compo_ests <- lapply(compos, function(x) {
      x <- apply(x, 2, mean, na.rm=TRUE)
      x
      })
    compo_ests <- do.call(rbind, compo_ests)
    colnames(compo_ests) <- paste0("AC",1:ncol(compo_ests))
    return(compo_ests)
  }
 #--------------------------------------------------------------------  
  rstart <- Sys.time()
  cat("blb_archetypal starts...\n")
  aa_var <- colnames(arches)
  batches <- r/n_core
  if (batches == round(batches)) 
    reps_pb <- as.integer(r/batches) else stop("r is not an integer multiple of n_core")
  probs <- c(pnorm(-ci_sigma), pnorm(ci_sigma))
  boots_needed <- ceiling(n_tails/{
    1 - probs[2]
  })
  if ({
    n * r
  } < boots_needed) warning(paste("generating", n * r, "bootstraps but", 
      boots_needed, "required for", ci_sigma, "sigma confidence intervals"),
      immediate. = TRUE)
  rm(boots_needed)
  if (!is.null(use_seed)) 
    set.seed(use_seed)
  n_boots <- n * r
  df <- df[, aa_var]
  compo_results <- list()
  aa_tests <- list()
  for (ss in 1:n) {
    subs <- df[sample(1:nrow(df),ss_size,replace = FALSE),]
    for (bb in 1:batches) {
      if(ss == 1 & bb == 1) start <- Sys.time()
      #
      batch <- aa_batch(subs = subs, reps_pb = reps_pb, n_core = n_core,
                        bs_size = bs_size, arches = arches, diag_less = diag_less, 
                        n_iter = n_iter)
      compo_results <- c(compo_results, list(batch))
      rm(batch)
      if (ss == 1 & bb == 1) {
        dt <- difftime(Sys.time(), start, units = "mins")
        dt <- round(1.01 * dt * n * batches,2)
          cat("projected run time is", dt, "minutes\n")
        }
    }
    rm(subs)
    cat(".")
  }
  cat("\n")
  # 
  stk_ests <- do.call(rbind, compo_results)
  rm(compo_results)
  ov_means <- apply(stk_ests, 2, mean)
  lower <- apply(stk_ests, 2, quantile, probs = probs[1])
  upper <- apply(stk_ests, 2, quantile, probs = probs[2])  
  rt <- difftime(Sys.time(), rstart, units = "mins")
  cat("BLB_archetypal completed in", round(rt, 2), "minutes\n")
  #
  out <- list(arches = arches, pop_compos = ov_means, 
              lower_ci = lower, upper_ci = upper, ci_sigma = ci_sigma,
              N = nrow(df))
  #
  class(out) <- "BLB_archetypal"
  #
  return(out)
}
  