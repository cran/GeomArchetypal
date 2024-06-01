BLB_archetypal <- function(x) UseMethod("BLB_archetypal")
#
BLB_archetypal <- function(df = NULL, 
                           group_var = NULL,
                           aa_var = NULL,
                           use_seed = NULL,
                           b = 0.6, n = 20, r = 100,
                           n_core = 1, n_iter = 30,
                           ci_sigma = 2,
                           n_tails = 10,
                           max_cor = 0.3,
                           verbose = TRUE,
                           diag_less = 1e-2)
{
  mx_cor <- function(x) {
    var_cor <- cor(x)
    var_cor <- abs(var_cor[lower.tri(var_cor)])
    return(max(var_cor))
  }
  rstart <- Sys.time()
  batches <- r/n_core
  if (batches == round(batches)) 
    reps_pb <- as.integer(r/batches)
  else stop("r is not an integer multiple of n_core")
  ss_size <- nrow(df)^b
  n_grps <- length(table(df[, group_var]))
  ss_size <- ss_size/n_grps
  ss_size <- as.integer(ceiling(ss_size))
  test <- as.vector(table(df[, group_var]))
  test2 <- length(test[which(test < ss_size)])
  if (test2 > 0) 
    stop(paste("check your grouping", test2, "of", n_grps, 
               "groups contain too few cases for subsampling"))
  rm(test, test2)
  probs <- c(pnorm(-ci_sigma), pnorm(ci_sigma))
  boots_needed <- ceiling(n_tails/{
    1 - probs[2]
  })
  if ({
    n * r
  } < boots_needed) 
    if(verbose){cat("warning: generating", n * r, "bootstraps but", boots_needed, 
        "required for", ci_sigma, "sigma confidence intervals\n")}
  rm(boots_needed)
  otest <- mx_cor(x = df[, aa_var])
  if (otest > max_cor) {
    if(verbose){cat("warning: check orthogonality of variables for archetypal analysis\n")
    cat("the absolute values of at least one correlation are greater than", 
        max_cor, "\n")}
  }
  rm(otest)
  if (!is.null(use_seed)) set.seed(use_seed)
  n_boots <- n * r
  if(verbose) 
  {
  cat("drawing subsamples of size", ss_size, "without replacement from each of", 
      n_grps, "groups\nfor a total subsample size of", n_grps * 
        ss_size, "\n", n, "subsamples and", r, "bootstraps per subsample = ", 
      n_boots, "bootstraps in total\nprocessed in batches of", reps_pb, 
      "(= n_core)\n")
  }
  #----------------------------------------------------------------------------
  min_var <- apply(df[, aa_var], 2, min, na.rm = TRUE)
  max_var <- apply(df[, aa_var], 2, max, na.rm = TRUE)
  temp <- as.data.frame.matrix(rbind(min_var, max_var))
  colnames(temp) <- aa_var
  list_minmax <- apply(temp, 2, as.list)
  rm(temp)
  arches <- data.matrix(do.call(expand.grid, list_minmax))
  arches <- as.data.frame(arches)
  vn <- c(group_var, aa_var)
  df <- df[, vn]
  compo_results <- list()
  aa_tests <- list()
  for (ss in 1:n) {
    if(verbose) cat("subsample",ss,"of",n,"\n")
    subs <- df %>% group_by(!!!syms(group_var)) %>% slice_sample(n = ss_size, 
                                                                 replace = FALSE)
    for (bb in 1:batches) {
      if(verbose) cat("batch",bb,"\n")
      reps <- list()
      for (rr in 1:reps_pb) reps[[rr]] <- subs[sample(1:nrow(subs), 
                                                      size = nrow(df), replace = TRUE), ]
      start <- Sys.time()
      reps_grp <- lapply(reps, "[", group_var)
      reps_aa <- lapply(reps, "[", aa_var)
      rm(reps)
      st_row <- nrow(reps_aa[[1]]) + 1
      en_row <- st_row + nrow(arches) - 1
      irows <- st_row:en_row
      reps_aa <- lapply(reps_aa, function(x) {
        rbind(x, arches)
      })
      cl <- make_cluster(n_core)
      aa_lst <- parLapply(cl = cl, X = reps_aa, fun = fast_archetypal, 
                          irows = irows, 
                          diag_less = diag_less,
                          niter = n_iter, 
                          verbose = FALSE, 
                          data_tables = FALSE)
      stop_cluster(cl)
      otests <- unlist(lapply(reps_aa, mx_cor))
      rm(reps_aa)
      converges <- unlist(lapply(aa_lst, "[", "converges"))
      varexpl <- unlist(lapply(aa_lst, "[", "varexpl"))
      prefix <- paste0("s", ss, "b", bb, "r")
      rnames <- paste0(prefix, 1:reps_pb)
      rm(prefix)
      temp <- data.frame(run = rnames, converges = converges, 
                         varexpl = varexpl, max_abs_cors = otests)
      temp$ortho <- ifelse(temp$max_abs_cors > max_cor, 
                           FALSE, TRUE)
      aa_tests <- c(aa_tests, list(temp))
      rm(temp, otests)
      compos <- lapply(aa_lst, "[[", "A")
      rm(aa_lst)
      compos <- lapply(compos, function(x) {
        x <- x[1:{
          st_row - 1
        }, ]
        colnames(x) <- paste0("A", 1:ncol(x))
        x
      })
      compos <- mapply(cbind, reps_grp, compos, SIMPLIFY = FALSE)
      rm(st_row, en_row, irows)
      rm(reps_grp)
      est_means <- function(one_rsp = NULL, group_var = NULL, 
                            cmp_col = NULL) {
        cmeans <- aggregate(x = one_rsp[, cmp_col], by = list(grp = unlist(one_rsp[, 
                                                                                   group_var])), FUN = mean, na.rm = TRUE)
        colnames(cmeans) <- c(group_var, paste0(cmp_col, 
                                                "e"))
        cmeans <- cmeans[order(cmeans[, group_var]), 
        ]
        return(cmeans)
      }
      compo_ests <- lapply(compos, est_means, group_var = group_var, 
                           cmp_col = paste0("A", 1:nrow(arches)))
      names(compo_ests) <- rnames
      compo_results <- c(compo_results, compo_ests)
      rm(compos, compo_ests)
      dt <- difftime(Sys.time(), start, units = "mins")
      dt <- round(1.05 * dt * n * batches)
      if (ss == 1 & bb == 1 & dt > 1) 
        if(verbose){cat(" ====================================================================\n", 
            " based on the first batch, the estimated total run time is", 
            dt, "minutes\n", 
            "===================================================================\n")}
    }
    rm(subs)
  }
  aa_tests <- do.call(rbind, aa_tests)
  check <- nrow(aa_tests) - sum(aa_tests$ortho)
  if (check > 0) 
    if(verbose){cat("warning:", check, "of", n_boots, "bootstraps not orthogonal with |r| >", 
        max_cor, "\n")}
  rm(check)
  check <- nrow(aa_tests) - sum(aa_tests$converges)
  if (check > 0) 
    if(verbose){cat("warning:", check, "runs did not converge, increase number of iterations?\n")}
  rm(check)
  pop_ests <- lapply(compo_results, "[", paste0("A", 1:nrow(arches), 
                                                "e"))
  groups <- lapply(compo_results, "[", group_var)
  groups <- groups[[1]]
  stk_pop_ests <- abind(pop_ests, along = 3)
  ov_means <- apply(stk_pop_ests, c(1, 2), mean)
  colnames(ov_means) <- paste0("A", 1:nrow(arches), "pm")
  rsp_errs <- lapply(pop_ests, function(x) {
    x - ov_means
  })
  rm(pop_ests)
  stk_rsp_errs <- abind(rsp_errs, along = 3)
  lower_e <- apply(stk_rsp_errs, c(1, 2), quantile, probs = probs[1])
  upper_e <- apply(stk_rsp_errs, c(1, 2), quantile, probs = probs[2])
  lower <- ov_means + lower_e
  colnames(lower) <- paste0("A", 1:nrow(arches), "l")
  upper <- ov_means + upper_e
  colnames(upper) <- paste0("A", 1:nrow(arches), "u")
  ov_means <- cbind(groups, ov_means)
  lower <- cbind(groups, lower)
  upper <- cbind(groups, upper)
  out <- list(arches = arches, aa_tests = aa_tests, pop_compos = ov_means, 
              lower_ci = lower, upper_ci = upper, ci_sigma = ci_sigma)
  
  rt <- difftime(Sys.time(), rstart, units = "mins")
  if(verbose){cat("run completed in", round(rt,2), "minutes\n")}
  class(out) <- "BLB_archetypal"
  return(out)
} 