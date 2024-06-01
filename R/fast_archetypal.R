fast_archetypal = function(df,                           
                           irows,
                           diag_less = 1e-2,
                           niter = 30 ,
                           verbose = TRUE,
                           data_tables = TRUE,
                           use_seed = NULL) {
  #
  # Set seed if it is given ..
  #
  if(!is.null(use_seed)){
    set.seed(use_seed)
  }
  # Set kappas
  kappas = length(irows)
  #
  Aupdate = function(A,
                     YYtBt,
                     BYYtBt,
                     muA,
                     SST,
                     SSE,
                     niter = 1,
                     muAup = 1.2,
                     muAdown = 0.5,
                     nAup = 0,
                     nAdown = 0,
                     SSE_A_conv = 1e-09,
                     verbose = verbose,
                     irows = irows,
                     diag_less = diag_less
                     ) {
    kappas = dim(A)[2]
    NN = dim(A)[1]
    ev = rbind(rep(1, kappas))
    #
    # Print header of run details if asked so ...
    #
    dheader_0 = sprintf(
      "| %6s | %12s | %12s | %15s |",
      "Iter",
      "SSE_i",
      "SSE_(i+1)",
      "(SSE_(i+1)-SSE_i)/SSE_i"
    )
    #
    dline_0 = sprintf(
      "|--------|--------------|--------------|-------------------------|"
    )
    #
	if(verbose){
	 cat(dline_0,"\n")
	 cat(dheader_0,"\n")
	 cat(dline_0,"\n")
	 }	 
    ###
    #
    fin_iters = 0
    diagtest = FALSE
    while (fin_iters < niter & !diagtest) {
      SSE_old = SSE
      gv = (A %*% BYYtBt - YYtBt) / (SST / NN)
      gv = gv - rowSums(A * gv) %*% ev
      stop = FALSE
      Aold = A
      while (!stop) {
        A = Aold - gv * muA
        A[A < 0] = 0
        A = A / rowSums(A)
        AtA = crossprod(A)
        SSE = SST - 2 * sum(A * YYtBt) + sum(AtA * BYYtBt)
        if (SSE <= SSE_old * (1 + SSE_A_conv)) {
          muA = muA * muAup
          nAup = nAup + 1
          fin_iters = fin_iters + 1
          stop = TRUE
        }
        else {
          muA = muA * muAdown
          nAdown = nAdown + 1
          fin_iters = fin_iters + 1
        }
        # Diag sum and test
        diagsum=sum(diag(A[irows,]))
        diagtest=(kappas - diagsum < kappas * diag_less)
        #
        # Print details if asked so ...
        #
        if(verbose)
        {
          #
          cat(
            sprintf(
              " %7.0f | %12.6e | %12.6e | %15.6f  \n",
              fin_iters,
              SSE_old,
              SSE,
              abs(SSE_old - SSE) / SSE
            )
          )
          #
        }
        #
      }
    }
    #
    ### Define convergence:
    #
    converges = ifelse(fin_iters<niter,TRUE,FALSE)
    #
    ### Return:
    #
    return(list(
      A = A,
      SSE = SSE,
      muA = muA,
      AtA = AtA,
      nAup = nAup,
      nAdown = nAdown,
      diagsum = diagsum,
      converges =  converges
    ))
  }
  #
  ### Main process ...
  #
  T1 = Sys.time()
  #
  if (data_tables) {
    if (dim(df)[2] <= 3) {
      data_tables = df
    } else {
      data_tables = lapply(1:dim(df)[2], function(i, df) {
        x = as.numeric(df[, i])
        dt = data.frame(table(x))
        dt$x = as.numeric(as.character(dt$x))
        dt$rf = dt$Freq / sum(dt$Freq)
        colnames(dt) = c(colnames(df)[i], "Freq", "rf")
        head(dt)
        return(dt)
      }, df)
    }
  } else{
    data_tables = NA
  }
  # Make matrix data
  Y = as.matrix(df)
  # Make matrix archetypes
  BY = Y[irows, ]
  # Create matrix B
  B = matrix(0,
             nrow = kappas,
             ncol = nrow(Y),
             byrow = T)
  # Set ones to columns that will create given archetypal points
  B[, irows] = diag(kappas)
  # B%*%Y-archs_matrix # ZERO
  # Create a random stochastic matrix A
  NN = 1:nrow(Y)
  A = -log(matrix(runif(kappas * length(NN)), length(NN),   kappas, byrow = T)) #random A
  A = A / rowSums(A)
  # head(rowSums(A))
  # Create initials for A update function
  SST = sum(Y ^ 2)
  YYtBt = Y %*% t(BY)
  BYYtBt = BY %*% t(BY)
  AtA = crossprod(A)
  SSE = SST - 2 * sum(A * YYtBt) + sum(AtA * BYYtBt)
  SSEold = SSE
  # print(SSEold)
  t1 = Sys.time()
  # Update A matrix
  nAup_old = 5
  nAdown_old = 5
  ya1 = Aupdate(
    A,
    YYtBt,
    BYYtBt,
    muA = 1,
    SST,
    SSE,
    niter = niter,
    muAup = 1.2,
    muAdown = 0.5,
    nAup = nAup_old,
    nAdown = nAdown_old,
    SSE_A_conv = 1e-09,
    verbose = verbose,
    irows = irows,
    diag_less = diag_less
  )
  # Results:
  A = ya1$A
  SSE = ya1$SSE
  converges = ya1$converges
  diagsum = ya1$diagsum
  muA = ya1$muA
  nAup = ya1$nAup
  nAdown = ya1$nAdown
  #
  iters_done = sum(c(nAup - nAup_old, nAdown - nAdown_old))
  #
  t2 = Sys.time()
  t12 = round(as.numeric(t2 - t1, units = "secs"), digits = 2)
  if (verbose) {
    cat(paste0("Time for the ", iters_done, " A updates was ",
               t12, " secs"),
        "\n")
    cat(" ","\n")
  }
  varexplv = (SST - SSE) / SST
  #
  vsse = paste0("SSE_", iters_done, " / SSE_0")
  dheader = sprintf(
    "|%5s|%10s| %12s | %15s | %9s|%10s|%7s|",
    "Iter",
    "VarExpl",
    "SSE",
    vsse,
    "muA",
    "t(sec)",
    "Aup;dwn"
  )
  dline = sprintf(
    "|-----|----------|--------------|-----------------|----------|----------|-------|"
  )
  #
  #
  if (verbose) {
    cat(dline, "\n")
    cat(dheader, "\n")
    cat(dline, "\n")
    cat(
      sprintf(
        "%5.0f | %8.6f | %12.6e | %15.6f | %7.2e | %8.1f | %5s \n",
        iters_done,
        varexplv,
        SSE,
        SSE / SSEold,
        muA,
        t12,
        paste0(c(nAup - nAup_old, nAdown - nAdown_old),
               collapse = ";")
      )
    )
    cat(dline, "\n")
  }
  #
  T2 = Sys.time()
  T12 = round(as.numeric(T2 - T1, units = "secs"), digits = 2)
  rescall <- match.call()
  initialsolution = Y[irows, ]
  #
  if(verbose){
    message(paste0("The sum of diagonal elements for the sub-matrix of archetypal rows is ",round(diagsum,3)))
    message(paste0("The ideal sum would be ",kappas))
  }
  #
  res <-
    list(
      BY = BY,
      A = A,
      B = B,
      SSE = SSE,
      varexpl = varexplv,
      initialsolution = initialsolution,
      freqstable = NA,
      iterations = iters_done,
      time = T12,
      converges = converges,
      nAup = nAup,
      nAdown = nAdown,
      nBup = 0,
      nBdown = 0,
      run_results = NA,
      Y = Y,
      data_tables = data_tables,
      call = rescall
    )
  #
  class(res) <- "archetypal"
  return(res)
}

