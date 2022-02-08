aposterioriBinaryMultiEntropiCond <- function(prl, method,
                                              non.informative.threshold = 0,
                                              non.informative.p = 0.5,
                                              marginal.imaginary.samplesize = 1
                                              ){
  if (method == "mme" || method == "aposteriori" || method == "marginal.averaging"){
    if ( (non.informative.threshold %% 1) != 0) stop("Provide an integer non.informative.threshold")

    if (method == "marginal.averaging"){
      use.bayes <- TRUE
      if (marginal.imaginary.samplesize <= 0) stop("Provide a valid marginal.imaginary.samplesize size")

      freqs <- do.call(rbind,
                     lapply(prl, function(appr){
                       .Call(`_RandomForestDist_computeBinaryMultiEntropyFrequencies`,
                             appr,
                             non.informative.threshold)
                     })
                     )

      aux.tbr <- do.call(rbind,
                     lapply(prl, function(appr){
                       .Call(`_RandomForestDist_computeBinaryMultiEntropyConditional`,
                             appr,
                             0)
                     })
                     )

    } else {
      use.bayes <- FALSE
    }

    use.marginals.noninformative <- FALSE

    tbr <- do.call(rbind,
                   lapply(prl, function(appr){
                     .Call(`_RandomForestDist_computeBinaryMultiEntropyConditional`,
                           appr,
                           non.informative.threshold)
                   })
            )

    n <- nrow(tbr)

    not.informed.values <- c()
    idx.noninformative <- which(tbr == -1)
    if (length(idx.noninformative) != 0){
      if (length(non.informative.p) == 1){
        if (non.informative.p == "marginals"){ use.marginals.noninformative <- TRUE }
        else non.informative.p <- rep(non.informative.p, ncol(tbr))
      }

      marg <- do.call(rbind, lapply(prl, colMeans))
      colidx <- 2^seq(0, to = (ncol(marg)-1), by = 1)

      for (ii in 2:length(colidx)){
        colidx[ii] <- colidx[ii]-1 +  colidx[ii]
      }


      if ( !use.marginals.noninformative && (length(non.informative.p) != ncol(tbr))){
        stop("Provide either a single non-informative value or a non-informative value for each variable")
      }
      for (ic in 1:ncol(tbr)){
        idx.noninformative <- which(tbr[,ic] == -1)
        not.informed.values <- c(not.informed.values, length(idx.noninformative))
        if (length(idx.noninformative) != 0){

          if (use.bayes){
            margidx <- min(which(ic <= colidx))

            if (!use.marginals.noninformative){
              for (iidx.ni in idx.noninformative) {
                tbr[iidx.ni, ic] <-
                  ( ( marginal.imaginary.samplesize*non.informative.p[margidx] )/(freqs[iidx.ni, ic] + marginal.imaginary.samplesize) +
                      ( (freqs[iidx.ni, ic]*aux.tbr[iidx.ni, ic])/(freqs[iidx.ni, ic] + marginal.imaginary.samplesize) )
                  )
              }
            } else {
              for (iidx.ni in idx.noninformative) {
                tbr[iidx.ni, ic] <-
  ( ( marginal.imaginary.samplesize*marg[iidx.ni, margidx] )/(freqs[iidx.ni, ic] + marginal.imaginary.samplesize) +
  ( (freqs[iidx.ni, ic]*aux.tbr[iidx.ni, ic])/(freqs[iidx.ni, ic] + marginal.imaginary.samplesize) )
                    )

              }
            }
            attr(tbr, "frequencies") <- freqs

          } else if (use.marginals.noninformative){
              margidx <- min(which(ic <= colidx))

              for (iidx.ni in idx.noninformative) {
                tbr[iidx.ni, ic] <- marg[iidx.ni, margidx]
              }

          } else {
            tbr[idx.noninformative, ic] <- non.informative.p[ic]
          }

        }
      }
    } else { not.informed.values <- rep(0, ncol(tbr)) }

    colnames(tbr) <- getBinaryMultiEntropyCondColnames(ncol(prl[[1]]))

    attr(tbr, "not.informed.values") <- not.informed.values

  } else if (method == "marginals"){
    tbr <- do.call(rbind, lapply(prl, colMeans))
    colnames(tbr) <- paste0("Y", 1:ncol(prl[[1]]))
  }

  return(tbr)
}
