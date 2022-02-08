
estimateLeavesBC3 <- function(leaves.info, noninformative.beta = 1){

  return(
    do.call(rbind,
            lapply(leaves.info, function(leaf.info){
              tbr <- .Call(`_RandomForestDist_estimateGammaBC3`,
                           leaf.info, noninformative.beta)
              return(tbr)
            })
            )
  )

}

