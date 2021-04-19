
estimateBC3 <- function(leaves.info){

  return(
    t(sapply(leaves.info, function(leaf.info){
      tbr <- .Call(`_RandomForestDist_estimateGammaBC3`, leaf.info)
      return(tbr)
    }))
  )

}

