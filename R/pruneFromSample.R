
guessPrunningFunction <- function(split.function){
  distr <-
    switch(split.function,
           anova = {mse},
           bernoulliGammaLLMME = {binCrossEntropylogMae},
           binaryCrossEntropyGammaDeviation = {binCrossEntropylogMae},
           multiBinaryGammaEntropy = {binCrossEntropylogMae},
           binaryMultiEntropyCond = {binaryEntropyMulti},
           {stop("Prunning method for this split function not implemented")}
    )

  return(distr)
}


binaryEntropyMulti <- function(tr, x, obs){
  fakerf <- list(tr)
  if (tr$method == "binaryMultiEntropyCond")  attr(fakerf, "multiresponse") <- T

  pred <- randomForestPredict(fakerf, x, method = "marginals")

  auc <-
    sapply(1:ncol(obs), function(icol){
      unlist(
        performance(prediction(pred[, icol], obs[, icol]),"auc")@y.values
      )
    })

  return(mean(auc))
}



mse <- function(tr, x, obs){
  fakerf <- list(tr)
  attr(fakerf, "multiresponse") <- FALSE

  pred <- randomForestPredict(fakerf, x, bagging.function = mean)
  return(mean( (pred-obs)^2 ))
}



pruneFromSample <- function(tr, x, y,
                            oob.prunning.function = NULL,
                            plot.tree.sequence = FALSE){

  if (is.null(oob.prunning.function)){
    prunningFunction <- guessPrunningFunction(tr$method)
  }


  dev.best <- prunningFunction(tr, x, y)

  if (plot.tree.sequence) plot(tr)

  nodes <- sort(as.numeric(rownames(tr$frame)), decreasing = T)

  for( prune.idx in 1:length(nodes)){
    pruned <- snip.rpart(tr, toss = nodes[prune.idx])

    if (!isTRUE(all.equal(pruned$frame, tr$frame))){
      pr <- randomForestPredict(list(pruned), x)
      dev <- prunningFunction(pruned, x, y)
      if (dev > dev.best) {
        dev.best <- dev
        tr <- pruned
        if (plot.tree.sequence) plot(pruned)
      }
    }
  }

  return(tr)
}
