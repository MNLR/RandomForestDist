randomForestPredict <- function(model, newdata,
                                bagging.function = mean,
                                method = NULL,
                                parallel.plan = NULL,
                                workers = NULL,
                                ...){
  # ... optional arguments to bagging function
  # bagging function = NA -> return all trees' predictions
  # method = "leaves": Return all leaves' observed values
  # else argument to fitdistrplus::fitdist()
  if (model[[1]]$method == "class") prediction.type = "prob"
  else prediction.type <- "matrix"

  if (is.null(dim(newdata))){
    newdata <- matrix(newdata, nrow = length(newdata))
    newdata <- data.frame(newdata)
    names(newdata) <- "x"
  } else{
    newdata <- data.frame(newdata)
  }

  split.function <- model[[1]]$method

  if (is.null(method)){
    tbr <- simplify2array(lapply(model,
                                 FUN = function(md) predict(md,
                                                            newdata = newdata,
                                                            type = prediction.type))
    )
    if (!is.na(bagging.function)){
      tbr <- apply(X = tbr, MARGIN = c(1,2), FUN = bagging.function, ... = ...)
    }
  } else {
    if (method == "leaves") tbr <- predictLeaves(model, newdata)
    else{
      if (!is.null(parallel.plan)) {
        o.plan <- plan()
        plan(parallel.plan,
             workers = if (is.null(workers) || workers == 0) (availableCores()) else workers
        )
        on.exit(plan(o.plan))
      }
      tbr <- aposterioriEstimation(model = model,
                                   newdata = newdata,
                                   method = method,
                                   split.function = split.function)
    }
  }

  return(tbr)
}

