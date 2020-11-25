randomForestPredict <- function(model, newdata,
                                bagging.function = mean,
                                method = NULL, ...){
  # ... optional arguments to bagging function

  prediction.type <- "matrix"

  if (is.null(method)){
    tbr <-
      apply(X = simplify2array(lapply(model,
                                    FUN = function(md) predict(md,
                                                               newdata = data.frame(newdata),
                                                               type = prediction.type))
      ), MARGIN = c(1,2), FUN = bagging.function, ... = ...)
  } else{
    tbr <- aposterioriEstimation(model = model,
                                 newdata = data.frame(newdata),
                                 method = method)
  }

  return(tbr)
}
