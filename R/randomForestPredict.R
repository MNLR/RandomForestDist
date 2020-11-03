randomForestPredict <- function(model, newdata, bagging.function = mean){

  prediction.type <- "matrix"

  return(
  apply(X = simplify2array(lapply(model,
                                  FUN = function(md) predict(md, newdata = data.frame(newdata), type = prediction.type))
                          ),
        MARGIN = c(1,2), FUN = bagging.function)
  )
}
