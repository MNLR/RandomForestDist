randomForestPredict <- function(model, newdata){
  colMeans(do.call(rbind, lapply(model, FUN = function(md) predict(md, newdata = newdata))))
}
