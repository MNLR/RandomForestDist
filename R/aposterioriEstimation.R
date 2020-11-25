aposterioriEstimation <- function(model, newdata, method){
  pr.leaves <- predictLeaves(model, newdata)

  if (method == "random.sample"){
    tbr <-
      lapply(pr.leaves, function(samp){
        sample(size = 1, x = samp)
      })
  } else {
    tbr <-
      lapply(pr.leaves, function(samp){
        fitdist(samp, "gamma", method)
      })
  }

  return(tbr)
}
