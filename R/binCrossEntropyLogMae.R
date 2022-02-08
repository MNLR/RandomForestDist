binCrossEntropylogMae <- function(y, mean, p){
  if (y == 0) mae <- 0
  else mae <- log(1 + abs(y - mean))

  py <- as.numeric(y > 0)
  if (p == 0){
    if (py == 0) ce <- 0
    else if (py == 1) ce <- -py*log(0.0001) ## dummy number for penalization, does not get much bigger due to log
  } else if (p == 1){
    if (py == 0) ce <- -(1-py)*log(0.0001)
    else if (py == 1) ce <- 0
  } else ce <- -py*log(p) - (1-py)*log(1-p)

  return(mae + ce)
}

