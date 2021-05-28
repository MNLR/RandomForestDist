oversample <- function(y, x, shuffle = TRUE, printm = F){
  original.length <- length(y)
  do.not.oversample <- FALSE
  ty <- table(as.numeric(!(y == 0)))
  offset <- ty["1"] - ty["0"]

  if (offset > 0){
    idx <- which(y == 0)
  } else if (offset < 0){
    idx <- which(y != 0)
  } else {
    do.not.oversample <- TRUE
  }

  idx <- sample(idx, replace = T, size = abs(offset))
  x <- rbind(x, x[idx, ])
  if (is.factor(y)){
    y <- as.factor(c(as.character(y), as.character(y[idx])))
  } else{
    y <- c(y, y[idx])
  }

  if (printm){
    print(paste0("Added ", abs(offset), " instances (+",
                 round(abs(offset)*100/original.length, 1 ), "%)"))
  }

  if (shuffle) idx <- sample(1:length(y), size = length(y), replace = F)
  y <- y[idx]
  x <- x[idx, ]
  return(list(y = y, x = x))
}
