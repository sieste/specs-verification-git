Detrend <- function(x) {
  if (class(x) == "matrix" | class(x) == "data.frame") {
    xx <- rowMeans(x)
  } else {
    xx <- x
  }
  N <- length(xx)
  m <- mean(unlist(x))
  trnd <- fitted(lm(xx~(t=1:N)))
  return(x - trnd + m)
}

