Detrend <- function(x, demean=TRUE) {
  if (class(x) == "matrix" | class(x) == "data.frame") {
    xx <- rowMeans(x)
  } else {
    xx <- x
  }
  N <- length(xx)
  trnd <- fitted(lm(xx~(t=1:N)))
  # if demean is false, add mean back to x-trend
  m <- ifelse(demean, 0, mean(unlist(x)))
  return(x - trnd + m)
}

