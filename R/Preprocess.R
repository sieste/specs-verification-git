.GetClass <- function(x) {
  # check class of object x, NA is logical
  if (class(x) %in% c("numeric", "matrix", "data.frame", "logical")) {
    if (length(x) == 1 && is.na(x)) cl <- "na"
    if (is.numeric(x) && length(x) == 1) cl <- "scalar"
    if (is.numeric(x) && length(x) > 1) cl <- "vector"
    if (is.matrix(x)) cl <- "matrix"
    if (is.data.frame(x)) cl <- "data.frame"
  } else {
    cl <- "unknown"
  }
  return(cl)
}

.TransformEns <- function(ens, the.class) {
  if (the.class == "vector") { 
    ens <- matrix(ens, nrow=1)
  } else if (the.class == "na") {
    ens <- NA
  } else if (the.class == "scalar") {
    ens <- matrix(ens, nrow=1, ncol=1)
  } else if (the.class == "vector") {
    ens <- matrix(ens, nrow=1)
  } else if (the.class == "data.frame") {
    ens <- as.matrix(ens)
  } else if (the.class == "matrix") {
    # do nothing
  } else if (the.class == "unknown") {
    # do nothing
  } else {
    error(paste(c("Unknown class:", the.class)))
  }
  return(ens)
}

.TransformObs <- function(obs, the.class) {
  if (the.class == "vector") { 
    # do nothing
  } else if (the.class == "na") {
    obs <- NA
  } else if (the.class == "scalar") {
    # do nothing
  } else if (the.class == "vector") {
    # do nothing
  } else if (the.class == "data.frame") {
    obs <- c(as.matrix(data.frame))
  } else if (the.class == "matrix") {
    obs <- c(obs)
  } else if (the.class == "unknown") {
    # do nothing
  } else {
    error(paste(c("Unknown class:", the.class)))
  }
  return(obs)
}

Preprocess <- function(ens=NA, ens.ref=NA, obs=NA, ...) {

  # get class of input argument 
  classes <- sapply(list(ens=ens, ens.ref=ens.ref, obs=obs), .GetClass)
  # transform to their appropriate matrix / vector representation
  ens <- .TransformEns(ens, classes[["ens"]])
  ens.ref <- .TransformEns(ens.ref, classes[["ens.ref"]])
  obs <- .TransformObs(obs, classes[["obs"]])

  if (all(classes == "na")) {
    ret <- list(ens=NA, ens.ref=NA, obs=NA)
  } else {

    # check equal length of time dimension
    N <- c(ens=nrow(ens), ens.ref=nrow(ens.ref), obs=length(obs))
    N[classes != "na"]


}

