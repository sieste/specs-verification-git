.PreprocessEns <- function(x) {
  # check class of x (note that it can have multiple classes, hence any(...))
  cl.x <- class(x) 
  if(!(any(cl.x %in% c("matrix", "data.frame", "numeric")))) {
    stop("Can only handle ensembles of class matrix, data.frame, or numeric.")
  }
  # interpret vector-valued ensemble as one K-member ensemble, i.e. one row
  if (class(x) == "numeric") {
    x <- matrix(x, nrow=1)
  } else {
    x <- as.matrix(x)
  }
  return(x)
}
.PreprocessObs <- function(x) {
  # check class of x (note that can have multiple classes)
  cl.x <- class(x) 
  if(!(any(cl.x %in% c("matrix", "data.frame", "numeric")))) {
    stop("Can only handle observations of class matrix, data.frame, or numeric.")
  }
  # collapse into a vector
  x <- drop(as.matrix(x))
  return(x)
}
.RemoveNaMembers <- function(x) {
  na.cols <- apply(x, 2, function(z) all(is.na(z)))
  return(x[, !na.cols, drop=FALSE])
}



Preprocess <- function(ens=NA, ens.ref=NA, obs=NA, ...) {

  # logical vector indicating which of ens, ens.ref, obs was provided as an argument
  was.provided <- sapply(list(ens=ens, ens.ref=ens.ref, obs=obs), 
                  function(x) !(class(x) == "logical" & all(is.na(x))))
  if (!was.provided["ens"]) ens <- NA
  if (!was.provided["ens.ref"]) ens.ref <- NA
  if (!was.provided["obs"]) obs <- NA

  ### transform the ensembles to matrices or leave as NA ###
  if (was.provided["ens"]) {
    ens <- .PreprocessEns(ens)
  }
  if (was.provided["ens.ref"]) {
    ens.ref <- .PreprocessEns(ens.ref)
  }

  ### transform observation to vector or leave as NA ### 
  if (was.provided["obs"]) {
    obs <- .PreprocessObs(obs)
  }
  
  # if more than one argument is provided, check for equal length of time
  # dimension and stop if lenghts are different
  if (sum(was.provided) > 1) {
    # the `as.matrix` below is required to avoid errors if ens == NA
    N.vec <- c(ens=nrow(as.matrix(ens)), ens.ref=nrow(as.matrix(ens.ref)), obs=length(obs))
    if(length(unique(N.vec[was.provided])) != 1) {
      stop("Inputs do not have equal time dimensions.")
    }
  }

  # erase ensemble members that are all NA
  if (was.provided["ens"]) {
    ens <- .RemoveNaMembers(ens)
  }
  if (was.provided["ens.ref"]) {
    ens.ref <- .RemoveNaMembers(ens.ref)
  }

  # return
  ret <- list(ens=ens, ens.ref=ens.ref, obs=obs)
  return(ret)

}

