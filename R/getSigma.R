getSigma <- function(pairs, Diag1=TRUE, IdentityMatrix=FALSE) {
  #' pairs wil be in the form c("jk", "kr", ...), etc.
  #' e.g. pairs=c("jk", "kr") means we want E(Xj'Xk * Xk'Xr)
  #' with where each of Xj, Xk, Xr is a vector of length p
  #' Assume all X ~ N(0,1) although rows can be correlated,
  #' but columns independent

  letters <- strsplit(paste0(pairs, collapse = ""), split="")[[1]]
  letters <- unique(letters)
  nvars <- length(letters)

  if(IdentityMatrix) {
    Sigma <- diag(nvars)
  } else {
    primes <- numbers::Primes(200)
    if(!Diag1) {
      nparams <- nvars*(nvars+1)/2
      Sigma <- matrix(0, nvars, nvars)
      Sigma[lower.tri(Sigma, diag=TRUE)] <- primes[1:nparams]
      Sigma[upper.tri(Sigma)] <- t(Sigma)[upper.tri(t(Sigma))]
    } else {
      nparams <- nvars*(nvars-1)/2
      Sigma <- diag(nvars)
      Sigma[lower.tri(Sigma, diag=FALSE)] <- primes[1:nparams]
      Sigma[upper.tri(Sigma)] <- t(Sigma)[upper.tri(t(Sigma))]
    }
  }

  colnames(Sigma) <- rownames(Sigma) <- letters
  return(Sigma)

}
