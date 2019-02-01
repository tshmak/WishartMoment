mergeTables <- function(..., remove0=TRUE) {

  o <- list(...)

  if(remove0) {
    Remove0 <- function(M) {
      MM <- M[,colnames(M) != "0", drop=FALSE]
      # class(MM) <- "CountTable"
      return(MM)
    }
  } else {
    Remove0 <- function(M) return(M)
  }

  if(length(o) == 1) {
    M <- Remove0(o[[1]])
  } else if(length(o) > 2) {
    M <- Remove0(o[[1]])
    for(i in 2:length(o)) {
      M <- mergeTables(M, o[[i]], remove0=remove0)
    }
  } else {
    A <- Remove0(o[[1]])
    B <- Remove0(o[[2]])
    stopifnot(rownames(A) == rownames(B))
    names <- unique(c(colnames(A), colnames(B)))
    M <- matrix(NA, nrow(A), length(names),dimnames = list(rownames(A), names))
    if(ncol(M) > 0) {
      for(i in 1:nrow(A)) {
        for(j in 1:length(names)) {
          M[i,names[j]] <- ifelse(names[j] %in% colnames(A),
                                  ifelse(names[j] %in% colnames(B),
                                         A[i,names[j]] + B[i,names[j]],
                                         A[i,names[j]]),
                                  B[i, names[j]])
        }
      }
    }
  }
  # class(M) <- "CountTable"
  return(M)

}
