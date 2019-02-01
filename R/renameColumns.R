renameColumns <- function(integers, Sigma) {

  if(length(integers) == 0) {
    return(integers)
  }

  S <- Sigma
  S[lower.tri(S)] <- 0
  tab <- as.data.frame(as.table(S))
  ss <- subset(tab, Freq > 1)
  ss <- ss[!duplicated(ss$Freq), ]

  ss$name <- paste0(ss$Var1, ss$Var2)
  rownames(ss) <- ss$Freq

  int <- as.integer(integers)
  out <- rep("", length(int))
  for(i in 1:length(int)) {
    if(int[i] <=1 ) out[i] <- int[i] else {
      decomp <- numbers::primeFactors(int[i])
      convert <- ss[as.character(decomp), "name"]
      newname <- paste(convert, collapse = "*")
      out[i] <- newname
    }
  }
  return(out)

}
