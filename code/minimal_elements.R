minimal_elements <- function(S) {
  
  if (is.numeric(S)) {
    
    S <- Matrix(S, sparse = TRUE)
    
  }
  
  # browser()
  S <- remove_duplicated(S)
  
  # print(S)
  #
  
  
  subsets <- .subset(S)
  # subsets <- subsets & !t(subsets)
  # diag(subsets) <- TRUE
  
  idx <- which(colSums(subsets) == 1)
  
  return(remove_duplicated(Matrix(S[, idx], sparse = TRUE)))
  
}
