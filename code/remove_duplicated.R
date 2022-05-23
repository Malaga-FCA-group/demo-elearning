remove_duplicated <- function(M) {

  equals <- fcaR:::.equal_sets(M)
  idx <- which(Matrix::colSums(equals) > 1)

  mark_to_remove <- rep(FALSE, ncol(M))

  if (length(idx) > 0) {

    for (i in idx) {

      j <- which(equals[, i] > 0)

      mark_to_remove[j[j > i]] <- TRUE

    }

  }

  return(Matrix::Matrix(M[, which(!mark_to_remove)], sparse = TRUE))

}
