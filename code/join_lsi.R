join_lsi <- function(L1, L2) {
  
  M1 <- L1$get_matrices()
  labels1 <- M1$labels
  intents1 <- M1$intents
  
  M2 <- L2$get_matrices()
  labels2 <- M2$labels
  intents2 <- M2$intents
  
  if (is.null(labels1)) return(L2)
  if (is.null(labels2)) return(L1)
  
  # Result
  res <- LSI$new(L1$get_attributes(), empty_set = FALSE)
  
  # which labels are duplicated
  equals <- .equal_sets(labels1, labels2)
  
  idx_dup_rows <- which(rowSums(equals) > 0)
  idx_rows <- setdiff(seq(nrow(equals)), idx_dup_rows)
  idx_cols <- which(colSums(equals) == 0)
  
  # labels not duplicated
  if (length(idx_rows) > 0) {
    
    res$join(Matrix(labels1[, idx_rows],
                    sparse = TRUE),
             intents1[idx_rows])
    
  }
  
  if (length(idx_cols) > 0) {
    
    res$join(Matrix(labels2[, idx_cols],
                    sparse = TRUE),
             intents2[idx_cols])
    
  }
  # now, duplicated labels
  if (length(idx_dup_rows) > 0) {
    
    for (i in idx_dup_rows) {
      
      j <- which(equals[i, ] > 0)
      
      MIN <- minimal_elements(cbind(intents1[[i]],
                                    intents2[[j]]))
      
      if (ncol(MIN) == 0) browser()
      
      res$join(Matrix(labels1[, i], sparse = TRUE),
               MIN)
      
    }
    
  }
  
  return(res)
  
}
