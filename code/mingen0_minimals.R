mingen0_minimals <- function(attributes, LHS, RHS) {

  phi <- LSI$new(attributes = attributes)

  # LHS <- imps$get_LHS_matrix()
  # RHS <- imps$get_RHS_matrix()
  #

  if (!is.null(LHS) && ncol(LHS) > 0) {

    minimals <- minimal_elements(LHS)

    for (i in seq(ncol(minimals))) {

      A <- Matrix(minimals[, i], sparse = TRUE)

      cl <- .compute_closure(A, LHS, RHS,
                             attributes = attributes,
                             reduce = TRUE)
      # S <- Set$new(attributes = attributes,
      #                    M = A)
      # cl <- imps$closure(S, reduce = TRUE)
      Aplus <- cl$closure
      print(Aplus)
      # imps_prime <- cl$implications
      # phi_sub <- mingen0(attributes, imps_prime)
      phi_sub <- mingen0(attributes,
                         cl$implications$lhs,
                         cl$implications$rhs)
      phi_sub$add(Aplus, A)
      # phi$print()
      # phi_sub$print()
      phi <- join_lsi(phi, phi_sub)
      phi$print()

    }

  }

  return(phi)

}
