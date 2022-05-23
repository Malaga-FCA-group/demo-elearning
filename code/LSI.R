LSI <- R6::R6Class(
  
  classname = "LSI",
  
  public = list(
    
    initialize = function(
      attributes,
      empty_set = FALSE) {
      
      
      if (empty_set) {
        
        emptyset <- Matrix(0,
                           nrow = length(attributes),
                           ncol = 1,
                           sparse = TRUE)
        private$labels <- emptyset
        private$intents <- list(emptyset)
        
      }
      private$attributes <- attributes
      
    },
    
    add = function(label, intent) {
      
      if (inherits(label, "Set")) {
        
        label <- label$get_vector()
        
      }
      
      if (inherits(intent, "Set")) {
        
        intent <- intent$get_vector()
        
      }
      
      if (is.null(private$labels)) {
        
        private$labels <- label
        private$intents <- intent
        
      } else {
        
        private$labels <- .union(private$labels, label)
        private$intents <- lapply(private$intents,
                                  function(I) {
                                    
                                    .union(I, intent)
                                    
                                  }
        )
        
      }

    },
    
    join = function(labels, intents) {
      
      private$labels <- cbind(private$labels, labels)
      private$intents <- c(private$intents, intents)
      
    },
    
    get_attributes = function() {
      
      return(private$attributes)
      
    },
    
    get_matrices = function() {
      
      return(list(labels = private$labels,
                  intents = private$intents))
      
    },
    
    print = function() {
      
      n_labels <- ncol(private$labels)
      
      for (i in seq(n_labels)) {
        
        cat("< ")
        
        s <- Set$new(attributes = private$attributes,
                     M = private$labels[, i])
        s$print(FALSE)
        cat(", ")
        
        I <- private$intents[[i]]
        txts <- c()
        for (j in seq(ncol(I))) {
          
          txts <- c(txts, .set_to_string(I[, j], private$attributes))
          
        }
        
        txts <- stringr::str_flatten(txts, collapse = ", ")
        cat("{", txts, "}>\n")
        
      }
      
    },
    
    to_latex = function() {
      
      n_labels <- ncol(private$labels)
      
      for (i in seq(n_labels)) {
        
        cat("\\langle ")
        
        s <- Set$new(attributes = private$attributes,
                     M = private$labels[, i])
        s$to_latex()
        cat(": ")
        
        I <- private$intents[[i]]
        txts <- c()
        for (j in seq(ncol(I))) {
          
          txts <- c(txts, set_to_latex(I[, j], private$attributes))
          
        }
        
        txts <- stringr::str_flatten(txts, collapse = ", ")
        cat("{", txts, "}\\rangle\n")
        
      }
      
    },
    
    to_implications = function(context = NULL) {
      
      reps <- sapply(private$intents, ncol)
      ids <- rep(seq(ncol(private$labels)), reps)
      RHS <- private$labels[, ids]
      LHS <- do.call(cbind, private$intents)
      
      RHS <- set_difference(RHS@i, RHS@p, RHS@x,
                            LHS@i, LHS@p, LHS@x,
                            nrow(RHS))
      
      idx <- which(colSums(RHS) > 0)
      
      imp <- ImplicationSet$new(
        attributes = private$attributes,
        lhs = LHS[, idx],
        rhs = RHS[, idx],
        I = context
      )
      
      return(imp)
      
    }
    
  ),
  
  private = list(
    
    labels = NULL,
    intents = list(),
    attributes = NULL
    
  )
  
)
