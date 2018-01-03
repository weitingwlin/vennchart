

vennCheckLogic <- function(A_all, B_all, C_all = NULL, 
                           AandB, AandC = NULL, BandC = NULL){
    out <- list("isOK" = TRUE)
    
    if (A_all <  AandB){
      out$isOK <- FALSE
      out$proble1 <- "A < (A and B)"
    }
    
    if (B_all <  AandB){
      out$isOK <- FALSE
      out$proble2 <- "B < (A and B)"
    }
    
    if (!is.null(C_all)){
    
        if (A_all <  AandC){
            out$isOK <- FALSE
            out$proble3 <- "A < (A and C)"
        }
    
        if (C_all <  AandC){
            out$isOK <- FALSE
            out$proble4 <- "C < (A and C)"
        }
    
        if (B_all <  BandC){
            out$isOK <- FALSE
            out$proble6 <- "B < (B and C)"
        }
    
        if (C_all <  BandC){
            out$isOK <- FALSE
            out$proble6 <- "C < (B and C)"
        }
    }
    
    return(out)
}