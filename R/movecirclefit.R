#source("circleoverlap.R")

moveCircleFit <- function(A_all, B_all, AandB, scale = 1, it = 100){
    # given two circle venn chart data, return centers and rs
    r1 <- sqrt( A_all  / scale/ ( pi) )
    r2 <- sqrt( B_all  / scale/ ( pi) )
    c1 <- c(0, 0)

    # no overlap
    if (AandB == 0){
        d <- (r1 + r2) * 1.05
    }
    
    # full overlap
    if (AandB == A_all){
        d <- r2 - r1
    } 
    if (AandB == B_all){
        d <- r1 - r2
    } 

    # partial overlap
    if (AandB > 0 & AandB < min(c(A_all, B_all))){
        d0 <- r1 + r2 # max
        decrement <- d0/it
        d <- d0
        overlap <- 0
        # start with two circle apart, and move closer
        while (overlap < AandB){
            overlap <- circleOverlap(c1, r1, c(d, 0), r2)$AB
           # overlap
            d <- d- decrement
        #    d
        }
    }

    
    c2 <- c(d, 0)
    return(list("c1" = c1, "r1" = r1, "c2" = c2, "r2" = r2))
}