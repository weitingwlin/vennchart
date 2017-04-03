
#source("circleintersect.R")

circleOverlap <- function(c1, r1, c2, r2){
    d <- sqrt(sum((c1 - c2) ^ 2)) # distance between center
    x <- (d^2 - r2^2 + r1^2) / (2 * d)
    A1 <-  pi * r1^2
    A2 <-  pi * r2^2
    
    if (d >= (r1 + r2)){
        # not overlap
        out <- list("A" = A1, "B" = A2, 
                    "A_only" = A1, "B_only" = A2, 
                    "AB" = 0)
    }
    
    if ( r2 < r1 & r1 > d + r2 ){
        # B inside A
        out <- list("A" = A1, "B" = A2, 
                    "A_only" = A1-A2, "B_only" = 0, 
                    "AB" = A2)
    }
    
    if ( r1 < r2 & r2 > d + r1 ){
        # A inside B
        out <- list("A" = A1, "B" = A2, 
                    "A_only" = 0, "B_only" = A2 - A1, 
                    "AB" = A1)
    }
    
    if( max( c(r1,r2) ) < (d + min( c(r1, r2) ) ) & d < (r1 + r2) ){
      # partially overlap
        out <- circleIntersect( c1, r1, c2, r2) 
    }
    
    return(out)
}

