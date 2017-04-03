
circleIntersect <- function( c1, r1, c2, r2){
    # notation and ref. from (athworld.wolfram.com/Circle-CircleIntersection.html)
    # assuming circle 1 is on the left, circle 2 on the right
    
    d <- sqrt(sum((c1 - c2) ^ 2)) # distance between center
    x <- (d^2 - r2^2 + r1^2) / (2*d)
    y <- sqrt(abs(r1^2 - x^2))
    a <- 2*y
    theta1 <- acos(abs(x)/r1) # between "d" line and "r1" line
    pie1 <-  theta1 * r1^2 
    tri1 <- a * abs(x) / 2
    
    theta2 <- acos(abs(d-x)/r2) # between "d" line and "r1" line, smaller piece
    pie2 <-  theta2 * r2^2 
    tri2 <- a * abs(d-x) / 2   
    A1 <- pi * r1^2
    A2 <- pi * r2^2
    
    # smaller lense
    len1 <- pie1 - tri1 # 
    len2 <- pie2 - tri2
    
    if (x <= d & (d-x) <= d) {
        overlap <- len1 + len2
    }
    
    if (x > d) {
        
        overlap <- len1 + (A2 - len2)
    }
    if((d-x) > d){
        overlap <- len2 + (A1 - len1)
    }
    
    ##
    Aonly <- A1 - overlap
    Bonly <- A2 - overlap
    
    out <- list("A" = A1, "B" = A2, 
                "A_only" = Aonly, "B_only" = Bonly, 
                "AB" = overlap)
    return(out)
}


