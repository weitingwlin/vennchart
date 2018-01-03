# angle of a vector going from point v2 to point v1

vecAngle <- function(v1, v2 = c(0, 0)){
    v <- v1 - v2
    r1 <- sqrt((v[1] ^ 2) + (v[2] ^ 2) )
    theta1 <- acos(v[1]/r1)
    theta2 <- asin(v[2]/r1)
    
    if (v[1] >= 0){
        if (v[2] >= 0){
            # quadrant 1
            theta <- theta1
        }
        if (v[2] < 0){
            # quadrant 4
           theta <- (2 * pi) - theta1
        }
            
    } else { 
        if (v[2] >= 0){
            # quadrant 2
            theta <- theta1
        }
        if (v[2] < 0){
            # quadrant 3
            theta <- pi -theta2
        }
    }
    return(theta)
    
}




