
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



angleTrim <- function(theta){
    if (theta > (2 * pi)) {
        while (theta > (2 * pi)){
            theta <- theta - (2 * pi)
        }
    } else {
        while (theta < 2 * pi){
            theta <- theta + (2 * pi)
        }
    }
    return(theta)
}


