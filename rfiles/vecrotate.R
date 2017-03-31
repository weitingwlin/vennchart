vecRotate <- function(v1 = 1, angle, v0 = c(0, 0)){
  
  # v1 can be the length of unit vector (r = 1) at angle = 0
  if (length(v1) == 1){
    v1 == c(v1, 0)
  }
  
  v <- v1 - v0
  r <- sqrt((v[1] ^ 2) + (v[2] ^ 2) )
  old <-vecAngle(v)
  theta2 <- old + angle
  v2 <- c( v0[1] + r * cos(theta2), v0[2] + r * sin(theta2))
 
  return(v2)
}

