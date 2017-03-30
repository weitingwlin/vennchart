vecRotate <- function(v1, angle, v0 = c(0, 0)){
  v <- v1 - v0
  r <- sqrt((v[1] ^ 2) + (v[2] ^ 2) )
  old <-vecAngle(v)
  theta2 <- old + angle
  v2 <- c( v0[1] + r * cos(theta2), v0[2] + r * sin(theta2))
  out <- list("r" = r, "theta2" = theta2, "v2" = v2)
  return(out)
}

