

source("circleoverlap.R")
source("vecrotate.R")
source("vecAngle.R")

rotateCircleFit <- function(B_all, C_all, BandC, c2, r2, c3, r3  ,  it = 100){
  
  scale <- B_all/ r2^2/ pi # scale used earlyer
  areaBC <- BandC /  scale # expected area of BC overlap 
  

  # Partial overlap
  overlap <- circleOverlap(c2, r2, c3, r3)$AB
   
    try <- seq(0, -pi, length.out = it)
    tryout <- numeric(it) # empty
    temp3 <- matrix(0, it, 2) # empty
    
    for (i in 1:it){
      temp3[i,] <- vecRotate(c3, angle = try[i])
      tryout[i] <- circleOverlap(c2, r2, temp3[i,], r3)$AB
    }
    besti <- which.min(abs(tryout - BandC))
    c3 <- temp3[besti,]

  return(list("c3" = c3))
}