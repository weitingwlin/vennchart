
source("vecangle.R")
source("vecrotate.R")



getTriangle <- function(c1 = c(0,0), r1, c2, r2){

  # c2 can take length c1 <--> c2 as input  
    if (c1 == c(0,0) && length(c2)==1){
      c2 <- c(c2, 0)
    }
  
  # rotate-move 
  # move so c1p is c(0, 0)
  
  c1p <- c(0, 0)
  c2temp <- c2 - c1
  thetaO <- vecAngle(c2temp) # "O" as in Origin
  
  # rotate so c1p-c2p is horisontal
  
  c2p <- vecRotate( c2temp , -1 * thetaO)
  
  ## get the numbers
  d <- sqrt(sum((c1p - c2p) ^ 2)) # distance between centers
  x <- (d^2 - r2^2 + r1^2) / (2*d) # mid point of circle intersection
                                   # x coordinate of c3p   
  y <- sqrt(abs(r1^2 - x^2)) # height
                             # y coordinate of c3p
  c3p <- c(x, y)
  c4p <- c(x, -1 * y)
  
  ## get thetas: easier in the transformed
  
  theta1 <- vecAngle(c3p)
  theta2 <- (2 * pi) - vecAngle(c2p- c3p)
  theta3 <- pi - theta1 - theta2
  
  # rotate- move backword
  c3 <- c1 + vecRotate(c3p, thetaO)
  c4 <- c1 + vecRotate(c4p, thetaO)
  
  ## report c3 that c1 -> c2 -> c3 goes counter-clockwise
  ## "the other" intersect is c4
  
  out <- list("x" = x, "y" = y, "c3" = c3, "c4" = c4, "d" = d,
              "theta0" = thetaO,
              "theta1" = theta1, "theta2" = theta2, "theta3" = theta3)
  
  return(out)
  
}
