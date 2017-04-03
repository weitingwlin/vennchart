#source("rotateCirclefit.R")
#source("movecirclefit.R")
#source("vennchecklogic.R")
# source("plotcircle.R")

vennThreeCircle <- function(A_all, B_all, C_all, AandB, AandC, BandC, try = 100){
  
  vennlogic <- vennCheckLogic(A_all, B_all, C_all, AandB, AandC, BandC)
  
  if (vennlogic$isOK == TRUE) {
  
  # align circles A and B
  n <- moveCircleFit(A_all, B_all, AandB)

   c1 <- n$c1 # center of A
   r1 <- n$r1 # radius of A
   c2 <- n$c2
   r2 <- n$r2
  
  # align circles A and C
  p <- moveCircleFit(A_all, C_all, AandC)
  
  # c3temp <- p$c2 # will change later
   r3 <- p$r2
  
  # align circles B and C by rotating C around the center of A
  q <- rotateCircleFit( B_all, C_all, BandC, 
                        c2 = n$c2, r2 = n$r2, c3 = p$c2, r3 =  p$r2, 
                        it = try )
  c3 <- q$c3
  
  out <- list("c1" = c1, "r1" = r1,
              "c2" = c2, "r2" = r2,
              "c3" = c3, "r3" = r3,
              "A" = A_all, "B" = B_all, "C" = C_all,
              "AB" = AandB, "AC" = AandC, "BC" = BandC)
  } else {
    out <- vennlogic
  }
  
  return(out)
  
}