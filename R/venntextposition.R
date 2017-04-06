# source("vecangle.R")
#source("vecrotate.R")
#source("gettriangle.R")

vennTextPosition <- function (vennout, textadj = c(0.5, 0.5)){
  
  # parsing the output of `vennThreeCircle`
    c1 <- vennout$c1
    r1 <- vennout$r1
    c2 <- vennout$c2
    r2 <- vennout$r2
    c3 <- vennout$c3
    r3 <- vennout$r3
    
  # transformation
    theta0 <- vecAngle(c2, c1) 
    c1p <- c(0,0)
    c2p <- vecRotate(c2 - c1, -1 * theta0)
    c3p <- vecRotate(c3 - c1, -1 * theta0)
    
  # position of the main compartment
    thetaB <- vecAngle(c3p -c2p)
    thetaA_BC <- vecAngle(c2p, c1p) 
    thetaB_AC <-  vecAngle(c2p - c3p) # perpendicular to line A-C
  
    textA <- r1 * vecRotate(c(1,0), thetaB- (pi)/2) * textadj[1]
    textB <- c2p + (r2 * vecRotate(c(1, 0), thetaB_AC))  * textadj[1]
    textC <- c3p + (c3p / sqrt(c3p[1]^2 + c3p[2]^2)) * r3 * textadj[1]
    
  # fromat the output list
    outtemp <- list("tpA" = textA, "tpB" = textB, "tpC" = textC)
    # the belows are optional

  # position of the partially overlapped area
    # if A and B partially overlap
    AB <- getTriangle(c1p, r1, c2p, r2)
    if (max( c(r1,r2) ) < (AB$d + min( c(r1, r2) ) ) & AB$d < (r1 + r2)){
      xp <- vecRotate( c(AB$x, 0), AB$theta0) + c1p # mid point
      outtemp$tpAB <- xp + vecRotate(c(0, max(c(r1, r2))), AB$theta0) * textadj[2]/1.1
    }
  
    # if A and C partially overlap
    AC <- getTriangle(c3p, r3, c1p, r1)
    if (max( c(r1, r3) ) < (AC$d + min( c(r1, r3) ) ) & AC$d < (r1 + r3)){
      xp <- vecRotate( c(AC$x, 0), AC$theta0) + c3p # mid point
      outtemp$tpAC <- xp + vecRotate(c(0, max(r1, r3)), AC$theta0) * textadj[2]/1.1
    }
    
    # if B and C partially overlap
    BC <- getTriangle(c2p, r2, c3p, r3)
    if (max( c(r2, r3) ) < (BC$d + min( c(r2, r3) ) ) & BC$d < (r2 + r3)){
      xp <- vecRotate( c(BC$x, 0), BC$theta0) + c2p # mid point
      outtemp$tpBC <- xp + vecRotate(c(0, max(r2, r3)), BC$theta0) * textadj[2]/1.1
    }
    
    
    # transform back
    out <- lapply(outtemp, function(x){vecRotate(x + c1, theta0)})
    out["A"] <- vennout$A
    out["B"] <- vennout$B
    out["C"] <- vennout$C
    out["AB"] <- vennout$AB
    out["AC"] <- vennout$AC
    out["BC"] <- vennout$BC
  
    return(out)
  
}