

getPlotLimit <- function(vennout, adj = 1.5){
  
  c1 <- vennout$c1
  r1 <- vennout$r1
  c2 <- vennout$c2
  r2 <- vennout$r2
  
  if ( ! "c3" %in% names(vennout)){
    c3 <- c1 # use this dummy so the non-existing circle 3 will not affect limit
    r3 <- r1
      
  } else {
    
    c3 <- vennout$c3
    r3 <- vennout$r3
  }
  

  
  xmax <- max(c1[1] + r1, c2[1] + r2, c3[1] + r3)
  xmin <- min(c1[1] - r1, c2[1] - r2, c3[1] - r3)
  ymax <- max(c1[2] + r1, c2[2] + r2, c3[2] + r3)
  ymin <- min(c1[2] - r1, c2[2] - r2, c3[2] - r3)
  
  size <- max( xmax - xmin, ymax - ymin) * adj 
  
  xlim <- (c(-0.5, 0.5) * size) +  (xmin + xmax)/2
  ylim <- (c(-0.5, 0.5) * size) +  (ymin + ymax)/2
  
  out <- list("xlim" = xlim, "ylim" = ylim)
  
  return(out)
  
}