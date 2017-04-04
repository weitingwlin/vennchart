
#source("plotcircle.R")
#source("getTriangle.R")
#source("getplotlimit.R")

plotVenn <- function(V, 
                     color = list(rgb(0,0,1,0.3), rgb(1,0, 0, 0.3), rgb(0, 1, 0, 0.3)),
                    line  = NA,lwd = 2, frameadj = 1.5,  lim = getPlotLimit(V, adj = frameadj)
                     ){
  
  p <- plot(1, type = "n", xlim = lim$xlim, ylim = lim$ylim,  
       xaxt = 'n', ann = FALSE, yaxt ='n')
  
  
  plotCircle(V$c1, V$r1, color[[1]], line = line, lwd = lwd)
  plotCircle(V$c2, V$r2, color[[2]], line = line, lwd = lwd)
  plotCircle(V$c3, V$r3, color[[3]], line = line, lwd = lwd)
 
}