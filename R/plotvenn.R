
source("plotcircle.R")
source("getTriangle.R")
source("getplotlimit.R")

plotVenn <- function(venn, 
                     color = list(rgb(0,0,1,0.3), rgb(1,0, 0, 0.3), rgb(0, 1, 0, 0.3)),
                    line  = NA,lwd = 2, frameadj = 1.5
                     ){
  
  lim <- getPlotLimit(venn, adj = frameadj)
  
  p <- plot(1, type = "n", xlim = lim$xlim, ylim = lim$ylim,  
       xaxt = 'n', ann = FALSE, yaxt ='n')
  
  
  plotCircle(venn$c1, venn$r1, color[[1]], line = line, lwd = lwd)
  plotCircle(venn$c2, venn$r2, color[[2]], line = line, lwd = lwd)
  plotCircle(venn$c3, venn$r3, color[[3]], line = line, lwd = lwd)
 

  
}