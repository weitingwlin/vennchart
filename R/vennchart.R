#source("plotvenn.R")
#' @export

vennChart <- function(A_all , B_all , C_all = NULL ,
                      AandB , AandC = NULL , BandC = NULL,
                      plot = TRUE, 
                      colors = list(rgb(0,0,1,0.3), rgb(1,0, 0, 0.3), rgb(0, 1, 0, 0.3)),
                      frameadj = 1.5, line = NA, lwd = 2, lim = NULL,
                      texts = TRUE, names = c("A", "B", "C"),
                      Value = TRUE, cex = 1, textadj = c(0.5, 0.5),
                      connect = "\n", unit = " ", digits = 3){
  
      vennlogic <- vennCheckLogic(A_all, B_all, C_all, AandB, AandC, BandC)
    
    if (vennlogic$isOK == TRUE) {
          
            s <- vennThreeCircle(A_all , B_all , C_all , AandB , AandC , BandC )
            p <- vennTextPosition(s, textadj)
    
            out <- list("circles" = s, "textspos" = p)
    
            if (is.null(lim)){
                lim <- getPlotLimit(s, adj = frameadj)
            } 
    
            if (plot == TRUE){
                plotVenn(s, color = colors,
                line  = line,lwd = lwd, frameadj = frameadj, lim = lim)
                if (texts == TRUE){
                    plotVennTexts(p, names = names,
                          Value = Value, cex = cex, connect = connect, unit = unit, digits = digits)
                } # end if (text
            }# end if (plot
    
            out$circles$limits <- lim
  
    } else {
            out <- vennlogic  
       
   
    } # end  if (vennlogic$isOK
    return(out)
}