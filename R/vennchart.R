#source("plotvenn.R")
#' @export

vennChart <- function(A_all , B_all , C_all ,
                      AandB , AandC , BandC,
                      plot = TRUE, 
                      colors = list(rgb(0,0,1,0.3), rgb(1,0, 0, 0.3), rgb(0, 1, 0, 0.3)),
                      frameadj = 1.5, line  = NA, lwd = 2, 
                      texts = TRUE, names = c("A", "B", "C"),
                      Value = TRUE, cex = 1, textadj = c(0.5, 0.5),
                      connect = "\n", unit = " ", digits = 3){
    
    s <- vennThreeCircle(A_all , B_all , C_all , AandB , AandC , BandC )
    p <- vennTextPosition(s, textadj)
    
    out <- list("circles" = s, "textspos" = p)
    
    if (plot == TRUE){
        plotVenn(s, color = colors,
         line  = line,lwd = lwd, frameadj = frameadj)
        if (texts == TRUE){
            plotVennTexts(p, Atext = names[1], Btext = names[2], Ctext = names[3],
                          Value = Value, cex = cex, connect = connect, unit = unit, digits = digits)
        }
    }
    return(out)
}