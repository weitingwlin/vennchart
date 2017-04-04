

plotVennTexts <- function(P, Atext = "A", Btext = "B", Ctext = "C", 
                          ABtext = " ", ACtext = " ", BCtext = " ",
                          Value = TRUE, cex = 1, connect = "\n", unit = " ",
                          digits = 3){
    
    if (Value == TRUE){
        At <- paste(Atext, connect, as.character(round(P$A, digits = digits)), unit)
        Bt <- paste(Btext, connect, as.character(round(P$B, digits = digits)), unit)
        Ct <- paste(Ctext, connect, as.character(round(P$C, digits = digits)), unit)
        
        ABt <-  paste(Atext, "&", Btext, connect, as.character(round(P$AB, digits = digits)), unit)
        ACt <-  paste(Atext, "&", Ctext, connect, as.character(round(P$AC, digits = digits)), unit)
        BCt <-  paste(Btext, "&", Ctext, connect, as.character(round(P$BC, digits = digits)), unit)
        
    } else {
        At <- Atext
        Bt <- Btext
        Ct <- Ctext
        ABt <- ABtext # won't print if no input
        ACt <- ACtext
        BCt <- BCtext
    }
    
    text(P$tpA[1], P$tpA[2], At, cex =cex, adj = c(0.5, 0.5))
    text(P$tpB[1], P$tpB[2], Bt, cex =cex, adj = c(0.5, 0.5))
    text(P$tpC[1], P$tpC[2], Ct, cex =cex, adj = c(0.5, 0.5))
    
    # only print if there is overlap
    if ( "tpAB" %in% names(P)){
        text(P$tpAB[1], P$tpAB[2], ABt, cex =cex, adj = c(0.5, 0.5))
    }
    if ( "tpAC" %in% names(P)){
            text(P$tpAC[1], P$tpAC[2], ACt, cex =cex, adj = c(0.5, 0.5))
    }
    if ( "tpBC" %in% names(P)){
           text(P$tpBC[1], P$tpBC[2], BCt, cex =cex, adj = c(0.5, 0.5))
    }
    
   
    
}




