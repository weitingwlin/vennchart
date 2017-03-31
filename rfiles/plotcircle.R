plotCircle <- function(c, r, color = rgb(0, 0.3, 1, 0.4), line = NULL, lwd = 1){

    theta <- seq(0, 2 *pi, 0.01)
    polygon(c[1] + sin(theta) * r, c[2] + cos(theta) * r, 
            col = color, border = line, lwd = lwd)
}