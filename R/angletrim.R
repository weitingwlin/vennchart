
# trim an angle so it's between 0 ~ 2 pi

angleTrim <- function(theta){
  if (theta > (2 * pi)) {
    while (theta > (2 * pi)){
      theta <- theta - (2 * pi)
    }
  } else {
    while (theta < 2 * pi){
      theta <- theta + (2 * pi)
    }
  }
  return(theta)
}

