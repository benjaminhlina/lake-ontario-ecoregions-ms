
# ---- convert degrees mins seconds to decimal decrees -----
ddm_dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split = " "))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + (y[2] / 60)
    # + (y[3] / 3600)
  })
  return(x)
}


