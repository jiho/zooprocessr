# Wether to show a progress bar in plyr functions
#
# @param pieces the pieces along which the plyr function works. Can be a list for l*ply or a vector (the splitting column) for d*ply
#
progress <- function(pieces=NULL) {
  # determine the number of pieces
  if (is.list(pieces)) {
    n <- length(pieces)
  } else {
    n <- length(unique(pieces))
  }
  # show a progress bar only in interactive sessions when reading more than 10 pieces
  if ( interactive() & n >= 10 ) {
    progress <- "text"
  } else {
    progress <- "none"
  }
  return(progress)
}