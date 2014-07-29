#' Select all variables useful for prediction and compute derived variables
#'
#' @param x data.frame from which to select/compute the variables, usually read with read_pid
#' @param var.removed names of variables to remove
#' 
#' @export
#' @importFrom stringr str_detect str_c
#' @importFrom plyr laply
get_prediction_variables <- function(x, var.removed=c("X", "Y", "XM", "YM", "BX", "XMg5", "YMg5", "Width", "Height", "Angle", "XStart", "YStart", "Compentropy", "Compmean", "Compslope", "CompM1", "CompM2", "CompM2")) {

  # select only variables useful for prediction
  # = remove variables provided in the arguments (usually variables denoting position or so, which are not meaningful for prediction) + identification variables
  x <- x[, ! names(x) %in% c(var.removed, "X.Item", "Item", "Tag", "Ident", "Status", "Pred", "Valid", "Label")]
  x <- x[, ! str_detect(names(x), "Valid")]

  # check that all are numeric
  classes <- laply(x, class)
  nonNum <- which(! classes %in% c("numeric", "integer"))
  if (length(nonNum) != 0) {
    warning("Column(s) ", str_c(names(x)[nonNum], collapse=", "), " is/are not numeric and will cause trouble during the prediction")
  }

  # add derived variables (see PkID for all possibilities)
  # TODO not all possibles derived variables are computed here, code a better mechanism to specify them
  x$ESD <- 2 * sqrt(x$Area / pi)
  x$Elongation <- x$Major / x$Minor
  x$Range <- x$Max - x$Min
  x$MeanPos <- (x$Mean - x$Max) / (x$Max - x$Min)
  x$CV <- 100 * (x$StdDev / x$Mean)
  x$SR <- 100 * (x$StdDev / x$Range)
  x$PerimFeret <- x$Perim. / x$Feret
  x$PerimMaj <- x$Perim. / x$Major
  x$Circexc <- (4 * pi * x$Area_exc) / x$Perim.^2

  return(x)
}
