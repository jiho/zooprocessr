#' Compute a confusion matrix
#'
#' @param pred vector of predicted categories
#' @param valid vector of true categories, after validation
#'
#' @family confusion statistics functions
#'
#' @return An object of class \code{\link[base]{table}} with the predicted categories as rows and the true, validated categories as columns. The list of categories is made the same for each which always result in a square matrix 
#'
#' @examples
#' n <- 50
#' pred <- sample(letters[1:5], size=n, replace=TRUE)
#' valid <- sample(letters[2:8], size=n, replace=TRUE)
#' confusion_matrix(pred, valid)
#'
#' @export
confusion_matrix <- function(pred, valid) {

  # force pred and valid to have the same levels
  # => detect which are in each and compile a common list
  if (is.factor(pred)) {
    levels_pred <- as.character(levels(pred))
  } else {
    levels_pred <- unique(pred)
  }
  if (is.factor(valid)) {
    levels_valid <- as.character(levels(valid))
  } else {
    levels_valid <- unique(valid)
  }

  all_levels <- unique(c(levels_pred, levels_valid))
  pred <- factor(pred, levels=all_levels)
  valid <- factor(valid, levels=all_levels)

  # compute the confusion matrix
  t <- table(pred=pred, valid=valid)

  return(t)
}


#' Blank-out the diagonal of a confusion matrix
#'
#' @param x confusion matrix (as output by \code{\link{confusion_matrix}})
#'
#' @details
#' Blanking-out the diagonal allows to better focus on the prediction errors, particularly for plots.
#'
#' @family confusion statistics functions
#'
#' @return The confusion matrix with NAs on the diagonal.
#'
#' @examples
#' n <- 50
#' pred <- sample(letters[1:5], size=n, replace=TRUE)
#' valid <- sample(letters[2:8], size=n, replace=TRUE)
#' m <- confusion_matrix(pred, valid)
#' m
#' blank_diagonal(m)
#' \dontrun{
#' library("ggplot2")
#' autoplot(blank_diagonal(m))
#' }
#'
#' @export
blank_diagonal <- function(x) {
  n <- nrow(x)
  x[cbind(1:n, 1:n)] <- NA
  return(x)
}


#' Plot a contingency table (i.e. confusion matrix) as a heat map
#'
#' @param object contingency table, returned by function \code{table()} (or \code{\link{confusion_matrix}}) 
#' @param norm normalisation method: "none", by row ("rows", to represent precision), or by column ("columns", to represent recall); can be abbreviated
#' @param trans function used to transform the counts in the contingency table (such as \code{sqrt}, \code{log}, \code{log1p})
#'
#' @family confusion statistics functions
#'
#' @examples
#' n <- 50
#' pred <- sample(letters[1:5], size=n, replace=TRUE)
#' valid <- sample(letters[2:8], size=n, replace=TRUE)
#' x <- confusion_matrix(pred, valid)
#' # autoplot(x)
#'
#' @importFrom ggplot2 ggplot geom_tile aes_string coord_fixed labs theme element_text scale_x_discrete scale_y_discrete autoplot
#' @importFrom stringr str_c
#' @method autoplot table
#' @export
autoplot.table <- function(object, norm="none", trans=NULL) {

  x <- object
  
  # normalise data
  norm <- match.arg(norm, c("none", "rows", "columns"))
  variableName <- "Freq"
  if ( norm == "rows") {
    n <- rowSums(x, na.rm=TRUE)
    x <- x / n
    variableName <- "Freq\nby row"
  }
  if ( norm == "columns") {
    n <- colSums(x, na.rm=TRUE)
    x <- t(t(x) / n)
    variableName <- "Freq\nby column"
  }

  # make table into a data.frame
  x <- as.data.frame(x)

  # transform frequencies
  if ( ! is.null(trans) ) {
    if ( is.function(trans) ) {
      x$Freq <- trans(x$Freq)
      fun <- deparse(substitute(trans))
      variableName <- str_c(fun, "(", variableName, ")")
    } else {
      stop("Cannot find function ", fun)
    }
  }

  # make the plot
  p <- ggplot(x) +
        geom_tile(aes_string(x=names(x)[2], y=names(x)[1], fill="Freq")) +
        coord_fixed(1) + labs(fill=variableName) +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))
  return(p)
}


#' Compute confusion statistics (recall, precision, etc.)
#'
#' @param x confusion matrix (table class with predictions as line and validated, true classes as columns), as returned by \code{confusion_matrix()}
#' @param sort.by column to sort the result by (usually "recall", "precision", or "F1"); can be abbreviated
#'
#' @family confusion statistics functions
#'
#'
#' @examples
#' n <- 50
#' pred <- sample(letters[1:5], size=n, replace=TRUE)
#' valid <- sample(letters[2:8], size=n, replace=TRUE)
#' x <- confusion_matrix(pred, valid)
#' confusion_stats(x)
#'
#' @export
confusion_stats <- function(x, sort.by=NULL) {

  # reduce to common categories
  rowCats <- rownames(x)
  colCats <- colnames(x)
  if ( any( ! c(rowCats %in% colCats, colCats %in% rowCats) ) ) {
    warning("Confusion statistics can only be computed for categories present in both lines and columns of the confusion matrix. Reducing data to common categories")
    commonCats <- intersect(rowCats, colCats)
    x <- x[rowCats %in% commonCats, colCats %in% commonCats]
  }

  # check if matrix is square
  if ( nrow(x) != ncol(x)) {
    stop("The confusion matrix needs to be square")
  }

  # compute base stats
  (tp <- diag(x))              # true positive
  (fp <- rowSums(x) - tp)      # false positive
  (fn <- colSums(x) - tp)      # false negative
  (tn <- sum(x) - tp - fp -fn) # true negative

  # store it
  stats <- data.frame(class=names(tp), tp, fp, fn, tot=tp+fn)

  # define a formatter for percentages
  format_percent <- function(x, precision=1) {
    round(x * 100, precision)
  }

  # precision = quantify how "pure" the identified signals are
  stats$precision <- format_percent(tp / (tp + fp))

  # recall = capacity to get signals of a given origin
  stats$recall <- format_percent(tp / (tp + fn))

  # F1 score = combination of precision and recall
  # http://en.wikipedia.org/wiki/F1_score
  # the higher the better
  stats$F1 <- with(stats, (2 * precision * recall) / (precision + recall))
  stats$F1 <- round(stats$F1, 1)

  if ( ! is.null(sort.by) ) {
    sort.by <- match.arg(sort.by, names(stats))
    stats <- stats[order(stats[,sort.by]),]
  }

  # remove row.names
  rownames(stats) <- NULL

  return(stats)
}


#--------------------------------------------------------------------------
# Test data

#
# set.seed(123)
# n <- 5
# cat <- letters[1:n]
# pred <- cat[ceiling(runif(50)*n)]
# n <- 5
# cat <- letters[1:n]
# true <- cat[ceiling(runif(50)*n)]
# x <- confusion_matrix(pred, true)
#
# norm <- rowSums(x)
# rowSums(x/norm)
#
# norm <- colSums(x)
# colSums(t(t(x)/norm))
#
# autoplot(x)
# autoplot(x)
# autoplot(x, norm="row")
# autoplot(x, norm="col")
# autoplot(x, norm="col", trans=log)
# autoplot(x, norm="col", trans=sqrt)
# autoplot(x, trans=sqrt)
#
