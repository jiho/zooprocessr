% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confusion.R
\name{confusion_stats}
\alias{confusion_stats}
\title{Compute confusion statistics (recall, precision, etc.)}
\usage{
confusion_stats(x, sort.by = NULL)
}
\arguments{
\item{x}{confusion matrix (table class with predictions as line and validated, true classes as columns), as returned by \code{confusion_matrix()}}

\item{sort.by}{column to sort the result by (usually "recall", "precision", or "F1"); can be abbreviated}
}
\description{
Compute confusion statistics (recall, precision, etc.)
}
\examples{
n <- 50
pred <- sample(letters[1:5], size=n, replace=TRUE)
valid <- sample(letters[2:8], size=n, replace=TRUE)
x <- confusion_matrix(pred, valid)
confusion_stats(x)

}
\seealso{
Other confusion statistics functions: \code{\link{autoplot.table}},
  \code{\link{blank_diagonal}},
  \code{\link{confusion_matrix}}
}

