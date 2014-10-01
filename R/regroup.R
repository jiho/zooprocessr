#' Relabel categories, possibly regrouping them
#'
#' @param x vector of categories (usually the "Valid" column of the data.frame produced by \code{\link{read_ids}})
#' @param labels a two columns data.frame or matrix with the old names in the first column and the new name in the second column; if some new names are equal, this effectively regroups the categories
#'
#' @seealso \code{\link{regroup_taxa}}
#' @examples
#' x <- c("append_fritill", "append_housing", "crust_copepods_calanus", "crust_copepods_euchaeta", "crust_larvae", "append_fritill")
#'
#' # change labels
#' labels <- data.frame(old=c("append_fritill", "append_housing"), new=c("larvacean_fritill", "larvacean_housing"))
#' relabel(x, labels=labels)
#' 
#' # combine some groups (see regroup_taxa for a more generic way of doing this)
#' grouping <- data.frame(old=c("append_fritill", "append_housing"), new="appendicularians")
#' print(grouping)
#' relabel(x, labels=grouping)
#' 
#' @export
relabel <- function(x, labels) {
  dims <- dim(labels)
  if ( is.null(dims) | dims[2] != 2 ) {
    stop("Need a two column data.frame or matrix as the `labels` argument")
  }
  
  # get where to find the new names of the element of x that need to be renamed
  idxOfMatch <- match(x, labels[,1])
  # TODO implement the possibility of using a list rather than a data.frame as the labels argument
  
  # detect which elements of x need to be renamed
  idxToChange <- which(!is.na(idxOfMatch))
  
  # rename them
  # NB: be careful with factors
  x[idxToChange] <- as.character(labels[idxOfMatch[idxToChange],2])
  
  # TODO check if we can do that by releveling factors, should be faster

  return(x)
}

#' Regroup some categories along a taxonomy
#'
#' Categories are usually labelled something like "crust_copepods_calanus", "crust_copepods_euchaeta", "crust_larvae", etc.; with different taxonomic levels separated by "_". This function regroups categories in higher level taxonomic units (such as "crust" here).
#'
#' @param x vector of categories (usually the "Valid" column of the data.frame produced by \code{\link{read_ids}})
#' @param level level at which to regroup (1 is what precedes the first separator, 2 is what precedes the second separator, etc.)
#' @param pattern when provided, only act on categories whose name matches the pattern (matching is done by \code{stringr::str_detect})
#' @param separator character used to separate taxonomic levels in category names; usually "_"
#' 
#' @examples
#' x <- c("append_fritill", "append_housing", "crust_copepods_calanus", "crust_copepods_euchaeta", "crust_larvae")
#' regroup_taxa(x)
#' regroup_taxa(x, level=2)
#' regroup_taxa(x, pattern="crust")
#' 
#' @export
#' @importFrom stringr str_detect str_split str_c
#' @importFrom plyr laply
regroup_taxa <- function(x, level=1, pattern=NULL, separator="_") {
  # get elements matching pattern, if any
  if ( ! is.null(pattern) ) {
    idx <- which(str_detect(x, pattern))
  } else {
    idx <- 1:length(x)
  }
  
  # split and recombine them up to the correct level
  bits <- str_split(x[idx], pattern=separator, n=level+1)
  xx <- laply(bits, function(s) {
    str_c(s[1:min(length(s), level)], collapse=separator)
  })
  
  # put back the regrouped categories
  x[idx] <- xx

  return(x)
}