#' Read the metadata for all samples in a project
#'
#' @inheritParams read_variables
#'
#' @family project data handling functions
#' @export
#' @importFrom stringr str_c
#' @importFrom plyr ldply laply llply
read_sample_meta <- function(project, verbose=TRUE, ...) {
  # find pid files
  files <- list.files(str_c(project, "/PID_process/Pid_results/Pid_predicted"), pattern="pid$", full.names=TRUE)

  # determine wether to show a progress bar
  if ( verbose & length(files) > 5 ) {
    progress <- "text"
  } else {
    progress <- "none"
  }

  # read all files
  D <- ldply(files, function(f, ...) {
    # read pid file
    m <- read_pid(f, metadata=T, data=F, ...)
    # flatten the list
    # NB: this converts everything into characters !
    m <- unlist(m)
    # remove empty elements
    m <- na.omit(m)
    # convert it into a one line data.frame
    m <- data.frame(t(m), stringsAsFactors=FALSE)
    return(m)
  }, ..., .progress=progress)

  # detect numeric columns
  suppressWarnings(Dnum <- llply(D, as.numeric))
  numCols <- laply(Dnum, function(x) {! all(is.na(x))})
  D[,numCols] <- Dnum[numCols]

  # convert dates for R
  D$sample.date <- strptime(D$sample.date, format="%Y%m%d-%H%M", tz="UTC")

  return(D)
}

# Need a function like unlist but which outputs a list to preserve class and attributes of all elements

# x <- list(a=list(foo=1, bar=3, bob=4), b=23)
#
# unlistl <- function(x) {
#   while( any(lapply(x, class) == "list") ) {
#     for (xx in x) {
#       if ( is.list(xx) ) {
#         unlistl(xx)
#       } else {
#
#       }
#     }
#
#   }
# }
