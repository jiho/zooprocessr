#' Read the descriptive variables of all objects in a project
#'
#' @param project path to the project directory
#' @param verbose when TRUE, show read progress (when there are many files to read)
#' @param ... passed to \code{\link{read_pid}}
#'
#' @family project data handling functions
#' @export
#' @importFrom stringr str_c
#' @importFrom plyr ldply
read_variables <- function(project, verbose=TRUE, ...) {
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
    p <- read_pid(f, data=T, ...)
    return(p)
  }, ..., .progress=progress)

  return(D)
}
