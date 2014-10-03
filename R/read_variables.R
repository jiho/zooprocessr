#' Read the descriptive variables of all objects in a project
#'
#' @param project path to the project directory
#' @param ... passed to \code{\link{read_pid}}
#'
#' @family project data handling functions
#' @export
#' @importFrom stringr str_c
#' @importFrom plyr ddply
read_variables <- function(project, ...) {

  meta <- read_meta(project)

  # read all files
  D <- ddply(meta, ~id, function(m, ...) {
    # construct file name
    f <- str_c(project, "/PID_process/Pid_results/Pid_predicted/", m$id, "_dat1.pid")

    if ( ! file.exists(f) ) {
      # warn if it does not exist and skip it
      warning(id, " is in the metadata table but has no corresponding *_dat1.pid file")
      p <- NULL
    } else {
      p <- read_pid(f, data=TRUE, ...)
    }
    return(p)
  }, ..., .progress=progress(meta$id))

  return(D)
}
