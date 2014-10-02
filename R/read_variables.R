#' Read the descriptive variables of all objects in a project
#'
#' @param project path to the project directory
#' @param ... passed to \code{\link{read_pid}}
#'
#' @family project data handling functions
#' @export
#' @importFrom stringr str_c
#' @importFrom plyr ldply
read_variables <- function(project, ...) {

  # get metadata table if it exists
  meta <- tryCatch(
    # get metadata table if it exists
    read_meta(project),

    error=function(e) {
      warning(e$message, ". Falling back on listing *_dat1.pid files", call.=FALSE)
      files <- list.files(str_c(project, "/PID_process/Pid_results/Pid_predicted"), pattern="pid$")

      # extract scanid
      scanid <- str_replace(scanid, fixed("_dat1.pid"), "")
      m <- data.frame(scanid)

      return(m)
    }
  )

  # read all files
  D <- ldply(meta$scanid, function(id, ...) {
    # construct file name
    f <- str_c(project, "/PID_process/Pid_results/Pid_predicted/", id, "_dat1.pid")

    if ( ! file.exists(f) ) {
      # warn if it does not exist and skip it
      warning(id, " is in the metadata table but has no corresponding *_dat1.pid file")
      p <- NULL
    } else {
      p <- read_pid(f, data=TRUE, ...)
    }
    return(p)
  }, ..., .progress=progress(files))

  return(D)
}
