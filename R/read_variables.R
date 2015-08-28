#' Read the descriptive variables of all objects in a project
#'
#' @param project path to the project directory
#' @param ... passed to \code{\link{read_pid}}
#'
#' @family project data handling functions
#' @export
read_variables <- function(project, ...) {
  project <- project_class(project)
  read_variables_in_project(project=project, ...)
}

read_variables_in_project <- function(project, ...) {
  UseMethod("read_variables_in_project")
}

#' @importFrom stringr str_c
#' @importFrom plyr ddply
read_variables_in_project.zooscan <- function(project, ...) {

  meta <- read_meta(project)

  # read all files
  D <- ddply(meta, ~id, function(m, ...) {
    # construct file name
    f <- str_c(project, "/Zooscan_scan/_work/", m$id, "/", m$id ,"_dat1.pid")

    if ( ! file.exists(f) ) {
      # warn if it does not exist and skip it
      warning(m$id, " is in the metadata table but has no corresponding *_dat1.pid file")
      p <- NULL
    } else {
      p <- read_pid(f, data=TRUE, ...)
    }
    return(p)
  }, ..., .progress=progress(meta$id))

  return(D)
}
