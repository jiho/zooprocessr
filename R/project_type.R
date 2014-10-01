#' Determine the type of project (zooscan or uvp)
#'
#' @param project path to the project directory
#'
#' @details
#' Extracts the first part of the name of the project which should be either Zooscan or uvp5.
#'
#' @return
#' A character string with the project type, all in lowercase
#'
#' @export
project_type <- function(project) {
  # remove the full path
  project <- basename(project)

  # extract the first bit of the name
  splitted <- str_split(project, "_")
  type <- tolower(splitted[[1]][1])

  return(type)
}

add_class <- function(project) {
  # add the project type as class
  class(project) <- c(project_type(project), class(project))
  return(project)
}