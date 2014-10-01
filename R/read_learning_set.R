#' Read the learning set of a project
#'
#' @param project path to the project directory
#'
#' @family project data handling functions
#' @export
#' @importFrom stringr str_c
read_learning_set_in_project <- function(project) {

  # list all learning set files
  learningSetFiles <- list.files(str_c(project, "/PID_process/Learning_set/"), pattern="pid$")
  if ( length(learningSetFiles) > 1 ) {
    # when there are several, ask which one to read
    choice <- menu(learningSetFiles, graphics=FALSE)
    learningSetFile <- learningSetFiles[choice]
  } else {
    learningSetFile <- learningSetFiles
  }
  
  # read the learning set file
  learn <- read.csv(str_c(project, "/PID_process/Learning_set/", learningSetFile), sep=";", skip=1, stringsAsFactors=FALSE)
  names(learn)[1] <- "Item"

  return(learn)
}


