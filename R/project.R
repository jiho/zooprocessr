#' Read all .pid files in a project
#'
#' Walk the hierarchy of a zooprocess project and read the pid + dat1.txt files for all records in the project merging the information in both
#' \itemize{
#'  \item pid = image characteristics
#'  \item dat1 = identification
#' }
#'
#' @param project path to the project directory
#' @param ... passed to \code{\link{read_pid}}
#' @export
#' @importFrom stringr str_c str_replace fixed
#' @importFrom plyr ldply
read_pids_in_project <- function(project, ...) {

  # find pid and dat1 files in project
  # NB: one pid/dat1 per profile
  pidFiles <- list.files(str_c(project, "/PID_process/Pid_results"), pattern="pid$", full.names=TRUE)

  dat1Files <- str_replace(pidFiles, "pid$", "txt")
  dat1Files <- str_replace(dat1Files, fixed("/Pid_results/"), "/Pid_results/Dat1_validated/")

  # read all files
  D <- ldply(1:length(pidFiles), function(i, pid, dat, ...) {
    # read pid
    p <- read_pid(pid[i], ...)

    # look for dat1 and read identifications from it when present
    if (file.exists(dat[i])) {
      d <- read_pid(dat[i], ...)
      if (nrow(d) != nrow(p)) {
        stop("dat1.txt and dat1.pid do not have the same number of lines. Something is wrong")
      }
      p$Pred <- d$Pred
      p$Valid <- d$Valid
    } else {
      warning("No dat1.txt file for pid: ", basename(pid[i]), "\nIdentifications (prediction and validation) will be absent", call.=FALSE)
    }

    return(p)
  }, pid=pidFiles, dat=dat1Files, ...)

  return(D)
}

#' Read the learning set of a project
#'
#' Walk the hierarchy of a zooprocess project and read the learning set file
#'
#' @param project path to the project directory
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


