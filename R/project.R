#' Read the descriptive variables of all objects in a project
#'
#' @param project path to the project directory
#' @param verbose when TRUE, show progress if needed
#' @param ... passed to \code{\link{read_pid}}
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


#' Read the metadata for all samples in a project
#'
#' @param project path to the project directory
#' @param verbose when TRUE, show progress if needed
#' @param ... passed to \code{\link{read_pid}}
#' @export
#' @importFrom stringr str_c
#' @importFrom plyr ldply laply llply
read_meta <- function(project, verbose=TRUE, ...) {
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

#' Read the identifications of all objects in a project
#'
#' @param project path to the project directory
#' @param last when TRUE, only output the prediction and the last identification; when false, output all identifications (but still label the last one "Valid")
#' @param verbose when TRUE, show progress if needed
#' @param ... passed to \code{\link{read_pid}}
#' @export
#' @importFrom stringr str_c str_detect
#' @importFrom plyr ldply
read_ids <- function(project, last=TRUE, verbose=TRUE, ...) {
  # find dat1.txt files with the identifications
  files <- list.files(str_c(project, "/PID_process/Pid_results/Dat1_validated"), pattern="dat1\\.txt$", full.names=TRUE)

  # determine wether to show a progress bar
  if ( verbose & length(files) > 5 ) {
    progress <- "text"
  } else {
    progress <- "none"
  }


  # read all files
  D <- ldply(files, function(f, ...) {

    # read dat1 file
    p <- read_pid(f, metadata=T, data=T, ...)
    # extract meta data before modifying p
    meta <- attr(p, "meta")

    # keep validation columns
    keep <- c("Item", "Label")
    if ( last == TRUE ) {
      keep <- c(keep, "Pred", "Valid")
    } else {
      keep <- c(keep, "Pres", names(p)[str_detect(names(p), "pred_valid_Id")], "Valid")
    }
    p <- p[,keep]
    
    # compute concentrations
    sub <- meta$subsample$subpart
    volume <- meta$sample$vol

    p$concentration <- 1 / volume * sub

    return(p)
  }, ..., .progress=progress)
  
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


