#' Read the identifications of all objects in a project
#'
#' @inheritParams read_variables
#' @param last when TRUE, only output the prediction and the last identification, called "Valid"; when FALSE, output all identifications (but still duplicate the last one and label it "Valid")
#'
#' @family project data handling functions
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
