#' Read the identifications of all objects in a project
#'
#' @inheritParams read_variables
#' @param last when TRUE, only output the prediction and the last validation, called "Valid"; when FALSE, output all validations (but still duplicate the last one and label it "Valid")
#'
#' @family project data handling functions
#' @export
#' @importFrom stringr str_c str_detect str_replace
#' @importFrom plyr ddply
read_ids <- function(project, last=TRUE, ...) {

  meta <- read_meta(project)
  
  # read all files
  D <- ddply(meta, ~id, function(m, ...) {
    # construct file name
    f <- str_c(project, "/PID_process/Pid_results/Dat1_validated/", m$id, "_dat1.txt")

    if ( ! file.exists(f) ) {
      # warn if it does not exist and skip it
      warning(m$id, " is in the metadata table but has no corresponding *_dat1.txt file")
      p <- NULL
    } else {
      # read the data
      p <- read_pid(f, metadata=FALSE, data=TRUE, ...)

      # compute concentrations
      p$concentration <- 1 / m$vol * m$fracnb

      # keep individual validation columns or not
      keep <- c("Item", "Label", "Pred", "Valid", "concentration")
      if ( ! last ) {
        keep <- c(keep, names(p)[str_detect(names(p), "pred_valid_Id")])
      }
      p <- p[,keep]
    
    }

    return(p)
  }, ..., .progress=progress(meta$id))
  
  return(D)
}
