#' Read the identifications of all objects in a project
#'
#' @inheritParams read_variables
#' @param last when TRUE, only output the prediction and the last validation, called "Valid"; when FALSE, output all validations (but still duplicate the last one and label it "Valid")
#'
#' @family project data handling functions
#' @export
#' @importFrom stringr str_c str_detect str_replace
#' @importFrom plyr ldply
read_ids <- function(project, last=TRUE, ...) {

  meta <- tryCatch(
    # get metadata table if it exists
    read_meta(project),
    # if it does not exist, reconstruct it from the headers in the dat1.txt files
    error=function(e) {
      warning(e$message, ". Falling back on extracting data from *_dat1.txt files", call.=FALSE)

      # list all dat1.txt files
      files <- list.files(str_c(project, "/PID_process/Pid_results/Dat1_validated"), pattern="dat1\\.txt$", full.names=TRUE)
      # extract the required metadata from them
      l <- llply(files, read_pid, metadata=TRUE, data=FALSE)
      m <- ldply(l, function(x) { c(fracnb=x$subsample$subpart, vol=x$sample$vol) })
      # add scanid
      scanid <- basename(files)
      scanid <- str_replace(scanid, fixed("_dat1.txt"), "")
      m$scanid <- scanid

      return(m)
    }
  )
  
  # read all files
  D <- adply(meta, 1, function(m, ...) {
    # construct file name
    f <- str_c(project, "/PID_process/Pid_results/Dat1_validated/", m$scanid, "_dat1.txt")

    if ( ! file.exists(f) ) {
      # warn if it does not exist and skip it
      warning(m$scanid, " is in the metadata table but has no corresponding *_dat1.txt file")
      p <- NULL
    } else {
      # read the data
      p <- read_pid(f, metadata=FALSE, data=TRUE, ...)

      # keep validation columns
      keep <- c("Item", "Label")
      if ( last ) {
        keep <- c(keep, "Pred", "Valid")
      } else {
        keep <- c(keep, "Pres", names(p)[str_detect(names(p), "pred_valid_Id")], "Valid")
      }
      p <- p[,keep]
    
      # compute concentrations
      p$concentration <- 1 / m$vol * m$fracnb
    }

    return(p)
  }, ..., .progress=progress(files), .expand=FALSE)
  # remove the index column
  D <- D[,-1]
  
  return(D)
}
