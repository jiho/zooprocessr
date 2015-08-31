#' Check consistency of data in a project
#'
#' @param project path to the project directory
#' @param nmax maximum number of problematic items to print out (50 by default)
#'
#' @details
#' This function
#' \enumerate{
#'   \item Reads the metadata table of the project
#'   \item Lists and reads all \code{.pid} files in the \code{work} directory
#'   \item Lists and reads all \code{dat1.tx} files in the \code{PID_process/Pid_results/Dat1_validated} directory
#'   \item Lists all images in the \code{PID_process/Sorted_vignettes} directory
#' }
#' Then it
#' \enumerate{
#'   \item Compares the list of samples in the metadata, \code{.pid}, and \code{.txt} files
#'   \item Compares the list of objects in the \code{.pid}, \code{.txt} files, and present as images (i.e. vignettes)
#'   \item For images present both in the \code{.txt} files and as vignettes, checks that the location of a vignette (its classification) matches the last column in the \code{txt} file
#' }
#'
#' @return
#' Returns a list with all problematic items (sample or object names), invisibly.
#'
#' @export
check <- function(project, nmax=50) {
  project <- project_class(project)
  check_project(project=project, nmax)
}

check_project <- function(project, nmax) {
  UseMethod("check_project")
}

#' @importFrom stringr str_c str_replace str_split
#' @importFrom plyr ldply
check_project.zooscan <- function(project, nmax) {

  # Helper function
  alert <- function(x, message, nmax) {
    # x is the vector of problematic items
    # if it contains something, print a message and the list of problems, limited to nmax
    n <- length(x)
    if ( n > 0) {
      message(message, " (",n ," elements)")
      if ( n > nmax ) {
        x <- x[1:nmax]
      }
      x <- str_c("  ", x, collapse="\n")
      if ( n > nmax) {
        x <- str_c(x, "\n  and ", (n - nmax), " others...")
      }
      message(x)
    }
  }


  message("CHECK SAMPLES")

  message("Gather data")
  # list all data referencing samples
  meta <- read_meta(project)
  pid_files <- list.files(str_c(project, "/Zooscan_scan/_work"), pattern=glob2rx("*.pid"), full=TRUE, recursive=TRUE)
  dat1_files <- list.files(str_c(project, "/PID_process/Pid_results/Dat1_validated"), pattern=glob2rx("*_dat1.txt"), full=TRUE)
  
  # get sample identifiers from names
  pids <- str_replace(basename(pid_files), "_dat1.pid", "")
  dat1 <- str_replace(basename(dat1_files), "_dat1.txt", "")

  in_meta_no_pid <- setdiff(meta$id, pids)
  alert(in_meta_no_pid, "Sample in meta but no .pid file", nmax)
  
  in_meta_no_dat1 <- setdiff(meta$id, dat1)
  alert(in_meta_no_dat1, "Sample in meta but not in Dat1_validated", nmax)

  pid_not_in_meta <- setdiff(pids, meta$id)
  alert(pid_not_in_meta, "Sample with .pid file but not in meta", nmax)

  dat1_not_in_meta <- setdiff(dat1, meta$id)
  alert(dat1_not_in_meta, "Sample in Dat1_validated but not in meta", nmax)
  
  pid_no_dat1 <- setdiff(pids, dat1)
  alert(pid_no_dat1, "Sample with .pid file but not in Dat1_validated", nmax)
  
  dat1_no_pid <- setdiff(dat1, pids)
  alert(dat1_no_pid, "Sample in Dat1_validated but no .pid file", nmax)

  
  message("\nCHECK OBJECTS")
  
  message("Gather data")
  # read pid and dat1 data records
  dpids <- ldply(pid_files, read_pid)
  ddat1 <- ldply(dat1_files, read_pid)
  
  # get image names from records
  dpids$img <- str_c(dpids$Label, "_", dpids$Item)
  ddat1$img <- str_c(ddat1$Label, "_", ddat1$Item)
  
  # get images on disk
  img_files <- list.files(str_c(project, "/PID_process/Sorted_vignettes"), pattern=glob2rx("*.jpg"), recursive=TRUE)
  imgs <- str_replace(basename(img_files), ".jpg", "")
  
  # Check image presence
  pid_no_img <- setdiff(dpids$img, imgs)
  alert(pid_no_img, "Object in pids but no vignette", nmax)

  dat1_no_img <- setdiff(ddat1$img, imgs)
  alert(dat1_no_img, "Object in Dat1_validated txt but no vignette", nmax)
  
  img_not_in_pid <- img_files[! imgs %in% dpids$img]
  alert(img_not_in_pid, "Vignette present but no object in pids", nmax)

  img_not_in_dat1 <- img_files[! imgs %in% ddat1$img]
  alert(img_not_in_dat1, "Vignette present but no object in Dat1_validated txt ", nmax)
  

  # Check identifications
  # reduce to vignettes on disk and in dat1 (and keep order)
  ddat1_common <- ddat1[ddat1$img %in% imgs,]
  img_files_common <- img_files[match(ddat1_common$img, imgs)]
  
  # extract identifications
  img_ids <- str_split(img_files_common, "/")
  img_ids <- sapply(img_ids, function(x) {x[length(x)-1]})
  ids <- ddat1_common[,c("img", "Valid")]
  names(ids) <- c("object", "from_dat1")
  ids$from_img <- img_ids

  # detect problems and report them
  wrong_ids <- ids[ids$from_dat1 != ids$from_img,]
  if ( nrow(wrong_ids) > 0) {
    message <- str_c(wrong_ids$object, " should be ", wrong_ids$from_dat1, " but vignette is in ", wrong_ids$from_img)
    alert(message, "Identification mismatch between Dat1_validated files and vignette location", nmax)
    
  }
  
  return(invisible(list(in_meta_no_pid, in_meta_no_dat1, pid_not_in_meta, dat1_not_in_meta, pid_no_dat1, dat1_no_pid, pid_no_img, dat1_no_img, img_not_in_pid, img_not_in_dat1, wrong_ids)))
}
