#' Read the metadata file for a project
#'
#' @param project path to the project directory
#'
#' @details
#' First guess the type of project from its name. If the project is a UVP project, read the file in \code{meta} and return the result. If the project is a Zooscan project, read the two files in \code{Zooscan_meta} (one for the samples and one for the scans), join the information in a single data.frame, and return it.
#'
#' @return
#' A data.frame with data appropriate for the type of project.
#'
#' @family project data handling functions
#' @export
read_meta <- function(project) {
  project <- add_class(project)
  read_meta_in_project(project)
}

read_meta_in_project <- function(project) {
  UseMethod("read_meta_in_project")
}

#' @importFrom stringr str_c
#' @importFrom plyr join
#' @importFrom assertthat assert_that
read_meta_in_project.zooscan <- function(project) {
  sample_file <- str_c(project, "/Zooscan_meta/zooscan_sample_header_table.csv")
  scan_file <- str_c(project, "/Zooscan_meta/zooscan_scan_header_table.csv")
  
  assert_that(file.exists(sample_file), file.exists(scan_file))
  
  scan <- read.csv(scan_file, sep=";")
  sample <- read.csv(sample_file, sep=";")
  
  d <- join(sample, scan, by="sampleid")

  return(d)
}

#' @importFrom stringr str_c str_replace
#' @importFrom assertthat assert_that
read_meta_in_project.uvp5 <- function(project) {
  meta_file <- str_c(project, "/meta/", str_replace(basename(project), "uvp5_", "uvp5_header_"), ".txt")
  
  assert_that(file.exists(meta_file))
  
  meta <- read.csv(meta_file, sep=";")

  return(meta)  
}