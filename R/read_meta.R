#' Read the metadata file for a project
#'
#' @param project path to the project directory
#' @param from.dat1 boolean, wether to skip the metadata tables and read data from the header of \code{dat1.txt} files
#'
#' @details
#' First guess the type of project from its name. If the project is a UVP project, read the file in \code{meta} and return the result. If the project is a Zooscan project, read the two files in \code{Zooscan_meta} (one for the samples and one for the scans), join the information in a single data.frame, and return it. If the metadata tables do not exist, or if \code{from.dat1} is \code{TRUE}, read the meta data from the header of the \code{dat1.txt} files.
#'
#' In addition to just reading the data, a basic processing is done: names are homogenised between dat1 files and the metadata table and dates are parsed.
#'
#' @return
#' A data.frame with data appropriate for the type of project.
#'
#' @family project data handling functions
#' @export
read_meta <- function(project, from.dat1=FALSE) {
  project <- project_class(project)
  read_meta_in_project(project=project, from.dat1=from.dat1)
}

read_meta_in_project <- function(project, from.dat1=FALSE) {
  UseMethod("read_meta_in_project")
}

#' @importFrom stringr str_c
#' @importFrom plyr join
#' @importFrom lubridate parse_date_time
read_meta_in_project.zooscan <- function(project, from.dat1=FALSE) {

  # construct file names of metadata tables
  sample_file <- str_c(project, "/Zooscan_meta/zooscan_sample_header_table.csv")
  scan_file <- str_c(project, "/Zooscan_meta/zooscan_scan_header_table.csv")

  # if they are not both present, rebuild the metadata from dat1 files
  if ( ! all(file.exists(sample_file), file.exists(scan_file)) ) {
    warning("No metadata table in ", project, ". Reconstructing metadata from dat1 file headers.")
    from.dat1 <- TRUE
  }

  if ( from.dat1 ) {
    # read metadata in dat1 files
    m <- read_meta_from_dat1(project)

    # homogenise the column names with the official metadata table
    # = extract the last bit of the current metadata names and look for an exact match in the usual zooscan metadata names
    n <- str_split(names(m), fixed("."))
    n <- laply(n, function(x) {x[length(x)]})
    meta_names <- c("sampleid", "ship", "scientificprog", "stationid", "latitude", "longitude", "depth", "ctdref", "otherref", "townb", "towtype", "nettype", "netmesh", "netsurf", "zmax", "zmin", "vol", "sample_comment", "vol_qc", "depth_qc", "sample_qc", "barcode", "latitude_end", "longitude_end", "net_duration", "ship_speed_knots", "cable_length", "cable_angle", "cable_speed", "nb_jar", "scanid", "scanop", "fracid", "fracmin", "fracsup", "fracnb", "observation", "code", "submethod", "cellpart", "replicates", "volini", "volprec")
    # NB: date is not included here because there are several date fields

    matches <- na.omit(match(meta_names, n))
    # NB: works because there is only one match for each
    #     check with
    # llply(meta_names, function(x) { which(n == x) })

    names(m)[matches] <- n[matches]

    # manually add the date column, which is the sample date
    m$date <- m$sample.date
    
    # reconstruct scanid which does not exist
    m$scanid <- str_c(m$sampleid, "_", m$fracid, "_1")
    # NB: the "_1" is a leftover from the time there were two bits of frame to be processed separately, because of computer memory limitations

  } else {
    scan <- read.csv(scan_file, sep=";", stringsAsFactors=FALSE, fileEncoding="latin1", encoding="utf8")
    sample <- read.csv(sample_file, sep=";", stringsAsFactors=FALSE, fileEncoding="latin1", encoding="utf8")

    m <- join(sample, scan, by="sampleid")
  }

  # parse dates
  date_cols <- c(
                "date",
                "image.scanning_date",
                "sample.date",
                "process.date",
                "image_process.date",
                "particules_process.particule_process_date",
                "particules_process.date",
                "validation.validation_upload_time"
                )
  for (col in intersect(names(m), date_cols) ) {
    m[,col] <- parse_date_time(m[,col], orders="ymdhm")
  }

  # homogenise names between zooscan and uvp
  m$id <- m$scanid

  return(m)
}

#' @importFrom stringr str_c str_replace
#' @importFrom lubridate parse_date_time
read_meta_in_project.uvp5 <- function(project, from.dat1=FALSE) {
  meta_file <- str_c(project, "/meta/", str_replace(basename(project), "uvp5_", "uvp5_header_"), ".txt")

  if ( ! file.exists(meta_file) ) {
    warning("No metadata table in ", project, ". Reconstructing metadata from dat1 file headers.")
    from.dat1 <- TRUE
  }

  if ( from.dat1 ) {
    # read metadata in dat1 files
    m <- read_meta_from_dat1(project)

    # homogenise the column names with the official metadata table
    # = extract the last bit of the current metadata names and look for an exact match in the usual uvp metadata names
    n <- str_split(names(m), fixed("."))
    n <- laply(n, function(x) {x[length(x)]})
    meta_names <- c("cruise", "ship", "filename", "profileid", "bottomdepth", "ctdrosettefilename", "latitude", "longitude", "firstimage", "volimage", "aa", "exp", "dn", "winddir", "windspeed", "seastate", "nebuloussness", "comment", "endimg", "yoyo", "stationid")

    matches <- na.omit(match(meta_names, n))
    # NB: works because there is only one match for each
    #     check with
    # llply(meta_names, function(x) { which(n == x) })

    # use those, simple, standard names instead of the compound one from the dat1 file
    names(m)[matches] <- n[matches]
  } else {
    m <- read.csv(meta_file, sep=";", fileEncoding="latin1", encoding="utf8")
  }

  # parse dates
  m$datetime <- parse_date_time(as.character(m$filename), orders="ymdhms")

  # homogenise names between uvp and zooscan
  m$id <- m$profileid
  m$fracnb <- 1
  m$vol <- m$volimage

  return(m)
}


#' @importFrom plyr ldply laply llply
read_meta_from_dat1 <- function(project) {

  # list all dat1.txt files
  files <- list.files(str_c(project, "/PID_process/Pid_results/Dat1_validated"), pattern="dat1\\.txt$", full.names=TRUE)

  if ( length(files) == 0 ) {
    stop("No dat1.txt files found in project ", project)
  }

  # extract all metadata from them
  m <- ldply(files, function(f) {
    # read pid file
    m <- read_pid(f, metadata=T, data=F)
    # flatten the list
    # NB: this converts everything into characters !
    m <- unlist(m)
    # remove empty elements
    m <- na.omit(m)
    # convert it into a one line data.frame
    m <- data.frame(t(m), stringsAsFactors=FALSE)
    return(m)
  }, .progress=progress(files))

  # convert numeric columns into numbers
  suppressWarnings(m_num <- llply(m, as.numeric))
  numCols <- laply(m_num, function(x) {! all(is.na(x))})
  m[,numCols] <- m_num[numCols]

  return(m)
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

