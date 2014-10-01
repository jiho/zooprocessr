#' Read the profile of particles in a UVP project
#'
#' This reads the characteristics of particles in each UVP image (in results/***_datfile.txt), reads the appropriate metadata (in meta), and aggregates the data over a given number of images to smooth out noise.
#'
#' @param project path to the project directory
#' @param verbose when TRUE, show read progress (when there are many files to read)
#' @param n number of images to aggregate data over. The higher the smoother the profiles.
#' @param ... passed to \code{\link{read.table}}
#'
#' @family project data handling functions
#' @export
#' @importFrom stringr str_c
#' @importFrom plyr ddply
#' @importFrom lubridate parse_date_time
read_particles_profile <- function(project, verbose=TRUE, n=21, ...) {

  # compute a moving average of the relevant data
  if ( (n %% 2) == 0) {
    n <- n+1
  }

  # read project metadata
  meta <- read_meta(project)
  
  # determine wether to show a progress bar
  if ( verbose & length(meta$profileid) > 10 ) {
    progress <- "text"
  } else {
    progress <- "none"
  }

  # read all files
  D <- ddply(meta, ~profileid, function(m, ...) {

    # generate datfile name
    f <- str_c(project, "/results/", m$profileid, "_datfile.txt")
    
    if (file.exists(f)) {
      # read datfile file
      d <- read.table(f, sep=";", col.names=c("img_nb", "date_time", "depth", str_c("uvp_", 1:11), "nb_part", "mean_area_px", "mean_grey", "nb_large_part", "mean_grey_large_part", "extra"))
      # TODO check with marc: is the total number of particles nb_part or nb_part + nb_large_part?

      # remove "rinsing" pre-cast
      d <- d[which(d$img_nb >= m$firstimage),]
      
      # remove start of upcast
      d <- d[1:which.max(d$depth),]

      # compute concentration
      d$concentration <- d$nb_part / m$volimage
      
      # compute Equivalent Spherical Diameter (esd)
      d$mean_area_mm2 <- m$aa * d$mean_area_px ^ m$exp
      d$mean_area_um2 <- d$mean_area_mm2 * 1000^2
      d$esd <- 2 * sqrt(d$mean_area_um2 / pi)

      # add profile id
      d$profileid <- m$profileid
      
      # convert depth to m
      d$depth <- d$depth / 10

      # parse the date
      d$date_time <- str_replace(d$date_time, "\t", "")
      d$date_time <- str_replace(d$date_time, "_", ".")
      d$date_time <- parse_date_time(d$date_time, "ymdhms")

      # compute moving averages of concentration, esd, and grey level
      d$concentration <- filter(d$concentration, rep(1/n, n), sides=2)
      d$esd <- filter(d$esd, rep(1/n, n), sides=2)
      d$mean_grey <- filter(d$mean_grey, rep(1/n, n), sides=2)

      # select relevant variables
      d <- d[,c("profileid", "img_nb", "nb_part", "date_time", "depth", "concentration", "esd", "mean_grey")]
    } else {
      message("Profile ", m$profileid, " not found. Please process it.")
      d <- NULL
    }
    return(d)
  }, .progress=progress)
  

  return(D)
}


# TODO read the data for each particle in the bru file