#' Read the profile of particles in a UVP project
#'
#' This reads the characteristics of particles in each UVP image (in results/***_datfile.txt), reads the appropriate metadata (in meta), and aggregates the data over a given number of images to smooth out noise.
#'
#' @param project path to the project directory
#' @param ... passed to \code{\link{read.table}}
#'
#' @family project data handling functions
#' @seealso \code{\link{smooth_particles_profiles}} to smooth the noisy raw signal (through a moving average with function \code{\link{mav}})
#' @export
#' @importFrom stringr str_c str_replace
#' @importFrom plyr ddply
#' @importFrom lubridate parse_date_time
read_particles_profiles <- function(project, ...) {

  if ( project_type(project) != "uvp5" ) {
    stop("Project ", project, " is not a UVP5 project")
  }

  meta <- read_meta(project)
  
  # read all files
  D <- ddply(meta, ~id, function(m, ...) {

    # generate datfile name
    f <- str_c(project, "/results/", m$id, "_datfile.txt")
    
    if (file.exists(f)) {
      # read datfile file
      d <- read.table(f, sep=";", col.names=c("img_nb", "date_time", "depth", str_c("uvp_", 1:11), "nb_part", "mean_area_px", "mean_grey", "nb_large_part", "mean_grey_large_part", "extra"), stringsAsFactors=FALSE, ...)
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
      d$id <- m$id
      
      # convert depth to m
      d$depth <- d$depth / 10

      # parse the date
      d$date_time <- str_replace(d$date_time, "\t", "")
      d$date_time <- str_replace(d$date_time, "_", ".")
      d$date_time <- parse_date_time(d$date_time, "ymdhms")

      # select relevant variables
      d <- d[,c("id", "date_time", "depth", "concentration", "esd", "mean_grey")]
    } else {
      message("Profile ", m$id, " not found. Please process it.")
      d <- NULL
    }
    return(d)
  }, .progress=progress(meta$id))
  
  return(D)
}

# TODO read the data for each particle in the bru file


#' Smooth small scale variability in UVP profiles
#'
#' @param x a data.frame resulting from \code{\link{read_particles_profile}}, with one or several UVP profiles
#' @param n number of images to smooth data over. The higher, the smoother the profiles.
#'
#' @details
#' Compute a moving average of \code{concentration}, \code{esd}, and \code{mean_grey} with function \code{link{mav}}
#'
#' @return
#' A data.frame identical to the input data.frame but with smoothed data
#'
#' @export
#' @importFrom plyr ddply
smooth_particles_profiles <- function(x, n=21) {
  
  # loop over all profiles, if there are several
  x <- ddply(x, ~id, function(X) {
    # filter the data series
    X$concentration <- mav(x=X$concentration, n=n)
    X$esd           <- mav(x=X$esd, n=n)
    X$mean_grey     <- mav(x=X$mean_grey, n=n)

    return(X)
  })

  return(x)
}


#' Bin UVP profiles over depth
#'
#' @param x a data.frame resulting from \code{\link{read_particles_profile}}, with one or several UVP profiles
#' @param bin the size of the depth bin in m
#'
#' @details
#' Average the concentration, esd, and mean grey level over depth bins
#'
#' @return
#' A data.frame similar to the input data.frame but binned in depth
#'
#' @export
#' @importFrom plyr ddply round_any rename
bin_particles_profiles <- function(x, bin=1) {

  # bin depth
  x$depth_binned <- round_any(x$depth, bin)

  # loop over all profiles, if there are several
  xm <- ddply(x, ~id+depth_binned, function(X) {
    xm <- colMeans(X[,c("concentration", "esd", "mean_grey")])
    xm <- c(xm, date_time=mean(X$date_time)) # NB: colMeans cannot deal with POSIX objects
    return(xm)
  })

  # rename depth_binned to depth
  xm <- rename(xb, c(depth_binned="depth"))

  return(xm)
}


#' Compute a moving average
#'
#' @param x a numeric vector
#' @param n size of the window over which to compute the moving average
#'
#' @details
#' The function computes a weighted, centred moving average: each data point is computed as the average of the points around it, with decreasing weights as one moves away from the center. The original data is padded on the left and on the right to be able to compute moving average values at the extremities.
#' 
#' @return A numeric vector of the same length as x, with the moving average values
#'
#' @export
# TODO add example
mav <- function(x, n) {
  # switch from window size to order
  o <- floor(n/2)

  # deal with very short series
  # NB: n is now the length of the series
  n <- length(x)
  if (n < o) {
    o <- n
  }
  
  # pad ends of the series to get a complete averaged series
  begin <- rep(mean(x[1:o], na.rm=TRUE), times=o)
  end <- rep(mean(x[(n-o+1):n], na.rm=TRUE), times=o)
  xp <- c(begin, x, end)
  
  # define the filtering weights/coefficients
  # # flat
  # window <- 2 * o + 1
  # w <- rep(1/window, times=window)
  # more weight at the middle
  w <- c(1:o, o+1, o:1)
  w <- w / sum(w)
  
  # compute the moving average
  xpf <- filter(xp, w, sides=2)
  
  # remove the padded bits (which are NA anyway)
  xf <- xpf[o + (1:n)]
  
  return(xf)
}
