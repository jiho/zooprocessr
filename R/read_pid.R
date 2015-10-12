#' Read dat1.pid or dat1.txt files
#'
#' @param file path to a \code{dat1.pid} or \code{dat1.txt} file
#' @param data boolean, whether to extract the data table
#' @param metadata boolean, whether to extract the metadata from the header
#' @param verbose boolean, whether to print information
#'
#' @return
#' When \code{data} is requested, the function returns it as a data.frame. When \code{metadata} is requested, it is returned as a named list (key-value pairs). When both are requested, the result is the data as a data.frame with an attribute named \code{meta} containing the metadata list.
#'
#' @importFrom stringr str_replace_all str_detect str_split str_trim fixed
#' @importFrom data.table fread
#' @export
read_pid <- function(file, data=TRUE, metadata=FALSE, verbose=FALSE) {

  if ( (! data) & (! metadata) ) {
    stop("Neither data nor metadata is requested. I'm left doing nothing...")
  }

  if ( verbose ) { message("Detect file structure") }
  # read the the file as text
  d <- scan(file, what="character", sep="\n", quiet=T, fileEncoding="ISO-8859-1", encoding="UTF-8")

  if ( data ) {
    if ( verbose ) { message("Read data") }
    # get line number where the data table starts
    dataIdx <- which(str_detect(d, fixed("[Data]")))
    # read data table
    dt <- fread(str_c(d, collapse="\n"), skip=dataIdx, sep=";", header=TRUE, verbose=FALSE, data.table=FALSE)
    names(dt) <- make.names(names(dt))
    names(dt)[1] <- "Item"

    # if there is/are validation(s) duplicate the latest validation in a column named "Valid"
    hasValidation <- any(str_detect(names(dt), "pred_valid_Id_"))
    if ( hasValidation ) {
      dt$Valid <- dt[,ncol(dt)]
    }
  }

  if ( metadata ) {
    if ( verbose ) { message("Read metadata") }

    # remove the first line if it only contains "PID"
    if (d[1] == "PID") {
      d <- d[-1]
    }
    # remove pseudo blank lines
    d <- d[d!=" "]
    # get line number where the data table now starts
    dataIdxWithoutBlanks <- which(str_detect(d, fixed("[Data]")))

    # read meta data in the header
    m <- list()
    count <- 0
    # walk every line and test for patterns
    for (i in 1:(dataIdxWithoutBlanks-1)) {
      if (str_detect(d[i], "^\\[")) {
        # item is group title with format '[title]'
        # create a new sub-list
        count <- count+1
        m[[count]] <- list()
        names(m)[count] <- tolower(str_replace_all(d[i], "\\[|\\]", ""))
      } else {
        # item is meta data with format 'name = value'
        # extract name and value
        line <- str_split(d[i], "=")[[1]]
        metaName <- tolower(line[1])
        # NB: switch to lowercase because some labels changed case in the history of Zooprocess
        metaData <- str_trim(line[2])
        # detect numbers with regular expressions
        if (!is.na(metaData)) {
          if (str_detect(metaData, "^-*[0-9]+\\.?[0-9]*$")) {
            metaData <- as.numeric(metaData)
          }
        }
        # store it
        m[[count]][metaName] <- metaData
      }
    }
  }

  if ( data ) {
    # when data is requested, return the data table
    out <- dt
    if ( metadata ) {
      # attach meta data as attribute when it is read
      attr(out, "meta") <- m
    }
  } else {
    # otherwise metadata only is requested and return the metadata list
    out <- m
  }

  return(out)
}
