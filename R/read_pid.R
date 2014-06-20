#' Read dat1.pid or dat1.txt files
#'
#' @param file path to a \code{dat1.pid} or \code{dat1.txt} file
#' @param data boolean, wether to extract the data table
#' @param metadata boolean, wether to extract the metadata from the header
#'
#' @return
#' A data.frame, possibly with an attribute named \code{meta} containing the metadata as a named list; or the list of metadata directly, if only metadata is requested
#'
#' @importFrom stringr str_replace_all str_detect str_split str_trim
#' @export
read_pid <- function(file, data=TRUE, metadata=FALSE, verbose=FALSE) {

  if ( (! data) & (! metadata) ) {
    stop("Neither data nor metadata is requested. I'm left doing nothing...")
  }

  if ( verbose ) { message("Detect file structure") }
  # read the first 2000 lines as text
  d <- scan(file, what="character", skip=1, sep="\n", quiet=T, fileEncoding="ISO-8859-1", encoding="UTF-8", n=2000)

  # get line number where the data table starts
  dataIdx <- which(str_detect(d, fixed("[Data]"))) + 1
  # NB: we skipped the first line in the scan() call above, so the line numbers in the file are those computed here + 1, hence the dataIdx+1

  if ( data ) {
    if ( verbose ) { message("Read data") }
    # read data table
    dt <- read.table(file, skip=dataIdx+1, sep=";", header=T, as.is=T)
    names(dt)[1] <- "Item"
    # TODO read from a text connection here and test wether this is faster

    # if there is/are validation(s) duplicate the latest validation in a column named "Valid"
    hasValidation <- any(str_detect(names(dt), "pred_valid_Id_"))
    if ( hasValidation ) {
      dt$Valid <- dt[,ncol(dt)]
    }
  }

  if ( metadata ) {
    if ( verbose ) { message("Read metadata") }

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
        names(m)[count] <- str_replace_all(d[i], "\\[|\\]", "")
      } else {
        # item is meta data with format 'name = value'
        # extract name and value
        line <- str_split(d[i], "=")[[1]]
        metaName <- line[1]
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
