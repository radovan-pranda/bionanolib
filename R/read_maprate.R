# read maprate file format as dataframe
read_maprate <- function(file_path, force = FALSE) {
    h <- character()
    r <- data.frame()
    line_no <- 0

    c = file(file_path, "r")
    while (TRUE) {
        line_no <- line_no + 1
        line = readLines(c, n = 1, skipNul = TRUE)
        val <- trimws(line, "both")
        if (nchar(val) > 0) {
            preparsed <- unlist(strsplit(trimws(val, "both"), '\\s*\\t\\s*'))
            if (substr(val, 0, 1) == '#') {
                len_prep <- length(preparsed)
                if (len_prep > 1) {
                    # get possible header names 
                    if (grepl("#\\s*", preparsed[1])) {
                        preparsed[1] <- sub('#\\s*', '', preparsed[1])
                        h <- as.character(preparsed[1:len_prep])
                    }
                }
            }
            else {
                r <- matrix(ncol = length(h))
                break
            }
        }
        else {
            if (force == FALSE) {
                stop(paste("Invalid file format at line ", line_no))
            }
        }
    }

    close(c)

    rm(c)
    rm(line)
    rm(line_no)
    rm(val)

    r <- read.table(file_path, sep = '\t', blank.lines.skip = TRUE, comment.char = '#')

    if (length(h) != ncol(r)) {
        if (length(h) > ncol(r)) {
            warning("WARNING: Invalid MAPRATE header \nHeader is longer than dimension of data structure. Some data may be lost.")
        }
        else {
            warning("WARNING: Invalid MAPRATE header \nHeader is shorter than dimension of data structure.")
        }
    }

    names(r) <- h

    rm(h)

    return(r)
}
