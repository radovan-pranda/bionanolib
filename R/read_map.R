# read CMAP, XMAP, SMAP, MAP, INDEL file format as dataframe
read_map <- function(file_path, force = FALSE) {
    h <- character()
    f <- character()
    f_no <- list()
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
                    # header names 
                    if (grepl("#h\\s*", preparsed[1])) {
                        preparsed[1] <- sub('#h\\s*', '', preparsed[1])
                        h <- as.character(preparsed[1:len_prep])
                        r <- matrix(ncol = length(h))
                    }

                    # types
                    if (grepl("#f\\s*", preparsed[1])) {
                        preparsed[1] <- sub('#f\\s*', '', preparsed[1])
                        f <- list(preparsed[1:len_prep])
                        break
                    }
                }
            }
            else {
                if (force == FALSE) {
                    stop(paste("Invalid file format at line ", line_no))
                }
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
    rm(f_no)
    rm(line)
    rm(line_no)
    rm(val)

    r <- read.table(file_path, sep = '\t', blank.lines.skip = TRUE, comment.char = '#')

    if (length(h) != ncol(r)) {
        if (length(h) > ncol(r)) {
            warning("WARNING: Invalid CMAP header \nHeader is longer than dimension of data structure. Some data may be lost.")
        }
        else {
            warning("WARNING: Invalid CMAP header \nHeader is shorter than dimension of data structure.")
        }
    }

    names(r) <- h

    rm(h)
    rm(f)

    return(r)
}

read_cmap <- read_map
read_smap <- read_map
read_xmap <- read_map
read_indel <- read_map