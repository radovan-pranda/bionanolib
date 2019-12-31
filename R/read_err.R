# read err file format as dataframe
read_err <- function(file_path) {
    r <- read.table(file_path, header = TRUE, sep = '\t', blank.lines.skip = TRUE,  comment.char = '#')
    return(r)
}
