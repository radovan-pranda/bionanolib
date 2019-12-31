# read bed file format as dataframe
read_bed <- function(file_path, customHeader = c('chrom', 'chromStart', 'chromEnding', 'name', 'score', 'strand', 'thickStart', 'thickEnd', 'itemRgb'), headerIncluded=FALSE) {
    if (headerIncluded == TRUE)
    {
        r <- read.table(file_path, header = TRUE, sep = '\t', blank.lines.skip = TRUE, comment.char = '#')
        return(r)
    }
    else
    {
        r <- read.table(file_path, header = FALSE, sep = '\t', blank.lines.skip = TRUE, comment.char = '#')
        names(r) <- customHeader
        return(r)
    }
}