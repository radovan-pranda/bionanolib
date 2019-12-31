# returns list of molecules
# each molecule contains list of informations predefined in header of bnx file

read_bnx <- function(file_path) {
    r <- list()
    h <- list()
    f <- list()
    f_no <- list()
    h_no <- list()

    speed_up_is_list <- function(element) {
        r <- 1
        if (grepl(".*\\[(([A-Z]|[a-z])+)\\]", element)) {
            r <- -1
        }
        else if (grepl(".*\\[([1-9][0-9]*)\\]", element)) {
            r <- as.integer(unlist(gregexpr("\\[(([1-9]|[0-9])+)\\]", element))[1])
            r <- as.integer(substr(h_act, r + 1, nchar(element) - 1))
        }

        r
    }

    clean_attrname <- function(element) {
        element <- sub("\\[(([1-9][0-9]*)|([A-Z]|[a-z])+)\\]", "", element)
        element
    }

    set_type <- function(element, type) {
        switch(type,
               "int" = { return(as.integer(element)) },
               "integer" = return(as.integer(element)),
               "float" = return(as.numeric(element)),
               "string" = return(as.character(element)),
               "bool" = return(as.logical(element)),
               "boolean" = return(as.logical(element)),
               return(as.character(element))
       )
    }

    preprocess <- function(line, h, f, f_no, h_no, ignoreQuality) {
        r <- list()
        line <- trimws(line, "both")
        if (nchar(line) > 0) {
            preparsed <- unlist(strsplit(line, '\\s*\\t\\s*'))
            len_prep <- length(preparsed)

            if (substr(line, 0, 1) == '#') {
                warning("Skipping line. Invalid file format.")
                return(NA)
            }

            hname <- as.character(preparsed[1])
            foundmatch <- match(names(h), unlist(strsplit(hname, '')))
            bestmatch <- which.min(foundmatch)

            if (!is.na(min(bestmatch)) & foundmatch[bestmatch] == 1) {
                definition <- names(h)[bestmatch]
            }
            else {
                stop("Invalid file format.")
            }

            
            if (ignoreQuality & regexpr("(Q|q).*", definition) == TRUE) {
                return(NA)
            }

            new_rb_element <- list()
            attr_i <- 1
            typeset <- unlist(f[[definition]])

            hset <- h[[definition]]
            hnoset <- h_no[[definition]]
            len_type <- length(typeset)

            for (i in 1:length(hset)) {
                h_act <- hset[[i]]
                hno_act <- hnoset[[i]]
                attr_act <- typeset[i]

                if (hno_act == 1) {
                    new_rb_element[[h_act]] <- preparsed[attr_i]
                }
                else if (hno_act < 1) {
                    new_rb_element[[h_act]] <- as.list(preparsed[attr_i:len_prep])
                }
                else if (hno_act > 1) {
                    new_rb_element[[h_act]] <- as.list(preparsed[attr_i:(attr_i + hno_act)])
                    attr_i <- attr_i + hno_act - 1
                }

                new_rb_element[[h_act]] <- set_type(new_rb_element[[h_act]], attr_act)

                attr_i <- attr_i + 1
            }

            r[[hname]] <- new_rb_element
            return(r)
        }
        return()
    }
        
    new_rb_element <- list()
    start <- ""
    line <- ""
    line_no <- 0
    no <- 0

    lines <- readLines(file_path, skipNul = TRUE)
    for (i in 1:length(lines)) {
        line_no <- line_no + 1
        line = trimws(lines[[i]], "both")
        if (nchar(line) > 0) {
            preparsed <- unlist(strsplit(line, '\\s*\\t\\s*'))
            len_prep <- length(preparsed)

            if (substr(line, 0, 1) == '#') {
                p_1st <- preparsed[1]
                if (len_prep > 1) {
                    # header names 
                    if (grepl("#([0-9]+|Q|q)h", p_1st)) {
                        p_1st <- substr(p_1st, 2, nchar(p_1st) - 1)
                        if (nchar(start) == 0) start <- p_1st
                        h[[p_1st]] <- list(preparsed[2:len_prep])
                        h_no[[p_1st]] <- lapply(unlist(h[[p_1st]]), speed_up_is_list) #speed up cond. is list???
                        h[[p_1st]] <- lapply(unlist(h[[p_1st]]), clean_attrname)
                    }

                    # types
                    if (grepl("#([0-9]+|Q|q)f", p_1st)) {
                        p_1st <- substr(p_1st, 2, nchar(p_1st) - 1)
                        f[[p_1st]] <- list(preparsed[2:len_prep])
                        f_no[[p_1st]] <- len_prep - 1
                    }
                }
            }
            else {
                lines <- lines[line_no:length(lines)]
                break
            }
        }
    }

    lines = parallel::mclapply(lines, preprocess, h = h, f = f, f_no = f_no, h_no = h_no, mc.preschedule = TRUE)

    molecule = list()
    hnames = names(h)
    first = hnames[[1]]
    m = 0
    mn = 1
    for (i in 1:length(lines)) {
        line = lines[[1]]
        lines = lines[-1]

        if (line == list()) {
            next
        }

        attrib = names(line)
        if (first == attrib) {
            if (m > 0) {
                r[[mn]] = molecule
                molecule = list()
                mn = mn + 1
                m = 0
            }
        }

        m = m + 1
        molecule[[attrib]] = line[[attrib]]
    }
    return(r)
}