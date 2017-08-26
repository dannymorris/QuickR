catStrings <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))

      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}

csvSQL <- function(x) {
  paste0("'", dplyr::pull(x), "'", collapse = ",")
}


floorDate <- function(x, format) {
  if (is.character(x) == FALSE) x <- as.character(x)
  if (format == 'yyyymm') {
    as.Date(paste0(substr(x, 1, 4), '-',
                   substr(x, 5, 6), '-',
                   '01'))
  }
}

headTbl <- function(tbl) {
  head(data.frame(tbl))
}

notIn <- function(reference, source) {
  !('%in%'(reference, source))
}

