##' rbindlist with names
##' 
##' This function implements \code{rbindlist} but adds an optional argument,
##' \code{names}, which allows us to keep the names of each
##' sub-list which we \code{rbindlist}-ify.
##' 
##' @param l A list of \code{data.frame}s, \code{data.table}s or 
##'   \code{data.frame} shaped \code{list}s.
##' @param names Either \code{NULL} or \code{FALSE}, indicating the names should
##'   be pulled from the names of \code{l}, a vector of the same length as 
##'   \code{l} which specifies the names to use, or a length one list or
##'   named vector specifying the column name we should assign these names.
##' @export
rbindlistn <- function(l, names=NULL) {
  
  if (is.null(names) || identical(names, FALSE)) {
    return( data.table::rbindlist(l) )
  }
  
  if (isTRUE(names)) {
    colname <- ".Names"
    if (is.null(names(l))) {
      names <- paste0("V", 1:length(l))
    } else {
      names <- names(l)
    }
  } else if (length(names) == length(l)) {
    names <- as.character(names)
    colname <- ".Names"
  } else if (is.list(names) && length(names) == 1) {
    colname <- names(names)
    names <- unname( unlist(names) )
  } else {
    stop("'names' argument is malformed")
  }
  
  if (colname %in% names(l[[1]])) {
    stop("'", colname, "' is a name in your data.frame; cannot be used",
      "to name a column in your output.")
  }
  
  names[ names == '' ] <- paste0("V", (1:length(names)))[ names == '' ]
  
  for (i in seq_along(l)) {
    l[[i]][[colname]] <- names[[i]]
  }
  
  return( data.table::rbindlist(l) )
  
}
