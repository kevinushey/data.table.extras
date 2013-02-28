#' Keep Columns from a data.table
#' 
#' This function removes all columns not in the set of column names passed.
#' 
#' Note: only one of the \code{...} and \code{list} arguments may be used at a time.
#' @param DT A \code{data.table} object.
#' @param ... A set of (quoted or unquoted) variable names.
#' @param list Optional: a list, or vector, of variable names from which to
#' search \code{DT} for variables to delete.
#' @export
#' @examples local({
#' DT <- data.table( x=1:5, y=letters[1:5], z=factor( letters[1:5] ) )
#' DT2 <- copy(DT)
#' str(DT)
#' keep(DT2, x)
#' str(DT2)
#' vars_to_keep <- c("y", "q")
#' keep(DT, list=vars_to_keep)
#' str(DT)
#' })
keep <- function(DT, ..., list=NULL) {
  
  .delete.col <- function(arg) {
    tryCatch( set(DT, NULL, as.character(arg), NULL),
              error=function(e) {
                message("Warning: ", arg, " is not a column name in DT")
              } )
  }
  
  if( is.null(list) ) {
    
    args <- as.character( match.call(expand.dots=FALSE)$`...` )
    remove <- names(DT)[ !(names(DT) %in% args) ]
    for( arg in remove ) {
      .delete.col(arg)
    }
    
  } else {
    remove <- names(DT)[ !(names(DT) %in% unlist(list)) ]
    for( arg in remove ) {
      .delete.col(arg)
    }
  }
  
}