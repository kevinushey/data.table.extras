#' Delete Columns from a data.table
#' 
#' This function deletes columns from a \code{data.table} through \code{set}.
#' This modifies the object in-place, and hence you do not need to re-assign
#' the result.
#' @param DT A \code{data.table} object.
#' @param ... A set of (quoted or unquoted) variable names.
#' @param list Optional: a list, or vector, of variable names from which to
#' search \code{DT} for variables to delete.
#' @export
#' @examples local({
#' DT <- data.table( x=1:5, y=letters[1:5], z=factor( letters[1:5] ) )
#' str(DT)
#' del(DT, x)
#' str(DT)
#' vars_to_remove <- c("y", "q")
#' del(DT, list=vars_to_remove)
#' str(DT)
#' })
del <- function(DT, ..., list=NULL) {
  
  .delete.cols <- function(arg) {
    tryCatch( set(DT, NULL, as.character(arg), NULL),
              error=function(e) {
                message("Warning: ", arg, " is not a column name in DT")
              } )
  }
  
  args <- match.call(expand.dots=FALSE)$`...`
  for( arg in args ) {
    .delete.cols(arg)
  }
  
  if( !is.null(list) ) {
    for( arg in list ) {
      .delete.cols(arg)
    }
  }
  
}