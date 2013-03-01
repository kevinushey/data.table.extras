#' Swap Elements in a Vector
#' 
#' This function swaps elements in a vector. See examples for usage.
#' 
#' @param vec the vector of items whose elements you will be replacing.
#' @param from the items you will be mapping 'from'.
#' @param to the items you will be mapping 'to'. must be same length and
#' order as \code{from}.
#' @param ... optional arguments passed to \code{match}.
#' @seealso \code{\link{match}}
swap <- function( vec, from, to=names(from), ... ) {
  tmp <- to[ match( vec, from, ... ) ]
  tmp[ is.na(tmp) ] <- vec[ is.na(tmp) ]
  return( tmp )
}
