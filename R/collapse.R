#' Collapse a data.table by Group
#' 
#' For each \code{data.table} subset (as subset by the \code{by} argument),
#' return the rows at indices \code{i}.
#' 
#' The code wraps to the impossible to remember but incredibly powerful and
#' fast \code{DT[ DT[, .I[i], by=by]$V1 ]}.
#' 
#' @param DT A \code{data.table}.
#' @param i Indices at which to subset each sub-\code{data.table}.
#' @param by Variable(s) to subset over, as a \code{list} of symbols.
#' @export
#' @examples
#' DT <- data.table(
#'   x=rep(letters, each=1E5),
#'   y=rnorm(26*1E5),
#'   z=rep(LETTERS, each=1E5),
#'   a=rep(LETTERS, each=1E5),
#'   b=rep(LETTERS, each=1E5),
#'   c=rep(LETTERS, each=1E5),
#'   d=rep(LETTERS, each=1E5),
#'   e=rep(LETTERS, each=1E5)
#' )
#' 
#' DT[, mean_y := mean(y), by=x]
#' collapse(DT, 1, by=list(x))
collapse <- function(DT, i=1, by) {
  call <- match.call()
  return( DT[ DT[, .I[i], by=eval( call$by )]$V1 ] )
}
