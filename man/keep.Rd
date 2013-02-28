\name{keep}
\alias{keep}
\title{Keep Columns from a data.table}
\usage{
  keep(DT, ..., list = NULL)
}
\arguments{
  \item{DT}{A \code{data.table} object.}

  \item{...}{A set of (quoted or unquoted) variable names.}

  \item{list}{Optional: a list, or vector, of variable
  names from which to search \code{DT} for variables to
  delete.}
}
\description{
  This function removes all columns not in the set of
  column names passed.
}
\details{
  Note: only one of the \code{...} and \code{list}
  arguments may be used at a time.
}
\examples{
local({
DT <- data.table( x=1:5, y=letters[1:5], z=factor( letters[1:5] ) )
DT2 <- DT
str(DT)
keep(DT2, x)
str(DT2)
vars_to_remove <- c("y", "q")
keep(DT, list=vars_to_remove)
str(DT)
})
}

