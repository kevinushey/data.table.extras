\name{str.data.table}
\alias{str.data.table}
\title{Structure of a data.table}
\usage{
  \method{str}{data.table} (object, ...)
}
\arguments{
  \item{object}{A \code{data.table}.}

  \item{...}{Optional arguments. Currently ignored.}
}
\description{
  You tried calling \code{str} on a big \code{data.table}?
  Say goodbye to your RAM! At least, what's what you'd be
  saying before this function. This implements \code{str}
  method that handles \code{data.table}s with atomic
  elements a bit better.
}

