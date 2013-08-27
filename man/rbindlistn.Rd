\name{rbindlistn}
\alias{rbindlistn}
\title{rbindlist with names}
\usage{
  rbindlistn(l, names = NULL)
}
\arguments{
  \item{l}{A list of \code{data.frame}s, \code{data.table}s
  or \code{data.frame} shaped \code{list}s.}

  \item{names}{Either \code{NULL} or \code{FALSE},
  indicating the names should be pulled from the names of
  \code{l}, a vector of the same length as \code{l} which
  specifies the names to use, or a length one list or named
  vector specifying the column name we should assign these
  names.}
}
\description{
  This function implements \code{rbindlist} but adds an
  optional argument, \code{keep.names}, which allows us to
  keep the names of each sub-list which we
  \code{rbindlist}-ify.
}

