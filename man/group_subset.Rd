\name{group_subset}
\alias{group_subset}
\title{Row Subset by Group}
\usage{
  group_subset(DT, i = 1, by)
}
\arguments{
  \item{DT}{A \code{data.table}.}

  \item{i}{Indices at which to subset each
  sub-\code{data.table}.}

  \item{by}{Variable(s) to subset over.}
}
\description{
  For each \code{data.table} subset (as subset by the
  \code{by} argument), return the rows at indices \code{i}.
}
\details{
  The code wraps to the impossible to remember but
  incredibly powerful and fast \code{DT[ DT[, .I[i],
  by=by]$V1 ]}.
}
\examples{
DT <- data.table(
  x=rep(letters, each=1E5),
  y=rnorm(26*1E5),
  z=rep(LETTERS, each=1E5),
  a=rep(LETTERS, each=1E5),
  b=rep(LETTERS, each=1E5),
  c=rep(LETTERS, each=1E5),
  d=rep(LETTERS, each=1E5),
  e=rep(LETTERS, each=1E5)
)

DT[, mean_y := mean(y), by=x]
group_subset(DT, 1, x)
}

