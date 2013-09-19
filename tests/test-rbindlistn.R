library(testthat)
library(data.table.extras)

l <- list( a=data.frame(x=c(1, 2, 3), y=2), b=data.frame(x=3, y=4) )

expect_identical(
  rbindlistn(l, FALSE),
  rbindlist(l)
)

expect_identical(
  rbindlistn(l, "Walter"),
  data.table(x=c(1, 2, 3, 3), y=c(2, 2, 2, 4), Walter=c('a', 'a', 'a', 'b'))
)
