#' Structure of a data.table
#' 
#' You tried calling \code{str} on a big \code{data.table}? Say goodbye to
#' your RAM! At least, that's what you'd be saying before this function.
#' This implements a \code{str} method that handles \code{data.table}s with
#' atomic elements a bit better.
#' 
#' @method str data.table
#' @S3method str data.table
#' @param object A \code{data.table}.
#' @param ... Optional arguments. Currently ignored.
#' @export
str.data.table <- function(object, ...) {
  
  n <- 3
  
  if( !is.data.frame(object) || 
        any( unlist( lapply( object, function(x) { !is.atomic(x) } ) ) )
  ) {
    return( utils::str(object) )
  }
  
  ## handle extra classes for object object
  class_statement <- paste( sep="", "'", class(object), "'", collapse=", " )
  
  if( all( dim( object ) == c(0, 0) ) ) {
    cat( paste( sep="", 
                class_statement, " with 0 rows and 0 columns\n" ) )
    return( invisible(NULL) )
  }
  
  if( nrow(object) < n ) {
    n <- nrow(object)
  }
  
  sapply( 1:ncol(object), function(i) {
    if( !is.atomic( object[[i]] ) ) {
      stop("non-atomic vector in ", deparse( substitute( object ) ),
           ". consider using 'str' or reforming ", deparse( substitute( object ) ), "."
      )
    }
  })
  
  ## make formatted column names
  formatted_names <- format( names(object) )
  classes <- sapply( 1:ncol(object), function(i) {
    class( object[[i]] )
  })
  
  ## make the classes smaller
  class_replace <- c("logical", "integer", "numeric", "complex", "character", "raw", "list", "factor")
  names(class_replace) <- c("logi", "int", "num", "cplx", "chr", "raw", "list", "Factor")
  classes <- swap( classes, class_replace )
  
  ## handle factor levels
  levs <- sapply( 1:ncol(object), function(i) {
    if( is.factor(object[[i]]) ) {
      return( nlevels(object[[i]]) )
    } else {
      return( NA )
    }
  })
  
  levs_statement <- ifelse( levs[!is.na(levs)] > 1, " levels", " level" )
  levs_dots <- ifelse( levs[!is.na(levs)] > n, ", ... ", "" )
  classes[ !is.na(levs) ] <- paste( sep="",
                                    classes[ !is.na(levs) ], " w/ ", levs[!is.na(levs)], levs_statement
  )
  
  formatted_classes <- format( classes )
  
  cat( paste( sep="", 
              class_statement, " with ",
              nrow(object), " row", if( nrow(object) > 1 || nrow(object) == 0 ) "s", 
              " and ", ncol(object), " column", if( ncol(object) > 1 || nrow(object) == 0 ) "s", 
              ":\n" ) )
  
  for( i in seq_along(names(object)) ) {
    
    var <- object[[i]][1:n]
    
    if( is.character(var) ) {
      tmp <- paste( sep="", '"', var, '"' )
    } else if( is.factor(var) ) {
      tmp <- paste( sep="", '"', as.character(var), '"' )
    } else if( is.numeric(var) ) {
      tmp <- prettyNum(var)
    } else {
      tmp <- var
    }
    
    tmp[ is.na(var) ] <- NA
    formatted_vec <- paste( sep="", collapse=", ", tmp )
    if( is.factor(var) ) {
      formatted_vec <- paste( sep="", formatted_vec, ": ", 
                              paste( collapse=", ", as.integer(var) )
      )
    }
    
    out <- paste( sep="", collapse="",
                  "  $ ", formatted_names[i], ": ",
                  formatted_classes[i], "  ",
                  formatted_vec,
                  if( nrow(object) > n ) { ", ... " },
                  "\n"
    )
    
    cat( format(out, )  )
  }
  
  ## print the attributes
  attrs <- attributes(object)
  interesting_attrs <- attrs[ !(names(attrs) %in% c("row.names", "names", "class")) ]
  if( length(interesting_attrs) ) {
    cat("\nAttributes:\n")
    for( i in seq_along(interesting_attrs) ) {
      cat( paste( sep="",
                  "-\t'", names(interesting_attrs)[i], "': ", interesting_attrs[i], "\n" 
      ) )
    }
    
  }
    
}
