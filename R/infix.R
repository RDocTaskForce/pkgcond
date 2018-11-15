
#' Collapse character Vectors
#'
#' @param x a character vector
#' @param with character to place between elements of x.
#'
#' @export
collapse <- function(x, with=' '){paste(x, collapse=with)}
#' @rdname collapse
#' @export
collapse0 <- function(x, with=''){paste(x, collapse=with)}

#' @name infix-concatenation
#' @title Infix string concatenation.
#'
#' @param lhs left string
#' @param rhs right string
#'
#' @description
#' The infix operators listed here are three versions of paste.
#' \itemize{
#'   \item \code{\%\\\%} is for preserving line breaks
#'   \item \code{\%<<\%} is an infix replacement for \code{\link{paste}}
#'   \item \code{\%<<<\%} is paste with no space and no break."
#' }
#' @aliases %\\%
#' @export %\% %<<% %<<<%
`%<<%` <- function(lhs, rhs){
    if (is.null(rhs)) return(collapse(lhs))
    else if (is.null(lhs)) return(collapse(rhs))
    else return(paste(collapse(lhs), collapse(rhs), sep=" "))
}
if(FALSE){#! @testing %<<%
    a <- 'A vain chalks above the integrated biscuit. '
    b <- 'Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %<<% b, paste(a,b))
    expect_equal(a %<<% b %<<% c, paste(a,b,c))

    expect_equal(a %<<% NULL, a)
    expect_equal(NULL %<<% a, a)
    expect_equal(NULL %<<% NULL, "")
}

#' @rdname infix-concatenation
`%<<<%` <- function(lhs, rhs) paste(collapse0(lhs), collapse(rhs), sep="")
if(FALSE){#! @testing %<<<%
    a <- 'A vain chalks above the integrated biscuit. '
    b <- ' Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %<<<% b, paste0(a,b))
    expect_equal(a %<<<% b %<<<% c, paste0(a,b, c, sep=''))

    expect_equal(a %<<<% NULL, a)
    expect_equal(NULL %<<<% a, a)
    expect_equal(NULL %<<<% NULL, '')
}

`%\\%` <- function(lhs, rhs) paste(collapse(lhs, '\n'), collapse(rhs, '\n'), sep="\n")
if(FALSE){#! @testing newline-concatenation
    a <- 'A vain chalks above the integrated biscuit. '
    b <- ' Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %\% b, paste(a,b, sep='\n'))
    expect_equal(a %\% b %\% c, paste(a,b, c, sep='\n'))
}

`%||%` <- function (x, y) if (is.null(x)) y else x
if(FALSE){#@testing
    expect_true( NULL %||% TRUE)
    expect_true( TRUE %||% FALSE)
}


#' Not in infix operator
#'
#' The same as \code{\link{\%in\%}} but negated.
#'
#' @inheritParams base::match
#' @export
#' @name not-in
`%!in%` <- function(x, table){!(`%in%`(x, table))}
