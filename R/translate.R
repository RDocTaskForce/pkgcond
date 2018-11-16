

#' Alias for gettext/gettextf
#'
#' This shortcut provides simple translation and formatting functionality.
#'
#' @param msg The message to translate.
#' @inheritDotParams base::gettextf
#' @param domain see [base::gettext]
#' @name dot-underscore
#' @export
._ <- function(msg, ..., domain=NULL){
    if (...length())
        gettextf(msg, ..., domain = domain)
    else
        gettext(msg, domain = domain)
}
if(FALSE){#@testing
    expect_identical(._('I am testing the function `._`')
                    , 'I am testing the function `._`')
    expect_identical(._('I am testing the function `%s`', '._')
                    , 'I am testing the function `._`')
}
