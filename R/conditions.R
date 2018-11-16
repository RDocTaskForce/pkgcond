#' @importFrom methods is getPackageName

.conditions <- c('message', 'warning', 'error', 'none')

.paste_scope <- function(...)paste(..., sep='::')

#' Raise a mutable and classed condition.
#'
#' Raising Classed conditions helps with catching errors.
#' These allow for typing errors as they arise and adding scopes
#' to better catch errors from specific locations.
#'
#' @details
#' The `condition()` function alone provides a flexible and dynamic way of
#' producing conditions in code. The functions `pkg_error`, `pkg_warning`,
#' and `pkg_message` do the same as condition except restricted to errors, warnings,
#' and messages respectively.
#'
#' @param msg The message to convey
#' @param cond The severity of the condition, or what to do;
#'             give a 'message' (default), a 'warning', an 'error'
#'             or do 'none' and ignore.
#' @param ... Attributes to be added to condition object for `condition`,
#'            arguments passed to condition for all others.
#' @param scope A character vector of the scope(s) of the signal.
#'              Defaults to the package name but could be longer such as
#'              package name, a class name, and a method call.
#'              This should be used as a where the error occurred.
#' @param type  Used with `scope` and `cond` to set the class of the condition object to raise.
#'              This should be a type of error; out of bounds, type mismatch, etcetera.
#' @param call  The call to use to include in the condition.
#'
#' @export
condition <-
function( msg
        , cond = .conditions
        , ... #< objects to be added to the condition as attributes.
        , scope = find_scope()
        , type = NULL #< optional type of the condition, used to create the class.
        , call = sys.call(1)
        ){
    cond = match.arg(cond)
    if (cond == 'none') return()

    throw <- function(ball){
        if (is(ball, 'error'  )) stop(ball) else
        if (is(ball, 'warning')) warning(ball) else
        if (is(ball, 'message')) message(ball)
    }
    while (length(scope) && scope[[1]] %in% c("", "R_GlobalEnv", "base"))
        scope <- scope[-1L]
    if (length(scope) > 1L)
        scope <- Reduce(.paste_scope, scope, accumulate=TRUE)
    classes <-{
        c( if (length(scope))
              c( if (!is.null(type))
                  paste0(scope, '-', cond, '-', type)
              , paste0(scope, '-', cond)
              , paste0(scope, '-condition')
              )
         , if (!is.null(type)) paste0(cond, '-', type)
         , cond
         , 'condition'
         )
    }
    ball <- structure( list( message = gettext(msg)
                           , call=call)
                     , class=classes
                     , ...)
    throw(ball)
}
if(FALSE){#@testing
    expect_silent( condition('testing', 'none', scope='base'))
    expect_null( condition('testing', 'none', scope='base'))

    expect_message( condition('testing', 'message', scope='base'), 'testing')
    expect_message( condition('testing', 'message', scope='base', type='testing')
                  , class = "message-testing"
                  )
    expect_message( condition('testing', 'message', scope='test', type='testing')
                  , class = "test-message-testing"
                  )

    expect_warning( condition('testing', 'warning', scope='base'), 'testing')
    expect_warning( condition('testing', 'warning', scope='base', type='testing')
                  , class = "warning-testing"
                  )
    expect_warning( condition('testing', 'warning', scope='test', type='testing')
                  , class = "test-warning-testing"
                  )

    expect_error( condition('testing', 'error', scope='base'), 'testing')
    expect_error( condition('testing', 'error', scope='base', type='testing')
                , class = "error-testing"
                )
    expect_error( condition('testing', 'error', scope='test', type='testing')
                , class = "test-error-testing"
                )

    tryCatch( condition('testing', 'error', type='testing'
                       , scope = c('test', 'my_class', 'my_method')
                       )
            , condition = function(obj){
                expect_is(obj, 'test-error-testing')
                expect_is(obj, 'test::my_class-error-testing')
                expect_is(obj, 'test::my_class::my_method-error-testing')
                expect_is(obj, 'test-error')
                expect_is(obj, 'test::my_class-error')
                expect_is(obj, 'test::my_class::my_method-error')
                expect_is(obj, 'error-testing')
                expect_is(obj, 'error')
                expect_is(obj, 'condition')
            })
}

#' @rdname condition
#' @export
pkg_error <- function(msg, ..., scope = find_scope(), call=sys.call(1)){
    condition(msg, cond = 'error', ..., scope=scope, call=call)
}
.test_pkg_error <- function(...){
    `find_scope::skipscope` <- FALSE
    pkg_error(...)
}
if(FALSE){#@testing pkg_error
    expect_error(.test_pkg_error("A package error."), "A package error.")
    x <- tryCatch( .test_pkg_error("A package error.")
                 , condition= function(e)e
                 )
    expect_is(x, 'pkgcond-error')
    expect_is(x, 'pkgcond-condition')
    expect_is(x, 'error')
    expect_is(x, 'condition')
}

#' @rdname condition
#' @export
pkg_warning <- function(msg, ..., scope = find_scope(), call=sys.call(1)){
    condition(msg, cond = 'warning', ..., scope=scope, call=call)
}
.test_pkg_warning <- function(...){
    `find_scope::skipscope` <- FALSE
    pkg_warning(...)
}
if(FALSE){#@testing pkg_warning
    expect_warning(.test_pkg_warning("A package warning."), "A package warning.")
    x <- tryCatch( .test_pkg_warning("A package warning.")
                 , condition= function(e)e
                 )
    expect_is(x, 'pkgcond-warning')
    expect_is(x, 'pkgcond-condition')
    expect_is(x, 'warning')
    expect_is(x, 'condition')
}

#' @rdname condition
#' @export
pkg_message <- function(msg, ..., scope = find_scope(), call=sys.call(1)){
    condition(msg, cond = 'message', ..., scope=scope, call=call)
}
.test_pkg_message <- function(...){
    `find_scope::skipscope` <- FALSE
    pkg_message(...)
}
if(FALSE){#@testing pkg_message
    expect_message(.test_pkg_message("A package message"), "A package message")
    x <- tryCatch( .test_pkg_message("A package message")
                 , condition= function(e)e
                 )
    expect_is(x, 'pkgcond-message')
    expect_is(x, 'pkgcond-condition')
    expect_is(x, 'message')
    expect_is(x, 'condition')
}
