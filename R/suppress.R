
#' @name suppress
#' @title Selectively suppress warnings and messages
#'
#' @description This collection of functions allow the suppression of condition messages,
#' warnings and messages, through filtering the condition message, the condition
#' class or a combination of the two.
#'
#' @param expr An expression to evaluate.
#' @param pattern A regular expression pattern to match on.
#' @param class The class or classes that you would like to filter.
#'              When more that one is given the condition may match any
#'              of the classes.
#' @inheritDotParams base::grepl
#'
#' @examples
#' \dontrun{
#' testit <- function(){
#'     warning("this function does nothing.")
#'     warning("it's pretty useless.")
#' }
#' suppress_warning(testit(), "useless")  # Will suppress only the second warning by pattern
#'
#'
#' # If my_pkg used pkgcond for conditions,
#' # This would suppress all messages and warnings originating
#' # in my_pkg functions.
#' suppress_conditions(my_function(), class='my_pkg-condition')
#' }
NULL

#' @describeIn suppress The general case of suppressing both messages and warnings.
#' @export
suppress_conditions <-
function(expr, pattern=NULL, class=NULL, ...){
    withCallingHandlers( expr
                       , warning = function(cond){
                           # browser()
                           if ( (is.null(pattern) || grepl(pattern=pattern, x=conditionMessage(cond), ...))
                             && (is.null(class) || any(class(cond) %in% class))
                              ) invokeRestart("muffleWarning")
                       }
                       , message = function(cond){
                           if ( (is.null(pattern) || grepl(pattern=pattern, x=conditionMessage(cond), ...))
                             && (is.null(class) || any(class(cond) %in% class))
                              ) invokeRestart("muffleMessage")
                       })
}
suppress_conditions <- skip_scope(suppress_conditions)
if(FALSE){#@testing suppress_conditions
    do_conditions <- function(){
        message('ignore me.')
        warning('ignore me.')
        message('but not me.')
        warning('but not me.')
        pkg_message('if the class matches', scope = 'test_ignore')
        pkg_warning('if the class matches', scope = 'test_ignore')
    }
    expect_silent(suppress_conditions(do_conditions()))

    capture_conditions <- function(code){
        warnings <- capture_warnings(
            messages <- capture_messages(code))
        list(warnings=warnings, messages=messages)
    }

    bare <- capture_conditions(do_conditions())

    expect_identical( capture_conditions(suppress_warnings(do_conditions()))
                    , list(warnings=character(0), messages=bare$messages)
                    )
    expect_identical( capture_conditions(suppress_messages(do_conditions()))
                    , list(warnings=bare$warnings, messages=character(0))
                    )

    expect_identical( capture_conditions(suppress_conditions(do_conditions(), 'ignore'))
                    , list( warnings=bare$warnings[-1]
                          , messages=bare$messages[-1]
                          ))
    expect_identical( capture_conditions(suppress_conditions(do_conditions(), class='test_ignore-condition'))
                    , list( warnings=bare$warnings[-3]
                          , messages=bare$messages[-3]
                          ))
}

#' @describeIn suppress A convenience wrapper that specifies warning class to suppress.
#' @export
suppress_warnings <- function(expr, pattern=NULL, class='warning', ...)
    suppress_conditions( expr, pattern = pattern, class=class)
suppress_warnings <- skip_scope(suppress_warnings)
if(FALSE){#@testing suppress_warnings
    do_warnings <- function(){
        warning('ignore me.')
        warning('but not me.')
        pkg_warning('if the class matches', scope = 'test_ignore')
    }
    expect_warning(do_warnings(), 'ignore me')
    expect_warning(do_warnings(), 'but not me')
    expect_warning(do_warnings(), 'if the class matches')


    expect_silent(suppress_warnings(do_warnings()))

    expect_identical( capture_warnings(suppress_warnings(do_warnings(), 'ignore'))
                    , c( "but not me."
                       , "if the class matches"
                       ))
    expect_identical( capture_warnings(suppress_warnings(do_warnings(), class='test_ignore-warning'))
                    , c( "ignore me."
                       , "but not me."
                       ))
}

#' @describeIn suppress A convenience wrapper that specifies warning class to suppress.
#' @export
suppress_messages <- function(expr, pattern=NULL, class='message', ...)
    suppress_conditions( expr, pattern = pattern, class=class)
suppress_messages <- skip_scope(suppress_messages)
if(FALSE){#@testing suppress_messages
    do_messages <- function(){
        message('ignore me.')
        message('but not me.')
        pkg_message('if the class matches', scope = 'test_ignore')
    }
    expect_message(do_messages(), 'ignore me')
    expect_message(do_messages(), 'but not me')
    expect_message(do_messages(), 'if the class matches')

    expect_silent(suppress_messages(do_messages()))

    expect_identical( capture_messages(suppress_messages(do_messages(), 'ignore'))
                    , c( "but not me.\n"
                       , "if the class matches"
                       ))
    expect_identical( capture_messages(suppress_messages(do_messages(), class='test_ignore-message'))
                    , c( "ignore me.\n"
                       , "but not me.\n"
                       ))
}



