#' Find the default scope of a call.
#'
#' This find the scope of the call.
#' It includes the package of the call,
#' the class if called from a method,
#' and the name of the function called.
#'
#' @param frame The frame to infer scope from.
#' @param include.global Should the global frame be listed in the scope.
#'
#' @export
find_scope <- function(frame=parent.frame(), include.global=FALSE){
    scope = character()
    pkg <- getPackageName(topenv(frame))
    if (include.global || pkg != ".GlobalEnv")
        scope <- pkg

    n <- which(sapply(sys.frames(), identical, frame))
    if (!length(n)) return(scope)
    caller <- sys.call(n)[[1]]
    fun <- eval(caller, frame)
    if (is(fun, 'refMethodDef')) {
        scope <- c(scope, fun@refClassName, fun@name)
    } else
    if (is.name(caller))
        scope <- c(scope, as.character(caller))
    return(scope)
}
test_find_scope <- function()find_scope()
test_find_scope_2 <- function(scope = find_scope(parent.frame(), TRUE))scope
if(FALSE){#@testing
    expect_identical( test_find_scope()
                    , c('pkgcond', 'test_find_scope')
                    )
    expect_identical(test_find_scope_2(), '.GlobalEnv')

    tc <- methods::setRefClass( 'test-class'
                              , methods = list(test_class_scope = function()find_scope())
                              , where = globalenv())
    obj <- tc()
    expect_identical( obj$test_class_scope()
                    , c('test-class', 'test_class_scope')
                    )
}


