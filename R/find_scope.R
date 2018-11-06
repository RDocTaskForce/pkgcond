`find_scope::skipscope` <- TRUE

#' Find the default scope of a call.
#'
#' This find the scope of the call.
#' It includes the package of the call,
#' the class if called from a method,
#' and the name of the function called.
#'
#' @param frame The frame to infer scope from.
#' @param global Should the global frame be listed in the scope.
#'
#' @export
find_scope <- function(frame=NULL, global=FALSE){

    if (is.null(frame)) n <- 1L
    else if (is.numeric(frame)) n <- as.integer(frame)
    else if (is.environment(frame))
        n <- which(sapply(sys.frames(), identical, frame))
    while ( is.environment(frame <- parent.frame(n))
         && !identical(frame, globalenv())
         && exists('find_scope::skipscope', frame, inherits = TRUE)
         && get('find_scope::skipscope', frame, inherits = TRUE)
          ) n <- n + 1L

    scope = character()
    pkg <- getPackageName(topenv(frame))
    if (global || pkg != ".GlobalEnv")
        scope <- pkg

    if (!length(n) || sys.parent(n) == 0) return(scope)
    caller <- sys.call(sys.parent(n))[[1]]
    fun <- eval(caller, frame)
    if (is(fun, 'refMethodDef')) {
        scope <- c(scope, fun@refClassName, fun@name)
    } else
    if (is.name(caller))
        scope <- c(scope, as.character(caller))
    return(scope)
}
.test_find_scope <- function(){
    `find_scope::skipscope` <- FALSE
    find_scope()
}
.test_find_scope_2 <- function(scope = find_scope(global=TRUE))scope
if(FALSE){#@testing
    expect_identical( .test_find_scope()
                    , c('pkgcond', '.test_find_scope')
                    )
    expect_identical(.test_find_scope_2(), '.GlobalEnv')

    tc <- methods::setRefClass( 'test-class'
                              , fields = list(`find_scope::skipscope`='logical')
                              , methods = list( test_class_scope = function()find_scope()
                                              , initialize = function()`find_scope::skipscope` <<- FALSE
                                              )
                              , where = globalenv())
    obj <- tc()
    expect_identical( obj$test_class_scope()
                    , c('test-class', 'test_class_scope')
                    )
}



