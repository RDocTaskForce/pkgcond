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
    if (is.null(frame)) n <- sys.parent(1L)
    else if (is.numeric(frame)) n <- sys.parent(as.integer(frame))
    else if (is.environment(frame))
        n <- which(sapply(sys.frames(), identical, frame))
    while ( n > 0
         && is.environment(frame <- sys.frame(n))
         && !identical(frame, globalenv())
         && ( (attr(sys.function(n), 'skipscope') %||% FALSE)
           || ( exists('.Generic', frame) && n > 2
             && is(sys.function(n-1L), 'MethodDefinition')
              )
           || (exists('find_scope::skipscope', frame, inherits = TRUE)
             && get('find_scope::skipscope', frame, inherits = TRUE)
              )
            )
          ) n <- n - 1L

    scope = character()
    pkg <- getPackageName(topenv(frame))
    if (global || pkg != ".GlobalEnv")
        scope <- pkg
    if (!length(n) || n == 0) return(scope)
    caller <- sys.call(n)[[1]]
    fun <- sys.function(n) # eval(caller, frame)
    if (is(fun, 'refMethodDef')) {
        scope <- c(scope, fun@refClassName, fun@name)
    } else
    if (is(fun, 'MethodDefinition')) {
        scope <- unname(c(scope, paste0(fun@generic, ',', paste(fun@target, collapse=','), "-method")))
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

    setGeneric("get_scope", function(object){
        stop('not implimented')
    })
    setMethod('get_scope', 'test-class', function(object){
        `find_scope::skipscope` = FALSE
        find_scope()
    })
    expect_identical(get_scope(obj), 'get_scope,test-class-method')
}

`%||%` <- function(a,b) if(is.null(a)) b else a

