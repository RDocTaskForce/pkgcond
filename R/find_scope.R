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
#'
#' @examples
#'
#' my_function <- function(){
#'     scope <- find_scope()
#'     "You are in" %<<% collapse(scope, '::')
#' }
#' my_function()
#'
#' my_sights <- my_function
#' my_sights()
#'
find_scope <- function(frame=NULL, global=FALSE){
    if (is.null(frame)) n <- sys.parent(1L)
    else if (is.numeric(frame)) n <- sys.parent(as.integer(frame))
    else if (is.environment(frame))
        n <- which(sapply(sys.frames(), identical, frame))
    while ( n > 0
         && is.environment(frame <- sys.frame(n))
         && !identical(frame, globalenv())
         && !is.primitive(fun <- sys.function(n))
         && !identical(fun, base::force)
         && ( (attr(fun, 'skipscope') %||% FALSE)
           || ( exists('.Generic', frame) && n > 2
             && is(sys.function(n-1L), 'MethodDefinition')
              )
           || (exists('find_scope::skipscope', frame, inherits = TRUE)
             && get('find_scope::skipscope', frame, inherits = TRUE)
              )
            )
          ) n <- n - 1L

    scope = character()
    fun <- sys.function(n)
    if (is.primitive(fun)) return(scope)
    if (!length(n) || n == 0) return(scope)
    pkg <- getPackageName(topenv(frame))
    if (global || pkg != ".GlobalEnv")
        scope <- pkg
    caller <- sys.call(n)[[1]]
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
.test_find_scope <- function(method=c('defaults', 'integer', 'environment')){
    `find_scope::skipscope` <- FALSE
    method = match.arg(method)
    switch( method
          , defaults = find_scope()
          , integer = find_scope(1)
          , environment= find_scope(environment())
          )
}
if(FALSE){#@testing
    expect_identical( .test_find_scope()
                    , c('pkgcond', '.test_find_scope')
                    )
    expect_identical( .test_find_scope('integer')
                    , c('pkgcond', '.test_find_scope')
                    )
    expect_identical( .test_find_scope('environment')
                    , c('pkgcond', '.test_find_scope')
                    )

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
    }, where = globalenv())
    setMethod('get_scope', 'test-class', function(object){
        `find_scope::skipscope` = FALSE
        find_scope()
    }, where = globalenv())
    expect_identical(tail(get_scope(obj), 1), 'get_scope,test-class-method')

    expect_identical(find_scope(1), character(0))
    expect_identical(find_scope(sys.nframe()), character(0))
}

`%||%` <- function(a,b) if(is.null(a)) b else a

