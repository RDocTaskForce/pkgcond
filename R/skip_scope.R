
.skipscope.env.name <- '.__SkipScope__.'

get_skipscope_env <- function(frame){
    assert_that(is.environment(frame))
    if (!isNamespace(frame))
        frame <- topenv(frame)
    if (exists(.skipscope.env.name, frame, inherits = FALSE))
        get(.skipscope.env.name, frame, inherits = FALSE)
}

create_skipscope_env <- function(frame){
    assert_that(is.environment(frame))
    if (!isNamespace(frame))
        frame <- topenv(frame)
    if (exists(.skipscope.env.name, frame, inherits = FALSE))
        condition( "Skip-scope environment exists."
                 , cond = 'error'
                 , scope = c('pkgcond', 'create_skipscope_env')
                 )
    env <- new.env(TRUE, parent = frame)
    attr(env, 'name') <-
        if (!is.null(. <- environmentName(frame)))
            attr(env, 'name') <- paste(., ':::', .skipscope.env.name)
        else
            .skipscope.env.name
    assign('find_scope::skipscope', TRUE, envir = env)
    lockEnvironment(env, TRUE)
    assign(.skipscope.env.name, env, envir = frame)
    invisible(env)
}

is_global_env <- function(env)identical(env, .GlobalEnv)

#' Exclude a function from find_scope
#'
#' In the course of work it will often be the case that
#' one would like to create a new condition function, such
#' such as for specific errors or warning.  These should
#' not be included in the scope when inferred.  The natural
#' solution would be to include the scope in every call to
#' condition or have it inferred in each function definition.
#' This however, gets very tedious.
#'
#' The `skip_scope` function tags a function as one that should be
#' excluded from consideration when determining scope via
#' [find_scope()].
#'
#' @param fun a function to tag
#'
#' @return The `fun` function with the `skipscope` attribute set to TRUE.
#'
#' @export
skip_scope = function(fun){structure(fun, skipscope=TRUE)}
if(FALSE){#@testing
    fun <- function()find_scope()
    val <- skip_scope(fun)
    expect_identical(val(), character())
}

condition <- skip_scope(condition)
pkg_error <- skip_scope(pkg_error)
pkg_warning <- skip_scope(pkg_warning)
pkg_message <- skip_scope(pkg_message)
find_scope <- skip_scope(find_scope)
