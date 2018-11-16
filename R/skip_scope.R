
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
skip_scope <- function(fun){structure(fun, skipscope=TRUE)}
if(FALSE){#@testing
    fun <- function()find_scope()
    environment(fun) <- globalenv()
    skip <- skip_scope(fun)
    val <- skip()
    cat(val)
    expect_identical(val, character())
}

condition <- skip_scope(condition)
pkg_error <- skip_scope(pkg_error)
pkg_warning <- skip_scope(pkg_warning)
pkg_message <- skip_scope(pkg_message)
find_scope <- skip_scope(find_scope)
