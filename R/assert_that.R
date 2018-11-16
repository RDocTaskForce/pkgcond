


#' Scoped Assertions
#'
#' The pkgcond package intentionally overrides the [assertthat::assert_that()]
#' function.  It provides the same utility but enhances the oririginal version
#' by throwing scoped and typed errors.  The type is 'assertion failure' and
#' the scope can be set or inferred from the calling frame.
#'
#' @inheritParams assertthat::assert_that
#' @param scope The scope of the error.
#' @param type  The error type.
#'
#' @export
assert_that <-
function ( ..., env = parent.frame(), msg = NULL
         , scope = find_scope(env)
         , type  = 'assertion failure'
         )
{
    res <- assertthat::see_if(..., env = env, msg = msg)
    if (res) return(TRUE)
    pkg_error( attr(res, "msg")
             , type = type
             , scope = scope
             )
}
.test_assert_that <- function(...){
    `find_scope::skipscope` <- FALSE
    assert_that(...)
}
if(FALSE){#@testing
    expect_true(assert_that(1==1))

    error <-
    tryCatch(.test_assert_that(1==2)
            , condition = function(e)e )
    expect_is(error, 'condition')
    expect_is(error, 'pkgcond-condition')
    expect_is(error, 'error')
    expect_is(error, 'pkgcond-error')
    expect_is(error, 'error-assertion failure')
    expect_is(error, 'pkgcond-error-assertion failure')
    expect_is(error, 'pkgcond::.test_assert_that-error-assertion failure')
    expect_is(error, 'pkgcond::.test_assert_that-error')
    expect_is(error, 'pkgcond::.test_assert_that-condition')
}

