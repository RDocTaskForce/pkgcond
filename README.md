# pkgcond <img src="man/figures/logo.png" align="right" height=140/>
[![Travis build status](https://travis-ci.org/RDocTaskForce/pkgcond.svg?branch=master)](https://travis-ci.org/RDocTaskForce/pkgcond)
[![Coverage status](https://codecov.io/gh/RDocTaskForce/pkgcond/branch/master/graph/badge.svg)](https://codecov.io/github/RDocTaskForce/pkgcond?branch=master)

The goal of pkgcond is to facilitate the creation of errors, warnings,
and messages (collectively called signals or conditions) that are more informative
than the base versions.

Signals can be created through `pkg_error()`, `pkg_warning()` and `pkg_message()`.
When these are used a scope is computed and used to create errors, warnings and 
signals, respectively, with classes set to the combinations of the scope.
The scope, while is could be set explicitly, infers where the condition is created,
and will typically include the function call and package name.


## Installation

You can install the released version of pkgcond from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("pkgcond")
```

## Understanding Scope

Let's consider a toy example to understand the scope.
Say that we are creating a package named `hw` with a function `hello_world()`
defined as follows:

```r
hello_world <- function(greeting = 'hello', who = 'world'){
    if (!is.character(greeting) && length(greeting) == 1L) 
        pkg_error("greeting must be a string.")
    if (!is.character(who) && length(who) == 1L) 
        pkg_error("who must be a string.")
    pkg_message(paste(greeting, who))
}
```
If the function is called with the defaults the `pkg_message()` function will 
be called.  The effect is the same as if a `base::message()` call were here; 
A message with "hello world" would appear in the console.  However,
`pkg_message()` does a little more.  The base call would create a '`message`' object 
and signal it.  With `pkg_message()` since it was invoked inside the `hello_world()`
function inside the `hw` package the scope would be set to `c('hw', 'hello_world')`
This in turn would be used to create the message signal with the following classes.

* `hw::hello_world-message`
* `hw::hello_world-condition`
* `hw-message`
* `hw-condition`
* `message`
* `condition`

The `pkg_error()` and `pkg_warning()` functions have similar scoping however with 
error and warning replacing message, respectively.

This becomes really useful if one wishes to capture conditions.  This is done with 
the `tryCatch()` function.  In this way one can easily catch the conditions that originate from a specific function call or a specific package while passing others through or handling them differently.

```r
tryCatch( hello_world("die", stop)
        , "hw::hello_world-error" = function(cond){
            # This would handle the error that is raised from passing
            # `stop` into the `hello_world function()
            }
        , "hw-condition" = function(cond){
            # This would capture all error originating in the hw package
            # that were signaled using any of the `pkg_*` functions.
        }
        # Errors that did not originate from the hw package or were created 
        # with the traditional stop, warning, or message functions
        # will be passed through since they will not be "Caught"
)
```


## Helpers included:

These functions are included with `pkgcond` to help create error messages.

* `assert_that()`  This intentionally masks `assertthat::assert_that()` when an assertion fails from this call the scope is set as above but a type is also created that indicates this is a 'assertion failure' that can also be used to catch specific errors.
* `comma_list()` takes a list of items and creates a correctly formatted (for English at least) comma separated list contained in a single string.
* `collapse()` takes a character vector and collapses it to a single string separated by a space.
* `collapse0()` same as previous but no space separating parts.
* `lhs %<<% rhs` an infix operator version of paste but will attempt to coerce and collapse lhs and rhs as well.
* `lhs %<<<%` an infix operator version of paste0, will use collapse0 on lhs and rhs prior to concatenation of the two.
* `lhs %\% rhs` Similar to the previous two but separates with a new line.  This will use collapse prior to concatenation.
* `._()` Used to enable translation for a signal message.  When used with a single argument acts as an alias for `gettext()` when given multiple arguments it wraps
`gettextf()`.

## Documentation

The `pkgcond` package is developed by the R Documentation Task Force, an 
[R Consortium](https://www.r-consortium.org)
[Infrastructure Steering Committee working group](https://www.r-consortium.org/projects/isc-working-groups).

