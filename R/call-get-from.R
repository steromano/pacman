# impl ------------------------------------------------
from_pkg <- function(what) {
  function(pkg, name, ..., path = ".") {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if(all(capsule$global_lib %in% .libPaths())) {
      return(eval(substitute(expr, list(expr = what(pkg, name)))))
    }
    suppressMessages(on(soft = TRUE))
    on.exit(suppressMessages({off(); on(path)}))
    eval(substitute(expr, list(expr = what(pkg, name))))
  }
}

#' Call functions and get objects from external libraries
#'
#' These functions allow the user to call functions and get objects
#' from any package installed globally while working in local mode,
#' without installing or even explicitly loading that package.
#' Use the \code{_internal} versions to access non-exported elements.
#'
#' @details
#' If the global libraries are present in \code{.libPath()} these function
#' are equivalent to \code{::} and \code{:::}. If not, they temporarily
#' switch to \code{on(soft = TRUE)}, then do the same thing.

#' @export
call_from <- from_pkg(call_exported_)
#' @export
call_from_internal <- from_pkg(call_internal_)
#' @export
get_from <- from_pkg(get_exported_)
#' @export
get_from_internal <- from_pkg(get_internal_)

# helper functions -------------------------------------
call_exported_ <- function(pkg, name) {
  substitute(pkg::name(...), list(pkg = pkg, name = name))
}
call_internal_ <- function(pkg, name) {
  substitute(pkg:::name(...), list(pkg = pkg, name = name))
}
get_exported_ <- function(pkg, name) {
  substitute(pkg::name, list(pkg = pkg, name = name))
}
get_internal_ <- function(pkg, name) {
  substitute(pkg:::name, list(pkg = pkg, name = name))
}
