# Ensure that a function can only be called in a certain pacman mode.

local_only <- function(fun) {
  fun <- match.fun(fun)

  function(...) {
    if (encap(mode) == "off") {
      stop("Turn pacman on to execute this function call.", call. = FALSE)
    }
    fun(...)
  }
}

global_only <- function(fun) {
  fun <- match.fun(fun)

  function(...) {
    if (encap(mode) == "on") {
      stop("Turn pacman off to execute this function call.", call. = FALSE)
    }
    fun(...)
  }
}

#' Call a global function
#'
#' Call a function using the global libraries (in addition to the local ones) when in pacman
#' mode, without polluting the local namespace.
#'
#' @usage globalise(pkg::fun)(...)
#' @export
globalise <- function(fun) {
  function(...) {
    expanded_mode(fun(...))
  }
}

# Simplification, assuming that mode will only be expanded to get global libraries,
# and not vice versa.
expanded_mode <- local_only(function(expr) {
  local_lib <- .libPaths()
  pkgs <- loaded_pkgs(attached_only = FALSE)
  on.exit(expanded_exit(local_lib, pkgs))
  encap(all_lib_paths())
  eval(expr)
})

# helper functions ------------------------------------------------------------------------------

expanded_exit <- function(local_lib, pkgs) {
  pkgs <- setdiff(loaded_pkgs(attached_only = FALSE), pkgs)
  unload_pkgs(pkgs, quiet = TRUE)
  .libPaths(local_lib)
}
