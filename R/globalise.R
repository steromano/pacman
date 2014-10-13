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
    cascade_mode(local_only(fun)(...))
  }
}

# This function evaluates an expression in a temporary "cascade" mode, where
# both local and global libraries are accessible, with precedence to local ones.
cascade_mode <- function(expr) {
  current_lib <- .libPaths()
  pkgs <- loaded_pkgs(attached_only = FALSE)
  on.exit(cascade_exit(current_lib, pkgs))
  encap(all_lib_paths())
  eval(expr)
}

# helper functions ------------------------------------------------------------------------------

cascade_exit <- function(current_lib, pkgs) {
  pkgs <- setdiff(loaded_pkgs(attached_only = FALSE), pkgs)
  unload_pkgs(pkgs, quiet = TRUE)
  .libPaths(current_lib)
}
