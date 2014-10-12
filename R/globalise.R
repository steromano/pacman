#' Call a global function
#'
#' Call a function using the global libraries (in addition to the local ones) when in pacman
#' mode, without polluting the local namespace.
#'
#' @usage globalise(pkg::fun)(...)
#' @export
globalise <- function(fun) {
  function(...) {
    expanded_mode(local_only(fun)(...))
  }
}

# This function evaluates an expression in "expanded" mode, i.e. with access
# to both local and global libraries (with local having the precedence).
# This can be used both to `globalise` a function while in pacman mode, or
# more generally to mix code which is supposed to run inside and outside the
# project (e.g. some `deployr` functions). Currently not exported, as `globalise`
# should cover most practical cases, but potentially useful for programming.
expanded_mode <- function(expr, path = NULL) {
  initial_lib <- expand_libs(path)
  pkgs <- loaded_pkgs(attached_only = FALSE)
  on.exit(expanded_exit(initial_lib, pkgs))

  eval(expr)
}

# helper functions ------------------------------------------------------------------------------

expand_libs <- function(path = NULL) {
  initial_lib <- .libPaths()
  local_lib <- if(is.null(path)) .libPaths() else local_lib_path(path)
  .libPaths(unique(c(local_lib, encap(global_lib))))
  initial_lib
}

expanded_exit <- function(initial_lib, pkgs) {
  pkgs <- setdiff(loaded_pkgs(attached_only = FALSE), pkgs)
  unload_pkgs(pkgs, quiet = TRUE)
  .libPaths(initial_lib)
}

# Ensure that a function can only be called in a certain pacman mode.

local_only <- function(fun) {
  fun <- match.fun(fun)

  function(...) {
    if (encap(mode) == "off") {
      stop("Function may only be called with pacman mode on.", call. = FALSE)
    }
    fun(...)
  }
}

global_only <- function(fun) {
  fun <- match.fun(fun)

  function(...) {
    if (encap(mode) == "on") {
      stop("Function may only be called with pacman mode off.", call. = FALSE)
    }
    fun(...)
  }
}
