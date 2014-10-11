#' Call a global function
#'
#' Call a function using the global libraries (in addition to the local ones) when in pacman
#' mode, without polluting the local namespace.
#'
#' @usage globalise(pkg::fun)(...)
#' @export
globalise <- function(fun) {
  function(...) {
    local_lib <- add_global_lib()
    pkgs <- loaded_pkgs(attached_only = FALSE)
    on.exit(globalise_exit(local_lib, pkgs))

    local_only(fun)(...)
  }
}

# helper functions ------------------------------------------------------------------------------

add_global_lib <- function() {
  local_lib <- .libPaths()
  encap(.libPaths(unique(c(.libPaths(), global_lib))))
  local_lib
}

globalise_exit <- function(local_lib, pkgs) {
  pkgs <- setdiff(loaded_pkgs(attached_only = FALSE), pkgs)
  unload_pkgs(pkgs, quiet = TRUE)
  .libPaths(local_lib)
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
