capsule <- new.env(hash = FALSE)

encap <- function(expr) {
  expr <- substitute(expr)
  eval(expr, capsule)
}

encap({
  # These bindings allow a state of a mode to be kept when moving out of it
  # and then back into it. Initial state is off; with no packages to attach on load in
  # either state; and the path for the on mode is not known.
  mode <- "off"
  to_attach <- list(off = NA, on = NA)
  path <- NA

  # Utility function to make the intent of the following functions clearer.
  switching_to <- function() switch(mode, on = "off", off = "on")

  # Function for switching modes
  switch_mode_impl <- function() mode <<- switching_to()

  # Set the lib paths of the mode we are switching to.
  mode_lib_path <- function() {
    if (switching_to() == "on") {
      .libPaths(local_lib_path(path, check = TRUE))
    }
    if (switching_to() == "off") {
      .libPaths(global_lib)
    }
  }

  # Set the library trees to the local and global lib paths respectively.
  all_lib_paths <- function() {
    if (switching_to() == "on") {
      .libPaths(c(local_lib_path(path, check = TRUE), global_lib))
    }
    if (switching_to() == "off") {
      .libPaths(c(.libPaths(), global_lib))
    }
  }
})

#' Inspect the capsule
#'
#' @details Taking inspiration from the source code in \url{https://github.com/wch/R6},
#'   a capsule (internal environment) is used to hold meta data relating to the different pacman
#'   modes. \code{inspect_capsule()} allows the user to inspect the contents of the capsule.
#'   It is intended for debugging only.
#' @seealso \code{\link{pacman_mode}()} which allows the user to see which mode they are
#'   currently in.
#' @export
inspect_capsule <- function() {
  as.list(capsule)
}
