capsule <- new.env(hash = FALSE)

encap <- function(expr) {
  expr <- substitute(expr)
  eval(expr, capsule)
}

encap({
  # These bindings allow a state of a mode to be kept when moving out of it
  # and then back into it.
  mode <- "off"
  to_attach <- list(off = NA, on = NA)
  cwd <- list(off = NA, on = NA)
  # When entering pacman mode on, the working directory is set to the root of the project.
  # When enetering pacman mode off, the working directory is set to what it previosuly
  # was in this mode.
  wd <- function(path) if (mode == "on") setwd(path) else setwd(cwd[[mode]])
})


#' Inspect the capsule
#'
#' @details Taking inspiration from the source code in \code{https://github.com/wch/R6},
#'   a capsule (internal environment) is used to hold meta data regarding the
#'   \code{\link{on}()} and \code{\link{off}()} modes. \code{inspect_capsule()} allows
#'   the user to inspect the contents of the capsule. It is intended for debugging only.
#' @export
inspect_capsule <- function() {
  as.list(capsule)
}

# Find the local library path

local_lib_path <- function(path = ".", check = FALSE) {
  path <- normalizePath(path, mustWork = TRUE)
  path <- file.path(path, "pacman", "lib", R.version$platform, getRversion())
  if (check && !file.exists(path)) {
    stop("Project does not have a valid private library.", call. = FALSE)
  }
  path
}

