#' Set library trees
#'
#' @param path Root directory of project.
#' @name lib_trees
NULL

#' @export
#' @rdname lib_trees
on <- function(path = ".") {
  if (pacman_on()) {
    return(invisible(FALSE))
  }
  path <- normalizePath(path, mustWork = TRUE)
  .libPaths(local_lib_path(path, check = TRUE))
  encapsulate({
    to_load <- unload_pkgs(other_pkgs())
    local_mode <- TRUE
  })
  # path not found in encapsulate
  capsule$proj_root <- path
  invisible(TRUE)
}

#' @export
#' @rdname lib_trees
off <- function() {
  if (!pacman_on()) {
    return(invisible(FALSE))
  }
  .libPaths(capsule$global_lib)
  load_pkgs(capsule$to_load)
  encapsulate({
    to_load <- NULL
    local_mode <- FALSE
  })
  invisible(TRUE)
}

#' @export
#' @rdname lib_trees
pacman_status <- function() {
  if (pacman_on()) {
    message("Using local libraries:")
  } else {
    message("Using global libraries:")
  }
  lapply(.libPaths(), function(x) message(sprintf("- %s", x)))
  invisible(pacman_on())
}

pacman_on <- function() {
  capsule$local_mode
}

# helper functions -------------------------------------------------------------------

local_lib_path <- function(path = ".", check = FALSE) {
  path <- normalizePath(path, mustWork = TRUE)
  path <- file.path(path, "packrat", "lib", R.version$platform, getRversion())
  if (check && !file.exists(path)) {
    stop("Project does not have a valid private library.", call. = FALSE)
  }
  path
}

local_libs <- function(path) {
  setNames(nm = list.files(local_lib_path(path)))
}

load_pkgs <- function(pkgs) {
  # Reverse to preserve ordering of search path.
  for (pkg in rev(pkgs)) {
    library(pkg, character.only = TRUE)
  }
}

unload_pkgs <- function(pkgs) {
  for (pkg in sprintf("package:%s", pkgs)) {
    detach(pkg, unload = TRUE, character.only = TRUE)
  }
  pkgs
}

# Need better method here: e.g. devtools namespace is loaded but not attached when building
# a package. This means it is not attached so when you try and install it, the user will
# be asked if they want to restart R.
other_pkgs <- function() {
  # Not sure whether to look at loadedNamespaces() too?
  pkgs <- grep("^package:", search(), value = TRUE)
  pkgs <- sub("^package:", "", pkgs)
  Filter(function(x) !pkg_is_base(x) && x != "pacman", pkgs)
}

pkg_is_base <- function(x) {
  desc <- packageDescription(x)
  !is.null(desc$Priority) && desc$Priority == "base"
}
