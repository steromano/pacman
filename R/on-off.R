#' Set library trees
#'
#' @param path Root directory of project.
#' @name lib_trees
NULL

#' @export
#' @rdname lib_trees
on <- function(path = ".") {
  if (pacman_on()) {
    return(!pacman_status())
  }
  path <- normalizePath(path, mustWork = TRUE)
  .libPaths(local_lib_path(path, check = TRUE))
  encapsulate({
    to_load <- other_pkgs(all = FALSE)
    unload_pkgs(other_pkgs(all = TRUE))
    local_mode <- TRUE
  })
  pacman_status()
}

#' @export
#' @rdname lib_trees
off <- function() {
  if (!pacman_on()) {
    return(pacman_status())
  }
  encapsulate({
    .libPaths(global_lib)
    load_pkgs(to_load)
    to_load <- NULL
    local_mode <- FALSE
  })
  !pacman_status()
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

other_pkgs <- function(all) {
  pkgs <- grep("^package:", search(), value = TRUE)
  pkgs <- sub("^package:", "", pkgs)
  if (all) {
    pkgs <- unique(c(pkgs, loadedNamespaces()))
  }
  pkgs <- Filter(function(x) !pkg_is_base(x) && x != "pacman", pkgs)
  if (length(pkgs) == 0) {
    return(NULL)
  }
  pkgs
}

load_pkgs <- function(pkgs) {
  # Reverse to preserve ordering of search path.
  for (pkg in rev(pkgs)) {
    library(pkg, character.only = TRUE)
  }
}

unload_pkgs <- function(pkgs) {
  for (pkg in pkgs) {
    pkg_ <- sprintf("package:%s", pkg)
    if (pkg_ %in% search()) {
      detach(pkg_, unload = TRUE, character.only = TRUE)
    } else {
      unloadNamespace(pkg)
    }
  }
}

pkg_is_base <- function(x) {
  desc <- packageDescription(x)
  !is.null(desc$Priority) && desc$Priority == "base"
}
