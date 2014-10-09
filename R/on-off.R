#' Set library trees
#'
#' @param path Root directory of project.
#' @name lib_trees
NULL

#' @export
#' @rdname lib_trees
on <- function(path = ".", keep_loaded = NULL) {
  if (pacman_on()) {
    return(Negate(pacman_mode)())
  }
  path <- normalizePath(path, mustWork = TRUE)
  encapsulate(.to_load <- other_pkgs(all = FALSE))
  unload_pkgs(other_pkgs(all = TRUE), keep_loaded = keep_loaded)
  encapsulate({
    to_load <- .to_load
    local_mode <- TRUE
    rm(.to_load)
  })
  .libPaths(local_lib_path(path, check = TRUE))
  for(pkg in keep_loaded) {
    suppressMessages(load_from_global(pkg, character.only = TRUE))
  }
  pacman_mode()
}

#' @export
#' @rdname lib_trees
off <- function() {
  if (!pacman_on()) {
    return(pacman_mode())
  }
  encapsulate({
    .libPaths(global_lib)
    load_pkgs(to_load)
    to_load <- NULL
    local_mode <- FALSE
  })
  Negate(pacman_mode)()
}

#' @export
#' @rdname lib_trees
pacman_mode <- function() {
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
  pkgs <- Filter(function(x) !pkg_is_base(x) && ! (x %in% c("pacman", "devtools")), pkgs)
  if (length(pkgs) == 0) {
    return(NULL)
  }
  pkgs
}

load_pkgs <- function(pkgs) {
  # Reverse to preserve ordering of search path.
  for (pkg in rev(pkgs)) {
    message(paste("- Loading package", pkg))
    library(pkg, character.only = TRUE)
  }
}

unload_pkgs <- function(pkgs, keep_loaded = NULL) {
  for (pkg in pkgs) {
    if(paste0("package:", pkg) %in% search() && !(pkg %in% keep_loaded)) {
      message("- Unloading package ", pkg)
    }
    suppressMessages(devtools::unload(devtools::inst(pkg)))
  }
}

pkg_is_base <- function(x) {
  desc <- packageDescription(x)
  !is.null(desc$Priority) && desc$Priority == "base"
}
