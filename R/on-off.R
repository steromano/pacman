#' @export
on <- function(path = ".", soft = FALSE) {
  local_lib_path <- local_lib_path(path, check = TRUE)
  if(local_lib_path %in% capsule$global_lib) {
    stop("Local library found in global libraries")
  }

  if(capsule$mode == "on") {
    if(soft) {
      .libPaths(c(local_lib_path, capsule$global_lib))
    }
    pacman_mode()
    return(invisible(FALSE))
  }
  encapsulate({
    to_attach[[mode]] <- other_pkgs(all = FALSE)
    mode <- "on"
    })
  unload_pkgs(other_pkgs(all = TRUE))
  .libPaths(if(soft) c(local_lib_path, capsule$global_lib) else local_lib_path)
  encapsulate(
    attach_pkgs(to_attach[["on"]])
  )
  pacman_mode()
}

#' @export
off <- function() {
  if(capsule$mode == "off") {
    pacman_mode()
    return(invisible(FALSE))
  }
  encapsulate({
    to_attach[[mode]] <- other_pkgs(all = FALSE)
    mode <- "off"
  })
  .libPaths(capsule$global_lib)
  unload_pkgs(other_pkgs(all = TRUE))
  encapsulate(
    attach_pkgs(to_attach[["off"]])
  )
  pacman_mode()
}

#' @export
pacman_mode <- function(quiet = FALSE) {
  if(!quiet) {
    message("pacman mode ", capsule$mode)
    message("Using libraries")
    lapply(.libPaths(), function(x) message(sprintf("- %s", x)))
  }
  invisible(capsule$mode)
}

# helper_functions -----------------------------

local_lib_path <- function(path = ".", check = FALSE) {
  path <- normalizePath(path, mustWork = TRUE)
  path <- file.path(path, "pacman", "lib", R.version$platform, getRversion())
  if (check && !file.exists(path)) {
    stop("Project does not have a valid private library.", call. = FALSE)
  }
  path
}


other_pkgs <- function(all) {
  pkgs <- grep("^package:", search(), value = TRUE)
  pkgs <- sub("^package:", "", pkgs)
  if (all) {
    pkgs <- unique(c(pkgs, loadedNamespaces()))
  }
  pkgs <- Filter(function(x) !pkg_is_base(x) && x != "pacman", pkgs)
  if (length(pkgs) == 0) {
    return(NA)
  }
  pkgs
}

pkg_is_base <- function(x) {
  desc <- packageDescription(x)
  !is.null(desc$Priority) && desc$Priority == "base"
}

unload_pkgs <- function(pkgs) {
  pkgs <- Filter(function(x) !is.na(x), pkgs)
  for (pkg in pkgs) {
    if(paste0("package:", pkg) %in% search()) {
      message("- Unloading package ", pkg)
    }
    suppressMessages(devtools::unload(devtools::inst(pkg)))
  }
  try(unloadNamespace("memoise"))
  try(unloadNamespace("digest"))
  try(unloadNamespace("devtools"))
}

attach_pkgs <- function(pkgs) {
  pkgs <- Filter(function(x) !is.na(x), pkgs)
  # Reverse to preserve ordering of search path.
  for (pkg in rev(pkgs)) {
    message(paste("- Attacing package", pkg))
    if(!suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE))) {
      message("-- Failed. Package not attached.")
    }
  }
}
