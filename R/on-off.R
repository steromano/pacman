switch_mode <- function(path = ".") {
  # Cache the path. This won't be used if turning pacman mode off.
  capsule$path <- normalizePath(path, mustWork = TRUE)

  # Cache the attached packages (and the working directory ?).
  # Unload all loaded packages.
  encap(to_attach[[mode]] <- other_pkgs(attached_only = TRUE))

  # Unload packages using all lib paths, then switch to the lib path of
  # the respective mode.
  encap(all_lib_paths())
  unload_pkgs(other_pkgs(attached_only = FALSE))
  encap(mode_lib_path())

  # Switch the mode, attach the packages previously loaded in that mode.
  encap(switch_mode_impl())
  pacman_mode()
  encap(attach_pkgs(to_attach[[mode]]))
}

#' Turn pacman mode on or off
#'
#' @details These functions are used to turn pacman mode on and off. In addition to changing
#'   the \code{\link{.libPaths}()} for packages, they will restore the
#'   \code{\link{loadedNamespaces}()} and \code{\link{attach}ed} packages using the respective
#'   library trees.
#' @name on_off
#' @seealso \code{\link{pacman_mode}()} which allows the user to see which mode they are
#'   currently in.
NULL

#' @usage on(path = ".")
#' @param path Path to root of pacman project.
#' @export
#' @rdname on_off
on <- global_only(switch_mode)

#' @usage off()
#' @export
#' @rdname on_off
off <- local_only(switch_mode)

#' Pacman mode
#'
#' This function generates a \code{\link{message}} which informs the user whether pacman mode
#' is \code{\link{on}()} or \code{\link{off}()}, as well as the library trees within which
#' packages are looked for.
#'
#' @param quiet Message the user or just return the mode (invisibly)?
#' @details A project with root directory \code{<root>} can be treated as a pacman project if
#'   it has a subdirectory:
#'   \code{file.path(<root>, "pacman", "lib", R.version$platform, getRversion())}
#'   i.e. a local library. Use \code{\link{init}()} to save having to contstruct this manually.
#'
#'   A pacman project can be leveraged to build a package using continuous integration,
#'   or to make a process deployable to production.
#' @export
pacman_mode <- function(quiet = FALSE) {
  if(!quiet) {
    message(sprintf("pacman mode %s, using libraries:", encap(mode)))
    lapply(.libPaths(), function(x) message(sprintf("- %s", x)))
  }
  invisible(encap(mode))
}

# helper_functions -----------------------------------------------------------------

# Find the local library lib path

local_lib_path <- function(path = ".", check = FALSE) {
  path <- normalizePath(path, mustWork = TRUE)
  path <- file.path(path, "pacman", "lib", R.version$platform, getRversion())
  if (check && !file.exists(path)) {
    stop("Project does not have a valid private library.", call. = FALSE)
  }
  path
}

# Generate list of 'other' (not base packages, pacman or packrat) attached or
# or loaded and attached (dependent on argument).

other_pkgs <- function(attached_only) {
  pkgs <- Filter(Negate(to_keep), loaded_pkgs(attached_only))
  if (length(pkgs) == 0) {
    return(NA)
  }
  pkgs
}

to_keep <- function(pkg) {
  pkg_is_base(pkg) || pkg %in% c("pacman", "packrat")
}

pkg_is_base <- function(pkg) {
  desc <- packageDescription(pkg)
  !is.null(desc$Priority) && desc$Priority == "base"
}

loaded_pkgs <- function(attached_only) {
  pkgs <- attached_pkgs()
  if (!attached_only) {
    pkgs <- unique(c(pkgs, loadedNamespaces()))
  }
  pkgs
}

attached_pkgs <- function() {
  pkgs <- grep("^package:", search(), value = TRUE)
  sub("^package:", "", pkgs)
}

# When unloading or attaching packages, some NA's may have crept in, either from
# the status of to_attach in the capsule, or a call to other_pkgs().
# The idea is to filter the NA's and if the length of the output is 0, exit the function
# call prematurely with invisible(TRUE).

filter_na <- function(fun) {
  function(x, ...) {
    x <- Filter(Negate(is.na), x)
    if (length(x) == 0) {
      return(invisible(TRUE))
    }
    fun(x, ...)
  }
}

# Unload packages.

unload_pkgs <- filter_na(function(pkgs, quiet = FALSE) {
  if (!quiet) {
    message("Unloading packages:")
  }
  for (pkg in pkgs) {
    unload_pkg(pkg, quiet)
  }
  try_unload_namespace(c("memoise", "digest", "devtools"))
})

unload_pkg <- function(pkg, quiet) {
  if (!quiet) {
    if (pkg %in% attached_pkgs()) {
      message(sprintf("- %s (attached)", pkg))
    }
    else {
      message(sprintf("- %s (loaded)", pkg))
    }
  }
  suppressMessages(devtools::unload(devtools::inst(pkg)))
}

try_unload_namespace_single <- {
  function(pkg, ...) try(unloadNamespace(pkg), ...)
}

try_unload_namespace <- function(pkgs, ...) {
  lapply(pkgs, try_unload_namespace_single, ...)
}

# Attach packages.

attach_pkgs <- filter_na(function(pkgs, quiet = FALSE) {
  if (!quiet) {
    message("Attaching packages:")
  }
  # Reverse to preserve ordering of search path.
  for (pkg in rev(pkgs)) {
    try_attach_pkg(pkg, quiet)
  }
})

try_attach_pkg <- function(pkg, quiet) {
  if (!quiet) {
    message(sprintf("- package %s", pkg))
  }
  status <- suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE))
  if (!status) {
    message(sprintf("- Failed to attach package %s.", pkg))
  }
  invisible(status)
}
