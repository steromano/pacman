switch_mode <- function(mode) {
  mode_lib_path <- switch(mode,
    on = local_lib_path,
    off = function(...) {
      encap(global_lib)
    }
  )

  all_lib_paths <- switch(mode,
    on = function(path = ".") .libPaths(c(mode_lib_path(path), encap(global_lib))),
    off = function(...) .libPaths(c(.libPaths(), encap(global_lib)))
  )

  function(path = NULL) {
    if (encap(mode) == mode) {
      return(pacman_mode())
    }

    # Cache the attached packages and the working directory.
    # Unload all loaded packages.
    encap({
      to_attach[[mode]] <- other_pkgs(attached_only = TRUE)
      cwd[[mode]] <- getwd()
    })

    # Unload packages using all lib paths, then switch to the lib path of
    # the respective mode.
    .libPaths(all_lib_paths(path))
    unload_pkgs(other_pkgs(attached_only = FALSE))
    .libPaths(mode_lib_path(path))

    # Switch the mode, attach the packages previously loaded in that mode.
    # If the mode is getting switched to on, set the working directory to
    # the root of the pacman project, else set it to what it previously was in
    # the off mode.
    capsule$mode <- mode
    pacman_mode()
    encap(attach_pkgs(to_attach[[mode]]))
    capsule$wd(path)
  }
}

#' @export
on <- switch_mode("on")

#' @export
off <- switch_mode("off")

#' @export
pacman_mode <- function(quiet = FALSE) {
  if(!quiet) {
    message(sprintf("pacman mode %s, using libraries:", encap(mode)))
    lapply(.libPaths(), function(x) message(sprintf("- %s", x)))
  }
  invisible(encap(mode))
}

# helper_functions -----------------------------------------------------------------

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
  try(unloadNamespace("memoise"))
  try(unloadNamespace("digest"))
  try(unloadNamespace("devtools"))
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
