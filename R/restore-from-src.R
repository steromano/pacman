#' Restore the private library from the local source files
#'
#' This function is similar to \code{packrat::\link[packrat]{restore}()},
#' but it installs the packages from the sources in \code{pacman/src}, instead of
#' obtaining them remotely.
#'
#' @param path Root directory of the project.
#' @export
restore_from_src <- function(path = ".") {
  path <- normalizePath(path, mustWork = TRUE)

  suppressMessages(dir.create(local_lib_path(path), recursive = TRUE))

  on(path)
  on.exit(off(), add = TRUE)

  # create a local repository
  repos_create(path)

  # set global options to use it
  .repos <- getOption("repos")
  .pkgType <- getOption("pkgType")
  options(repos = sprintf("file://%s", repos_path(path)), pkgType = "source")
  on.exit(options(repos = .repos, pkyType = .pkgType), add = TRUE)
  on.exit(unlink(repos_path(path), recursive = TRUE), add = TRUE)

  # install packages from local repository
  for (pkg in pkg_names_from_src(path)) {
    install.packages(pkg)
  }
}

# helper functions ------------------------------------------------------------------

repos_create <- function(path) {
  contrib <- file.path(repos_path(path), "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  file.copy(tarballs(path), contrib)
  tools::write_PACKAGES(contrib)
}

pkg_names_from_src <- function(path) {
  files <- list.files(src_path(path), full.name = FALSE)
  sub("_.*", "", files)
}

repos_path <- function(path) {
  file.path(path, "pacman", "repos")
}
tarballs <- function(path) {
  list.files(
    file.path(path, "pacman", "src"),
    pattern = "*.tar.gz",
    recursive = TRUE,
    full.names = TRUE
  )
}
