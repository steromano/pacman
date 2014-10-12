#' Restore from source
#'
#' Restore the private library using source files obtained using \code{\link{get_source}()}.
#'
#' @param path Root directory of the project.
#' @export
restore_from_src <- function(path = ".") {
  # NB: if some libraries are loaded which the user is trying to restore from source,
  # then they will be prompted whether they would like to restart R. This is not good.
  # Maybe all namespaces should be unloaded at the start of this function call: they
  # can always be reloaded afterwards.
  path <- normalizePath(path, mustWork = TRUE)

  suppressWarnings(dir.create(local_lib_path(path), recursive = TRUE))

  on(path)
  on.exit(off(), add = TRUE)

  # create a local repository
  repos_create(path)

  # set global options to use it
  repos_old <- getOption("repos")
  pkg_type_old <- getOption("pkgType")
  options(repos = sprintf("file://%s", repos_path(path)), pkgType = "source")
  on.exit(options(repos = repos_old, pkyType = pkg_type_old), add = TRUE)
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
