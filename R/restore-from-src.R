#' Restore the private library from the local source files
#'
#' This function is similar to \code{packrat::\link[packrat]{restore}()},
#' but it installs the packages from the sources in \code{packrat/src}, instead of 
#' obtaining them remotely.
#'
#' @param path Root directory of the project.
#' @export

restore_from_src_impl <- function(path = ".") {
  path <- normalizePath(path)
  
  opts <- lapply(list(repos = "repos", pkgType = "pkgType"), getOption)
  on.exit(do.call(options, opts))
  
  repo_create(path)
  options(repos = paste0("file://", repo_(path)),
          pkgType = "source")
  
  for (pkg in list.files(src_(path), full.name = FALSE)) {
    install.packages(pkg)
  }
  
  system(paste("rm -r", repo_(path)))
}


# helper functions -------------------------------------

in_packrat <- function(...) {
  function(path)
    file.path(path, "packrat", ...)
}

src_ <- in_packrat("src")

lib_ <- in_packrat("lib", R.version$platform, getRversion())

repo_ <- in_packrat("repo")


repo_create <- function(path) {
  contrib <- file.path(repo_(path), "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  file.copy(tarballs(path), contrib)
  tools::write_PACKAGES(contrib)
}

tarballs <- function(path) {
  list.files(
    file.path(path, "packrat", "src"),
    pattern = "*.tar.gz",
    recursive = TRUE,
    full.names = TRUE
  )
}
