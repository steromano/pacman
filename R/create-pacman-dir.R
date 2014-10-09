#' Create a packrat sub-directory
#'
#' This function create a packrat sub-directory for a project.
#'
#' @param path Root directory of project.
#' @export
create_pacman_subdir <- function(path = ".") {
  path <- normalizePath(path, mustWork = TRUE)
  copy_pacman_template(path)
}

# helper functions -------------------------------------------------------------------

copy_pacman_template <- function(path) {
  template <- system.file("templates", "pacman", package = "pacman")
  file.copy(template, path, recursive = TRUE)
  dir.create(local_lib_path(path), recursive = TRUE)
  invisible(TRUE)
}

