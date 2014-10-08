#' Create a packrat sub-directory
#' 
#' This function create a packrat sub-directory for a project.
#' 
#' @param path Root directory of project.
#' @export
create_packrat_subdir <- function(path = ".") {
  path <- normalizePath(path, mustWork = TRUE)
  copy_packrat_template(path)
}

# helper functions -------------------------------------------------------------------

copy_packrat_template <- function(path) {
  template <- system.file("templates", "packrat", package = "pacman")
  file.copy(template, path, recursive = TRUE)
  dir.create(local_lib_path(path), recursive = TRUE)
  invisible(TRUE)
}

