#' Initialise a pacman project
#'
#' @param path Root directory of project
#' @param enter Boolean: enter pacman mode after init?
#' @name pacman_project
NULL

#' @export
#' @rdname pacman_project
init <- function(path = ".", enter = TRUE) {
  message("Initialising pacman project ...")

  dir.create(path, showWarnings = FALSE)
  if (file.exists(file.path(path, 'pacman'))) {
    message("Error: The project already contains a pacman folder")
    return(invisible(FALSE))
  }

  create_pacman_subdir(path)

  if (enter) {
    on(path)
    setwd(path)
  }

  invisible(TRUE)
}

#' @export
#' @rdname pacman_project
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
