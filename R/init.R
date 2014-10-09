#' Initialise a pacman project

#' This function creates a pacman project in a given folder
#'
#' @param path Root directory of project
#' @param enter Boolean: enter pacman mode after init?
#' @export
init <- function(path = ".", enter = TRUE) {
  message("Initialising pacman project ...")
  dir.create(path, showWarnings = FALSE)
  if(file.exists(file.path(path, 'pacman'))) {
    message("Error: The project already contains a pacman folder")
    return(invisible(FALSE))
  }

  create_pacman_subdir(path)
  if(enter) {
    on(path)
    setwd(path)
  }

  invisible(TRUE)
}
