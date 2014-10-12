#' Clean the default library of user-installed packages
#'
#' This function moves all the non-system packages (those with \code{"Priority" = NA}) from the
#' default library \code{\link{.Library}} to the user library specified in the environment
#' variable \code{\link{R_LIBS}}. If a package is present in both libraries, it is just removed
#' from the default library.
#'
#' @export
clean_default_library <- function() {
  user_library <- Sys.getenv("R_LIBS")
  if(!file.exists(user_library) || !file.info(user_library)$isdir) {
    stop("Invalid user library. Please set your user library by adding 'R_LIBS = <user library> to .Renviron'")
  }
  all_pkgs <- utils::installed.packages(.Library)
  user_pkgs <- rownames(all_pkgs[is.na(all_pkgs[, "Priority"]), , drop = F])

  if(is.null(user_pkgs)) {
    message("default library clean")
    return(invisible(TRUE))
  }

  message(sprintf("The following packages will be removed from %s", .Library))
  message(sprintf("and placed in %s (if not already present):", user_library))
  message(paste0(sapply(user_pkgs, sprintf, fmt = "- %s"), collapse = "\n"))

  if(readline("Continue? [Y/n] ") == "Y") {
    for (pkg in user_pkgs) {
      if(file.exists(file.path(user_library, pkg))) {
        system(sprintf("rm -r %s", file.path(.Library, pkg)))
        message(sprintf("- %s found in %s and removed from default library", pkg, user_library))
      }
      else {
        system(sprintf("mv %s %s", file.path(.Library, pkg), user_library))
        message(sprintf("- %s moved to %s", pkg, user_library))
      }
    }
  }
  invisible(TRUE)
}

