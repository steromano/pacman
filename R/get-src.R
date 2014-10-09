#' Get the source files for a project
#'
#' @param path Root directory of the project.
#' @export
get_src <- function(path = ".") {
  path <- normalizePath(path, mustWork = TRUE)
  pkgs <- local_libs(path)
  lapply(pkgs, get_src_single, path = path)
}

# helper functions --------------------------------------------------------------------

# Source for a single package.
get_src_single <- function(path, pkg) {
  desc <- packageDescription(pkg, local_lib_path(path))
  if (identical(desc$Repository, "CRAN")) {
    get_cran_single(path, desc)
  } else if (identical(desc$RemoteType, "github")) {
    get_github_single(path, desc)
  } else {
    stop("Don't know how to get source for: ", pkg, call. = FALSE)
  }
}

# Source if the package is on CRAN.
# Sometimes the source version may be different to the binary.
get_cran_single <- function(path, desc) {
  cran_pkgs <- available.packages(type = "source")
  src <- cran_pkgs[cran_pkgs[, "Package"] == desc$Package, ]
  if (src["Version"] != desc$Version) {
    warning(sprintf(
      "For package: %s, binary version is: %s, source version is %s.",
      desc$Package, desc$Version, src["Version"]
    ), call. = FALSE)
  }
  dir_url <- contrib.url("http://cran.rstudio.com", "source")
  base_url <- sprintf("%s_%s.tar.gz", desc$Package, src["Version"])
  url <- sprintf("%s/%s", dir_url, base_url)
  invisible(download.file(url, src_path(path, base_url)))
}

# Source if the package is on github.
get_github_single <- function(path, desc) {
  url <- sprintf(
    "https://%s/repos/%s/%s/zipball/%s",
    desc$RemoteHost, desc$RemoteUsername, desc$RemoteRepo, desc$RemoteRef
  )
  base_name <- sprintf("%s_%s", desc$Package, desc$Version)
  zip_file <- src_path(path, sprintf("%s.zip", base_name))

  devtools:::download(zip_file, url)
  on.exit(unlink(zip_file), add = TRUE)

  unzip(zip_file, exdir = src_path(path))
  unzip_dir <- list.dirs(src_path(path), recursive = FALSE)
  on.exit(unlink(unzip_dir, recursive = TRUE), add = TRUE)

  tar_file <- sprintf("%s.tar.gz", base_name)
  tar(src_path(path, tar_file), files = unzip_dir)
}

# path to source files in project
src_path <- function(path, ...) {
  file.path(path, "pacman", "src", ...)
}
