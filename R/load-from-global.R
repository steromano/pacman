#' @export
library_global <- function(pkg, ...) {
  eval(substitute(library(pkg, lib = capsule$global_lib, ...)))
}

#' @export
loadNamespace_global <- function(pkg, ...) {
  loadNamespace(pkg, lib = capsule$global_lib, ...)
}

