#' @export

load_from_global <- function(pkg, ...) {
  eval(substitute(library(pkg, lib = capsule$global_lib, ...)))
}
