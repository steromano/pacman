# #' @export
# load_package_global <- function(pkg, with_imports = TRUE, ...) {
#   library(pkg, lib = capsule$global_lib, character.only = TRUE, ...)
#   if(with_imports) {
#     invisible(lapply(pkg_import_closure(pkg), load_namespace_global))
#   }
# }
#
# #' @export
# load_namespace_global <- function(pkg, ...) {
#   invisible(loadNamespace(pkg, lib = capsule$global_lib, ...))
# }
#
#
# # helper functions -----------------------------------------
# pkg_imports <- function(pkg) {
#   imports <- utils::packageDescription(pkg, lib.loc = capsule$global_lib)$Imports
#   if(is.null(imports)) {
#     return(character(0))
#   }
#   imports <- stringr::str_replace_all(imports, "\\\n", "")
#   imports <- stringr::str_replace_all(imports, "\\(.+?\\)", "")
#   stringr::str_trim(unlist(stringr::str_split(imports, ",")))
# }
#
# import_closure <- function(pkgs) {
#   expanded <- Reduce(union, lapply(pkgs, pkg_imports), pkgs)
#   if(length(setdiff(expanded, pkgs)) == 0) expanded
#   else import_closure(expanded)
# }
#
# pkg_import_closure <- function(pkg) setdiff(import_closure(pkg), pkg)
