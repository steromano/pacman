# cache the lib paths so that the lib tree can be reset to these
# set the variable local to FALSE: on start up the local library isn't being used.
.onLoad <- function(libname, pkgname) {
  encapsulate({
    message("Setting global libraries:")
    message(sapply(.libPaths(), function(x) sprintf("- %s\n", x)))
    global_lib <- .libPaths()
    mode <- "off"
    to_attach <- list(off = NA, on = NA)
  })
}
