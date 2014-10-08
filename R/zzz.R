# cache the lib paths so that the lib tree can be reset to these
# set the variable local to FALSE: on start up the local library isn't being used.
.onLoad <- function(libname, pkgname) {
  encapsulate({
    global_lib <- .libPaths()
    local_mode <- FALSE
  })
}