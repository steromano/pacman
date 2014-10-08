capsule <- new.env(hash = FALSE)

encapsulate <- function(expr) {
  expr <- substitute(expr)
  eval(expr, capsule)
}