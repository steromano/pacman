% Generated by roxygen2 (4.0.2): do not edit by hand
\name{pacman_mode}
\alias{pacman_mode}
\title{Pacman mode}
\usage{
pacman_mode(quiet = FALSE)
}
\arguments{
\item{quiet}{Message the user or just return the mode (invisibly)?}
}
\description{
This function generates a \code{\link{message}} which informs the user whether pacman mode
is \code{\link{on}()} or \code{\link{off}()}, as well as the library trees within which
packages are looked for.
}
\details{
A project with root directory \code{<root>} can be treated as a pacman project if
  it has a subdirectory:
  \code{file.path(<root>, "pacman", "lib", R.version$platform, getRversion())}
  i.e. a local library. Use \code{\link{init}()} to save having to contstruct this manually.

  A pacman project can be leveraged to build a package using continuous integration,
  or to make a process deployable to production.
}

