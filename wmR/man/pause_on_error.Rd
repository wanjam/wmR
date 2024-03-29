% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wmR.oneliner.R
\name{pause_on_error}
\alias{pause_on_error}
\title{Debug on error behavior shortcut}
\usage{
pause_on_error(do_pause = NA)
}
\arguments{
\item{do_pause}{logical. enable or disable recovering upon errors. NA
(default) checks what the current state is and switches debugging on or off.}
}
\value{
Nothing.
}
\description{
sets global options to use \code{recover} or not upon non-
catastrophical errors. If used without input, it will work like a switch,
changing whatever the current state is to the opposite.
}
\examples{
\dontrun{
library(wmR)
pause_on_error(TRUE)
\donttest{sum('THREEPLUSFOUR')}
pause_on_error(FALSE)
\donttest{sum('THREEPLUSFOUR')}
}
}
\seealso{
\code{options(error)}
}
\author{
Wanja Mössing
}
