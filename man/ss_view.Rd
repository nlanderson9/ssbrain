% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssbrain_views.R
\name{ss_view}
\alias{ss_view}
\title{Set a Viewpoint}
\usage{
ss_view(side = "lateral", rotation = c(0, 90, 90))
}
\arguments{
\item{side}{Which side of the brain to show, either "lateral" or "medial". Defaults to "lateral".}

\item{rotation}{The rotation applied to the \code{side} chosen. Either a pre-defined rotation (e.g. "orbitofrontal"), or a vector of x/y/z angles (e.g. c(90,120,90)).}
}
\description{
This function sets the view of the brain in the viewer window. It determines if a lateral or medial view is used, and the x/y/z axis rotation of the brain.
}
\details{
Note: \code{ss_view} is the only ss_* function that \strong{overwrites} any prexisting view, rather than adding on additional elements.
}
\examples{
\dontrun{
my_brain = ss_surf(surf="fsaverage6") +
  ss_view(side="lateral", rotation = "inferior_temporal")

my_brain = ss_surf(surf="fsaverage6")

my_brain_new = my_brain +
  ss_view(side="medial", rotation = c(30,90,90))
}

}
