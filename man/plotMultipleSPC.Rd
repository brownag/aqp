% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMultipleSPC.R
\name{plotMultipleSPC}
\alias{plotMultipleSPC}
\title{Plot Multiple SoilProfileCollection Objects}
\usage{
plotMultipleSPC(
  spc.list,
  group.labels,
  args = rep(list(NA), times = length(spc.list)),
  arrow.offset = 2,
  bracket.base.depth = 95,
  ...
)
}
\arguments{
\item{spc.list}{a list of \code{SoilProfileCollection} objects}

\item{group.labels}{a vector of group labels, one for each
\code{SoilProfileCollection} object}

\item{args}{a list of arguments passed to \code{plotSPC}, one for each
\code{SoilProfileCollection} object}

\item{arrow.offset}{vertical offset in depth from base of start / end
profiles and group bracket arrows}

\item{bracket.base.depth}{baseline depth used for group brackets}

\item{\dots}{additional arguments to the first call to \code{plotSPC}}
}
\description{
Combine multiple SoilProfileCollection objects into a single profile sketch,
with annotated groups.
}
\details{
See examples below for usage.
}
\note{
Multiple color legends for thematic profile sketches are not currently
supported, use with caution.
}
\examples{

# load sample data
data(sp3)
data(sp4)

# convert soil colors
sp3$h <- NA ; sp3$s <- NA ; sp3$v <- NA
sp3.rgb <- with(sp3, munsell2rgb(hue, value, chroma, return_triplets=TRUE))
sp3[, c('h','s','v')] <- t(with(sp3.rgb, rgb2hsv(r, g, b, maxColorValue=1)))

# promote to SoilProfileCollection
depths(sp3) <- id ~ top + bottom
depths(sp4) <- id ~ top + bottom

# combine into a list
spc.list <- list(sp3, sp4)

# plot multiple SPC objects, with list of named arguments for each call to plotSPC
par(mar=c(1,1,3,3))
plotMultipleSPC(spc.list, group.labels=c('Collection 1', 'Collection 2'),
args=list(list(name='name', id.style='top'),
list(name='name', id.style='side')), bracket.base.depth=120)

}
\seealso{
\code{\link{profileGroupLabels}}
}
\author{
D.E. Beaudette and Ben Marshall
}
\keyword{hplots}
