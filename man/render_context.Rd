% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raster-helpers.R
\name{render_context}
\alias{render_context}
\alias{viewport_location}
\alias{index_raster}
\alias{get_raster_area}
\alias{set_raster_area}
\alias{get_viewport_area}
\alias{set_viewport_area}
\alias{viewport_is_clipping}
\alias{current_resolution}
\alias{to_pixels}
\alias{from_pixels}
\title{Rendering information}
\usage{
viewport_location()

index_raster(raster, cols, rows)

get_raster_area(raster, xmin, ymin, xmax, ymax)

set_raster_area(raster, value, xmin, ymin)

get_viewport_area(raster)

set_viewport_area(raster, value)

viewport_is_clipping()

current_resolution()

to_pixels(x, y_axis = FALSE, location = FALSE)

from_pixels(x)
}
\arguments{
\item{raster}{A \code{raster} or \code{nativeRaster} object}

\item{cols, rows}{Column and row indices}

\item{xmin, ymin, xmax, ymax}{Boundaries of the area in pixels. \{0,0\} is the
top-left corner}

\item{value}{An object of the same type as \code{raster}}

\item{x}{A numeric or unit object}

\item{y_axis}{is the unit pertaining to the y-axis? Defaults to \code{FALSE} (i.e.
it is measured on the x-axis)}

\item{location}{is the unit encoding a location? Defaults to \code{FALSE} (i.e. it
is encoding a dimension). Pixel locations are encoded based on a top-left
starting point, as opposed to grid's bottom-left coordinate system. This
means that y-axis locations will flip around when converted to pixels.}
}
\value{
Depends on the function - see details.
}
\description{
These utility functions can help when creating custom filters (using
\code{\link[=with_custom]{with_custom()}}) as they can provide information about the current rendering
context.
}
\details{
\itemize{
\item \code{viewport_location()}: Returns the bounding box defining the current
viewport in pixels in the order \code{xmin}, \code{ymin}, \code{xmax}, \code{ymax}
\item \code{index_raster()}: Is a version of the classic \verb{[,]} indexing that is aware
of the row-major order of rasters
\item \code{get_raster_area()}: Extracts an area of a raster based on a bounding box
\item \code{set_raster_area()}: Sets an area of a raster to a new raster value
\item \code{get_viewport_area()}: A version of \code{get_raster_area()} that specifically
extract the area defined by the current viewport
\item \code{set_viewport_area()}: A version of \code{set_raster_area()} that specifically
sets the area defined by the current viewport
\item \code{viewport_is_clipping()}: Returns \code{TRUE} if the current viewport has
clipping turned on
\item \code{current_resolution()}: Returns the resolution of the active device in ppi
(pixels-per-inch)
\item \code{to_pixels(x)}: Converts \code{x} to pixels if \code{x} is given as a unit object. It
is assumed that x encodes a dimension and not a location. If \code{x} is a
numeric it is assumed to already be in pixels
\item \code{from_pixels}: Converts a numeric giving some pixel dimension to a unit
object.
}
}
\examples{
# These functions are intended to be used inside filter functions, e.g.
library(ggplot2)

flip_raster <- function(raster, horizontal = TRUE) {
  # Get the viewport area of the raster
  vp <- get_viewport_area(raster)

  # Get the columns and rows of the raster - reverse order depending on
  # the value of horizontal
  dims <- dim(vp)
  rows <- seq_len(dims[1])
  cols <- seq_len(dims[2])
  if (horizontal) {
    cols <- rev(cols)
  } else {
    rows <- rev(rows)
  }

  # change the order of columns or rows in the viewport raster
  vp <- index_raster(vp, cols, rows)

  # Assign the modified viewport back
  set_viewport_area(raster, vp)
}

ggplot() +
  with_custom(
    geom_text(aes(0.5, 0.75, label = 'Flippediflop!'), size = 10),
    filter = flip_raster,
    horizontal = TRUE
  )

}
