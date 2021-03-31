## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "ragg_png",
  fig.width = 6
)

## ----setup--------------------------------------------------------------------
library(ggfx)

## -----------------------------------------------------------------------------
native_rep <- farver::encode_native('#45fe2a')
native_rep

farver::decode_native(native_rep)

## -----------------------------------------------------------------------------
library(ggplot2)
speckle <- function(x, colour, proportion) {
  raster_dim <- dim(x)
  n_pixels <- prod(raster_dim)
  n_speckles <- n_pixels * proportion
  x[sample(length(x), n_speckles)] <- farver::encode_native(colour)
  x
}

ggplot(mtcars, aes(x = factor(gear), y = disp)) +
  with_custom(
    geom_boxplot(),
    filter = speckle,
    colour = 'forestgreen',
    proportion = 0.05
  )

## -----------------------------------------------------------------------------
fill_patch_naive <- function(x, colour, x_range, y_range) {
  cols <- seq(x_range[1], x_range[2])
  rows <- seq(y_range[1], y_range[2])
  cat(dim(x))
  x[rows, cols] <- farver::encode_native(colour)
  x
}
ggplot(mtcars, aes(x = factor(gear), y = disp)) +
  with_custom(
    geom_boxplot(),
    filter = fill_patch_naive,
    colour = 'forestgreen',
    x_range = c(80, 250),
    y_range = c(50, 100)
  )

## -----------------------------------------------------------------------------
fill_patch <- function(x, colour, x_range, y_range) {
  patch <- get_raster_area(
    raster = x, 
    xmin = x_range[1], 
    ymin = y_range[1],
    xmax = x_range[2],
    ymax = y_range[2]
  )
  patch[] <- farver::encode_native(colour)
  set_raster_area(
    raster = x,
    value = patch,
    xmin = x_range[1], 
    ymin = y_range[1]
  )
}

ggplot(mtcars, aes(x = factor(gear), y = disp)) +
  with_custom(
    geom_boxplot(),
    filter = fill_patch,
    colour = 'forestgreen',
    x_range = c(80, 250),
    y_range = c(50, 100)
  )

## -----------------------------------------------------------------------------
fill_patch <- function(x, colour, x_range, y_range) {
  x_range <- to_pixels(x_range, location = TRUE)
  y_range <- sort(to_pixels(y_range, y_axis = TRUE, location = TRUE))
  patch <- get_raster_area(
    raster = x, 
    xmin = x_range[1], 
    ymin = y_range[1],
    xmax = x_range[2],
    ymax = y_range[2]
  )
  patch[] <- farver::encode_native(colour)
  set_raster_area(
    raster = x,
    value = patch,
    xmin = x_range[1], 
    ymin = y_range[1]
  )
}

ggplot(mtcars, aes(x = factor(gear), y = disp)) +
  with_custom(
    geom_boxplot(),
    filter = fill_patch,
    colour = 'forestgreen',
    x_range = unit(c(0.3, 0.7), 'npc'),
    y_range = unit(c(0.4, 0.6), 'npc')
  )

## -----------------------------------------------------------------------------
flip_image <- function(x, horizontal = TRUE) {
  dims <- dim(x)
  rows <- seq_len(dims[1])
  cols <- seq_len(dims[2])
  if (horizontal) {
    cols <- rev(cols)
  } else {
    rows <- rev(rows)
  }
  index_raster(x, cols, rows)
}

ggplot(mtcars, aes(x = factor(gear), y = disp)) +
  with_custom(
    geom_boxplot(aes(fill = as.factor(gear))),
    filter = flip_image
  )

## -----------------------------------------------------------------------------
flip_image <- function(x, horizontal = TRUE) {
  vp <- get_viewport_area(x)
  dims <- dim(vp)
  rows <- seq_len(dims[1])
  cols <- seq_len(dims[2])
  if (horizontal) {
    cols <- rev(cols)
  } else {
    rows <- rev(rows)
  }
  vp <- index_raster(vp, cols, rows)
  set_viewport_area(x, vp)
}

ggplot(mtcars, aes(x = factor(gear), y = disp)) +
  with_custom(
    geom_boxplot(aes(fill = as.factor(gear))),
    filter = flip_image
  )

## -----------------------------------------------------------------------------
implode <- function(x, factor = 0.5) {
  vp <- magick::image_read(get_viewport_area(x))
  vp <- magick::image_implode(vp, factor)
  set_viewport_area(x, as.raster(vp, native = TRUE))
}

ggplot(mtcars, aes(x = factor(gear), y = disp)) +
  with_custom(
    geom_boxplot(aes(fill = as.factor(gear))),
    filter = implode
  )

