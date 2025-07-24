## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "ragg_png",
  fig.width = 6
)

## ----setup--------------------------------------------------------------------
library(ggfx)
library(ggplot2)

## ----message=FALSE------------------------------------------------------------
ggplot(mtcars, aes(x = mpg, y = disp)) +
  with_blur(
    geom_point(),
    sigma = unit(1, 'mm')
  ) + 
  geom_smooth()

## -----------------------------------------------------------------------------
p <- ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_point() + 
  geom_smooth()

with_blur(p, sigma = unit(1, 'mm'))

## -----------------------------------------------------------------------------
library(grid)

circle_left <- circleGrob(x = 0.25, y = 0.5, r = 0.2)
circle_right <- with_blur(circleGrob(x = 0.75, y = 0.5, r = 0.2),
                          sigma = unit(1, 'mm'))
grid.newpage()
grid.draw(circle_left)
grid.draw(circle_right)

## -----------------------------------------------------------------------------
checker <- expand.grid(x = 1:6, y = 1:6)
checker <- checker[checker$x %% 2 == checker$y %% 2, ]

ggplot() + 
  as_reference(
    geom_tile(aes(x = x, y = y), checker),
    id = 'pattern'
  ) + 
  with_blend(
    geom_text(aes(x = 3.5, y = 3.5, label = '🚀GGFX🚀'), size = 15),
    bg_layer = 'pattern',
    blend_type = 'xor'
  )

## -----------------------------------------------------------------------------
volcano_raster <- as.raster((volcano - min(volcano)) / diff(range(volcano)))

ggplot() + 
  with_blend(
    geom_text(aes(x = 3.5, y = 3.5, label = '🚀GGFX🚀'), size = 15),
    bg_layer = volcano_raster,
    blend_type = 'copy_green',
    alpha = 'src'
  )

## -----------------------------------------------------------------------------
wave <- function(width, height) {
  mat <- matrix(0, ncol = width, nrow = height)
  as.raster((sin(col(mat) / 5) + 1) / 2)
}

ggplot() + 
  with_blend(
    geom_tile(aes(x = x, y = y), checker),
    bg_layer = wave,
    blend_type = 'copy_red',
    alpha = 'src'
  )

## -----------------------------------------------------------------------------
ggplot() + 
  with_blend(
    geom_tile(aes(x = x, y = y), checker),
    bg_layer = wave,
    blend_type = 'copy_red',
    alpha = 'src',
    id = 'wave-checker'
  ) + 
  with_variable_blur(
    geom_abline(aes(intercept = -6:6, slope = 1)),
    x_sigma = ch_hue('wave-checker'),
    y_sigma = ch_luminance('wave-checker'),
    x_scale = unit(3, 'mm')
  )

