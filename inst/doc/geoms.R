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

## -----------------------------------------------------------------------------
GeomBlurrySmooth <- ggproto('GeomBlurrySmooth', GeomSmooth,
  setup_params = function(data, params) {
    params$max_range <- max(data$ymax - data$ymin)
    params
  },
  draw_group = function (data, panel_params, coord, max_range, ...) {
    # Create a raster object representing the width oof the ribbon
    sigma <- ((data$ymax - data$ymin) / max_range)^1.5
    sigma_raster <- as.raster(matrix(sigma, nrow = 1))
    
    # Use the annotate_raster geom to convert it to a raster that spans the x-range
    # of the line
    sigma_grob <- GeomRasterAnn$draw_panel(
      data, panel_params, coord, 
      raster = sigma_raster, 
      xmin = min(data$x), 
      xmax = max(data$x),
      ymin = -Inf,
      ymax = Inf
    )
    
    # Convert it to a reference layer
    ref_name <- paste0('GeomBlurrySmooth_<', data$group[1], '>')
    sigma_grob <- as_reference(sigma_grob, ref_name)
    
    # Figurer out the maximum sigma relative to the y scale
    max_sigma <- 0.5 * max_range / diff(panel_params$y$dimension())
    
    # Create a line grob using geom_line
    line_grob <- GeomLine$draw_panel(data, panel_params, coord)
    
    # Add variable blur. Turn off blur in the x-direction and use the calulated max sigma
    # in the y direction
    line_grob <- with_variable_blur(line_grob, ch_red(ref_name), x_scale = 0, y_scale = unit(max_sigma, 'npc'))
    
    # Return the two grobs combined, making sure that the reference grob comes first
    grid::gList(
      sigma_grob,
      line_grob
    )
  }
)

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) + 
  geom_smooth()

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) + 
  stat_smooth(geom = GeomBlurrySmooth, size = 5, alpha = 1)

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) + 
  as_reference(
    geom_rect(
      aes(xmin = min(displ), xmax = max(displ), ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE
    ),
    id = 'draw_area'
  ) +
  with_mask(
    stat_smooth(geom = GeomBlurrySmooth, size = 5, alpha = 1),
    mask = ch_alpha('draw_area')
  )

