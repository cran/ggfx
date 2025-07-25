#' Supported object types
#'
#' The different filters provided by ggfx are applicable to a wide range of
#' object types. Rather than documenting how to use them with each type in every
#' documentation entry, the information is collected here. While the examples
#' will use [with_blur()] they are general and applicable to all filters in
#' ggfx.
#'
#' @section Method specific arguments:
#'
#' - `id`: A string that identifies the result of this filter, to be referenced
#'   by other filters in the same graphic.
#' - `include`: A logical flag that indicates whether the filtered image should
#'   be displayed. By default, the result will not be displayed if it is given
#'   an `id` (as it is assumed that it is meant for later use), but this can be
#'   overewritten by setting `include = TRUE`.
#' - `ignore_background`: Should the background of the plot be removed before
#'   applying the filter and re-added afterwards?
#' - `background`: A grob to draw below the result of the filter. Mainly for
#'   internal use for supporting `ignore_background`.
#'
#' @section Filtering layers:
#' This is perhaps the most common and obvious use of ggfx, and the one
#' show-cased in the respective docs of each filter. In order to apply a filter
#' to a ggplot2 layer you wrap it around the layer constructor (usually a
#' `geom_*()` function) and pass in additional parameters after it:
#'
#' ```r
#' ggplot(mtcars) +
#'   with_blur(
#'     geom_point(aes(x = mpg, y = disp)),
#'     sigma = 4
#'   )
#' ```
#'
#' Apart from the arguments specific to the filter, layer filters also take an
#' `id`, and `include` argument. Providing an id (as a string) will make this
#' filter be referable by other filters. By default this turns of rendering of
#' the result, but setting `include = TRUE` will turn rendering back on (while
#' still making it referable). Referable layers should **always** come before
#' whatever other layer ends up referring to them, since ggfx does not have
#' control over the rendering order. Not following this rule will have undefined
#' consequences (either an error or a weird plot - or maybe the correct result)
#'
#' @section Filtering layer references:
#' While the first argument to a filter is mostly some sort of graphic
#' generating object, it can also be a text string referring to another filter.
#' This allows you to string together filters, should you so choose. The big
#' caveat is that filtering a reference will always result in a layer - i.e. it
#' is not compatible outside of ggplot2.
#'
#' ```r
#' ggplot(mtcars) +
#'   with_blur(
#'     geom_point(aes(x = mpg, y = disp)),
#'     sigma = 4,
#'     id = 'blurred_points'
#'   ) +
#'   with_shadow(
#'     'blurred_points'
#'   )
#' ```
#'
#' @section Filtering guides:
#' ggplot2 does not only consist of layers - there are all sort of other graphic
#' elements around them. Guides are one such type of element and these can be
#' filtered by wrapping the filter around the guide constructor:
#'
#' ```r
#' ggplot(mtcars) +
#'   geom_point(aes(x = mpg, y = disp, colour = gear)) +
#'   guides(colour = with_blur(guide_colourbar(), sigma = 4))
#' ```
#'
#' There is a caveat here in that it is not possible to use this with the string
#' shorthand (i.e. `with_blur('colourbar')` won't work) — you have to use the
#' functional form.
#'
#' @section Filtering theme elements:
#' Theme elements, like guides, is another non-layer graphic that is amenable to
#' filtering. It can be done by wrapping the `element_*()` constructor with a
#' filter:
#'
#' ```r
#' ggplot(mtcars) +
#'   geom_point(aes(x = mpg, y = disp)) +
#'   ggtitle("A blurry title") +
#'   theme(plot.title = with_blur(element_text(), sigma = 4))
#' ```
#'
#' There is a caveat here as well. The filtering doesn't get carried through
#' inheritance so you cannot set filtering at a top-level element and expect all
#' child elements to be filtered.
#'
#' @section Filtering ggplots:
#' While you normally only want to add a filter to a part of the plot, it is
#' also possible to add it to everthing, simply by wrapping the filter function
#' around the plot. You can elect to remove the background element while
#' applying the filter and add it back on afterwards by setting
#' `ignore_background = TRUE` on the filter
#'
#' ```r
#' p <- ggplot(mtcars) +
#'   geom_point(aes(x = mpg, y = disp))
#'
#' with_blur(p, sigma = 4)
#' ```
#'
#' An alternative is to put the filter around the [`ggplot()`][ggplot2::ggplot]
#' call, which will have the same effect and may fit better with your plot
#' construction code
#'
#' ```r
#' with_blur(ggplot(mtcars), sigma = 4) +
#'   geom_point(aes(x = mpg, y = disp))
#' ```
#'
#' @section Filtering grobs:
#' At the lowest level, it is possible to apply a filter to a grob. This is what
#' powers all of the above at some level and that power is also available to
#' you. It is done in the same manner as all of the above, by wrapping the grob
#' in a filter:
#'
#' ```r
#' blurred_circle <- with_blur(circleGrob(), sigma = 4)
#'
#' grid.newpage()
#' grid.draw(blurred_circle)
#' ```
#'
#' As with layers, filters applied to grobs also take an `id` and `include`
#' argument and they have the same effect. It should be noted that it can be
#' difficult to grasp the rendering order of elements in a manually created grid
#' graphics, so take care when using filters that refer to each other as the
#' rule about the rendering order still applies.
#'
#' There are not a lot of people who use grid directly, but if you develop
#' ggplot2 extensions the ability to apply filters to grobs means that you can
#' create geoms with filters build right into them!
#'
#' @rdname object_support
#' @name object_support
#'
#' @return All filters will generally return a new version of the same object,
#' the only exception being filtering of rasters, functions, and references
#' which returns a Layer object
NULL
