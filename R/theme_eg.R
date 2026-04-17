#' @title ggplot2 Theme for Escapement Goal Analysis Figures
#'
#' @description
#' This function contains the basic theme elements to be repeated across the
#' escapement goal figures. It uses theme_bw() as a base
#'
#' @param base_size The base font size, in points.
#' @param ... Other ggplot theme arguments to be passed.
#'
#' @returns A `ggplot2` theme object to be added to a plot.
#'
#' @examples
#' \donttest{
#'   library(ggplot2)
#'   ggplot(mtcars, aes(wt, mpg)) +
#'     geom_point() +
#'     theme_eg()
#' }
#' @export

theme_eg <- function(base_size = 16, ...) {
  ggplot2::theme_bw(base_size = base_size) +
  ggplot2::theme(text = ggplot2::element_text(family = "sans"),
               plot.caption = ggplot2::element_text(
                 hjust = 0,
                 size = 10),
               plot.subtitle = ggplot2::element_text(size = 10),
               plot.caption.position = "plot",
               legend.position = "none")
}
