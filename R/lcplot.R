#' Plot formatted Lorenz Curves
#'
#' Plut one or multiple Lorenz curves using ggplot2
#'
#' @param lor.df A data frame with the same format as produced by
#' \code{\link{LcLong}}.
#' @param alpha Numeric describing the desired alpha for each polygon
#' @param eq.ray Logical TRUE if we want the 45 degree ray from
#' the origin displayed
#' @param ray.col Color for the ray from the origin
#' @param outline.col Color for the box outline. 
#' @return A \code{\link{ggplot}} object.
#' 
#' @import ggplot2
#' @export

gg_Lorenz <- function(lor.df, alpha = 1, eq.ray = FALSE,
                      ray.col = "blue", outline.col = "black") {
  
  area <- geom_ribbon(data = lor.df,
                      aes(x = x,
                          ymax = y,
                          ymin = 0,
                          fill = Name),
                      alpha = alpha)
  # Ugh, this is so stupid. All this to hide slashes
  dummy_fill <- geom_ribbon(data = lor.df,
                            aes(x = x,
                                ymax = y,
                                ymin = 0,
                                fill = Name),
                            alpha = 0,
                            colour = outline.col,
                            show_guide = FALSE)
  
  ray <- geom_line(
    data = data.frame(x = c(0,1), y = c(0,1), Fill = 0),
    aes(x = x, y = y),
    colour = ray.col, linetype = "dotted")
  
  p <- ggplot(NULL) + dummy_fill + area +
    guides(fill = guide_legend(override.aes = list(alpha = 1)))
  if(eq.ray) {
    p <- p + ray
  }
  return(p)
                
}

