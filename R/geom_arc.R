#' Curved lines.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "arc")}
#'
#' @inheritParams geom_point
#' @param arrow specification for arrow heads, as created by arrow()
#' @param lineend Line end style (round, butt, square)
#' @export
#' @examples
#' library(grid) # needed for arrow function
#'
#' # Adding arcs
#' library(grid) # needed for arrow function
#' b <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' b + geom_arc(aes(x = 2, y = 15, xend = 2, yend = 25))
#' b + geom_arc(aes(x = 2, y = 15, xend = 3, yend = 15))
#' b + geom_arc(aes(x = 5, y = 30, xend = 3.5, yend = 25),
#'    arrow = arrow(length = unit(0.5, "cm")))
geom_arc <- function (mapping = NULL, 
                      data = NULL, 
                      stat = "identity",
                      position = "identity", 
                      arrow = NULL, 
                      shape=1,
                      lineend = "butt", 
                      na.rm = FALSE, 
                      ...) {
  GeomArc$new(mapping = mapping, 
              data = data, 
              stat = stat,
              position = position, 
              arrow = arrow, 
              lineend = lineend, 
              na.rm = na.rm, 
              ...)
}

GeomArc <- proto(Geom, {
  objname <- "arc"

  draw <- function(., data, scales, coordinates, arrow = NULL, 
                   lineend = "butt", na.rm = FALSE, ...) {

    data <- remove_missing(data, na.rm = na.rm,
      c("x", "y", "xend", "yend", "linetype", "size", "curvature"),
        name = "geom_arc")
    
    if (empty(data)) return(zeroGrob())

    if (is.linear(coordinates)) {
      
      # Note: curveGrob does not support multiple values for the curvature 
      # argument, but we want to be able to set it using aes(). We thus create
      # separate grobs (one for each curvature value), that we combine using 
      # gList
      dat <- coord_transform(coordinates, data, scales)
      grob_list <- sapply(seq.int(nrow(dat)), 
                          function(line, dat) { 
                              with(dat[line, ], 
                                  curveGrob(x, y, xend, yend, 
                                            default.units="native", 
                                            curvature = curvature, 
                                            ncp = 10, 
                                            square = FALSE,
                                            gp = gpar(col=alpha(colour, alpha), 
                                                      fill = alpha(colour, alpha),
                                                      lwd=size * .pt, 
                                                      lty=linetype, 
                                                      lineend = lineend),
                                            arrow = arrow))
                          }, dat = dat, simplify = FALSE)
      
      return(do.call(gList, grob_list))
    }
  }
  
  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "xend", "yend")
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, 
                                 alpha = NA, curvature=grid::arcCurvature(60))
  guide_geom <- function(.) "path"
})

