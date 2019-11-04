#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

##' Convenience 'not-in' operator
##'
##' Complement of the built-in operator \code{\%in\%}. Returns the elements of \code{x} that are not in \code{y}.
##' @title \%nin\%
##' @param x vector of items
##' @param y vector of all values
##' @return logical vecotor of items in x not in y
##' @author Jason Heppler
##' @rdname nin
##' @examples
##' fruit <- c("apples", "oranges", "banana")
##' "apples" %nin% fruit
##' "pears" %nin% fruit
##' @export
"%nin%" <- function(x, y) {
  return( !(x %in% y) )
}

#'  Easily merge a data frame to a spatial data frame
#'
#'  The pages of StackOverflow are littered with questions about how to merge a regular data frame to a
#'  spatial data frame in R. The \code{merge} function from the sp package operates under a strict set of
#'  assumptions, which if violated will break your data. This function wraps a couple StackOverflow answers
#'  I've seen that work in a friendlier syntax.
#' @param spatial_data A spatial data frame to which you want to merge data.
#' @param data_frame A regular data frame that you want to merge to your spatial data.
#' @param by_sp The column name you'll use for the merge from your spatial data frame.
#' @param by_df The column name you'll use for the merge from your regular data frame.
#' @export
geo_join <- function(spatial_data, data_frame, by_sp, by_df) {

  spatial_data@data <- data.frame(spatial_data@data,
                                  data_frame[match(spatial_data@data[[by_sp]],
                                                   data_frame[[by_df]]), ])

  spatial_data

}

#'  Quick transform of spatial objects to WGS84
#'
#'  The function will use \code{spTransform} from the \code{rgdal} package to automatically transform your
#'  spatial data to the WGS84 geographic coordinate system.  As this is the coordinate system required for mapping
#'  with the \code{leaflet} package, it can come in handy when mapping spatial objects in R.
#' @param sp_object an R object of class \code{Spatial*} that you'd like to transform to WGS84.
#' @import rgdal
#' @export

transform_xy <- function(sp_object) {
  sp_xy <- spTransform(sp_object, CRS("+proj=longlat +datum=WGS84"))
  sp_xy
}

#'  Create nice-looking quantile labels for Leaflet mapping.
#'
#'  At present, the amazing \code{leaflet} package uses percentiles in its quantile legends; however, sometimes you
#'  want to show the actual values.  This function allows you to do just that.
#' @param vec The column of data that you are visualizing on your choropleth map
#' @param n The number of classes you've chosen
#' @export

quantile_labels <- function(vec, n) {
  qs <- round(quantile(vec, seq(0, 1, 1/n), na.rm = TRUE), 1)
  len <- length(qs) - 1
  qlabs <- c()
  for (i in 1:len) {
    j <- i + 1
    v <- paste0(as.character(qs[i]), "-", as.character(qs[j]))
    qlabs <- c(qlabs, v)
  }
  final_labs <- c(qlabs, "Data unavailable")
  final_labs
}

#' Removes tick marks from all axes
#'
#' @md
#' @export
remove_ticks <- function() {
  ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank())
}

##' Arrange ggplot2 plots in an arbitrary grid
##'
##' The function takes arguments of the form `list(plot, row(s),
##'     column(s))` where `plot` is a ggplot2 plot object, and the
##'     rows and columns identify an area of the grid that you want
##'     that plot object to occupy. See
##'     http://stackoverflow.com/questions/18427455/multiple-ggplots-of-different-sizes
##' @title Arrange ggplot2 plots in an arbitrary grid
##' @return A grid of ggplot2 plots
##' @author Extracted from the [wq] package
##' @param ... A series lists of of ggplot objects
##' @examples
##' library(ggplot2)
##' p1 <- qplot(x=wt,y=mpg,geom="point",main="Scatterplot of wt vs.
##'     mpg", data=mtcars)
##' p2 <- qplot(x=wt,y=disp,geom="point",main="Scatterplot of wt vs
##'     disp", data=mtcars)
##' p3 <- qplot(wt,data=mtcars)
##' lay_out(list(p1, 1:2, 1:4),
##'       list(p2, 3:4, 1:2),
##'       list(p3, 3:4, 3:4))
##' @export
lay_out = function(...) {
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))

  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]],
                                           layout.pos.col = x[[i]][[3]]))
  }
}

#' Minimal ggplot2 theme using the Roboto Condensed and Roboto Bold fonts
#'
#' @param base_size base font size
#' @param ... Other arguments passed to \code{theme_minimal}
#'
#' @details The Roboto Condensed and Roboto Bold fonts are both Google fonts;
#' they can be found at \url{https://fonts.google.com/specimen/Roboto+Condensed}
#' and \url{https://fonts.google.com/specimen/Roboto}. These fonts must be
#' installed locally on your computer for this theme to work.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'     geom_point() +
#'     labs(title = "A Lovely Plot",
#'          subtitle = "What can the subtitle tell us?") +
#'     theme_roboto()
#'}
#'
#' @export
theme_roboto <- function(base_size = 11, ...) {
  ret <- ggplot2::theme_minimal(base_family = "RobotoCondensed-Regular",
                                base_size = base_size, ...)
  ret$plot.title <- ggplot2::element_text(family="Roboto-Bold",
                                          size = rel(1.2),
                                          hjust = 0, vjust = 1,
                                          margin = ggplot2::margin(b = base_size / 2 * 1.2)
  )
  ret
}

#' A helper to rotate and adjust x-axis labels 90 degrees.
rotate_x_axis <- function() {
  ggplot2::theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))
}
