#' Load fonts
#' @importFrom sysfonts font.add.google
#' @importFrom showtext showtext.auto
#' @export
load_fonts <- function() {
  font.add.google("Open Sans Condensed",  regular.wt = 300, bold.wt = 700)
  font.add.google("PT Sans Narrow")
  font.add.google("Lato")
  font.add.google("Roboto")
  font.add.google("Source Sans Pro")
  font.add.google("Roboto Condensed")
  showtext.auto()
  invisible()
}

#' My ggplot2 theme
#' @param base_family base_family
#' @param plot_title_family plot_title_family
#' @param plot_title_face plot_title_face
#' @param striptext_family striptext_family
#' @param striptext_face striptext_face
#' @param strip_inverse strip_inverse
#' @param base_size base_size
#' @import ggplot2
#' @export
theme_jah <- function(
  base_family        = "Source Sans Pro Light",
  plot_title_family  = "Source Sans Pro",
  plot_title_face    = "bold",
  striptext_family   = plot_title_family,
  striptext_face     = plot_title_face,
  strip_inverse      = FALSE,
  base_size          = 11
) {

  # init
  thm <- theme_minimal(
    base_size = base_size,
    base_family = base_family
  )

  # general
  thm <- thm +
    theme(
      legend.background = element_blank(),
      legend.key = element_blank()
    )

  # font
  thm <- thm +
    theme(
      plot.title = element_text(family = plot_title_family, face = plot_title_face, hjust = 0),
      strip.text = element_text(family = striptext_family, face = striptext_face, hjust = 0, size = base_size)
    )

  if (strip_inverse) {
    thm <- thm +
      theme(
        strip.background = element_rect(fill = "#858585", color = NA),
        strip.text = element_text(color = "white")
      )
  }

  thm

}


## Another theme
fte_theme <- function() {

  # Generate the colors for the chart procedurally with RColorBrewer

  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]

  # Begin construction of chart

  theme_bw(base_size=9) +

    # Set the entire chart region to a light gray color

    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +

    # Format the grid

    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +

    # Format the legend, but hide by default

    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +

    # Set title and axis labels, and format these and tick marks

    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    theme(text=element_text(family="Lato")) +

    # Plot margins

    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
