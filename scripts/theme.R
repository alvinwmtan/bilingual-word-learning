# library(extrafont)
# font_import()
# loadfonts()

theme_mikabr <- function(base_size = 12, base_family = "Open Sans") {
  ggplot2::`%+replace%`(
    ggplot2::theme_bw(base_size = base_size, base_family = base_family),
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank())
  )
}

# .font <- "Helvetica Neue"
# .font <- "Source Sans Pro"
.font <- "CMU Serif"
# .uni_font <- "Roboto Mono"
theme_set(theme_mikabr(base_family = .font))
# theme_update(plot.margin = margin(0, 0, 2, 0, "pt"),
#              legend.margin = margin(0, 0, 0, 0, "pt"))
