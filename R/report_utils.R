rag_tile <- function(status, file) {
  library(ggplot2)
  cols <- c(GREEN = "#1EA672", AMBER = "#FFB200", RED = "#D64545")
  col <- cols[[status]]
  if (is.null(col)) {
    col <- "#6C757D"
  }
  g <- ggplot() +
    theme_void() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = col) +
    annotate("text", x = .5, y = .5, label = status, size = 10, fontface = "bold", color = "white")
  ggplot2::ggsave(file, g, width = 3, height = 2, dpi = 200)
}
