forestplot <- function(df,
                       title = NULL,
                       xlabel = names(df)[2],
                       ylabel = "",
                       xlim = NA,
                       mark = NA,
                       footnote = NULL) {

  index <- NULL
  df$index <- 1:nrow(df)

  p <- ggplot(
    df,
    aes(
      y = index,
      x = get(names(df)[2]),
      xmin = get(names(df)[3]),
      xmax = get(names(df)[4])
    )
  )

  p <- p + geom_point() +
    geom_errorbarh(height = 0.1) +
    scale_y_continuous(breaks = 1:nrow(df), labels = df[[1]]) +
    labs(title = title, x = xlabel, y = ylabel) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())

  if (!identical(mark, NA)) {
    col <- if (is.null(names(mark))) "grey50" else names(mark)
  }
    p <- p + geom_vline(xintercept = mark, color = col, linetype = "dashed")

  if (!is.null(footnote)) {
    p <- p +
      labs(caption = footnote) +
      theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0))
  }

  if (!is.null(title)) {
    p <- p +
      labs(title = title) +
      theme(plot.title.position = "plot")
  }

  if (!identical(xlim, NA)) p <- p + xlim(xlim[1], xlim[2])

  print(p)

}
