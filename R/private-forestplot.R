forestplot <- function(df,
                       title = "",
                       xlabel = names(df)[2],
                       ylabel = "",
                       xlim = NA,
                       mark = NA) {

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

  if (!identical(mark, NA))
    p <- p + geom_vline(xintercept = mark, color = "grey50", linetype = "dashed")

  if (!identical(xlim, NA)) p <- p + xlim(xlim[1], xlim[2])

  print(p)

}
