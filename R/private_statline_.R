
.create_statline_data_frame <- function(design) {

  data_phase <- data.frame(
    case = rep(
      names(design),
      sapply(design, function(x) length(x$stop_mt) * 2)
    ),
    phase = unlist(lapply(design, function(x) rep(x$values, each = 2)))
  )

  data_phase$x <- unlist(lapply(design, function(x) {
    out <- c()
    for(y in 1:length(x$start_mt))
      out <- c(out, x$start_mt[y], x$stop_mt[y])
    out
  }))

  data_phase
}

.statline_fixed_each <- function(data, line, design, p, fun, args) {

  data_phase <- .create_statline_data_frame(design)

  y <- c()
  for(i in seq(1, nrow(data_phase), by = 2)) {
    filter <- data$case == data_phase$case[i] &
      data$phase == data_phase$phase[i]
    value <- do.call(fun, args = c(list(x = data[filter, line$variable]), args))
    #value <- mean(data[filter, line$variable], na.rm = TRUE)
    y <- c(y, value, value)
  }
  data_phase$y <- y

  p <- p + geom_line(
    data = data_phase,
    aes(x = x, y = y, group = phase),
    linetype = line$line$linetype,
    color = line$line$colour,
    size = line$line$size
  )

  p
}
