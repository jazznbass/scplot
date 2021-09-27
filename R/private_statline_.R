
.statline_fixed_each <- function(data, line, dvar, mvar, pvar, p, fun, args) {

  data <- rename(data,
    mt = !!sym(mvar),
    phase = !!sym(pvar),
    values = !!sym(dvar)
  )

  dat_stat <- data %>%
    group_by(case, phase) %>%
    summarise(stat = do.call(fun, c(list(values), args)))

  data <- full_join(data, dat_stat, by = c("case", "phase"))

  p <- p + geom_line(
    data = data,
    aes(x = mt, y = stat, group = phase),
    linetype = line$line$linetype,
    color = line$line$colour,
    size = line$line$size
  )

  p
}
