
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

.statline_fixed_first <- function(data, line, dvar, mvar, pvar, p, fun, args) {

  data <- rename(data,
                 mt = !!sym(mvar),
                 phase = !!sym(pvar),
                 values = !!sym(dvar)
  )

  first_phase <- levels(data$phase)[1]

  dat_stat <- data %>%
    filter(phase == first_phase) %>%
    group_by(case) %>%
    summarise(stat = do.call(fun, c(list(values), args)))

  data <- full_join(data, dat_stat, by = c("case"))

  p <- p + geom_line(
    data = data,
    aes(x = mt, y = stat),
    linetype = line$line$linetype,
    color = line$line$colour,
    size = line$line$size
  )

  p
}
