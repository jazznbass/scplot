
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

.statline_trend <- function(data, line, dvar, mvar, pvar, p) {

  data <- rename(data,
     mt = !!sym(mvar),
     phase = !!sym(pvar),
     values = !!sym(dvar)
  )


  dat_stat <- data %>%
    group_by(case, phase) %>%
    summarise(
      int = coef(lm(values ~ mt))[1],
      b = coef(lm(values ~ mt))[2]
    )

  data$y <- NA

  for(i in 1: nrow(dat_stat)) {
    case <- dat_stat[["case"]][i]
    phase <- dat_stat[["phase"]][i]
    int <- dat_stat[["int"]][i]
    b <- dat_stat[["b"]][i]

    filter <- which(data$case == case & data$phase == phase)
    data[filter, "y"] <- data[["mt"]][filter] * b + int
  }

  p <- p + geom_line(
    data = data,
    aes(x = mt, y = y, group = phase),
    linetype = line$line$linetype,
    color = line$line$colour,
    size = line$line$size
  )

  p
}

.statline_trendA <- function(data, line, dvar, mvar, pvar, p) {

  data <- rename(data,
                 mt = !!sym(mvar),
                 phase = !!sym(pvar),
                 values = !!sym(dvar)
  )

  first_phase <- levels(data$phase)[1]

  dat_stat <- data %>%
    group_by(case) %>%
    summarise(
      int = coef(lm(values ~ mt, subset = phase == first_phase))[1],
      b = coef(lm(values ~ mt, subset = phase == first_phase))[2]
    )

  data$y <- NA

  for(i in 1: nrow(dat_stat)) {
    case <- dat_stat[["case"]][i]
    int <- dat_stat[["int"]][i]
    b <- dat_stat[["b"]][i]

    filter <- which(data$case == case)
    data[filter, "y"] <- data[["mt"]][filter] * b + int
  }

  p <- p + geom_line(
    data = data,
    aes(x = mt, y = y),
    linetype = line$line$linetype,
    color = line$line$colour,
    size = line$line$size
  )

  p
}
