
.statline_fixed_each <- function(data, line, dvar, mvar, pvar, fun) {

  data <- rename(data,
    mt = !!sym(mvar),
    phase = !!sym(pvar),
    values = !!sym(dvar)
  )

  if (is.null(line$args$na.rm)) line$args$na.rm <- TRUE


  dat_stat <- data %>%
    group_by(case, phase) %>%
    summarise(y = do.call(fun, c(list(values), line$args)))

  data <- full_join(data, dat_stat, by = c("case", "phase"))

  .statline_geom_phase(data, line$line)
}

.statline_fixed_first <- function(data, line, dvar, mvar, pvar, fun) {

  data <- rename(data,
    mt = !!sym(mvar),
    phase = !!sym(pvar),
    values = !!sym(dvar)
  )

  if (is.null(line$args$na.rm)) line$args$na.rm <- TRUE

  first_phase <- levels(data$phase)[1]

  dat_stat <- data %>%
    filter(phase == first_phase) %>%
    group_by(case) %>%
    summarise(y = do.call(fun, c(list(values), line$args)))

  data <- full_join(data, dat_stat, by = c("case"))

  .statline_geom(data, line$line)
}

.statline_trend <- function(data, line, dvar, mvar, pvar) {

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
    data[filter, "y"] <- data$mt[filter] * b + int
  }

  .statline_geom_phase(data, line$line)
}

.statline_trendA <- function(data, line, dvar, mvar, pvar) {

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
    data$y[filter] <- data$mt[filter] * b + int
  }

  .statline_geom(data, line$line)
}

.statline_moving_average <- function(data, line, dvar, mvar, pvar, fun) {

  data <- rename(data,
    mt = !!sym(mvar),
    phase = !!sym(pvar),
    values = !!sym(dvar)
  )

  if (is.null(line$args$lag)) line$args$lag <- 1

  data$y <- NA

  for(case in unique(data$case)) {
    filter <- which(data$case == case)
    data$y[filter] <- .moving_average(data$values[filter], line$args$lag, fun)
  }

  .statline_geom(data, line$line)
}

.statline_loreg <- function(data, line, dvar, mvar, pvar) {

  data <- rename(data,
    mt = !!sym(mvar),
    phase = !!sym(pvar),
    values = !!sym(dvar)
  )

  if (is.null(line$args$f)) line$args$f <- 0.5

  data$y <- NA

  for(case in unique(data$case)) {
    filter <- which(data$case == case)
    data$y[filter] <-
      do.call(
        "lowess",
        c(list(data$values[filter] ~ data$mt[filter]), line$args)
      )$y
  }

  .statline_geom(data, line$line)

}


.moving_average <- function(x, xlag, fun) {
  if (length(x) < xlag * 2 + 1) {
    warning("To few datapoints to calculate with lag ", xlag)
    return(x)
  }
  for(i in (xlag + 1):(length(x) - xlag))
    x[i] <- fun(x[(i - xlag):(i + xlag)], na.rm = TRUE)

  x
}

.statline_geom <- function(data, line) {

  geom_line(
    data = data,
    aes(x = mt, y = y),
    linetype = line$linetype,
    color = line$colour,
    size = line$size
  )

}

.statline_geom_phase <- function(data, line) {

  geom_line(
    data = data,
    aes(x = mt, y = y, group = phase),
    linetype = line$linetype,
    color = line$colour,
    size = line$size
  )

}
