
.statline_constant_by_phase <- function(data, line, fun, label) {

  dat_stat <- data  |>
    split(~case + phase)  |>
    lapply(function(x)
      c(y = as.numeric(do.call(fun, c(list(x$values), line$args))))
    )  |>
    .ungroup()

  data <- merge(
    data,
    dat_stat,
    by = c("case", "phase"),
    all = TRUE,
    sort = FALSE
  )

  .statline_geom_phase(data, line$line, label = label)
}

.statline_constant <- function(data, line, dvar, mvar, pvar, fun, reference_phase = 1) {

  label <- paste(fun, dvar)

  data <- .rename_scdf_var(data, dvar, mvar, pvar)
  if (is.null(line$args$na.rm)) line$args$na.rm <- TRUE

  if (is.null(reference_phase)) {
    return(.statline_constant_by_phase(data, line, fun, label))
  }

  if (is.numeric(reference_phase))
    reference_phase <- levels(data$phase)[reference_phase]


  dat_stat <- data[data$phase %in% reference_phase,] %>%
    split(~case) %>%
    lapply(function(x)
      c(y = as.numeric(do.call(fun, c(list(x$values), line$args))))
    ) %>%
    .ungroup()

  data <- merge(data, dat_stat, by = "case", all = TRUE, sort = FALSE)

  .statline_geom(data, line$line, label = label)
}

.statline_trend_by_phase <- function(data, line, dvar, mvar, pvar) {

  data <- .rename_scdf_var(data, dvar, mvar, pvar)

  if (is.null(line$args$method)) line$args$method <- "lm"

  label <- paste(line$stat, dvar)

  dat_stat <- data  |>
    split(~case + phase) |>
    lapply(function(x) {
      if(line$args$method %in% c("theil-sen", "mblm")) {
        param <- coef(mblm::mblm(values ~ mt, data = x, repeated = FALSE))
      } else if (line$args$method %in% c("lm", "ols")) {
        param <- coef(lm(values ~ mt, data = x))
      }
      c(int = as.numeric(param[1]), b = as.numeric(param[2]))
    })  |>
    .ungroup()

  data$y <- NA

  for(i in 1: nrow(dat_stat)) {
    case <- dat_stat[["case"]][i]
    phase <- dat_stat[["phase"]][i]
    int <- dat_stat[["int"]][i]
    b <- dat_stat[["b"]][i]

    filter <- which(data$case == case & data$phase == phase)
    data[filter, "y"] <- data$mt[filter] * b + int
  }

  .statline_geom_phase(data, line$line, label = label)
}

.statline_trend <- function(data,
                            line,
                            dvar, mvar, pvar,
                            reference_phase = 1) {

  data <- .rename_scdf_var(data, dvar, mvar, pvar)

  reference_phase <- levels(data$phase)[reference_phase]


  label <- paste(line$stat, dvar)

  dat_stat <- data  |>
    split(~case)  |>
    lapply(function(x) {
      tmp_dat <- subset(x, phase %in% reference_phase)
      if(line$args$method %in% c("theil-sen", "mblm")) {
        param <- coef(mblm::mblm(values ~ mt, data = tmp_dat, repeated = FALSE))
      } else if (line$args$method %in% c("lm", "ols")) {
        param <- coef(lm(values ~ mt, data = tmp_dat))
      }
      c(int = as.numeric(param[1]), b = as.numeric(param[2]))
    } )  |>
    .ungroup()

  data$y <- NA

  for(i in 1: nrow(dat_stat)) {
    case <- dat_stat[["case"]][i]
    int <- dat_stat[["int"]][i]
    b <- dat_stat[["b"]][i]

    filter <- which(data$case == case)
    data$y[filter] <- data$mt[filter] * b + int
  }

  .statline_geom(data, line$line, label = label)
}

.statline_moving_average <- function(data, line, dvar, mvar, pvar, fun) {

  if (fun == "mean") label <- paste("moving mean", dvar)
  if (fun == "median") label <- paste("moving median", dvar)

  data <- .rename_scdf_var(data, dvar, mvar, pvar)

  if (is.null(line$args$lag)) line$args$lag <- 1

  data$y <- NA

  for(case in unique(data$case)) {
    filter <- which(data$case == case)
    data$y[filter] <- .moving_average(data$values[filter], line$args$lag, fun)
  }

  .statline_geom(data, line$line, label = label)
}

.statline_loreg <- function(data, line, dvar, mvar, pvar, fun) {

  label <- paste(fun, dvar)

  data <- .rename_scdf_var(data, dvar, mvar, pvar)

  data$y <- NA

  for(case in unique(data$case)) {
    filter <- which(data$case == case)
    model <- do.call(fun,
      c(list(data$values[filter] ~ data$mt[filter]), line$args)
    )

    if (fun == "lowess") data$y[filter] <- model$y
    if (fun == "loess") data$y[filter] <- model$fitted
  }

  .statline_geom(data, line$line, label = label)

}

.moving_average <- function(x, xlag, fun) {
  if (length(x) < xlag * 2 + 1) {
    warning("Too few datapoints to calculate with lag ", xlag)
    return(x)
  }
  for(i in (xlag + 1):(length(x) - xlag))
    x[i] <- do.call(fun, list(x[(i - xlag):(i + xlag)], na.rm = TRUE))

  x
}

# geom_functions --------

# across case
.statline_geom <- function(data, line, label) {

  geom_line(
    data = data,
    aes(x = mt, y = y, color = !!label),
    linetype = line$linetype,
    #color = line$colour,
    linewidth = line$linewidth
  )

}

# by phase
.statline_geom_phase <- function(data, line, label) {

  geom_line(
    data = data,
    aes(x = mt, y = y, group = phase, color = !!label),
    linetype = line$linetype,
    #color = line$colour,
    linewidth = line$linewidth
  )

}

.rename_scdf_var <- function(data, dvar, mvar, pvar) {

  names(data)[which(names(data) == dvar)] <- "values"
  names(data)[which(names(data) == mvar)] <- "mt"
  names(data)[which(names(data) == pvar)] <- "phase"
  data

}

.ungroup <- function(data, groups = c("case", "phase")) {
  if (identical(names(data), "")) names(data) <- ".A"
  data <- do.call("rbind", data)
  df <- do.call("rbind", strsplit(row.names(data), ".", fixed = "TRUE"))
  colnames(df) <- groups[1:ncol(df)]
  out <- cbind(as.data.frame(df), data)
  row.names(out) <- 1:nrow(out)
  out
}
