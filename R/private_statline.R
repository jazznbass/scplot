
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

.statline_constant <- function(data, line, fun, reference_phase = 1) {

  dvar <- line$variable
  mvar <- attr(data, "mvar")
  pvar <- attr(data, "pvar")
  label <- paste(fun, dvar)

  data <- .rename_scdf_var(data, dvar, mvar, pvar)
  if (is.null(line$args$na.rm)) line$args$na.rm <- TRUE

  if (is.null(reference_phase)) {
    return(.statline_constant_by_phase(data, line, fun, label))
  }

  if (is.numeric(reference_phase))
    reference_phase <- levels(data$phase)[reference_phase]


  dat_stat <- data[data$phase %in% reference_phase,]  |>
    split(~case) |>
    lapply(function(x)
      c(y = as.numeric(do.call(fun, c(list(x$values), line$args))))
    )  |>
    .ungroup()

  data <- merge(data, dat_stat, by = "case", all = TRUE, sort = FALSE)

  .statline_geom(data, line$line, label = label)
}

.statline_trend_by_phase <- function(data, line) {

  dvar <- line$variable
  mvar <- attr(data, "mvar")
  pvar <- attr(data, "pvar")
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

.statline_continuous <- function(data, line, fun) {

  dvar <- line$variable
  mvar <- attr(data, "mvar")
  pvar <- attr(data, "pvar")
  data <- .rename_scdf_var(data, dvar, mvar, pvar)

  if (fun %in% c("lowess", "loreg")) {
    func <- function(data, ...) {
      do.call(lowess,
        c(list(x = data[["mt"]], y = data[["values"]]), list(...))
      )$y
    }
  }


  if (fun == "trendA bisplit") {
    func <- function(data, ...) {
      filter_first_phase <- 1:rle(as.character(data$phase))$lengths[1]
      mt <- data[["mt"]][filter_first_phase] #x
      values <- data[["values"]][filter_first_phase] #y

      # na.rm = FALSE for now to prevent misuse;
      # will draw no line if NA present
      md1 <- c(
        median(values[1:floor(length(values) / 2)], na.rm = FALSE),
        median(mt[1:floor(length(mt) / 2)], na.rm = FALSE)
      )
      md2 <- c(
        median(values[ceiling(length(values) / 2 + 1):length(values)], na.rm = FALSE),
        median(mt[ceiling(length(mt) / 2 + 1):length(mt)], na.rm = FALSE)
      )
      md <- as.data.frame(rbind(md1, md2))
      colnames(md) <- c("values", "mt")
      model <- lm(values~mt, data = md)

      predict(model, data[, "mt", drop = FALSE])
    }
  }

  if (fun == "trendA trisplit") {
    func <- function(data, ...) {
      filter_first_phase <- 1:rle(as.character(data$phase))$lengths[1]
      mt <- data[["mt"]][filter_first_phase] #x
      values <- data[["values"]][filter_first_phase] #y

      # na.rm = FALSE for now to prevent misuse;
      # will draw no line if NA present
      md1 <- c(
        median(values[1:floor(length(values) / 3)], na.rm = FALSE),
        median(mt[1:floor(length(mt) / 3)], na.rm = FALSE)
      )
      md2 <- c(
        median(values[ceiling(length(values) / 3 * 2 + 1):length(values)], na.rm = FALSE),
        median(mt[ceiling(length(mt) / 3 * 2 + 1):length(mt)], na.rm = FALSE)
      )
      md <- as.data.frame(rbind(md1, md2))
      colnames(md) <- c("values", "mt")
      model <- lm(values~mt, data = md)

      predict(model, data[, "mt", drop = FALSE])
    }
  }

  if (fun == "loess") {
    func <- function(data, ...) {
      do.call(loess,
        c(list(formula = as.formula("values ~ mt")), list(data = data), list(...))
      )$fitted
    }
  }

  if (fun %in% c("moving mean", "movingMean")) {
    func <- function(data, ...) {
      do.call(.moving_average,
        c(list(x = data[["values"]]), list(fun = "mean"), list(...))
      )
    }
  }

  if (fun %in% c("moving median", "movingMedian")) {
    func <- function(data, ...) {
      do.call(.moving_average,
        c(list(x = data[["values"]]), list(fun = "median"), list(...))
      )
    }
  }

  if (fun == "trendA") {
    func <- function(data, ...) {
      filter_first_phase <- 1:rle(as.character(data$phase))$lengths[1]
      mt <- data[["mt"]][filter_first_phase]
      values <- data[["values"]][filter_first_phase]
      model <- lm(values ~ mt, ...)
      predict(lm(values ~ mt), data[, c("values", "mt")],)
      #do.call(lm,
      #  c(list(formula = as.formula("values ~ mt")), list(data = data), list(...))
      #)$fitted.values
    }
  }

  if (fun == "trendA theil-sen") {
    func <- function(data, ...) {
      filter_first_phase <- 1:rle(as.character(data$phase))$lengths[1]
      mt <- data[["mt"]][filter_first_phase]
      values <- data[["values"]][filter_first_phase]
      predict(mblm::mblm(values ~ mt, repeated = FALSE), data[, c("values", "mt")], ...)

      #do.call(mblm::mblm,
      #  c(list(formula = as.formula("values ~ mt")), list(dataframe = data), list(...))
      #)$fitted.values
    }
  }

  data$y <- NA

  for(case in unique(data$case)) {
    filter <- which(data$case == case)
    data$y[filter] <- do.call(func, c(list(data[filter, ]), line$args))
  }

  label <- paste(fun, dvar)

  .statline_geom(data, line$line, label = label)

}

.moving_average <- function(x, lag = 1, fun) {
  if (length(x) < lag * 2 + 1) {
    warning("Too few datapoints to calculate with lag ", lag)
    return(x)
  }
  for(i in (lag + 1):(length(x) - lag))
    x[i] <- do.call(fun, list(x[(i - lag):(i + lag)], na.rm = TRUE))

  x
}

# geom_functions --------

# across case
.statline_geom <- function(data, line, label) {

  geom_line(
    data = data,
    aes(x = mt, y = y, color = {{label}}),
    linetype = line$linetype,
    linewidth = line$linewidth,
    na.rm = TRUE
  )

}

# by phase
.statline_geom_phase <- function(data, line, label) {

  geom_line(
    data = data,
    aes(x = mt, y = y, group = phase, color = {{label}}),
    linetype = line$linetype,
    linewidth = line$linewidth,
    na.rm = TRUE
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

