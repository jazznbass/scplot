# statline --------

.add_statline <- function(data, line) {

  data <- data.frame(
    case = data[["case"]],
    values = data[[line$variable]],
    mt = data[[attr(data, "mvar")]],
    phase = data[[attr(data, "pvar")]]
  )

  if (is.numeric(line$phase))
    line$phase <- levels(data[["phase"]])[line$phase]

  ## constants ----
  stat_selection <- c("mean", "median", "min", "max", "quantile", "sd", "mad")
  if (line$stat %in% stat_selection) {

    func <- function(data, ...) {
      args <- list(...)

      if (is.null(line$phase)) {
        data_list <- split(data, data[["phase"]])
        stats <- lapply(data_list, function(x) do.call(line$stat, c(list(x$values), args))) |> unlist()
        return(unname(stats[data$phase]))
      }

      x <- if (identical(line$phase, "all")) {
       data[["values"]]
      } else {
       data[["values"]][data[["phase"]] %in% line$phase]
      }
      y <- do.call(line$stat, c(list(x), args))
      return(rep(y, nrow(data)))
    }

  }

  ## trends ----
  if (line$stat %in% "trend") {
    if (is.null(line$args$method)) line$args$method <- "lm"
    func <- function(data, ...) {
      args <- list(...)
      regression <- if (line$args$method %in% c("theil-sen", "mblm")) {
        function(data) mblm::mblm(values ~ mt, repeated = FALSE, data = data)
      } else if (line$args$method %in% c("lm", "ols")) {
        function(data) lm(values ~ mt, data = data)
      }
      if (is.null(line$phase)) {
        data_list <- split(data, data[["phase"]])
        y <- lapply(data_list, function(x) {
          fit <- regression(x)
          fitted.values(fit)
        })
        return(unname(unlist(y)))
      }

      data_filter <- if (identical(line$phase, "all")) {
        data
      } else {
        data[data[["phase"]] %in% line$phase,]
      }

      fit <- regression(data_filter)
      return(predict(fit, data))
    }

  }

  if (line$stat %in% c("lowess", "loreg")) {
    func <- function(data, ...) {
      do.call(lowess,
        c(list(x = data$mt, y = data$values), list(...))
      )$y
    }
  }

  if (line$stat == "trendA bisplit") {
    func <- function(data, ...) {
      filter_first_phase <- 1:rle(as.character(data$phase))$lengths[1]
      mt <- data$mt[filter_first_phase] #x
      values <- data$mt[filter_first_phase] #y

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
      model <- lm(values ~ mt, data = md)

      predict(model, data[, "mt", drop = FALSE])
    }
  }

  if (line$stat == "trendA trisplit") {
    func <- function(data, ...) {
      filter_first_phase <- 1:rle(as.character(data$phase))$lengths[1]
      mt <- data$mt[filter_first_phase] #x
      values <- data$values[filter_first_phase] #y

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

  if (line$stat == "loess") {
    func <- function(data, ...) {
      do.call(loess,
        c(list(formula = "values ~ mt"), list(data = data), list(...))
      )$fitted
    }
  }

  if (line$stat %in% c("moving mean", "movingMean")) {
    func <- function(data, ...) {
      do.call(.moving_average,
        c(list(x = data$values), list(fun = "mean"), list(...))
      )
    }
  }

  if (line$stat %in% c("moving median", "movingMedian")) {
    func <- function(data, ...) {
      do.call(.moving_average,
        c(list(x = data$values), list(fun = "median"), list(...))
      )
    }
  }

  data$y <- NA

  for(case in unique(data$case)) {
    filter <- which(data$case == case)
    data$y[filter] <- do.call(func, c(list(data[filter, ]), line$args))
  }

  p <- geom_line(
    data = data,
    aes(x = mt,
        y = y,
        group = if (line$segmented) !!sym("phase") else NULL,
        color = line$label),
    linetype = line$line@linetype,
    linewidth = line$line@linewidth,
    na.rm = TRUE
  )

  p
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

