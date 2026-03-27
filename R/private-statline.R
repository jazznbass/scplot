# statline --------

.add_statline <- function(data, line) {

  # prepare data ----
  data <- data.frame(
    case = data[["case"]],
    values = data[[line$variable]],
    mt = data[[attr(data, "mvar")]],
    phase = data[[attr(data, "pvar")]]
  )

  if (is.numeric(line$phase))
    line$phase <- levels(data[["phase"]])[line$phase]

  ## horizontal constants ----
  stat_selection <- c("mean", "median", "min", "max", "quantile", "sd", "mad")
  if (line$stat %in% stat_selection) {

    func <- function(data, ...) {
      args <- list(...)

      if (is.null(line$phase)) {
        data_list <- split(data, data[["phase"]])
        stats <- lapply(data_list, function(x)
                        do.call(line$stat, c(list(x$values), args))) |> unlist()
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

  if (line$stat == "trend") {

    if (is.null(line$args$method)) line$args$method <- "lm"

    # define regression function based on method argument
    func <- function(data, ...) {
      args <- list(...)
      regression <- if (line$args$method %in% c("theil-sen", "mblm")) {
        function(data) theil_sen(values ~ mt, data = data)
      } else if (line$args$method %in% c("lm", "ols")) {
        function(data) lm(values ~ mt, data = data)
      } else if (line$args$method == "bisplit") {
        function(data) .split_func(data = data, splits = 2)
      } else if (line$args$method == "trisplit") {
        function(data) .split_func(data = data, splits = 3)
      } else {
        stop("Unknown method for trend statline: ", line$args$method)
      }

      # if no phase specified, calculate regression for each phase separately
      if (is.null(line$phase)) {
        data_list <- split(data, data[["phase"]])
        y <- lapply(data_list, function(x) {
          fit <- regression(x)
          x$mt * fit$coefficients[2] + fit$coefficients[1]
        })
        return(unname(unlist(y)))
      }

      # if phase specified, calculate regression for filtered data
      data_filter <- if (identical(line$phase, "all")) {
        data
      } else {
        data[data[["phase"]] %in% line$phase,]
      }

      fit <- regression(data_filter)
      return(data$mt * fit$coefficients[2] + fit$coefficients[1])
    }

  }

  # smoothers ----

  if (line$stat %in% c("lowess", "loreg")) {
    func <- function(data, ...) {
      do.call(lowess,
        c(list(x = data$mt, y = data$values), list(...))
      )$y
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

  # calculate statline values ----

  data$y <- NA

  cases <- unique(data$case)

  if (!is.null(line$case)) {
    if (is.numeric(line$case)) {
      if (any(line$case > length(cases))) {
        warn("Some case numbers in case argument are higher than the number of ",
             "cases in data. Please check case argument.")
        line$case <- line$case[line$case <= length(cases)]
      }
      line$case <- cases[line$case]
    }
    cases <- cases[cases %in% line$case]
    if (length(cases) == 0) {
      warn("No cases selected for statline. Please check case argument.")
    }
  }
  for(case in cases) {
    filter <- which(data$case == case)
    data$y[filter] <- do.call(func, c(list(data[filter, ]), line$args))
  }

  # add statline to plot ----
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
    warn("Too few datapoints to calculate a moving average with lag ", lag)
    return(x)
  }
  for(i in (lag + 1):(length(x) - lag))
    x[i] <- do.call(fun, list(x[(i - lag):(i + lag)], na.rm = TRUE))

  x
}

.split_func <- function(data, splits) {

  mt <- data$mt
  values <- data$values

  part <- floor(length(values) / splits)
  first_part <- 1:part
  last_part <- (length(values) - part + 1) : length(values)

  md1 <- c(
    median(values[first_part], na.rm = FALSE),
    median(mt[first_part], na.rm = FALSE)
  )
  md2 <- c(
    median(values[last_part], na.rm = FALSE),
    median(mt[last_part], na.rm = FALSE)
  )
  md <- as.data.frame(rbind(md1, md2))

  colnames(md) <- c("values", "mt")

  lm(values ~ mt, data = md)
  #predict(model, data[, "mt", drop = FALSE])
}

