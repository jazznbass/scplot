# deprecated -------

# .statline_loreg <- function(data, line, fun) {
#
#   dvar <- line$variable
#   mvar <- attr(data, "mvar")
#   pvar <- attr(data, "pvar")
#   label <- paste(fun, dvar)
#   data <- .rename_scdf_var(data, dvar, mvar, pvar)
#
#   data$y <- NA
#
#   if (fun == "lowess") {
#     func <- function(data, ...) {
#       do.call("lowess", c(list(x = data[["mt"]], y = data[["values"]]), list(...)))$y
#     }
#   }
#
#   if (fun == "loess") {
#     func <- function(data, ...) {
#       do.call("loess",
#               c(list(formula = as.formula("values ~ mt")), list(data = data), list(...))
#       )$fitted
#     }
#   }
#
#   for(case in unique(data$case)) {
#     filter <- which(data$case == case)
#     data$y[filter] <- do.call(func, c(list(data[filter, ]), line$args))
#   }
#
#   .statline_geom(data, line$line, label = label)
#
# }
#
# .statline_moving_average <- function(data, line, fun) {
#
#   dvar <- line$variable
#   mvar <- attr(data, "mvar")
#   pvar <- attr(data, "pvar")
#
#   if (fun == "mean") label <- paste("movingMean", dvar)
#   if (fun == "median") label <- paste("movingMedian", dvar)
#
#   data <- .rename_scdf_var(data, dvar, mvar, pvar)
#
#   #if (is.null(line$args$lag)) line$args$lag <- 1
#
#   data$y <- NA
#   func <- ".moving_average"
#
#   for(case in unique(data$case)) {
#     filter <- which(data$case == case)
#     args <- c(list(data$values[filter]), line$args, list(fun = fun))
#     data$y[filter] <- do.call(func, args)
#   }
#
#   .statline_geom(data, line$line, label = label)
# }

# .statline_trend <- function(data,
#                             line,
#                             reference_phase = 1) {
#
#   dvar <- line$variable
#   mvar <- attr(data, "mvar")
#   pvar <- attr(data, "pvar")
#
#   data <- .rename_scdf_var(data, dvar, mvar, pvar)
#
#   reference_phase <- levels(data$phase)[reference_phase]
#
#
#   label <- paste(line$stat, dvar)
#
#   dat_stat <- data  |>
#     split(~case)  |>
#     lapply(function(x) {
#       tmp_dat <- subset(x, phase %in% reference_phase)
#       if(line$args$method %in% c("theil-sen", "mblm")) {
#         param <- coef(mblm::mblm(values ~ mt, data = tmp_dat, repeated = FALSE))
#       } else if (line$args$method %in% c("lm", "ols")) {
#         param <- coef(lm(values ~ mt, data = tmp_dat))
#       }
#       c(int = as.numeric(param[1]), b = as.numeric(param[2]))
#     } )  |>
#     .ungroup()
#
#   data$y <- NA
#
#   for(i in 1: nrow(dat_stat)) {
#     case <- dat_stat[["case"]][i]
#     int <- dat_stat[["int"]][i]
#     b <- dat_stat[["b"]][i]
#
#     filter <- which(data$case == case)
#     data$y[filter] <- data$mt[filter] * b + int
#   }
#
#   .statline_geom(data, line$line, label = label)
# }
