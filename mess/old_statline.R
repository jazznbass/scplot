
.statline_trend_by_phase <- function(data, line) {

  dvar <- line$variable
  mvar <- attr(data, "mvar")
  pvar <- attr(data, "pvar")

  if (is.null(line$args$method)) line$args$method <- "lm"

  dat_stat <- data  |>
    split(list(data$case, data[[pvar]])) |>
    lapply(function(x) {

      values <- x[[dvar]]
      mt <- x[[mvar]]

      if(line$args$method %in% c("theil-sen", "mblm")) {
        param <- coef(mblm::mblm(values ~ mt, repeated = FALSE))
      } else if (line$args$method %in% c("lm", "ols")) {
        param <- coef(lm(values ~ mt))
      }
      c(int = as.numeric(param[1]), b = as.numeric(param[2]))
    })  |>
    .ungroup(groups = c("case", pvar))

  data$y <- NA

  for(i in 1: nrow(dat_stat)) {
    case <- dat_stat[["case"]][i]
    phase <- dat_stat[[pvar]][i]
    int <- dat_stat[["int"]][i]
    b <- dat_stat[["b"]][i]

    filter <- which(data$case == case & data[[pvar]] == line$phase)
    data[filter, "y"] <- data[[mvar]][filter] * b + int
  }

  .statline_geom_phase(data, line$line, label = line$label, mt = mvar, phase = pvar)
}


.statline_constant_by_phase <- function(data, line) {

  dvar <- line$variable
  mvar <- attr(data, "mvar")
  pvar <- attr(data, "pvar")

  if (is.null(line$args$na.rm)) line$args$na.rm <- TRUE

  dat_stat <- data  |>
    split(list(data$case, data[[pvar]]))  |>
    lapply(function(x)
      c(y = as.numeric(do.call(line$stat, c(list(x[[dvar]]), line$args))))
    )  |>
    .ungroup(groups = c("case", pvar))

  data <- merge(
    data,
    dat_stat,
    by = c("case", pvar),
    all = TRUE,
    sort = FALSE
  )

  .statline_geom_phase(data, line$line, label = line$label, mt = mvar, phase = pvar)
}

.statline_constant <- function(data, line) {

  if (is.null(line$phase)) {
    return(.statline_constant_by_phase(data, line))
  }

  dvar <- line$variable
  mvar <- attr(data, "mvar")
  pvar <- attr(data, "pvar")

  if (is.null(line$args$na.rm)) line$args$na.rm <- TRUE

  if (is.numeric(line$phase))
    line$phase <- levels(data[[pvar]])[line$phase]

  dat_stat <- data[data[[pvar]] %in% line$phase,]
  dat_stat <- dat_stat |>
    split(dat_stat[["case"]]) |>
    lapply(function(x)
      c(y = as.numeric(do.call(line$stat, c(list(x[[dvar]]), line$args))))
    )  |>
    .ungroup(groups = c("case", pvar))

  data <- merge(data, dat_stat, by = "case", all = TRUE, sort = FALSE)

  .statline_geom(data, line$line, label = line$label, mt = mvar)
}

# geom_functions --------

# across case
.statline_geom <- function(data, line, label, mt) {

  geom_line(
    data = data,
    aes(x = .data[[mt]], y = y, color = label),
    linetype = line$linetype,
    linewidth = line$linewidth,
    na.rm = TRUE
  )

}

# by phase
.statline_geom_phase <- function(data, line, label, mt, phase) {

  geom_line(
    data = data,
    aes(x = .data[[mt]],
        y = y,
        group = .data[[phase]],
        color = label),
    linetype = line$linetype,
    linewidth = line$linewidth,
    na.rm = TRUE
  )

}


.ungroup <- function(data, groups) {

  if (identical(names(data), "")) names(data) <- ".A"
  data <- do.call("rbind", data)
  df <- do.call("rbind", strsplit(row.names(data), ".", fixed = "TRUE"))
  colnames(df) <- groups[1:ncol(df)]
  out <- cbind(as.data.frame(df), data)
  row.names(out) <- 1:nrow(out)
  out
}
