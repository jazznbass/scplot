
#' @rdname scplot
#' @export
set_xlabel <- function(object, label = NULL, ...) {

  args <- list(...)

  if (!is.null(label)) object$xlabel <- label
  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$axis.title.x <- .merge_element(args, object$theme$axis.title.x)

  object
}

#' @rdname scplot
#' @param orientation of the label: 0 = vertical; 1 = horizontal
#' @export
set_ylabel <- function(object, label, ...) {

  args <- list(...)

  if (!missing(label)) object$ylabel <- label
  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$axis.title.y <- .merge_element(args, object$theme$axis.title.y)

  object
}

#' @rdname scplot
#' @param limits Lower and upper limits of the axis (e.g., \code{limits = c(0,
#' 20)} sets the axis to a scale from 0 to 20). With multiple single-cases
#' you can use \code{limits = c(0, NA)} to scale the axis from 0 to the maximum
#' of each case. \code{limits} is not set by default, which makes \code{scan} set
#' a proper scale based on the given data.
#' @param increment An integer. Increment of the x-axis. 1 :each mt value will be printed, 2 : every other value, 3 : every third values etc.
#' @export
set_xaxis <- function(object,
                      limits = NULL,
                      increment = NULL,
                      increment_from = NULL,
                      positions = NULL, ...) {


  args <- list(...)

  if (!is.null(limits)) object$xaxis$lim <- limits
  if (!is.null(increment)) object$xaxis$inc <- increment
  if (!is.null(increment_from)) object$xaxis$inc_from <- increment_from
  if (!is.null(positions)) object$xaxis$pos <- positions

  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$axis.text.x <- .merge_element(args, object$theme$axis.text.x)

  object
}

#' @rdname scplot
#' @export
set_yaxis <- function(object,
                      limits = NULL,
                      increment = NULL,
                      increment_from = NULL,
                      positions = NULL,
                      ...) {

  args <- list(...)

  if (!is.null(limits)) object$yaxis$lim <- limits
  if (!is.null(increment)) object$yaxis$inc <- increment
  if (!is.null(increment_from)) object$yaxis$inc_from <- increment_from
  if (!is.null(positions)) object$yaxis$pos <- positions

  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$axis.text.y <- .merge_element(args, object$theme$axis.text.y)

  object
}


#' @rdname scplot
#' @param label Character string.
#' @param align Character string. One of "left", "right", "center"
#' @param wrap Number that defines the maximum characters per line before a break.
#' If set to FALSE, no automatic linebreak is set.
#' @param parse If TRUE, the label is interpreted as an expression. Default = FALSE.
#' @export
add_title <- function(object, label, ...) {

  args <- list(...)

   if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$plot.title <- .merge_element(args, object$theme$plot.title)
  object$title <- label

  object
}

#' @rdname scplot
#' @param label Character string.
#' @export
add_caption <- function(object, label, ...) {

  args <- list(...)

  object$caption <- label

  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$plot.caption <- .merge_element(args, object$theme$plot.caption)

  object
}


#' @rdname scplot
#' @export
set_casenames <- function(object, ...,
                          x = NULL, y = NULL,
                          type = NULL,
                          text = list(),
                          background = list()) {

  args_text <- text
  args_rect <- background

  labels <-  c(...)
  if (!is.null(labels)) object$casenames$labels <- labels

  if (!is.null(type)) object$theme$casenames.type <- type
  if (!is.null(args_text$size)) args_text$size <- rel(args_text$size)

  if (identical(type, "strip")) {
    if (is.null(args_text$angle)) args_text$angle <- 270
    if (is.null(args_text$hjust)) args_text$hjust <- 0.5
  }

  object$theme$casenames.strip <- .merge_element(
    args_rect, object$theme$casenames.strip)

  object$theme$casenames <- .merge_element(args_text, object$theme$casenames)

  object
}

#' @rdname scplot
#' @export
set_phasenames <- function(object, ...,
                           color = NULL,
                           size = NULL,
                           x = NULL,
                           y = NULL,
                           text = list()) {

  args_text <- text
  if (!is.null(args_text$size)) args_text$size <- rel(args_text$size)

  labels <-  c(...)
  if (!is.null(labels)) object$phasenames$labels <- labels
  if (!is.null(x)) object$theme$phasenames.position.x <- x

  object$theme$phasenames <- .merge_element(args_text, object$theme$phasenames)

  object
}

#' @rdname scplot
#' @param extent A number between 0 and 1 given the proportion of the plot that is covert by the line or character string "full" or "scale".
#' @export
set_seperator <- function(object, extent = NULL, label = NULL, ...) {

  args <- list(...)
  object$theme$seperators <- .merge_element(
    args, object$theme$seperators
  )

  object
}


#' @rdname scplot
#' @export
add_grid <- function(object, ...) {

  args <- do.call("element_line", list(...))
  object$theme$grid <- args

  object
}


#' @rdname scplot
#' @param round Number of digits of the labels.
#' @export
add_labels <- function(object,
                       nudge_y = NULL,
                       nudge_x = NULL,
                       round = NULL,
                       text = list(),
                       background = list(),
                       variable = ".dvar") {

  args_text <- text
  args_rect <- background

  new <- list(
    variable = variable,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    round = round,
    text = .merge_element(args_text, object$theme$label.text),
    background = .merge_element(args_rect, object$theme$label.background)
  )
  object$labels <- c(
    object$label,
    list(new)
  )

  object
}

#' @rdname scplot
#' @export
add_dataline <- function(object,
                         variable,
                         color,
                         width,
                         linetype,
                         dots,
                         shape,
                         size) {

  object$dvar <- c(object$dvar, variable)

  if (missing(color)) color <- object$theme$dataline.col
  if (missing(width)) width <- object$theme$dataline.width
  if (missing(linetype)) linetype <- object$theme$dataline.linetype
  if (missing(dots)) dots <- color
  if (missing(shape)) shape <- object$theme$datadots.shape
  if (missing(size)) size <- object$theme$datadots.size

  new_line <- list(
    variable = variable,
    col = color,
    width = width,
    linetype = linetype,
    dots = dots,
    shape = shape,
    size = size
  )

  object$datalines <- c(object$datalines, list(new_line))

  object
}

#' @rdname scplot
#' @export
set_dataline <- function(object,
                         variable,
                         color,
                         width,
                         linetype,
                         dots,
                         shape,
                         size) {

  if (missing(variable)) variable <- ".dvar"

  vars <- sapply(object$datalines,
                 function(x) if(is.null(x$variable)) ".dvar" else x$variable
  )
  id <- which(vars == variable)

  if (length(id) != 1) stop("Wrong variable defintion.")

  if (!missing(color)) object$datalines[[id]]$col <- color
  if (!missing(width)) object$datalines[[id]]$width <- width
  if (!missing(linetype)) object$datalines[[id]]$linetype <- linetype
  if (!missing(dots)) object$datalines[[id]]$dots <- dots
  if (!missing(shape)) object$datalines[[id]]$shape <-shape
  if (!missing(size)) object$datalines[[id]]$size <- size

  #object$datalines[[id]]$line <- .merge_element(
  #  element_line(
  #    colour = color, size = width,
  #    linetype = linetype,
  #  ), object$theme$dataline
  #)

  #object$datalines[[id]]$dots <- .merge_element(
  #  list(
  #    colour = dots, size = size,
  #    shape = shape,
  #  ), object$theme$datadots
  #)


  object
}

#' @rdname scplot
#' @param stat A character string for defining a statistical line or curve to
#' be plotted. Possible values: "median", "mean", "min", "max", "quantile",
#' "trend", "trendA",
#' "trendA_bisplit", "trendA_trisplit",
#' "movingMean", "movingMedian", "loreg".
#' @param phase Either a numeric or a character vector specifying the reference phase (see details)
#' @param width A number defining the line width
#' @param color A character string or a number defining the color of an element.
#' @param width A number deifning the width of the line.
#' @param linetype A character string with the line type: "solid", "dashed", "dotted"
#' @param ... additional parameters passed to the statistical function.
#' @details The 'phase' argument defines the reference phase for some statistiscal functions ("median", "mean", "min", "max", "quantile"). The default is NULL which calculates and plots statistics for each phase seperately. The arguments takes a numeric vector (phase number(s)) or a character vector (phase name(s)). When more than one phase is defines, statistics are based on the combined values of these phases.
#' Some of the functions defined in 'stats' have additional arguments.
#' The 'mean' function has a trim argument (e.g. 'trim = 0.1').
#' 'quantile has a proportion argument (e.g. prob = 0.75 for calculating
#' the 75% quantile).
#' 'movingMean' and 'movingMedian' have a lag argument (e.g. lag = 2).
#' The local-regression curve function 'lowess' (or 'loreg') has a proportion
#' argument (e.g. f = 0.5) and the
#' local-regression curve function 'loess' has a span argument
#' (e.g. span = 0.75).
#'
#' @export
add_statline <- function(object,
                         stat,
                         phase = NULL,
                         color = NULL,
                         width = NULL,
                         linetype = NULL,
                         variable = NULL,
                         ...) {

  if (is.null(color)) color <- object$theme$statline$colour
  if (is.null(width)) width <- object$theme$statline$size
  if (is.null(linetype)) linetype <- object$theme$statline$linetype
  if (is.null(variable)) variable <- ".dvar"

  new <- list(
    stat = stat,
    phase = phase,
    args = list(...),
    line = element_line(
      colour = color,
      size = width,
      linetype = linetype
    ),
    variable = variable
  )

  object$statlines <- c(object$statlines, list(new))
  object

}

#' @rdname scplot
#' @export
set_background <- function(object, ...) {

  args <- list(...)
  object$theme$plot.background <- .merge_element(
    args, object$theme$plot.background)

  object
}

#' @rdname scplot
#' @export
set_panel <- function(object, alpha = NULL, ...) {

  args <- list(...)

  if (is.null(alpha)) {
    if (length(args$fill > 1)) alpha <- 0.5 else alpha <- 1
  }

  if (!is.null(args$fill)) args$fill <- alpha(args$fill, alpha)

  object$theme$panel.background <- .merge_element(
    args, object$theme$panel.background)

  object
}

#' @rdname scplot
#' @param theme A character string with a predefined graphical theme.
#' Possible values: default, yaxis, tiny, small, big, chart, ridge,
#' annotate, grid, grid2, dark, nodots, sienna, phase_color, phase_shade
#'
#' @export
add_theme <- function(object, theme, ...) {

  themes <- c(theme, ...)

  if (!(all(themes %in% names(.scplot_themes)))) {
    stop("Unknown theme template.")
  }

  for(i in themes)
    object$theme <- .merge_theme(.scplot_themes[[i]], object$theme)

  object

}

#' @rdname scplot
#' @param positions Either a vector indicating the dots to be highlighted or a
#' character string with a logical expression (e.g. values < mean(values))
#' @param shape Number. See pch graphical parameter on par help page.
#' @export
add_marks <- function(object,
                      case = 1,
                      positions,
                      color = "red",
                      size = 1,
                      shape = 1,
                      variable = ".dvar") {

  # Marks on the outliers from outlier()
  if (identical(class(positions), "sc_outlier")) {
    for(i in seq_along(positions$dropped.mt))
      object$marks <- c(
        object$marks,
        list(
          list(
            case = i, positions = positions$dropped.mt[[i]],
            color = color, size = size, shape = shape, variable = variable
          )
        )
      )
    return(object)
  }

  for(i in case) {
    object$marks <- c(
      object$marks,
      list(
        list(
          case = i, positions = positions,
          color = color, size = size, shape = shape, variable = variable
        )
      )
    )
  }

  object
}

#' @rdname scplot
#' @param x x position
#' @param y y position
#' @export
add_text <- function(object,
                     label,
                     case = 1,
                     x,
                     y,
                     color = "black",
                     size = 2,
                     angle = 0) {

  text <- list(
    case = case, labels = label,
    x = x,
    y = y,
    colour = color,
    size = size,
    angle = angle
  )

  object$texts <- c(object$texts, list(text))
  object
}

#' @rdname scplot
#' @param case Numerical vector with the csae number or character string "all" for all cases.
#' @param x0 Origin x position of the line.
#' @param y0 Origin y position of the line.
#' @param x1 End x position of the line.
#' @param y1 End y position of the line.
#' @param length Size of the aroow angels.
#' @export
add_arrow <- function(object,
                      case = 1,
                      x0, y0, x1, y1,
                      color = "black",
                      angle = 30,
                      length = unit(5, "points"),
                      ends = "last",
                      type = "open") {
  arrow <- list(
    case = case,
    x0 = x0,
    y0 = y0,
    x1 = x1,
    y1 = y1,
    colour = color,
    angle = angle,
    length = length,
    ends = ends,
    type = type
  )
  object$arrows <- c(object$arrows, list(arrow))
  object
}

#' @rdname scplot
#' @param ... various style parameter
#'
#' @export
set_theme_element <- function(object, ...) {


  object$theme <- .merge_theme(list(...), object$theme)
  object
}

#' @rdname scplot
#' @export
add_ridge <- function(object, color = "grey98", variable = ".dvar") {

  new_ridge <- list(
    variable = variable,
    colour = color
  )

  object$ridges <- c(object$ridges, list(new_ridge))
  object
}

#' @rdname scplot
#' @export
add_legend <- function(object,
                       labels = ".default",
                       case = 1,
                       position = "right",
                       datalines = TRUE,
                       statlines = TRUE,
                       title = "Lines") {

  object$legend$labels <- labels
  object$legend$statlines <- statlines
  object$legend$datalines <- datalines
  #object$theme$legend.position.case <- case
  object$legend$title <- title
  object$theme$legend.position <- position
  object
}



