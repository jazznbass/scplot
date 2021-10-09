
#' @rdname scplot
#' @export
set_xlabel <- function(object,
                       label = NULL,
                       color = NULL,
                       size = NULL,
                       family = NULL,
                       face = NULL,
                       hjust = NULL,
                       vjust = NULL,
                       angle = NULL,
                       lineheight = NULL,
                       margin = NULL) {


  if (!is.null(label)) object$xlabel <- label

  if (!is.null(size)) size <- rel(size)

  object$theme$axis.title.x <- .merge_element(
    element_text(
      family = family, face = face, colour = color, size = size,
      hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight,
      margin = margin
    ), object$theme$axis.title.x
  )

  object
}

#' @rdname scplot
#' @param orientation of the label: 0 = vertical; 1 = horizontal
#' @export
set_ylabel <- function(object, label,
                       color = NULL,
                       size = NULL,
                       family = NULL,
                       face = NULL,
                       hjust = NULL,
                       vjust = NULL,
                       angle = NULL,
                       lineheight = NULL,
                       margin = NULL) {

  if (!missing(label)) object$ylabel <- label

  if (!is.null(size)) size <- rel(size)

  object$theme$axis.title.y <- .merge_element(
    element_text(
      family = family, face = face, colour = color,
      size = size,
      hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight,
      margin = margin
    ), object$theme$axis.title.y
  )

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
                      color = NULL,
                      size = NULL,
                      line = NULL,
                      angle = NULL,
                      positions = NULL) {

  #  if (!is.null(line)) object$theme$vjust.xlab <- line

  if (!is.null(limits)) object$xaxis$lim <- limits
  if (!is.null(increment)) object$xaxis$inc <- increment
  if (!is.null(increment_from)) object$xaxis$inc_from <- increment_from
  if (!is.null(positions)) object$xaxis$pos <- positions

  if (!is.null(size)) size <- rel(size)

  object$theme$axis.text.x <- .merge_element(
    element_text(
      colour = color,
      size = size,
      angle = angle
    ),
    object$theme$axis.text.x)

  object
}

#' @rdname scplot
#' @export
set_yaxis <- function(object,
                      limits = NULL,
                      color = NULL,
                      size = NULL,
                      increment = NULL,
                      increment_from = NULL,
                      positions = NULL) {


  if (!is.null(limits)) object$yaxis$lim <- limits
  if (!is.null(increment)) object$yaxis$inc <- increment
  if (!is.null(increment_from)) object$yaxis$inc_from <- increment_from
  if (!is.null(positions)) object$yaxis$pos <- positions

  if (!is.null(size)) size <- rel(size)

  object$theme$axis.text.y <- .merge_element(
    element_text(
      colour = color,
      size = size,
    ),
    object$theme$axis.text.y
  )

  object
}


#' @rdname scplot
#' @param label Character string.
#' @param align Character string. One of "left", "right", "center"
#' @param wrap Number that defines the maximum characters per line before a break.
#' If set to FALSE, no automatic linebreak is set.
#' @param parse If TRUE, the label is interpreted as an expression. Default = FALSE.
#' @export
add_title <- function(object,
                      label,
                      color = NULL,
                      size = NULL,
                      family = NULL,
                      face = NULL,
                      hjust = NULL,
                      vjust = NULL,
                      angle = NULL,
                      lineheight = NULL,
                      margin = NULL,
                      parse = NULL) {

  # if (!is.null(parse)) object$theme$title.parse <- parse

  if (!is.null(size)) size <- rel(size)

  object$theme$plot.title <- .merge_element(
    element_text(
      family = family, face = face, colour = color, size = size,
      hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight,
      margin = margin
    ), object$theme$plot.title
  )


  object$title <- label
  object
}

#' @rdname scplot
#' @param label Character string.
#' @export
add_caption <- function(object, label, color = NULL, size = NULL, face = NULL, hjust= NULL, vjust = NULL,
                        wrap = NULL, margin = NULL, parse = NULL) {


  #if (!is.null(wrap)) object$theme$caption.wrap <- wrap
  #if (!is.null(margin)) object$theme$caption.margin <- margin
  #if (!is.null(parse)) object$theme$caption.parse <- parse

  object$caption <- label

  if (!is.null(size)) size <- rel(size)

  object$theme$plot.caption <- .merge_element(
    element_text(
      colour = color,
      size = size,
      face = face,
      hjust = hjust,
      vjust = vjust,
      margin = margin
    ), object$theme$plot.caption
  )


  object
}


#' @rdname scplot
#' @export
set_casenames <- function(object, ...,
                          x = NULL, y = NULL,
                          color  = NULL,
                          size = NULL,
                          hjust = NULL, vjust = NULL,
                          width = NULL,
                          angle = NULL,
                          fill = NULL,
                          linetype = NULL,
                          frame = NULL,
                          type = NULL) {

  labels <-  c(...)
  if (!is.null(labels)) object$casenames$labels <- labels

  if (!is.null(type)) object$theme$casenames.type <- type
  if (!is.null(size)) size <- rel(size)

  if (identical(type, "strip")) {
    if (is.null(angle)) angle <- 270
    if (is.null(hjust)) hjust <- 0.5
  }

  object$theme$casenames.strip <- .merge_element(
    element_rect(
      colour = frame, fill = fill, size = width, linetype = linetype
    ), object$theme$casenames.strip
  )

  object$theme$casenames <- .merge_element(
    element_text(
      #family = family, face = face,
      colour = color,
      size = size,
      hjust = hjust,
      vjust = vjust,#,
      angle = angle,
      #lineheight = lineheight,
      #margin = margin
    ), object$theme$casenames
  )

  object
}

#' @rdname scplot
#' @export
set_phasenames <- function(object, ...,
                           color = NULL,
                           size = NULL,
                           x = NULL,
                           y = NULL,
                           hjust = NULL,
                           vjust = NULL,
                           angle = NULL,
                           box = NULL,
                           frame = NULL,
                           face = NULL,
                           family = NULL) {

  labels <-  c(...)
  if (!is.null(labels)) object$phasenames$labels <- labels

  if (!is.null(x)) object$theme$phasenames.position.x <- x

  object$theme$phasenames <- .merge_element(
    element_text(
      family = family,
      face = face,
      colour = color,
      size = size,
      hjust = hjust,
      vjust = vjust,
      angle = angle,
    ), object$theme$phasenames
  )

  object
}

#' @rdname scplot
#' @param extent A number between 0 and 1 given the proportion of the plot that is covert by the line or character string "full" or "scale".
#' @export
set_seperator <- function(object, color = NULL, width = NULL, linetype = NULL, extent = NULL, label = NULL, size = NULL) {

  object$theme$seperators <- .merge_element(
    element_line(
      colour = color,
      size = width,
      linetype = linetype
    ), object$theme$seperators
  )

  object
}


#' @rdname scplot
#' @export
add_grid <- function(object,
                     color = NULL,
                     size = NULL,
                     linetype = NULL,
                     lineend = NULL,
                     arrow = NULL) {

  object$theme$grid <- element_line(
    colour = color,
    size = size,
    linetype = linetype,
    lineend = lineend,
    arrow = arrow,
  )

  object
}


#' @rdname scplot
#' @param round Number of digits of the labels.
#' @export
add_labels <- function(object,
                       color = NULL,
                       size = NULL,
                       nudge_y,
                       nudge_x,
                       vjust = NULL,
                       hjust = NULL,
                       angle = NULL,
                       round,
                       box = NULL,
                       frame = NULL) {

  if (!missing(nudge_y)) object$theme$labels.nudge_y <- nudge_y
  if (!missing(nudge_x)) object$theme$labels.nudge_x <- nudge_x
  if (!missing(round)) object$theme$labels.round <- round

  object$theme$labels.text <- .merge_element(
    element_text(
      #family = family, face = face,
      colour = color, size = size,
      hjust = hjust, vjust = vjust,#,
      angle = angle,
      #lineheight = lineheight,
      #margin = margin
    ), object$theme$labels.text
  )

  object$theme$labels.box <- .merge_element(
    element_rect(
      colour = frame, fill = box,
    ), object$theme$labels.box
  )

  object$labels <- TRUE

  object
}

#' @rdname scplot
#' @export
add_dataline <- function(object, variable, color, width, linetype, dots, shape, size) {

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
set_dataline <- function(object, variable, color, width, linetype, dots, shape, size) {

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
#' @param stat A character string defining a line or curve to be
#' plotted. Possible values: "median", "mean", "trend", "trendA",
#' "trendA_bisplit", "trendA_trisplit", "maxA", "minA", "meanA", "medianA",
#' "plm", "movingMean", "movingMedian", "loreg"
#' @param width A number defining the line width
#' @param color A character string or a number defining the color of an element.
#' @param size A number deifning the size of an element.
#' @param type A character string with the line type: "solid", "dashed", "dotted"
#' @export
add_statline <- function(object,
                         stat,
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
    args = list(...),
    line = element_line(
      colour = color,
      size = width,
      linetype = linetype
    ),
    variable = variable,

    col = color,
    width = width,
    linetype = linetype

  )

  object$statlines <- c(object$statlines, list(new))
  object

}

#' @rdname scplot
#' @export
set_background <- function(object, color = NULL, fill = NULL, linetype = NULL, size = NULL) {

  if (!is.null(fill)) object$theme$plot.background.fill <- fill
  if (!is.null(frame)) object$theme$plot.background.col <- color

  object$theme$plot.background <- .merge_element(
    element_rect(
      colour = color,
      fill = fill,
      size = size,
      linetype = linetype
    ), object$theme$plot.background
  )

  object
}

#' @rdname scplot
#' @export
set_panel <- function(object,
                      color = NULL,
                      fill = NULL,
                      linetype = NULL,
                      size = NULL,
                      alpha = NULL) {

  if (!is.null(fill)) object$theme$panel.col <- fill
  if (!is.null(color)) object$theme$panel.frame.col <- color
  if (!is.null(size)) object$theme$panel.frame.width <- size
  if (!is.null(linetype)) object$theme$panel.frame.linetype <- linetype

  if (is.null(alpha)) {
    if (length(fill > 1)) alpha <- 0.5 else alpha <- 1
  }

  if (!is.null(fill)) fill <- alpha(fill, alpha)

  object$theme$panel.background <- .merge_element(
    element_rect(
      colour = color,
      fill = fill,
      size = size,
      linetype = linetype
    ), object$theme$panel.background
  )

  object
}

.merge_element <- function(new, old) {

  #class(new) <- class(old)
  #modifyList(new, old, keep.null = TRUE)
  merge_element(new, old)

}

.merge_theme <- function(new, old) {

  out <- old

  ids <- which(!(names(new) %in% names(old)) | !sapply(new, is.list))

  out[names(new)[ids]] <- new[ids]

  if (length(ids) > 1) new <- new[-ids]

  for(i in seq_along(new)) {
    label <- names(new)[i]
    if ("element" %in% class(out[[label]])) {
      out[[label]] <- merge_element(new[[i]], out[[label]])
    } else if ("list" %in% class(out[[label]])) {
      out[[label]] <- modifyList(out[[label]], new[[i]])
    }
  }

  out

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

  object$theme$ridge.col <- color
  object
}


