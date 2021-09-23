library(scan)
library(ggplot2)

# print()

scplot(exampleABC) %>%
  print()

# add_title

scplot(exampleABC) %>%
  add_title("Mein Titel", color = "red") %>%
  print()

# set x/y labels

scplot(exampleABC) %>%
  set_ylabel("ylabel", color = "red", size= 12, angle = 0) %>%
  set_xlabel("xlabel", color = "blue") %>%
  print()

# add_grid

scplot(exampleABC) %>%
  add_grid("lightblue", size = 0.5, linetype = "dotted") %>%
  print()

# set axis

scplot(exampleABC) %>%
  set_yaxis(limits = c(0, 100), increment = 25) %>%
  set_xaxis(increment = 5, color = "red", increment_from = 0) %>%  #color, size, line, position)
  print()

# add_label ---------

scplot(exampleABC) %>%
  set_yaxis(limits = c(30, 120), increment = 25) %>%
  add_labels(angle = 23, color = "black", size = 2, box = "lightblue",
             frame = "white", nudge_y = 15) %>%
  print()


# set datalines

scplot(exampleAB_add) %>%
  set_dataline(color = "red", width = 0.5) %>%
  add_dataline(variable = "depression", color = "blue", dots = "black", shape = 3) %>%
  print()


# set datalines

scplot(exampleAB_add) %>%
  set_dataline(color = "red", width = 0.5) %>%
  add_dataline(variable = "depression", color = "blue", dots = "black", shape = 3) %>%
  print()

# set casenames

scplot(exampleABC) %>%
  #set_casenames("hamster", "triller", "maus", size = 4, color = "red", angle = 0, x = c(3, 10, 6), y = 80, hjust = 0, vjust = 0) %>%
  print()






#' #' @rdname scplot
#' #' @export
#' style_box <- function(fill = NULL, size = NULL, linetype = NULL,
#'                       color = NULL) {
#'
#'   #structure(,
#'   #class = c("scplot_style_box"))
#'   style <- list(
#'     fill = fill, color = color, size = size,
#'     linetype = linetype
#'   )
#'   style
#' }
#'
#' #' @rdname scplot
#' #' @export
#' style_text <- function (family = NULL, face = NULL, size = NULL,
#'                         hjust = NULL, vjust = NULL, angle = NULL,
#'                         lineheight = NULL, color = NULL, margin = NULL) {
#'
#'   style <- list(family = family, face = face, color = color,
#'                 size = size, hjust = hjust, vjust = vjust, angle = angle,
#'                 lineheight = lineheight, margin = margin)
#' }




