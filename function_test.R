library(scan)
library(ggplot2)
library(scplot)

# print()

scplot(exampleABC)

# add_title

scplot(exampleABC) %>%
  add_title("Mein Titel", color = "red")

# set x/y labels

scplot(exampleABC) %>%
  set_ylabel("ylabel", color = "red", size= 12, angle = 0) %>%
  set_xlabel("xlabel", color = "blue")

# add_grid

scplot(exampleABC) %>%
  add_grid("lightblue", size = 0.5, linetype = "dotted")

# set axis

scplot(exampleABC) %>%
  set_yaxis(limits = c(0, 100), increment = 25) %>%
  set_xaxis(increment = 5, color = "red", increment_from = 0)  #color, size, line, position)

# add_label ---------

scplot(exampleABC) %>%
  set_yaxis(limits = c(30, 120), increment = 25) %>%
  add_labels(angle = 23, color = "black", size = 2, box = "lightblue",
             frame = "white", nudge_y = 15)

# set datalines

scplot(exampleAB_add) %>%
  set_dataline(color = "red", width = 0.5) %>%
  add_dataline(variable = "depression", color = "blue", dots = "black", shape = 3)

# set datalines / add datalines

scplot(exampleAB_add) %>%
  set_dataline(color = "red", width = 0.5) %>%
  add_dataline(
    variable = "depression", color = "blue", dots = "black", shape = 3
  )

# set casenames -------

scplot(exampleABC) %>%
  set_casenames(
    "hamster", "triller", "maus",
    size = 4, color = "red", angle = 0, x = c(3, 10, 6), y = 80,
    hjust = 0, vjust = 0
  )

scplot(exampleABC) %>%
  set_casenames(
    type = "stripe",
    size = 8, color = "orange", angle = 270, hjust = 0, width = 0.5,
    frame = "orange", fill = "grey10"
  )

# set phasenames ---------------

scplot(exampleABC) %>%
  set_phasenames(
    "hamster", "triller", "maus",
    color = "darkred", size = 3, x = "left", hjust = 0, face = "italic"
  )


# set separator ----------------

scplot(exampleABC) %>%
  set_seperator(color = "orange", width = 0.3, linetype = "solid")

# add_statline

scplot(exampleABC) %>%
  add_statline("mean", color = "red")

scplot(exampleAB_add) %>%
  set_dataline(color = "red", width = 0.5) %>%
  add_dataline(variable = "depression", color = "blue", dots = "black") %>%
  add_statline("min", color = "darkred", width = 0.3) %>%
  add_statline("max", color = "darkred", width = 0.3) %>%
  add_statline(
    "min", color = "darkblue", width = 0.3, variable = "depression") %>%
  add_statline(
    "max", color = "darkblue", width = 0.3, variable = "depression")




# crap --------------------------------------------------------------------



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

dat <- as.data.frame(exampleABC)

dat_stat <- dat %>% group_by(case, !!sym(pvar)) %>% summarise(stat = mean(values, na.rm = TRUE))


full_join(dat, dat_stat, by = c("case", "phase"))

