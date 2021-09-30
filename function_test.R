library(scan)
library(tidyverse)
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
  add_grid("lightblue", size = 0.2, linetype = "dotted")

# set axis

scplot(exampleABC) %>%
  set_yaxis(limits = c(0, 100), increment = 25, color = "blue") %>%
  set_xaxis(increment = 5, color = "red", increment_from = 0)

# add_label ---------

scplot(exampleABC) %>%
  set_yaxis(limits = c(30, 120), increment = 25) %>%
  set_panel(fill = "grey96") %>%
  add_labels(angle = 23, color = "black", size = 2, box = "lightblue",
             frame = "white", nudge_y = 10)

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
  add_statline("trendA", color = "darkred")

scplot(exampleAB_add) %>%
  set_dataline(color = "red", width = 0.5) %>%
  add_dataline(variable = "depression", color = "blue", dots = "black") %>%
  add_statline("min", color = "darkred", width = 0.3) %>%
  add_statline("max", color = "darkred", width = 0.3) %>%
  add_statline("loreg", color = "grey50", width = 0.5) %>%
  add_statline(
    "min", color = "darkblue", width = 0.3, variable = "depression") %>%
  add_statline(
    "max", color = "darkblue", width = 0.3, variable = "depression") %>%
  add_statline(
    "trend", color = "grey50", width = 0.5, variable = "depression")

scplot(exampleABC) %>%
  add_statline("movingMean", lag = 2, color = "darkred")

scplot(exampleAB_add) %>%
  add_statline("loreg", f = 0.5, color = "darkred")

# panel/ background color

scplot(exampleABC) %>%
  set_background(fill = "lightblue", color = "darkblue", size = 1) %>%
  set_panel(fill = "lightblue", size = 0.5)


# add ridge

scplot(exampleABC) %>%
  add_ridge("lightblue")

# add caption

scplot(exampleABC) %>%
  add_caption("Note. Hallo", color = "darkred", hjust = 0)





p <- scplot(exampleABC)

p <- print(p)

p <- p + theme(legend.position = c(.95, .95))


# Right -> inside the plot area
p + theme(
  legend.position = c(.95, .95),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)


# crap --------------------------------------------------------------------
scplot(exampleA1B1A2B2) %>%
  add_ridge("white") %>%
  set_xaxis(increment = 4, size = 3, color = "brown") %>%
  set_yaxis(color = "sienna3", size = 1) %>%
  set_ylabel("Points", color = "sienna3", size = 7, orientation = 0) %>%
  set_xlabel("Weeks", size = 7, color = "brown") %>%
  add_title("Points by week", color = "sienna4", size = 12, face = 3) %>%
  add_caption("Note: An extensive Example.", color = "black", size = 7, face = 3) %>%
  set_phasenames("Baseline", "Intervention", "Fall-Back", "Intervention 2", color = "darkgreen", size = 3) %>%
  set_casenames(sample_names(3), color = "steelblue4", size = 3) %>%
  set_panel(fill = c("grey94", "grey99"), color = "sienna4") %>%
  add_grid(color = "grey85", size = 0.5) %>%
  set_dataline(color = "black", width = 1, linetype = "solid", dots = "sienna4", size = 1, shape = 18) %>%
  add_labels(color = "sienna", size = 0.5) %>%
  set_seperator(extent = 0.8, width = 0.5, linetype = "solid", color = "sienna") %>%
  add_statline(stat = "trendA", color = "tomato2") %>%
  add_statline(stat = "maxA", color = "lightblue") %>%
  add_marks(case = 1:2, positions = 14, color = "red3", size = 2, shape = 4) %>%
  add_marks(case = "all", positions = 'points < quantile(points, 0.1)', color = "blue3", size = 1.5) %>%
  add_marks(positions = outlier(exampleABAB), color = "brown", size = 2) %>%
  add_text(case = 1, x = 5, y = 35, "Interesting", color = "darkgreen", angle = 20, size = 0.5) %>%
  add_arrow(case = 1, 5, 30, 5, 22, color = "steelblue") %>%
  set_background(fill = "sienna1") #%>%
#add_legend(case = 2, title = "Legend") %>%
#set_theme_element(legend.text.size = 0.5)



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

