# scplot themes -----------------------------------------------------------

.scplot_themes <- list()

# basic -----------------------------------------------------------------

.scplot_themes$basic = list(

  text = element_text(colour = "black", size = 11, family = "sans",
                      face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
                      lineheight = 1),

  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  panel.spacing.y = unit(1.5, "lines"),

  dataline = list(
    "1" = element_line(colour = "#000000", linetype = "solid", linewidth = 0.5),
    "2" = element_line(colour = "#0072B2", linetype = "solid", linewidth = 0.5),
    "3" = element_line(colour = "#D55E00", linetype = "solid", linewidth = 0.5),
    "4" = element_line(colour = "#009E73", linetype = "solid", linewidth = 0.5),
    "5" = element_line(colour = "#E69F00", linetype = "solid", linewidth = 0.5)
  ),

  datapoint = list(
    "1" = element_point(colour = "#000000", shape = 16, size = 2),
    "2" = element_point(colour = "#0072B2", shape = 16, size = 2),
    "3" = element_point(colour = "#D55E00", shape = 16, size = 2),
    "4" = element_point(colour = "#009E73", shape = 16, size = 2),
    "5" = element_point(colour = "#E69F00", shape = 16, size = 2)
  ),

  statline = list(
    a = element_line(color = "tomato3", linewidth = 0.7, linetype = "solid"),
    b = element_line(color = "royalblue3", linewidth = 0.7, linetype = "solid"),
    c = element_line(color = "olivedrab4", linewidth = 0.7, linetype = "solid"),
    d = element_line(color = "orange3", linewidth = 0.7, linetype = "solid")
  ),

  axis.expand.x = c(0, 0.6, 0, 0.6),
  axis.expand.y = c(0.05, 0, 0.05, 0),
  axis.line.x = element_line(
    color = "black", linewidth = 0.4, linetype = "solid"),
  axis.line.y = element_line(
    color = "black", linewidth = 0.4, linetype = "solid"),

  axis.ticks.length = unit(2.75, "points"),
  axis.ticks = element_line(
    color = "black", linewidth = 0.4, linetype = "solid"),

  axis.title.y = element_text(angle = 90, margin = margin(r = 1.5)),
  axis.title.x = element_text(margin = margin(t = 2)),

  axis.text.x = element_text(size = rel(0.8), hjust = 0.5,
                             margin = margin(t = 1.5)),
  axis.text.y = element_text(size = rel(0.8), hjust = 1,
                             margin = margin(r = 1.5)),

  plot.title = element_text(
                            margin = margin(0,0,2,0, unit = "lines"),
                            hjust = 0.5),

  plot.caption = element_text(margin = margin(t = 1, r = 0, b = 0, l = 2,
                              unit = "lines"),
                              hjust = 0,
                              face = "italic"),

  plot.margin = margin(t = 1, 0.5, 0.5, 0.5, unit = "lines"),

  casenames = element_text(vjust = 1, hjust = 0,
                           margin = margin(0.3, 0.3, 0.3, 0.3, unit = "lines")),

  casenames.strip = element_rect(color = "grey60",
                                 fill = "grey98",
                                 size = 0.5,
                                 linetype = "solid"),
  casenames.background = element_rect(size = 0.1,
                                      linetype = "solid"),

  casenames.position = "topleft",

  phasenames = element_text(vjust = 0, hjust = 0.5, margin = margin(b = 1.5)),

  phasenames.position.x = "centre",

  separators = element_line(
    color = "black", linewidth = 0.4, linetype = "dashed"),

  separators.extent = "full",

  label.text = element_text(
    color = "black", vjust = 0.5, hjust = 0.5, angle = 0, size = rel(1)),
  label.background = element_rect(),

  label.padding = 0.1,

  grid = element_line(linewidth = 0),

  # legend

  legend.position = "none",
  legend.background = element_rect(),
  legend.text = element_text(size = rel(0.7), colour = "black"),
  legend.title = element_text(size = rel(0.7), colour = "black"),
  legend.margin = margin(0.3, 0.3, 0.3, 0.3, unit = "lines"),


  NULL
)


# grid --------------------------------------------------------------------

.scplot_themes$grid <- list(
  grid = element_line(colour = "lightblue", linewidth = 0.2),
  panel.background = element_rect(fill = "grey95", size = 0)
)


# default -----------------------------------------------------------------

.scplot_themes$default <- .merge_theme(
  .scplot_themes$grid, .scplot_themes$basic
)

# sizes -------------------------------------------------------------------

.scplot_themes$small <- list(
  text = element_text(size = 8),
  panel.spacing.y = unit(1, "lines")
)

.scplot_themes$tiny <- list(
  text = element_text(size = 6),
  panel.spacing.y = unit(0.5, "lines")
)

.scplot_themes$big <- list(
  text = element_text(size = 14),
  panel.spacing.y = unit(1.5, "lines")
)

# minimal -----------------------------------------------------------------

.scplot_themes$minimal = list(

  panel.spacing.y = unit(1, "lines"),
  panel.background = element_blank(),
  datapoint = list(
    "1" = element_point(size = 1.5)),

  axis.ticks.length = unit(0, "points"),
  axis.ticks =  element_blank(),

  axis.line.x = element_blank(),

  axis.text.x = element_blank(),
  axis.text.y = element_blank(),

  separators = element_line(
    color = "black", linewidth = 0.3, linetype = "solid"),

  grid = element_blank(),
  NULL
)

# dark --------------------------------------------------------------------

.scplot_themes$dark <- list(

  panel.background = element_rect(fill = "#16213E"),
  plot.background = element_rect(fill = "#1A1A2E"),

  grid = element_line(colour = "#999999", linewidth = 0.2),

  casenames = element_text(color = "white"),

  phasenames = element_text(colour  = "white"),

  axis.title.y = element_text(color = "white"),
  axis.title.x = element_text(color = "white"),

  axis.text.x = element_text(color = "white"),
  axis.text.y = element_text(color = "white"),

  axis.ticks = element_line(color = "#DDDDDD"),

  separators = element_line(
    color = "gold", linewidth = 0.3, linetype = "solid"),

  dataline = list(
    "1" = element_line(colour = "#DDDDDD")),

  datapoint = list(
    "1" = element_point(colour = "#E94560", shape = 17))

)

# sienna ------------------------------------------------------------------

.scplot_themes$sienna <- list(

  grid = element_line(colour = "orange", linewidth = 0.2),

  panel.background = element_rect(colour = "#16213E", fill = "white"),
  plot.background = element_rect(fill = "moccasin", colour = "darkseagreen"),

  casenames = element_text(color = "sienna4"),
  phasenames = element_text(colour  = "sienna4"),

  separators = element_line(color = "sienna4"),

  dataline = list(
    "1" = element_line(colour = "darkolivegreen")),
  datapoint = list(
    "1" = element_point(colour = "seagreen4", shape = 18)),

  text = element_text(family = "serif")

)

.scplot_themes$phase_color <- list(
  panel.background = element_rect(
    fill = alpha(c("aliceblue", "mistyrose1", "honeydew"), 0.5))
)

.scplot_themes$phase_shade <- list(
  panel.background = element_rect(
    fill = alpha(c("grey80", "grey99", "grey90"), 0.5))
)

# grid2 ---------------------------------------------------

.scplot_themes$grid2 <- list(
  grid = element_line(colour = "lightgreen", linewidth = 0.2),
  panel.background = element_rect(fill = "grey95", size = 1)
)

# illustration -----------------------------------------------------------------

.scplot_themes$illustration = list(

  text = element_text(size = 16, family = "sans",
                      face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
                      lineheight = 1),

  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  panel.spacing.y = unit(1.5, "lines"),

  dataline = list(
    "1" = element_line(colour = "#000000", linetype = "solid", linewidth = 0.5),
    "2" = element_line(colour = "#0072B2", linetype = "solid", linewidth = 0.5),
    "3" = element_line(colour = "#D55E00", linetype = "solid", linewidth = 0.5),
    "4" = element_line(colour = "#009E73", linetype = "solid", linewidth = 0.5),
    "5" = element_line(colour = "#E69F00", linetype = "solid", linewidth = 0.5)
  ),

  datapoint = list(
    "1" = element_point(colour = "#000000", shape = 15, size = 3),
    "2" = element_point(colour = "#0072B2", shape = 15, size = 3),
    "3" = element_point(colour = "#D55E00", shape = 15, size = 3),
    "4" = element_point(colour = "#009E73", shape = 15, size = 3),
    "5" = element_point(colour = "#E69F00", shape = 15, size = 3)
  ),

  statline = list(
    a = element_line(color = "black", linewidth = 0.7, linetype = "solid"),
    b = element_line(color = "royalblue3", linewidth = 0.7, linetype = "solid"),
    c = element_line(color = "olivedrab4", linewidth = 0.7, linetype = "solid"),
    d = element_line(color = "orange3", linewidth = 0.7, linetype = "solid")
  ),

  axis.expand.x = c(0, 0.6, 0, 0.6),
  axis.expand.y = c(0.05, 0, 0.05, 0),
  axis.line.x = element_line(
    color = "black", linewidth = 0.4, linetype = "solid"),
  axis.line.y = element_line(
    color = "black", linewidth = 0.4, linetype = "solid"),

  axis.ticks.length = unit(2.75, "points"),
  axis.ticks = element_line(
    color = "black", linewidth = 0.4, linetype = "solid"),

  axis.title.y = element_text(color = "white", angle = 90, margin = margin(r = 1.5)),
  axis.title.x = element_text(color = "white", margin = margin(t = 1.5)),

  axis.text.x = element_text(size = rel(0.8), hjust = 0.5,
                             margin = margin(t = 1.5)),
  axis.text.y = element_text(size = rel(0.8), hjust = 1,
                             margin = margin(r = 1.5)),

  plot.title = element_text(
    margin = margin(0,0,2,0, unit = "lines"),
    hjust = 0.5),

  plot.caption = element_text(margin = margin(0,0,0,2, unit = "lines"),
                              hjust = 0),

  plot.margin = margin(t = 1, 0.5, 0.5, 0.5, unit = "lines"),

  casenames = element_text(vjust = 1, hjust = 0, color = "white",
                           margin = margin(0.3, 0.3, 0.3, 0.3, unit = "lines")),

  casenames.strip = element_rect(color = "grey60",
                                 fill = "grey98",
                                 size = 0.5,
                                 linetype = "solid"),
  casenames.background = element_rect(size = 0.1,
                                      linetype = "solid"),

  casenames.position = "topleft",

  phasenames = element_text(color = "white", vjust = 0, hjust = 0.5,
                            margin = margin(b = 1.5)),

  phasenames.position.x = "centre",

  separators = element_line(
    color = "black", linewidth = 0.4, linetype = "solid"),

  separators.extent = "full",

  label.text = element_text(
    color = "black", vjust = 0.5, hjust = 0.5, angle = 0, size = rel(1)),
  label.background = element_rect(),

  label.padding = 0.1,

  grid = element_line(linewidth = 0),

  # legend

  legend.position = "none",
  legend.background = element_rect(),
  legend.text = element_text(size = rel(0.7), colour = "black"),
  legend.title = element_text(size = rel(0.7), colour = "black"),
  legend.margin = margin(0.3, 0.3, 0.3, 0.3, unit = "lines"),


  NULL
)
