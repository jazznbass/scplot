# scplot themes -----------------------------------------------------------

.scplot_themes <- list()


# default -----------------------------------------------------------------

.scplot_themes$basic = list(

  text = element_text(colour = "black", size = 11, family = "sans",
                      face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
                      lineheight = 1),

  plot.background = element_rect(),
  panel.background = element_rect(),
  panel.spacing.y = unit(1.5, "lines"),

  dataline.col = "black",
  dataline.width = 0.5,
  dataline.linetype = "solid",

  datadots.col = "black",
  datadots.shape = 16,
  datadots.size = 2,

  statline = element_line(color = "grey", size = 0.7, linetype = "solid"),

  axis.line.x = element_line(color = "black", size = 0.4, linetype = "solid"),
  axis.line.y = element_line(color = "black", size = 0.4, linetype = "solid"),

  axis.ticks.length = unit(2.75, "points"),
  axis.ticks = element_line(color = "black", size = 0.4, linetype = "solid"),

  axis.title.y = element_text(angle = 90, margin = margin(r = 1.5)),
  axis.title.x = element_text(margin = margin(t = 1.5)),

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

  casenames = element_text(vjust = 1, hjust = 0,
                           margin = margin(0.3, 0.3, 0.3, 0.3, unit = "lines")),

  casenames.strip = element_rect(color = "grey60",
                                 fill = "grey98",
                                 size = 0.5,
                                 linetype = "solid"),

  casenames.position = "topleft",

  phasenames = element_text(vjust = 0, hjust = 0.5, margin = margin(b = 1.5)),

  phasenames.position.x = "centre",

  seperators = element_line(color = "black", size = 0.4, linetype = "dashed"),

  seperators.extent = "full",

  label.text = element_text(
    color = "black", vjust = 0.5, hjust = 0.5, angle = 0, size = rel(1)),
  label.background = element_rect(),

  label.padding = 0.1,

  grid = element_blank(),

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
  grid = element_line(colour = "lightblue", size = 0.2),
  panel.background = element_rect(fill = "grey95", size = 0)
)


# default -----------------------------------------------------------------

.scplot_themes$default <- .merge_theme(.scplot_themes$grid, .scplot_themes$basic)

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
  datadots.size = 1.5,

  axis.ticks.length = unit(0, "points"),
  axis.ticks = element_blank(),

  axis.line.x = element_blank(),

  axis.text.x = element_blank(),
  axis.text.y = element_blank(),

  seperators = element_line(color = "black", size = 0.3, linetype = "solid"),
  grid = element_blank(),
  NULL
)

# dark --------------------------------------------------------------------

.scplot_themes$dark <- list(

  panel.background = element_rect(fill = "#16213E"),
  plot.background = element_rect(fill = "#1A1A2E"),

  grid = element_line(colour = "#999999", size = 0.2),

  casenames = element_text(color = "white"),

  phasenames = element_text(colour  = "white"),

  axis.title.y = element_text(color = "white"),
  axis.title.x = element_text(color = "white"),

  axis.text.x = element_text(color = "white"),
  axis.text.y = element_text(color = "white"),

  axis.ticks = element_line(color = "#DDDDDD"),

  seperators = element_line(color = "gold", size = 0.3, linetype = "solid"),

  dataline.col = "#DDDDDD",
  datadots.col = "#E94560",
  datadots.shape = 17
)


# sienna ------------------------------------------------------------------

.scplot_themes$sienna <- list(

  grid = element_line(colour = "orange", size = 0.2),

  panel.background = element_rect(colour = "#16213E", fill = "white"),
  plot.background = element_rect(fill = "moccasin", colour = "darkseagreen"),

  casenames = element_text(color = "sienna4"),
  phasenames = element_text(colour  = "sienna4"),

  seperators = element_line(color = "sienna4"),

  dataline.col = "darkolivegreen",

  datadots.col = "seagreen4",
  datadots.shape = 18,

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






# !!! translation missing ---------------------------------------------------

.scplot_themes$grid2 <- list(
  ridge.col = "white", grid.col = "lightgreen", panel.frame.col = "black",
  panel.col = "grey95",
  lwd.line = 0.7, pch = 1, xaxis.text.size = 0.8, yaxis.text.size = 0.8
)






