# scplot themes -----------------------------------------------------------

.scplot_themes <- list()

.scplot_themes$default = list(

  text = element_text(colour = "black", size = 11),

  plot.background = element_rect(),
  panel.background = element_rect(),
  panel.spacing.y = unit(2, "lines"),

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

  axis.title.y = element_text(color = "black", size = rel(1),
                              angle = 90, margin = margin(r = 1.5)),
  axis.title.x = element_text(color = "black", size = rel(1),
                              angle = 0, margin = margin(t = 1.5)),

  axis.text.x = element_text(color = "black", size = rel(1),
                             hjust = 0.5, margin = margin(t = 1.5)),
  axis.text.y = element_text(color = "black", size = rel(1),
                             hjust = 1, margin = margin(r = 1.5)),


  plot.title = element_text(color = "black",
                            margin = margin(0,0,2,0, unit = "lines"),
                            hjust = 0.5, size = rel(1)),

  plot.caption = element_text(color = "black",
                              margin = margin(0,0,0,2, unit = "lines"),
                              hjust = 0, size = rel(1)),

  plot.margin = margin(t = 1, 0.5, 0.5, 0.5, unit = "lines"),

  casenames = element_text(color = "black", vjust = 1, hjust = 0,
                           angle = 0, size = rel(1),
                           margin = margin(0.3, 0.3, 0.3, 0.3, unit = "lines")),

  casenames.strip = element_rect(color = "grey60",
                                 fill = "grey98",
                                 size = 0.5,
                                 linetype = "solid"),

  casenames.type = "within",

  phasenames = element_text(
    color = "black",
    size = 1,
    vjust = 0,
    hjust = 0.5,
    angle = 0,
    family = "sans", face = "plain",
    margin = margin(b = 1.5)
  ),

  phasenames.position.x = "centre",

  seperators = element_line(color = "black", size = 0.4, linetype = "dashed"),

  seperators.extent = "full",

  label.text = element_text(
    color = "black", vjust = 0.5, hjust = 0.5, angle = 0, size = rel(1)),
  label.background = element_rect(),

  labels.padding = 0.1,

  grid = element_line(),

  # legend

  legend.position = "none",
  legend.background = element_rect(),
  legend.text = element_text(),
  legend.margin = margin(0.3, 0.3, 0.3, 0.3, unit = "lines"),


  NULL
)

.scplot_themes$grid <- list(
  grid = element_line(colour = "lightblue", size = 0.2),
  panel.background = element_rect(fill = "grey95", size = 0)
)

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

.scplot_themes$grid2 <- list(
  ridge.col = "white", grid.col = "lightgreen", panel.frame.col = "black",
  panel.col = "grey95",
  lwd.line = 0.7, pch = 1, xaxis.text.size = 0.8, yaxis.text.size = 0.8
)

.scplot_themes$dark <- list(
  panel.col = "#16213E",

  plot.background.fill = "#1A1A2E",

  grid.col = "#999999",
  casenames.col = "white", phasenames.col = "white",
  xaxis.title.col = "white", yaxis.title.col = "white",
  xaxis.text.col = "white", yaxis.text.col = "white",
  xaxis.line.col = "#DDDDDD", xaxis.ticks.col = "#DDDDDD",
  yaxis.line.col = "#DDDDDD", yaxis.ticks.col = "#DDDDDD",


  seperators.col = "gold",

  dataline.col = "#DDDDDD",
  dataline.width = 2,
  dataline.linetype = "solid",

  datadots.col = "#E94560",
  datadots.shape = 17,
  datadots.size = 0.8
)

.scplot_themes$nodots <- list(
  ridge.col = "grey95",
  datadots.col = NULL,

  plot.background.fill = "grey95",

  grid.col = "grey80", panel.col = "grey99"
)

.scplot_themes$sienna <- list(
  grid.col = "orange",

  plot.background.fill = "seashell",

  panel.col = "moccasin", panel.frame.col = "darkseagreen",

  casenames.col = "sienna4", phasenames.col = "sienna4",
  yaxis.title.size = 0.8, xaxis.title.size = 0.8, xaxis.text.size = 0.7, yaxis.text.size = 0.7,
  casenames.size = 0.8, phasenames.size = 0.8,

  seperators.col = "sienna4",


  dataline.col = "darkolivegreen",
  dataline.width = 2,
  dataline.linetype = "solid",

  datadots.col = "seagreen4",
  datadots.shape = 18,
  datadots.size = 0.8,

  font = "serif"
)

.scplot_themes$phase_color <- list(
  panel.background = element_rect(
    fill = alpha(c("aliceblue", "mistyrose1", "honeydew"), 0.5))
)

.scplot_themes$phase_shade <- list(
  panel.background = element_rect(
    fill = alpha(c("grey80", "grey99", "grey90"), 0.5))
)



