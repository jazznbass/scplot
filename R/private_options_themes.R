# scplot themes -----------------------------------------------------------

.scplot_themes <- list()

.scplot_themes$default = list(

  mar = c(bottom = 0, left = 0, top = 1, right = 0.5),
  oma = c(0, 0, 0, 0),

  font = "sans",

  panel.col = "white",
  panel.frame.col = NULL,
  panel.frame.width = 1,
  panel.frame.linetype = "solid",

  xaxis.text.col = "black",
  xaxis.text.size = 0.8,
  xaxis.text.angel = 0,

  xaxis.ticks.col = "black",
  xaxis.ticks.length = -0.3,

  xaxis.line.col = "black",

  xaxis.title.col = "black",
  xaxis.title.size = 1,
  xaxis.title.vjust = 0,

  yaxis.text.col = "black",
  yaxis.text.size = 0.8,
  yaxis.text.angel = 0,

  yaxis.title.col = "black",
  yaxis.title.size = 1,
  yaxis.title.hjust = 0,
  yaxis.title.angle = 0,

  yaxis.ticks.col = "black",
  yaxis.line.col = "black",
  yaxis.ticks.length = -0.3,


  ridge.col = NULL,

  title.col = "black",
  title.margin = 0,
  #title.hjust = "center",
  title.align = "center",
  title.wrap = NULL,
  title.parse = FALSE,

  caption.col = "black",
  caption.margin = 0,
  #caption.hjust = "left",
  caption.align = "left",
  caption.wrap = NULL,
  caption.parse = FALSE,

  plot.background.fill = NULL,
  plot.background.col = NULL,
  plot.background.width = 1,
  plot.background.linetype = "solid",

  labels.col = NULL,
  labels.size = 0.6,
  labels.round = 1,
  labels.vjust = 0.5,
  labels.hjust = 0.5,
  labels.nudge_x = 0,
  labels.nudge_y = 1.5,
  labels.family = "sans",
  labels.face = 0,
  labels.box.fill = NULL,
  labels.box.col = NULL,
  labels.box.width = 1,
  labels.box.type = "solid",
  labels.box.margin = 0.2,

  phasenames.col = "black",
  phasenames.size = 1,

  phasenames.position.y = "above",
  phasenames.family = "sans",
  phasenames.face = "plain",
  phasenames.box.fill = NULL,
  phasenames.box.col = NULL,
  phasenames.box.width = 1,
  phasenames.box.type = "solid",
  phasenames.box.margin = 0.2,

  casenames.col = "black",
  casenames.size = 1,
  casenames.position.x = NULL,
  casenames.position.y = NULL,

  grid.width = 1,
  grid.linetype = "dotted",
  grid.col = NULL,

  seperators.linetype = "dashed",
  seperators.width = 1.5,
  seperators.col = "black",

  legend.text.size = 0.8,
  legend.text.col = "black",
  legend.bg.col = "white",
  legend.position.case = 1,
  legend.position.x = NULL,
  legend.position.y = NULL,
  legend.line.length = 1,

  statline.col = "grey",
  statline.width = 1,
  statline.linetype = "solid",

  # ggplot2 ---------------------

  dataline.col = "black",
  dataline.width = 0.5,
  dataline.linetype = "solid",

  datadots.col = "black",
  datadots.shape = 16,
  datadots.size = 2,

  statline = element_line(color = "grey", size = 0.7, linetype = "solid"),

  axis.line = element_line(color = "black", size = 0.7, linetype = "solid"),

  axis.title.y = element_text(color = "black", angle = 90),
  axis.title.x = element_text(color = "black", angle = 0),

  plot.title = element_text(
    color = "black",
    margin = margin(0,0,2,0, unit = "lines"),
    hjust = 0.5
  ),

  plot.margin = margin(t = 1, 0.5, 0.5, 0.5, unit = "lines"),

  casenames = element_text(
    color = "black", size = 4, vjust = 1, hjust = 0, angle = 0
  ),

  casenames.strip = element_rect(
    color = "black", fill = "grey80", size = 1, linetype = "solid"
  ),

  casenames.type = "within",

  phasenames = element_text(
    color = "black",
    size = 4,
    vjust = 0,
    hjust = 0.5,
    angle = 0,
    family = "sans", face = "plain"
  ),

  phasenames.position.x = "centre",

  seperators = element_line(
    color = "black", size = 0.4, linetype = "dashed"
  ),

  seperators.extent = "full",

  labels.text = element_text(
    color = "black", vjust = 0.5, hjust = 0.5, angle = 0, size = 3
  ),

  labels.box = element_rect(fill = NULL, colour = "black"),
  labels.padding = 0.1,



  grid = element_line(),

  NULL
)

.scplot_themes$tiny <- list(

  xaxis.title.size = 0.5, yaxis.title.size = 0.5,
  xaxis.text.size = 0.5, yaxis.text.size = 0.5,
  casenames.size = 0.5,
  phasenames.size = 0.5,
  grid.width = 0.7,
  dataline.width = 0.7,
  datadots.size = 0.5,
  seperators.width = 0.7

)

.scplot_themes$small <- list(

  xaxis.title.size = 0.75, yaxis.title.size = 0.75,
  xaxis.text.size = 0.75, yaxis.text.size = 0.75,
  casenames.size = 0.75, phasenames.size = 0.75,
  grid.width = 0.85,
  dataline.width = 0.85,
  datadots.size = 0.75,
  seperators.width = 0.85
)

.scplot_themes$big <- list(
  xaxis.title.size = 1.25, yaxis.title.size = 1.25,
  xaxis.text.size = 1.25, yaxis.text.size = 1.25,
  casenames.size = 1.25, phasename.sizes = 1.25,
  grid.width = 1.5,
  dataline.width = 1.5,
  datadots.size = 1.25,
  seperators.width = 1.5
)


.scplot_themes$chart <- list(
  ridge.col = "grey50",
  panel.col = "grey98",

  grid.col = "grey75",
  yaxis.title.size = 0.8, xaxis.title.size = 0.8,
  casenames.size = 0.8, phasenames.size = 0.8,
  dataline.width = 0.7,
  labels.col = "black"

)

.scplot_themes$grid <- list(
  grid.col = "lightblue", panel.col = "grey95",
  lwd.line = 0.7, pch = 19, xaxis.text.size = 0.8, yaxis.text.size = 0.8,
  casenames.size = 0.8, phasenames.size = 0.8
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
  panel.col = c("aliceblue", "mistyrose1", "honeydew")
)

.scplot_themes$phase_shade <- list(
  panel.col = c("grey94", "grey99", "grey90")
)



