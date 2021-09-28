# scplot themes -----------------------------------------------------------

.scplot_themes <- list()

.scplot_themes$default = list(

  plot.background = element_rect(size = 1, linetype = "solid"),
  panel.background = element_rect(colour = "white", size = 1, linetype = "solid"),

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

  axis.text.x = element_text(color = "black", angle = 0, size = 7),
  axis.text.y = element_text(color = "black", angle = 0, size = 7),


    plot.title = element_text(
    color = "black",
    margin = margin(0,0,2,0, unit = "lines"),
    hjust = 0.5
  ),

  plot.caption = element_text(
    color = "black",
    margin = margin(0,0,0,2, unit = "lines"),
    hjust = 0
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



