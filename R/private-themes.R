# scplot themes -----------------------------------------------------------

.scplot_themes <- list()

# basic -----------------------------------------------------------------

.scplot_themes$basic = list(

  theme_type = "complete",
  theme_name = "basic",

  text = element_text(colour = "black", size = 11, family = "sans",
                      face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
                      lineheight = 1),

  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  panel.spacing.y = unit(1.5, "lines"),

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
                                 linewidth = 0.5,
                                 linetype = "solid"),
  casenames.background = element_rect(linewidth = 0.1,
                                      linetype = "solid"),

  casenames.position = "topleft",

  phasenames = element_text(vjust = 0, hjust = 0.5, margin = margin(b = 1.5)),

  phasenames.position.x = "centre",

  separators = element_line(
    color = "black", linewidth = 0.4, linetype = "dashed"),

  separators.extent = "full",
  separators.staircase = FALSE,

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
    a = element_line(colour = "#B22222", linewidth = 0.7, linetype = "solid"),  # firebrick
    b = element_line(colour = "#3A5FCD", linewidth = 0.7, linetype = "solid"),  # medium blue
    c = element_line(colour = "#556B2F", linewidth = 0.7, linetype = "solid"),  # dark olive green
    d = element_line(colour = "#CD8500", linewidth = 0.7, linetype = "solid"),  # dark goldenrod
    e = element_line(colour = "#6A3D9A", linewidth = 0.7, linetype = "solid"),  # dark violet
    f = element_line(colour = "#1F6F78", linewidth = 0.7, linetype = "solid")   # dark teal
  )

)

# colour styles --------

## colours_cool_muted ------

.scplot_themes$colours_muted <- list(

  theme_type = "colours",
  theme_name = "colours_muted",

  dataline = list(
    "1" = element_line(colour = "#2F2F2F", linetype = "solid", linewidth = 0.5),  # soft black / charcoal
    "2" = element_line(colour = "#4C78A8", linetype = "solid", linewidth = 0.5),  # muted blue
    "3" = element_line(colour = "#C27D38", linetype = "solid", linewidth = 0.5)   # muted amber-brown
  ),

  datapoint = list(
    "1" = element_point(colour = "#2F2F2F", shape = 16, size = 2),
    "2" = element_point(colour = "#4C78A8", shape = 16, size = 2),
    "3" = element_point(colour = "#C27D38", shape = 16, size = 2)
  ),

  statline = list(
    a = element_line(colour = "#8E3B46", linewidth = 0.7, linetype = "solid"),  # muted wine red
    b = element_line(colour = "#4F6D9A", linewidth = 0.7, linetype = "solid"),  # muted cobalt
    c = element_line(colour = "#5C7A3A", linewidth = 0.7, linetype = "solid"),  # muted olive green
    d = element_line(colour = "#A36A2B", linewidth = 0.7, linetype = "solid"),  # muted ochre
    e = element_line(colour = "#7A4E8A", linewidth = 0.7, linetype = "solid"),  # muted violet
    f = element_line(colour = "#2F7C7C", linewidth = 0.7, linetype = "solid")   # muted teal
  )

)

## colours_black ---------------------------------------------------------------

.scplot_themes$colours_bw <- list(

  dataline = list(
    "1" = element_line(colour = "#000000", linetype = "solid",    linewidth = 0.5),
    "2" = element_line(colour = "#000000", linetype = "dashed",   linewidth = 0.5),
    "3" = element_line(colour = "#000000", linetype = "dotted",   linewidth = 0.5),
    "4" = element_line(colour = "#000000", linetype = "dotdash",  linewidth = 0.5),
    "5" = element_line(colour = "#000000", linetype = "longdash", linewidth = 0.5)
  ),

  datapoint = list(
    "1" = element_point(colour = "#000000", shape = 16, size = 2),  # filled circle
    "2" = element_point(colour = "#000000", shape = 17, size = 2),  # filled triangle
    "3" = element_point(colour = "#000000", shape = 15, size = 2),  # filled square
    "4" = element_point(colour = "#000000", shape = 18, size = 2),  # filled diamond
    "5" = element_point(colour = "#000000", shape = 8,  size = 2)   # star
  )
)

## colours_publication ---------------------------------------------------------------
.scplot_themes$colors_publication <- list(

  theme_type = "colours",

  dataline = list(
    "1" = element_line(colour = "#000000", linetype = "solid", linewidth = 0.5),
    "2" = element_line(colour = "#5A5A5A", linetype = "solid", linewidth = 0.5),
    "3" = element_line(colour = "#8A8A8A", linetype = "solid", linewidth = 0.5)
  ),

  datapoint = list(
    "1" = element_point(colour = "#000000", shape = 16, size = 2),
    "2" = element_point(colour = "#5A5A5A", shape = 16, size = 2),
    "3" = element_point(colour = "#8A8A8A", shape = 16, size = 2)
  ),

  statline = list(
    a = element_line(colour = "#7A1E1E", linewidth = 0.7, linetype = "solid"),    # dark red
    b = element_line(colour = "#1F3F7A", linewidth = 0.7, linetype = "dashed"),   # dark blue
    c = element_line(colour = "#3F5A1F", linewidth = 0.7, linetype = "dotdash"),  # dark olive
    d = element_line(colour = "#8A5A1A", linewidth = 0.7, linetype = "twodash"),  # dark ochre
    e = element_line(colour = "#5A3A7A", linewidth = 0.7, linetype = "longdash"), # dark violet
    f = element_line(colour = "#1F5A5A", linewidth = 0.7, linetype = "dotted")    # dark teal
  )
)

## colours_blind ------
.scplot_themes$colors_blind <- list(

  theme_type = "colours",

  dataline = list(
    "1" = element_line(colour = "#000000", linetype = "solid", linewidth = 0.5),
    "2" = element_line(colour = "#0072B2", linetype = "solid", linewidth = 0.5),
    "3" = element_line(colour = "#D55E00", linetype = "solid", linewidth = 0.5)
  ),

  datapoint = list(
    "1" = element_point(colour = "#000000", shape = 16, size = 2),
    "2" = element_point(colour = "#0072B2", shape = 16, size = 2),
    "3" = element_point(colour = "#D55E00", shape = 16, size = 2)
  ),

  statline = list(
    a = element_line(colour = "#CC79A7", linewidth = 0.8, linetype = "solid"),    # reddish purple
    b = element_line(colour = "#009E73", linewidth = 0.8, linetype = "solid"),    # bluish green
    c = element_line(colour = "#E69F00", linewidth = 0.8, linetype = "solid"),    # orange
    d = element_line(colour = "#56B4E9", linewidth = 0.8, linetype = "solid"),    # sky blue
    e = element_line(colour = "#D55E00", linewidth = 0.8, linetype = "solid"),    # vermillion
    f = element_line(colour = "#F0E442", linewidth = 1.0, linetype = "solid")     # yellow
  )
)

## colours_presentation ------

.scplot_themes$colors_presentation <- list(

  theme_type = "colours",

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
    a = element_line(colour = "#B22222", linewidth = 0.7, linetype = "solid"),  # firebrick
    b = element_line(colour = "#3A5FCD", linewidth = 0.7, linetype = "solid"),  # medium blue
    c = element_line(colour = "#556B2F", linewidth = 0.7, linetype = "solid"),  # dark olive green
    d = element_line(colour = "#CD8500", linewidth = 0.7, linetype = "solid"),  # dark goldenrod
    e = element_line(colour = "#6A3D9A", linewidth = 0.7, linetype = "solid"),  # dark violet
    f = element_line(colour = "#1F6F78", linewidth = 0.7, linetype = "solid")   # dark teal
  )
)


# element styles -----------------------------------------------------------------

## grid --------------------------------------------------------------------

.scplot_themes$grid <- list(

  theme_type = "element",

  grid = element_line(colour = "lightblue", linewidth = 0.2),
  panel.background = element_rect(fill = "grey95", linewidth = 0)
)

## grid2 ---------------------------------------------------

.scplot_themes$grid2 <- list(

  theme_type = "element",

  grid = element_line(colour = "lightgreen", linewidth = 0.2),
  panel.background = element_rect(fill = "grey95", linewidth = 1)
)


## grid3 --------------------------------------------------------------------

.scplot_themes$grid3 <- list(

  theme_type = "element",

  grid = element_line(colour = "grey90", linewidth = 0.2),
  panel.background = element_rect(fill = "grey99", linewidth = 0)
)

## sizes -------------------------------------------------------------------

.scplot_themes$small <- list(

  theme_type = "element",

  text = element_text(size = 8),
  panel.spacing.y = unit(1, "lines")
)

.scplot_themes$tiny <- list(

  theme_type = "element",

  text = element_text(size = 6),
  panel.spacing.y = unit(0.5, "lines")
)

.scplot_themes$big <- list(

  theme_type = "element",

  text = element_text(size = 14),
  panel.spacing.y = unit(1.5, "lines")
)

## phase colour and shade -----------------------------------------------------------------

.scplot_themes$phase_color <- list(

  theme_type = "element",

  panel.background = element_rect(
    fill = alpha(c("aliceblue", "mistyrose1", "honeydew"), 0.5))
)

.scplot_themes$phase_shade <- list(

  theme_type = "element",

  panel.background = element_rect(
    fill = alpha(c("grey80", "grey99", "grey90"), 0.5))
)


## add staircase -----------------------------------------------------------------

.scplot_themes$staircase = list(

  theme_type = "element",
  theme_name = "staircase separators",

  separators = element_line(
    color = "black", linewidth = 1.2, linetype = "solid"),
  separators.staircase = TRUE,
  phasenames = element_text(color = "black", vjust = 1.5, hjust = 0.5,
                            margin = margin(b = 1.5)),
  phasenames.position.x = "centre"

)

.scplot_themes$strip = list(

  theme_type = "element",
  theme_name = "strip",

  plot.background = element_rect(linewidth = 0),
  panel.background = element_rect(linewidth = 0),

  axis.line.x = element_line(linewidth = 0),
  axis.line.y = element_line(linewidth = 0.3, linetype = "solid"),

  axis.ticks.length = unit(0, "points"),
  axis.ticks = element_line(linewidth = 0),
  axis.text.x = element_text(size = 0, margin = margin(t = 0)),
  axis.text.y = element_text(size = 0, margin = margin(r = 0)),

  plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "lines"),

  casenames.background = element_rect(linewidth = 0),

  separators = element_line(linewidth = 0.3, linetype = "solid"),

  separators.staircase = FALSE,

  label.background = NULL,

  grid = element_line(linewidth = 0),

  legend.position = "none",

  NULL
)

# complete themes --------

## minimal -----------------------------------------------------------------

.scplot_themes$minimal = list(

  theme_type = "complete",
  theme_name = "minimal",

  text = element_text(
    colour = "black", size = 11, family = "sans",
    face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
    lineheight = 1
  ),

  plot.background = element_rect(
    fill = alpha("white", 1),
    colour = alpha("white", 1),
    linewidth = 0,
    linetype = "solid"
  ),
  panel.background = element_rect(
    fill = alpha("white", 1),
    colour = alpha("white", 1),
    linewidth = 0,
    linetype = "solid"
  ),
  panel.spacing.y = unit(1, "lines"),

  axis.expand.x = c(0, 0.4, 0, 0.4),
  axis.expand.y = c(0.03, 0, 0.03, 0),

  axis.line.x = element_line(
    colour = "black", linewidth = 0, linetype = "solid"
  ),
  axis.line.y = element_line(
    colour = "black", linewidth = 0.3, linetype = "solid"
  ),

  axis.ticks.length = unit(0, "points"),
  axis.ticks = element_line(
    colour = "black", linewidth = 0, linetype = "solid"
  ),

  axis.title.y = element_text(
    angle = 90, margin = margin(r = 1.5)
  ),
  axis.title.x = element_text(
    margin = margin(t = 1.5)
  ),

  axis.text.x = element_text(
    size = 0, colour = "black", margin = margin(t = 0)
  ),
  axis.text.y = element_text(
    size = 0, colour = "black", margin = margin(r = 0)
  ),

  plot.title = element_text(
    margin = margin(0, 0, 1, 0, unit = "lines"),
    hjust = 0.5
  ),

  plot.caption = element_text(
    margin = margin(t = 0.5, r = 0, b = 0, l = 0, unit = "lines"),
    hjust = 0,
    face = "plain"
  ),

  plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "lines"),

  casenames = element_text(
    vjust = 1, hjust = 0,
    margin = margin(0.15, 0.15, 0.15, 0.15, unit = "lines")
  ),

  casenames.strip = element_rect(
    colour = NA,
    fill = NA,
    linewidth = 0,
    linetype = "solid"
  ),
  casenames.background = element_rect(
    colour = NA,
    fill = NA,
    linewidth = 0,
    linetype = "solid"
  ),

  casenames.position = "topleft",

  phasenames = element_text(
    vjust = 0, hjust = 0.5,
    margin = margin(b = 1)
  ),

  phasenames.position.x = "center",

  separators = element_line(
    colour = "black", linewidth = 0.3, linetype = "solid"
  ),

  separators.extent = "full",
  separators.staircase = FALSE,

  label.text = element_text(
    colour = "black", vjust = 0.5, hjust = 0.5, angle = 0, size = rel(1)
  ),
  label.background = NULL,

  label.padding = 0.1,

  grid = element_line(
    colour = "black", linewidth = 0, linetype = "solid"
  ),

  legend.position = "none",
  legend.background = element_rect(
    fill = NA, colour = NA, linewidth = 0, linetype = "solid"
  ),
  legend.text = element_text(size = rel(0.7), colour = "black"),
  legend.title = element_text(size = rel(0.7), colour = "black"),
  legend.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "lines"),

  dataline = list(
    "1" = element_line(colour = "black", linetype = "solid", linewidth = 0.5),
    "2" = element_line(colour = "grey40", linetype = "solid", linewidth = 0.5),
    "3" = element_line(colour = "grey65", linetype = "solid", linewidth = 0.5)
  ),

  datapoint = list(
    "1" = element_point(colour = "black", shape = 16, size = 1.5),
    "2" = element_point(colour = "grey40", shape = 16, size = 1.5),
    "3" = element_point(colour = "grey65", shape = 16, size = 1.5)
  ),

  statline = list(
    a = element_line(colour = "black", linewidth = 0.6, linetype = "dotted"),
    b = element_line(colour = "grey35", linewidth = 0.6, linetype = "dotted"),
    c = element_line(colour = "grey50", linewidth = 0.6, linetype = "dotted"),
    d = element_line(colour = "grey60", linewidth = 0.6, linetype = "dotted"),
    e = element_line(colour = "grey70", linewidth = 0.6, linetype = "dotted"),
    f = element_line(colour = "grey80", linewidth = 0.6, linetype = "dotted")
  )

)


## dark --------------------------------------------------------------------

.scplot_themes$dark <- list(

  theme_type = "complete",

  text = element_text(colour = "white"),

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
    "1" = element_line(colour = "#F2F2F2", linetype = "solid", linewidth = 0.6),  # near white
    "2" = element_line(colour = "#56B4E9", linetype = "solid", linewidth = 0.6),  # light blue
    "3" = element_line(colour = "#E69F00", linetype = "solid", linewidth = 0.6)   # warm orange
  ),

  datapoint = list(
    "1" = element_point(colour = "#F2F2F2", shape = 16, size = 2.2),
    "2" = element_point(colour = "#56B4E9", shape = 16, size = 2.2),
    "3" = element_point(colour = "#E69F00", shape = 16, size = 2.2)
  ),

  statline = list(
    a = element_line(colour = "#FF6B6B", linewidth = 0.8, linetype = "solid"),  # coral red
    b = element_line(colour = "#7CB9FF", linewidth = 0.8, linetype = "solid"),  # bright blue
    c = element_line(colour = "#7EDC82", linewidth = 0.8, linetype = "solid"),  # light green
    d = element_line(colour = "#FFC857", linewidth = 0.8, linetype = "solid"),  # amber
    e = element_line(colour = "#C792EA", linewidth = 0.8, linetype = "solid"),  # soft violet
    f = element_line(colour = "#4DD0C8", linewidth = 0.8, linetype = "solid")   # turquoise
  )

)


## illustration -----------------------------------------------------------------

.scplot_themes$illustration = list(

  theme_type = "complete",
  theme_name = "illustration",

  text = element_text(size = 16, family = "sans",
                      face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
                      lineheight = 1),

  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  panel.spacing.y = unit(1.5, "lines"),


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
                                 linewidth = 0.5,
                                 linetype = "solid"),
  casenames.background = element_rect(linewidth = 0.1,
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



## clarity -----------------------------------------------------------------

.scplot_themes$clarity = list(

  theme_type = "complete",
  theme_name = "clarity",

  text = element_text(colour = "#1A1A1A"),

  panel.spacing.y = unit(1.1, "lines"),

  axis.expand.y = c(0.04, 0, 0.06, 0),

  axis.line.x = element_line(
    colour = "#3A3A3A", linewidth = 0.45, linetype = "solid"
  ),
  axis.line.y = element_line(
    colour = "#3A3A3A", linewidth = 0.45, linetype = "solid"
  ),

  axis.ticks.length = unit(2.5, "points"),
  axis.ticks = element_line(
    colour = "#3A3A3A", linewidth = 0.4, linetype = "solid"
  ),

  axis.title.y = element_text(
    margin = margin(r = 4), colour = "#1A1A1A"
  ),
  axis.title.x = element_text(
    margin = margin(t = 4), colour = "#1A1A1A"
  ),

  axis.text.x = element_text(
    size = rel(0.82), colour = "#2F2F2F",
    margin = margin(t = 2)
  ),
  axis.text.y = element_text(
    size = rel(0.82), colour = "#2F2F2F",
    margin = margin(r = 2)
  ),

  plot.title = element_text(
    colour = "#111111", face = "bold", size = rel(1.1),
    margin = margin(0, 0, 0.9, 0, unit = "lines")
  ),

  plot.caption = element_text(
    margin = margin(t = 0.8, r = 0, b = 0, l = 0, unit = "lines"),
    colour = "#4A4A4A",
    size = rel(0.8)
  ),

  plot.margin = margin(t = 0.8, r = 0.6, b = 0.6, l = 0.6, unit = "lines"),

  casenames = element_text(
    vjust = 0.5, colour = "#1A1A1A", face = "bold", size = rel(0.92),
    margin = margin(0.22, 0.35, 0.22, 0.35, unit = "lines")
  ),

  casenames.strip = element_rect(
    colour = "#B8B8B8",
    fill = "#F4F4F4",
    linewidth = 0.5,
    linetype = "solid"
  ),

  casenames.background = element_rect(
    fill = NA,
    colour = NA,
    linewidth = 0
  ),

  casenames.position = "strip-right",

  phasenames = element_text(
    colour = "#555555", size = rel(0.82),
    margin = margin(b = 2)
  ),

  label.text = element_text(
    colour = "#1A1A1A", size = rel(0.92)
  ),

  label.background = NULL,
  label.padding = 0.12,

  separators.staircase = TRUE,
  separators = element_line(
    colour = "#4F4F4F", linewidth = 0.65, linetype = "solid"
  ),

  grid = element_line(
    colour = "#ECECEC", linewidth = 0.25, linetype = "solid"
  ),

  legend.background = element_rect(fill = "white", colour = NA),
  legend.text = element_text(size = rel(0.72), colour = "#2A2A2A"),
  legend.title = element_text(size = rel(0.72), colour = "#1A1A1A", face = "bold"),
  legend.margin = margin(0.25, 0.25, 0.25, 0.25, unit = "lines"),

  dataline = list(
    "1" = element_line(colour = "#1F1F1F", linetype = "solid", linewidth = 0.65),
    "2" = element_line(colour = "#3E6FB0", linetype = "solid", linewidth = 0.65),
    "3" = element_line(colour = "#C06C2B", linetype = "solid", linewidth = 0.65)
  ),

  datapoint = list(
    "1" = element_point(colour = "#1F1F1F", shape = 16, size = 2.1),
    "2" = element_point(colour = "#3E6FB0", shape = 16, size = 2.1),
    "3" = element_point(colour = "#C06C2B", shape = 16, size = 2.1)
  ),

  statline = list(
    a = element_line(colour = "#B03030", linewidth = 0.8, linetype = "solid"),
    b = element_line(colour = "#355CBE", linewidth = 0.8, linetype = "solid"),
    c = element_line(colour = "#4F772D", linewidth = 0.8, linetype = "solid"),
    d = element_line(colour = "#B7791F", linewidth = 0.8, linetype = "solid"),
    e = element_line(colour = "#7B4FA3", linewidth = 0.8, linetype = "solid"),
    f = element_line(colour = "#1F7A7A", linewidth = 0.8, linetype = "solid")
  )

)

## midnight -------------------------------------------------------------------

.scplot_themes$midnight = list(

  theme_type = "complete",
  theme_name = "midnight",

  text = element_text(
    colour = "#EAEAF2", size = 11, family = "sans",
    face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
    lineheight = 1
  ),

  plot.background = element_rect(fill = "#141824", colour = NA),
  panel.background = element_rect(fill = "#1B2233", colour = NA),
  panel.spacing.y = unit(1.2, "lines"),

  axis.expand.x = c(0, 0.6, 0, 0.6),
  axis.expand.y = c(0.04, 0, 0.06, 0),

  axis.line.x = element_line(
    colour = "#B8C0D9", linewidth = 0.45, linetype = "solid"
  ),
  axis.line.y = element_line(
    colour = "#B8C0D9", linewidth = 0.45, linetype = "solid"
  ),

  axis.ticks.length = unit(2.5, "points"),
  axis.ticks = element_line(
    colour = "#B8C0D9", linewidth = 0.4, linetype = "solid"
  ),

  axis.title.y = element_text(
    angle = 90, margin = margin(r = 4), colour = "#F4F6FB"
  ),
  axis.title.x = element_text(
    margin = margin(t = 4), colour = "#F4F6FB"
  ),

  axis.text.x = element_text(
    size = rel(0.82), hjust = 0.5, colour = "#D5DAE8",
    margin = margin(t = 2)
  ),
  axis.text.y = element_text(
    size = rel(0.82), hjust = 1, colour = "#D5DAE8",
    margin = margin(r = 2)
  ),

  plot.title = element_text(
    colour = "#FFFFFF", face = "bold", size = rel(1.12),
    margin = margin(0, 0, 0.9, 0, unit = "lines"),
    hjust = 0.5
  ),

  plot.caption = element_text(
    margin = margin(t = 0.8, r = 0, b = 0, l = 0, unit = "lines"),
    hjust = 0,
    colour = "#B8C0D9",
    face = "italic",
    size = rel(0.8)
  ),

  plot.margin = margin(t = 0.8, r = 0.6, b = 0.6, l = 0.6, unit = "lines"),

  casenames = element_text(
    vjust = 0.5, hjust = 0,
    colour = "#F4F6FB", face = "bold", size = rel(0.92),
    margin = margin(0.22, 0.35, 0.22, 0.35, unit = "lines")
  ),

  casenames.strip = element_rect(
    colour = "#59627A",
    fill = "#242D42",
    linewidth = 0.5,
    linetype = "solid"
  ),

  casenames.background = element_rect(
    fill = NA,
    colour = NA,
    linewidth = 0
  ),

  casenames.position = "strip-right",

  phasenames = element_text(
    vjust = 0, hjust = 0.5,
    colour = "#BFC7DA", face = "plain", size = rel(0.82),
    margin = margin(b = 2)
  ),

  phasenames.position.x = "center",

  separators = element_line(
    colour = "#B8C6E3", linewidth = 0.55, linetype = "dashed"
  ),
  separators.staircase = TRUE,

  label.text = element_text(
    colour = "#F5F7FC", vjust = 0.5, hjust = 0.5,
    angle = 0, size = rel(0.92)
  ),

  label.background = element_rect(
    fill = "#2A344A",
    colour = "#7E8AA8",
    linewidth = 0.3,
    linetype = "solid"
  ),

  label.padding = 0.12,

  grid = element_line(
    colour = "#34405A", linewidth = 0.3, linetype = "solid"
  ),

  legend.position = "none",
  legend.background = element_rect(fill = "#1B2233", colour = NA),
  legend.text = element_text(size = rel(0.72), colour = "#DCE2F0"),
  legend.title = element_text(size = rel(0.72), colour = "#FFFFFF", face = "bold"),
  legend.margin = margin(0.25, 0.25, 0.25, 0.25, unit = "lines"),

  dataline = list(
    "1" = element_line(colour = "#F2F2F2", linetype = "solid", linewidth = 0.65),
    "2" = element_line(colour = "#6EC5FF", linetype = "solid", linewidth = 0.65),
    "3" = element_line(colour = "#FFB454", linetype = "solid", linewidth = 0.65)
  ),

  datapoint = list(
    "1" = element_point(colour = "#F2F2F2", shape = 16, size = 2.2),
    "2" = element_point(colour = "#6EC5FF", shape = 16, size = 2.2),
    "3" = element_point(colour = "#FFB454", shape = 16, size = 2.2)
  ),

  statline = list(
    a = element_line(colour = "#FF7B72", linewidth = 0.8, linetype = "solid"),
    b = element_line(colour = "#79A8FF", linewidth = 0.8, linetype = "solid"),
    c = element_line(colour = "#8BD450", linewidth = 0.8, linetype = "solid"),
    d = element_line(colour = "#FFC857", linewidth = 0.8, linetype = "solid"),
    e = element_line(colour = "#C792EA", linewidth = 0.8, linetype = "solid"),
    f = element_line(colour = "#5FD3BC", linewidth = 0.8, linetype = "solid")
  )

)

## journal -----------------------------------------------------------------

.scplot_themes$journal = list(

  theme_type = "complete",
  theme_name = "journal",

  text = element_text(
    colour = "#000000", size = 10.5, family = "sans",
    face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
    lineheight = 1
  ),

  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.spacing.y = unit(0.9, "lines"),

  axis.expand.x = c(0, 0.5, 0, 0.5),
  axis.expand.y = c(0.03, 0, 0.05, 0),

  axis.line.x = element_line(
    colour = "#000000", linewidth = 0.45, linetype = "solid"
  ),
  axis.line.y = element_line(
    colour = "#000000", linewidth = 0.45, linetype = "solid"
  ),

  axis.ticks.length = unit(2.2, "points"),
  axis.ticks = element_line(
    colour = "#000000", linewidth = 0.4, linetype = "solid"
  ),

  axis.title.y = element_text(
    angle = 90, margin = margin(r = 3.5),
    colour = "#000000", face = "plain", size = rel(0.95)
  ),
  axis.title.x = element_text(
    margin = margin(t = 3.5),
    colour = "#000000", face = "plain", size = rel(0.95)
  ),

  axis.text.x = element_text(
    size = rel(0.8), hjust = 0.5,
    colour = "#000000", margin = margin(t = 1.8)
  ),
  axis.text.y = element_text(
    size = rel(0.8), hjust = 1,
    colour = "#000000", margin = margin(r = 1.8)
  ),

  plot.title = element_text(
    colour = "#000000", face = "plain", size = rel(1.0),
    margin = margin(0, 0, 0.7, 0, unit = "lines"),
    hjust = 0.5
  ),

  plot.caption = element_text(
    margin = margin(t = 0.7, r = 0, b = 0, l = 0, unit = "lines"),
    hjust = 0,
    colour = "#000000",
    face = "italic",
    size = rel(0.78)
  ),

  plot.margin = margin(t = 0.6, r = 0.45, b = 0.45, l = 0.45, unit = "lines"),

  casenames = element_text(
    vjust = 0.5, hjust = 0,
    colour = "#000000", face = "plain", size = rel(0.85),
    margin = margin(0.16, 0.22, 0.16, 0.22, unit = "lines")
  ),

  casenames.strip = element_rect(
    colour = "#808080",
    fill = "#F2F2F2",
    linewidth = 0.4,
    linetype = "solid"
  ),

  casenames.background = element_rect(
    fill = NA,
    colour = NA,
    linewidth = 0
  ),

  casenames.position = "strip-right",

  phasenames = element_text(
    vjust = 0, hjust = 0.5,
    colour = "#000000", face = "plain", size = rel(0.78),
    margin = margin(b = 1.5)
  ),

  phasenames.position.x = "center",

  separators = element_line(
    colour = "#3F3F3F", linewidth = 0.45, linetype = "dashed"
  ),
  separators.staircase = TRUE,

  label.text = element_text(
    colour = "#000000", vjust = 0.5, hjust = 0.5,
    angle = 0, size = rel(0.82)
  ),

  label.background = NULL,
  label.padding = 0.08,

  grid = element_line(
    colour = "#E6E6E6", linewidth = 0.25, linetype = "solid"
  ),

  legend.position = "none",
  legend.background = element_rect(fill = "white", colour = NA),
  legend.text = element_text(size = rel(0.72), colour = "#000000"),
  legend.title = element_text(size = rel(0.72), colour = "#000000", face = "plain"),
  legend.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "lines"),

  dataline = list(
    "1" = element_line(colour = "#000000", linetype = "solid", linewidth = 0.6),
    "2" = element_line(colour = "#4D4D4D", linetype = "solid", linewidth = 0.6),
    "3" = element_line(colour = "#7A7A7A", linetype = "solid", linewidth = 0.6)
  ),

  datapoint = list(
    "1" = element_point(colour = "#000000", shape = 16, size = 1.9),
    "2" = element_point(colour = "#4D4D4D", shape = 17, size = 1.9),
    "3" = element_point(colour = "#7A7A7A", shape = 15, size = 1.9)
  ),

  statline = list(
    a = element_line(colour = "#000000", linewidth = 0.75, linetype = "solid"),
    b = element_line(colour = "#2F2F2F", linewidth = 0.75, linetype = "dashed"),
    c = element_line(colour = "#4D4D4D", linewidth = 0.75, linetype = "dotdash"),
    d = element_line(colour = "#666666", linewidth = 0.75, linetype = "longdash"),
    e = element_line(colour = "#808080", linewidth = 0.75, linetype = "twodash"),
    f = element_line(colour = "#999999", linewidth = 0.75, linetype = "dotted")
  )

)



## playful -----------------------------------------------------------------
.scplot_themes$playful = list(

  theme_type = "complete",

  text = element_text(
    colour = "#2B2B2B", size = 11, family = "sans",
    face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
    lineheight = 1
  ),

  plot.background = element_rect(fill = "#FFFDF8", colour = NA),
  panel.background = element_rect(
    fill = alpha(c("#FFF8E8", "#F6FBF3", "#FFF4F0"), 0.9),
    colour = NA
  ),
  panel.spacing.y = unit(1.3, "lines"),

  axis.expand.x = c(0, 0.7, 0, 0.7),
  axis.expand.y = c(0.05, 0, 0.08, 0),

  axis.line.x = element_line(
    colour = "#6B6B6B", linewidth = 0.45, linetype = "solid"
  ),
  axis.line.y = element_line(
    colour = "#6B6B6B", linewidth = 0.45, linetype = "solid"
  ),

  axis.ticks.length = unit(2.5, "points"),
  axis.ticks = element_line(
    colour = "#6B6B6B", linewidth = 0.4, linetype = "solid"
  ),

  axis.title.y = element_text(
    angle = 90, margin = margin(r = 4), colour = "#2B2B2B", face = "bold"
  ),
  axis.title.x = element_text(
    margin = margin(t = 4), colour = "#2B2B2B", face = "bold"
  ),

  axis.text.x = element_text(
    size = rel(0.82), hjust = 0.5, colour = "#4A4A4A",
    margin = margin(t = 2)
  ),
  axis.text.y = element_text(
    size = rel(0.82), hjust = 1, colour = "#4A4A4A",
    margin = margin(r = 2)
  ),

  plot.title = element_text(
    colour = "#2B2B2B", face = "bold", size = rel(1.15),
    margin = margin(0, 0, 0.9, 0, unit = "lines"),
    hjust = 0.5
  ),

  plot.caption = element_text(
    margin = margin(t = 0.8, r = 0, b = 0, l = 0, unit = "lines"),
    hjust = 0,
    colour = "#6A6A6A",
    face = "italic",
    size = rel(0.8)
  ),

  plot.margin = margin(t = 0.8, r = 0.7, b = 0.7, l = 0.7, unit = "lines"),

  casenames = element_text(
    vjust = 0.5, hjust = 0,
    colour = "#2B2B2B", face = "bold", size = rel(0.92),
    margin = margin(0.22, 0.35, 0.22, 0.35, unit = "lines")
  ),

  casenames.strip = element_rect(
    colour = "#D9CBB8",
    fill = "#FCECCF",
    linewidth = 0.5,
    linetype = "solid"
  ),

  casenames.background = element_rect(
    fill = NA,
    colour = NA,
    linewidth = 0
  ),

  casenames.position = "strip-right",

  phasenames = element_text(
    vjust = 0, hjust = 0.5,
    colour = "#7A5C3E", face = "plain", size = rel(0.84),
    margin = margin(b = 2)
  ),

  phasenames.position.x = "center",

  separators = element_line(
    colour = "#B08968", linewidth = 0.55, linetype = "dashed"
  ),

  separators.extent = "full",
  separators.staircase = FALSE,

  label.text = element_text(
    colour = "#2B2B2B", vjust = 0.5, hjust = 0.5,
    angle = 0, size = rel(0.92)
  ),

  label.background = element_rect(
    fill = "#FFF4D6",
    colour = "#D8B36A",
    linewidth = 0.3,
    linetype = "solid"
  ),

  label.padding = 0.14,

  grid = element_line(
    colour = "#F1DEC9", linewidth = 0.35, linetype = "solid"
  ),

  legend.position = "none",
  legend.background = element_rect(fill = "#FFF9F0", colour = NA),
  legend.text = element_text(size = rel(0.72), colour = "#3A3A3A"),
  legend.title = element_text(size = rel(0.72), colour = "#2B2B2B", face = "bold"),
  legend.margin = margin(0.25, 0.25, 0.25, 0.25, unit = "lines"),

  dataline = list(
    "1" = element_line(colour = "#5B5F97", linetype = "solid", linewidth = 0.7),
    "2" = element_line(colour = "#E07A5F", linetype = "solid", linewidth = 0.7),
    "3" = element_line(colour = "#3D9970", linetype = "solid", linewidth = 0.7)
  ),

  datapoint = list(
    "1" = element_point(colour = "#5B5F97", shape = 16, size = 2.3),
    "2" = element_point(colour = "#E07A5F", shape = 16, size = 2.3),
    "3" = element_point(colour = "#3D9970", shape = 16, size = 2.3)
  ),

  statline = list(
    a = element_line(colour = "#D1495B", linewidth = 0.85, linetype = "solid"),
    b = element_line(colour = "#3A86C8", linewidth = 0.85, linetype = "solid"),
    c = element_line(colour = "#5C9E31", linewidth = 0.85, linetype = "solid"),
    d = element_line(colour = "#F4A259", linewidth = 0.85, linetype = "solid"),
    e = element_line(colour = "#8E5EA2", linewidth = 0.85, linetype = "solid"),
    f = element_line(colour = "#2A9D8F", linewidth = 0.85, linetype = "solid")
  )

)

## atelier -----------------------------------------------------------------

.scplot_themes$atelier = list(

  theme_type = "complete",

  text = element_text(
    colour = "#2E2A26", size = 11, family = "serif",
    face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
    lineheight = 1
  ),

  plot.background = element_rect(fill = "#F7F3EE", colour = NA),
  panel.background = element_rect(
    fill = alpha(c("#F8F3EC", "#F2EADF", "#EEF3EA"), 0.95),
    colour = NA
  ),
  panel.spacing.y = unit(1.35, "lines"),

  axis.expand.x = c(0, 0.7, 0, 0.7),
  axis.expand.y = c(0.05, 0, 0.08, 0),

  axis.line.x = element_line(
    colour = "#7C746B", linewidth = 0.4, linetype = "solid"
  ),
  axis.line.y = element_line(
    colour = "#7C746B", linewidth = 0.4, linetype = "solid"
  ),

  axis.ticks.length = unit(2.5, "points"),
  axis.ticks = element_line(
    colour = "#7C746B", linewidth = 0.35, linetype = "solid"
  ),

  axis.title.y = element_text(
    angle = 90, margin = margin(r = 4),
    colour = "#2E2A26", face = "plain"
  ),
  axis.title.x = element_text(
    margin = margin(t = 4),
    colour = "#2E2A26", face = "plain"
  ),

  axis.text.x = element_text(
    size = rel(0.82), hjust = 0.5,
    colour = "#5E564F", margin = margin(t = 2)
  ),
  axis.text.y = element_text(
    size = rel(0.82), hjust = 1,
    colour = "#5E564F", margin = margin(r = 2)
  ),

  plot.title = element_text(
    colour = "#2A2521", face = "bold", size = rel(1.15),
    margin = margin(0, 0, 0.9, 0, unit = "lines"),
    hjust = 0.5
  ),

  plot.caption = element_text(
    margin = margin(t = 0.8, r = 0, b = 0, l = 0, unit = "lines"),
    hjust = 0,
    colour = "#756C63",
    face = "italic",
    size = rel(0.8)
  ),

  plot.margin = margin(t = 0.8, r = 0.7, b = 0.7, l = 0.7, unit = "lines"),

  casenames = element_text(
    vjust = 0.5, hjust = 0,
    colour = "#2E2A26", face = "bold", size = rel(0.9),
    margin = margin(0.24, 0.36, 0.24, 0.36, unit = "lines")
  ),

  casenames.strip = element_rect(
    colour = "#CBBFB0",
    fill = "#EDE3D6",
    linewidth = 0.45,
    linetype = "solid"
  ),

  casenames.background = element_rect(
    fill = NA,
    colour = NA,
    linewidth = 0
  ),

  casenames.position = "strip-right",

  phasenames = element_text(
    vjust = 0, hjust = 0.5,
    colour = "#85776A", face = "plain", size = rel(0.82),
    margin = margin(b = 2)
  ),

  phasenames.position.x = "center",

  separators = element_line(
    colour = "#A49585", linewidth = 0.5, linetype = "dashed"
  ),

  separators.extent = "full",
  separators.staircase = FALSE,

  label.text = element_text(
    colour = "#2E2A26", vjust = 0.5, hjust = 0.5,
    angle = 0, size = rel(0.9)
  ),

  label.background = element_rect(
    fill = "#F5EBDD",
    colour = "#C9B79E",
    linewidth = 0.3,
    linetype = "solid"
  ),

  label.padding = 0.14,

  grid = element_line(
    colour = "#E8DED2", linewidth = 0.3, linetype = "solid"
  ),

  legend.position = "none",
  legend.background = element_rect(fill = "#FBF8F3", colour = NA),
  legend.text = element_text(size = rel(0.72), colour = "#4A433D"),
  legend.title = element_text(size = rel(0.72), colour = "#2E2A26", face = "bold"),
  legend.margin = margin(0.25, 0.25, 0.25, 0.25, unit = "lines"),

  dataline = list(
    "1" = element_line(colour = "#2F4858", linetype = "solid", linewidth = 0.72),  # deep slate blue
    "2" = element_line(colour = "#C06C52", linetype = "solid", linewidth = 0.72),  # muted terracotta
    "3" = element_line(colour = "#7A8E76", linetype = "solid", linewidth = 0.72)   # sage green
  ),

  datapoint = list(
    "1" = element_point(colour = "#2F4858", shape = 16, size = 2.3),
    "2" = element_point(colour = "#C06C52", shape = 16, size = 2.3),
    "3" = element_point(colour = "#7A8E76", shape = 16, size = 2.3)
  ),

  statline = list(
    a = element_line(colour = "#9E3D33", linewidth = 0.85, linetype = "solid"),  # oxblood
    b = element_line(colour = "#3E5C76", linewidth = 0.85, linetype = "solid"),  # steel blue
    c = element_line(colour = "#6B7A3E", linewidth = 0.85, linetype = "solid"),  # olive
    d = element_line(colour = "#B07A3B", linewidth = 0.85, linetype = "solid"),  # ochre
    e = element_line(colour = "#7A4E6A", linewidth = 0.85, linetype = "solid"),  # plum
    f = element_line(colour = "#2E6F68", linewidth = 0.85, linetype = "solid")   # deep teal
  )

)


## bauhaus -----------------------------------------------------------------
.scplot_themes$bauhaus = list(

  theme_type = "complete",

  text = element_text(
    colour = "#111111", size = 11, family = "sans",
    face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
    lineheight = 1
  ),

  plot.background = element_rect(fill = "#F5F1E8", colour = NA),
  panel.background = element_rect(
    fill = alpha(c("#F7F3E8", "#E6D98A", "#DCE6F2"), 1),
    colour = NA
  ),
  panel.spacing.y = unit(1.2, "lines"),

  axis.expand.x = c(0, 0.6, 0, 0.6),
  axis.expand.y = c(0.04, 0, 0.06, 0),

  axis.line.x = element_line(
    colour = "#111111", linewidth = 0.5, linetype = "solid"
  ),
  axis.line.y = element_line(
    colour = "#111111", linewidth = 0.5, linetype = "solid"
  ),

  axis.ticks.length = unit(2.6, "points"),
  axis.ticks = element_line(
    colour = "#111111", linewidth = 0.45, linetype = "solid"
  ),

  axis.title.y = element_text(
    angle = 90, margin = margin(r = 4), colour = "#111111", face = "bold"
  ),
  axis.title.x = element_text(
    margin = margin(t = 4), colour = "#111111", face = "bold"
  ),

  axis.text.x = element_text(
    size = rel(0.82), hjust = 0.5,
    colour = "#222222", margin = margin(t = 2)
  ),
  axis.text.y = element_text(
    size = rel(0.82), hjust = 1,
    colour = "#222222", margin = margin(r = 2)
  ),

  plot.title = element_text(
    colour = "#111111", face = "bold", size = rel(1.15),
    margin = margin(0, 0, 0.9, 0, unit = "lines"),
    hjust = 0.5
  ),

  plot.caption = element_text(
    margin = margin(t = 0.8, r = 0, b = 0, l = 0, unit = "lines"),
    hjust = 0,
    colour = "#333333",
    face = "italic",
    size = rel(0.8)
  ),

  plot.margin = margin(t = 0.8, r = 0.6, b = 0.6, l = 0.6, unit = "lines"),

  casenames = element_text(
    vjust = 0.5, hjust = 0,
    colour = "#111111", face = "bold", size = rel(0.9),
    margin = margin(0.2, 0.35, 0.2, 0.35, unit = "lines")
  ),

  casenames.strip = element_rect(
    colour = "#111111",
    fill = "#E8E2D3",
    linewidth = 0.55,
    linetype = "solid"
  ),

  casenames.background = element_rect(
    fill = NA,
    colour = NA,
    linewidth = 0
  ),

  casenames.position = "strip-right",

  phasenames = element_text(
    vjust = 0, hjust = 0.5,
    colour = "#222222", face = "plain", size = rel(0.82),
    margin = margin(b = 2)
  ),

  phasenames.position.x = "center",

  separators = element_line(
    colour = "#111111", linewidth = 0.6, linetype = "solid"
  ),
  separators.staircase = TRUE,

  label.text = element_text(
    colour = "#111111", vjust = 0.5, hjust = 0.5,
    angle = 0, size = rel(0.9)
  ),

  label.background = element_rect(
    fill = "#F3E04F",
    colour = "#111111",
    linewidth = 0.35,
    linetype = "solid"
  ),

  label.padding = 0.12,

  grid = element_line(
    colour = "#D8D1C3", linewidth = 0.3, linetype = "solid"
  ),

  legend.position = "none",
  legend.background = element_rect(fill = "#FAF7F0", colour = NA),
  legend.text = element_text(size = rel(0.72), colour = "#222222"),
  legend.title = element_text(size = rel(0.72), colour = "#111111", face = "bold"),
  legend.margin = margin(0.25, 0.25, 0.25, 0.25, unit = "lines"),

  dataline = list(
    "1" = element_line(colour = "#111111", linetype = "solid", linewidth = 0.72),  # black
    "2" = element_line(colour = "#2F5DA9", linetype = "solid", linewidth = 0.72),  # bauhaus blue
    "3" = element_line(colour = "#C4472D", linetype = "solid", linewidth = 0.72)   # bauhaus red-orange
  ),

  datapoint = list(
    "1" = element_point(colour = "#111111", shape = 16, size = 2.3),
    "2" = element_point(colour = "#2F5DA9", shape = 16, size = 2.3),
    "3" = element_point(colour = "#C4472D", shape = 16, size = 2.3)
  ),

  statline = list(
    a = element_line(colour = "#C4472D", linewidth = 0.86, linetype = "solid"),  # red
    b = element_line(colour = "#2F5DA9", linewidth = 0.86, linetype = "solid"),  # blue
    c = element_line(colour = "#D9A321", linewidth = 0.86, linetype = "solid"),  # mustard yellow
    d = element_line(colour = "#3F6B4B", linewidth = 0.86, linetype = "solid"),  # muted green
    e = element_line(colour = "#111111", linewidth = 0.86, linetype = "solid"),  # black
    f = element_line(colour = "#7A4E2D", linewidth = 0.86, linetype = "solid")   # earthy brown
  )

)

## poster -----------------------------------------------------------------

.scplot_themes$poster = list(

  theme_type = "complete",

  text = element_text(
    colour = "#1A1A1A", size = 16, family = "sans",
    face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
    lineheight = 1
  ),

  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.spacing.y = unit(1.6, "lines"),

  axis.expand.x = c(0, 0.8, 0, 0.8),
  axis.expand.y = c(0.05, 0, 0.08, 0),

  axis.line.x = element_line(
    colour = "#333333", linewidth = 0.7, linetype = "solid"
  ),
  axis.line.y = element_line(
    colour = "#333333", linewidth = 0.7, linetype = "solid"
  ),

  axis.ticks.length = unit(4, "points"),
  axis.ticks = element_line(
    colour = "#333333", linewidth = 0.65, linetype = "solid"
  ),

  axis.title.y = element_text(
    angle = 90, margin = margin(r = 6), face = "bold", size = rel(1.05)
  ),
  axis.title.x = element_text(
    margin = margin(t = 6), face = "bold", size = rel(1.05)
  ),

  axis.text.x = element_text(
    size = rel(0.9), hjust = 0.5, colour = "#333333",
    margin = margin(t = 3)
  ),
  axis.text.y = element_text(
    size = rel(0.9), hjust = 1, colour = "#333333",
    margin = margin(r = 3)
  ),

  plot.title = element_text(
    colour = "#111111", face = "bold", size = rel(1.25),
    margin = margin(0, 0, 1.1, 0, unit = "lines"),
    hjust = 0.5
  ),

  plot.caption = element_text(
    margin = margin(t = 0.8, r = 0, b = 0, l = 0, unit = "lines"),
    hjust = 0,
    colour = "#4D4D4D",
    face = "italic",
    size = rel(0.78)
  ),

  plot.margin = margin(t = 1, r = 0.8, b = 0.8, l = 0.8, unit = "lines"),

  casenames = element_text(
    vjust = 0.5, hjust = 0,
    colour = "#1A1A1A", face = "bold", size = rel(0.95),
    margin = margin(0.25, 0.4, 0.25, 0.4, unit = "lines")
  ),

  casenames.strip = element_rect(
    colour = "#AFAFAF",
    fill = "#F2F2F2",
    linewidth = 0.7,
    linetype = "solid"
  ),

  casenames.background = element_rect(fill = NA, colour = NA, linewidth = 0),

  casenames.position = "strip-right",

  phasenames = element_text(
    vjust = 0, hjust = 0.5,
    colour = "#4D4D4D", size = rel(0.88),
    margin = margin(b = 3)
  ),

  phasenames.position.x = "center",

  separators = element_line(
    colour = "#666666", linewidth = 1, linetype = "dashed"
  ),

  separators.extent = "full",
  separators.staircase = TRUE,

  label.text = element_text(
    colour = "#1A1A1A", vjust = 0.5, hjust = 0.5,
    angle = 0, size = rel(0.95)
  ),

  label.background = element_rect(
    fill = "#FFFFFFF0",
    colour = "#C8C8C8",
    linewidth = 0.4,
    linetype = "solid"
  ),

  label.padding = 0.14,

  grid = element_line(
    colour = "#E3E3E3", linewidth = 0.45, linetype = "solid"
  ),

  legend.position = "none",
  legend.background = element_rect(fill = "white", colour = NA),
  legend.text = element_text(size = rel(0.78), colour = "#2A2A2A"),
  legend.title = element_text(size = rel(0.78), colour = "#111111", face = "bold"),
  legend.margin = margin(0.3, 0.3, 0.3, 0.3, unit = "lines"),

  dataline = list(
    "1" = element_line(colour = "#1F1F1F", linetype = "solid", linewidth = 1.0),
    "2" = element_line(colour = "#2F6DB3", linetype = "solid", linewidth = 1.0),
    "3" = element_line(colour = "#C46A2D", linetype = "solid", linewidth = 1.0)
  ),

  datapoint = list(
    "1" = element_point(colour = "#1F1F1F", shape = 16, size = 3.0),
    "2" = element_point(colour = "#2F6DB3", shape = 16, size = 3.0),
    "3" = element_point(colour = "#C46A2D", shape = 16, size = 3.0)
  ),

  statline = list(
    a = element_line(colour = "#B03030", linewidth = 1.05, linetype = "solid"),
    b = element_line(colour = "#355CBE", linewidth = 1.05, linetype = "solid"),
    c = element_line(colour = "#4F772D", linewidth = 1.05, linetype = "solid"),
    d = element_line(colour = "#B7791F", linewidth = 1.05, linetype = "solid"),
    e = element_line(colour = "#7B4FA3", linewidth = 1.05, linetype = "solid"),
    f = element_line(colour = "#1F7A7A", linewidth = 1.05, linetype = "solid")
  )

)


## handout -----------------------------------------------------------------
.scplot_themes$handout = list(

  theme_type = "complete",

  text = element_text(
    colour = "#000000", size = 11, family = "sans",
    face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
    lineheight = 1
  ),

  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.spacing.y = unit(1.1, "lines"),

  axis.expand.x = c(0, 0.6, 0, 0.6),
  axis.expand.y = c(0.05, 0, 0.06, 0),

  axis.line.x = element_line(
    colour = "#000000", linewidth = 0.55, linetype = "solid"
  ),
  axis.line.y = element_line(
    colour = "#000000", linewidth = 0.55, linetype = "solid"
  ),

  axis.ticks.length = unit(3, "points"),
  axis.ticks = element_line(
    colour = "#000000", linewidth = 0.5, linetype = "solid"
  ),

  axis.title.y = element_text(angle = 90, margin = margin(r = 4), face = "plain"),
  axis.title.x = element_text(margin = margin(t = 4), face = "plain"),

  axis.text.x = element_text(
    size = rel(0.82), hjust = 0.5, colour = "#000000",
    margin = margin(t = 2)
  ),
  axis.text.y = element_text(
    size = rel(0.82), hjust = 1, colour = "#000000",
    margin = margin(r = 2)
  ),

  plot.title = element_text(
    colour = "#000000", face = "plain", size = rel(1.05),
    margin = margin(0, 0, 0.8, 0, unit = "lines"),
    hjust = 0.5
  ),

  plot.caption = element_text(
    margin = margin(t = 0.7, r = 0, b = 0, l = 0, unit = "lines"),
    hjust = 0, colour = "#000000", face = "italic", size = rel(0.78)
  ),

  plot.margin = margin(t = 0.7, r = 0.5, b = 0.5, l = 0.5, unit = "lines"),

  casenames = element_text(
    vjust = 0.5, hjust = 0,
    colour = "#000000", face = "plain", size = rel(0.86),
    margin = margin(0.18, 0.25, 0.18, 0.25, unit = "lines")
  ),

  casenames.strip = element_rect(
    colour = "#000000",
    fill = "white",
    linewidth = 0.5,
    linetype = "solid"
  ),

  casenames.background = element_rect(fill = NA, colour = NA, linewidth = 0),

  casenames.position = "strip-right",

  phasenames = element_text(
    vjust = 0, hjust = 0.5,
    colour = "#000000", size = rel(0.78),
    margin = margin(b = 1.8)
  ),

  phasenames.position.x = "center",

  separators = element_line(
    colour = "#000000", linewidth = 0.85, linetype = "dashed"
  ),

  separators.extent = "full",
  separators.staircase = TRUE,

  label.text = element_text(
    colour = "#000000", vjust = 0.5, hjust = 0.5,
    angle = 0, size = rel(0.84)
  ),

  label.background = NULL,
  label.padding = 0.08,

  grid = element_line(
    colour = "#D9D9D9", linewidth = 0.3, linetype = "solid"
  ),

  legend.position = "none",
  legend.background = element_rect(fill = "white", colour = NA),
  legend.text = element_text(size = rel(0.72), colour = "#000000"),
  legend.title = element_text(size = rel(0.72), colour = "#000000"),
  legend.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "lines"),

  dataline = list(
    "1" = element_line(colour = "#000000", linetype = "solid", linewidth = 0.8),
    "2" = element_line(colour = "#000000", linetype = "dashed", linewidth = 0.8),
    "3" = element_line(colour = "#000000", linetype = "dotdash", linewidth = 0.8)
  ),

  datapoint = list(
    "1" = element_point(colour = "#000000", shape = 16, size = 2.2),
    "2" = element_point(colour = "#000000", shape = 17, size = 2.2),
    "3" = element_point(colour = "#000000", shape = 15, size = 2.2)
  ),

  statline = list(
    a = element_line(colour = "#000000", linewidth = 0.9, linetype = "solid"),
    b = element_line(colour = "#000000", linewidth = 0.9, linetype = "dashed"),
    c = element_line(colour = "#000000", linewidth = 0.9, linetype = "dotdash"),
    d = element_line(colour = "#000000", linewidth = 0.9, linetype = "longdash"),
    e = element_line(colour = "#000000", linewidth = 0.9, linetype = "twodash"),
    f = element_line(colour = "#000000", linewidth = 0.9, linetype = "dotted")
  )

)

## sparse -----------------------------------------------------------------

.scplot_themes$sparse = list(

  theme_type = "complete",

  text = element_text(
    colour = "#222222", size = 10.5, family = "sans",
    face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
    lineheight = 1
  ),

  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.spacing.y = unit(0.9, "lines"),

  axis.expand.x = c(0, 0.45, 0, 0.45),
  axis.expand.y = c(0.03, 0, 0.04, 0),

  axis.line.x = element_line(
    colour = "#7A7A7A", linewidth = 0.35, linetype = "solid"
  ),
  axis.line.y = element_line(
    colour = "#7A7A7A", linewidth = 0.35, linetype = "solid"
  ),

  axis.ticks.length = unit(2, "points"),
  axis.ticks = element_line(
    colour = "#7A7A7A", linewidth = 0.35, linetype = "solid"
  ),

  axis.title.y = element_text(
    angle = 90, margin = margin(r = 2.5), size = rel(0.9)
  ),
  axis.title.x = element_text(
    margin = margin(t = 2.5), size = rel(0.9)
  ),

  axis.text.x = element_text(
    size = rel(0.72), hjust = 0.5, colour = "#5A5A5A",
    margin = margin(t = 1.5)
  ),
  axis.text.y = element_text(
    size = rel(0.72), hjust = 1, colour = "#5A5A5A",
    margin = margin(r = 1.5)
  ),

  plot.title = element_text(
    colour = "#222222", face = "plain", size = rel(1.0),
    margin = margin(0, 0, 0.5, 0, unit = "lines"),
    hjust = 0.5
  ),

  plot.caption = element_text(
    margin = margin(t = 0.5, r = 0, b = 0, l = 0, unit = "lines"),
    hjust = 0, colour = "#666666", face = "plain", size = rel(0.72)
  ),

  plot.margin = margin(t = 0.4, r = 0.35, b = 0.35, l = 0.35, unit = "lines"),

  casenames = element_text(
    vjust = 0.5, hjust = 0,
    colour = "#3A3A3A", face = "plain", size = rel(0.8),
    margin = margin(0.12, 0.18, 0.12, 0.18, unit = "lines")
  ),

  casenames.strip = element_rect(
    colour = NA,
    fill = "#F5F5F5",
    linewidth = 0
  ),

  casenames.background = element_rect(fill = NA, colour = NA, linewidth = 0),

  casenames.position = "strip-right",

  phasenames = element_text(
    vjust = 0, hjust = 0.5,
    colour = "#7A7A7A", size = rel(0.72),
    margin = margin(b = 1.2)
  ),

  phasenames.position.x = "center",

  separators = element_line(
    colour = "#A0A0A0", linewidth = 0.4, linetype = "dashed"
  ),

  separators.extent = "full",
  separators.staircase = FALSE,

  label.text = element_text(
    colour = "#333333", vjust = 0.5, hjust = 0.5,
    angle = 0, size = rel(0.8)
  ),

  label.background = NULL,
  label.padding = 0.06,

  grid = element_line(linewidth = 0),

  legend.position = "none",
  legend.background = element_rect(fill = "white", colour = NA),
  legend.text = element_text(size = rel(0.68), colour = "#444444"),
  legend.title = element_text(size = rel(0.68), colour = "#333333"),
  legend.margin = margin(0.15, 0.15, 0.15, 0.15, unit = "lines"),

  dataline = list(
    "1" = element_line(colour = "#222222", linetype = "solid", linewidth = 0.6),
    "2" = element_line(colour = "#4C78A8", linetype = "solid", linewidth = 0.6),
    "3" = element_line(colour = "#C27D38", linetype = "solid", linewidth = 0.6)
  ),

  datapoint = list(
    "1" = element_point(colour = "#222222", shape = 16, size = 1.9),
    "2" = element_point(colour = "#4C78A8", shape = 16, size = 1.9),
    "3" = element_point(colour = "#C27D38", shape = 16, size = 1.9)
  ),

  statline = list(
    a = element_line(colour = "#8E3B46", linewidth = 0.7, linetype = "solid"),
    b = element_line(colour = "#4F6D9A", linewidth = 0.7, linetype = "solid"),
    c = element_line(colour = "#5C7A3A", linewidth = 0.7, linetype = "solid"),
    d = element_line(colour = "#A36A2B", linewidth = 0.7, linetype = "solid"),
    e = element_line(colour = "#7A4E8A", linewidth = 0.7, linetype = "solid"),
    f = element_line(colour = "#2F7C7C", linewidth = 0.7, linetype = "solid")
  )

)

## sienna -----------------------------------------------------------------

.scplot_themes$sienna = list(

  theme_type = "complete",

  text = element_text(
    colour = "sienna4", size = 11, family = "serif",
    face = "plain", angle = 0, hjust = 0.5, vjust = 0.5,
    lineheight = 1
  ),

  plot.background = element_rect(
    fill = "moccasin",
    colour = "darkseagreen4",
    linewidth = 0.6,
    linetype = "solid"
  ),
  panel.background = element_rect(
    fill = "white",
    colour = "tan4",
    linewidth = 0.5,
    linetype = "solid"
  ),
  panel.spacing.y = unit(1.4, "lines"),

  axis.expand.x = c(0, 0.6, 0, 0.6),
  axis.expand.y = c(0.05, 0, 0.05, 0),

  axis.line.x = element_line(
    colour = "sienna4", linewidth = 0.4, linetype = "solid"
  ),
  axis.line.y = element_line(
    colour = "sienna4", linewidth = 0.4, linetype = "solid"
  ),

  axis.ticks.length = unit(2.75, "points"),
  axis.ticks = element_line(
    colour = "sienna4", linewidth = 0.4, linetype = "solid"
  ),

  axis.title.y = element_text(
    angle = 90, margin = margin(r = 1.5), colour = "sienna4"
  ),
  axis.title.x = element_text(
    margin = margin(t = 2), colour = "sienna4"
  ),

  axis.text.x = element_text(
    size = rel(0.8), hjust = 0.5,
    colour = "sienna4", margin = margin(t = 1.5)
  ),
  axis.text.y = element_text(
    size = rel(0.8), hjust = 1,
    colour = "sienna4", margin = margin(r = 1.5)
  ),

  plot.title = element_text(
    colour = "sienna4",
    face = "bold",
    margin = margin(0, 0, 2, 0, unit = "lines"),
    hjust = 0.5
  ),

  plot.caption = element_text(
    colour = "sienna4",
    margin = margin(t = 1, r = 0, b = 0, l = 2, unit = "lines"),
    hjust = 0,
    face = "italic"
  ),

  plot.margin = margin(t = 1, r = 0.5, b = 0.5, l = 0.5, unit = "lines"),

  casenames = element_text(
    colour = "sienna4",
    vjust = 1, hjust = 0,
    face = "bold",
    margin = margin(0.3, 0.3, 0.3, 0.3, unit = "lines")
  ),

  casenames.strip = element_rect(
    colour = "tan4",
    fill = "cornsilk",
    linewidth = 0.5,
    linetype = "solid"
  ),
  casenames.background = element_rect(
    fill = NA,
    colour = NA,
    linewidth = 0.1,
    linetype = "solid"
  ),

  casenames.position = "topleft",

  phasenames = element_text(
    colour = "sienna4",
    vjust = 0, hjust = 0.5,
    margin = margin(b = 1.5)
  ),

  phasenames.position.x = "center",

  separators = element_line(
    colour = "sienna4", linewidth = 0.45, linetype = "dashed"
  ),

  separators.extent = "full",
  separators.staircase = FALSE,

  label.text = element_text(
    colour = "sienna4", vjust = 0.5, hjust = 0.5, angle = 0, size = rel(1)
  ),
  label.background = element_rect(
    fill = "cornsilk",
    colour = "tan4",
    linewidth = 0.3,
    linetype = "solid"
  ),

  label.padding = 0.1,

  grid = element_line(
    colour = "orange2", linewidth = 0.1, linetype = "solid"
  ),

  legend.position = "none",
  legend.background = element_rect(
    fill = "moccasin",
    colour = "tan4",
    linewidth = 0.3,
    linetype = "solid"
  ),
  legend.text = element_text(size = rel(0.7), colour = "sienna4"),
  legend.title = element_text(size = rel(0.7), colour = "sienna4", face = "bold"),
  legend.margin = margin(0.3, 0.3, 0.3, 0.3, unit = "lines"),

  dataline = list(
    "1" = element_line(
      colour = "darkolivegreen4", linetype = "solid", linewidth = 0.6
    ),
    "2" = element_line(
      colour = "seagreen4", linetype = "solid", linewidth = 0.6
    ),
    "3" = element_line(
      colour = "peru", linetype = "solid", linewidth = 0.6
    )
  ),

  datapoint = list(
    "1" = element_point(
      colour = "seagreen4", shape = 18, size = 2.2
    ),
    "2" = element_point(
      colour = "darkolivegreen4", shape = 17, size = 2.2
    ),
    "3" = element_point(
      colour = "peru4", shape = 16, size = 2.2
    )
  ),

  statline = list(
    a = element_line(colour = "sienna3", linewidth = 0.75, linetype = "solid"),
    b = element_line(colour = "darkgoldenrod3", linewidth = 0.75, linetype = "solid"),
    c = element_line(colour = "darkolivegreen3", linewidth = 0.75, linetype = "solid"),
    d = element_line(colour = "cadetblue4", linewidth = 0.75, linetype = "solid"),
    e = element_line(colour = "indianred3", linewidth = 0.75, linetype = "solid"),
    f = element_line(colour = "tan4", linewidth = 0.75, linetype = "solid")
  )

)





