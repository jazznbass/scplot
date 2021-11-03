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



# it's gppplot ->

p1 <- scplot(exampleABC) %>% set_xaxis(increment = 4) %>% print()
p2 <- scplot(example_A24) %>% set_xaxis(angle = 90, increment = 2) %>% print()

gridExtra::grid.arrange(p1, p2, ncol = 2,
                        top = "Just an example",
                        bottom = "Two scplots arranged with ggplot2.")



p1 <- scplot(exampleABC$Marie) %>% set_xaxis(increment = 4) %>% print()


mytheme <- gridExtra::ttheme_default(base_size = 6, padding = unit(c(2, 2), "mm"))

t1 <- gridExtra::tableGrob(example_A24[[1]], theme = mytheme)

gridExtra::grid.arrange(
  t1,
  p2,
  ncol = 2,
  widths = c(1, 2),
  clip = FALSE
)


scplot(exampleAB_add) %>%
  set_xaxis(increment = 4) %>%
  add_dataline("depression", color = "blue") %>%
  add_dataline("cigarrets", color = "darkred")  %>%
  add_legend()
p <- q %>% print()
q

object <- q

object$datalines[[1]]$col <- "black"
.color <- unlist(lapply(object$datalines, function(x) x$col))
.color <- setNames(.color, .color)

object$datalines[[1]]$variable <- "wellbeing"
.label <- unlist(lapply(object$datalines, function(x) x$variable))



p +
  #geom_line(aes(y = 0, colour = "black")) +
  #geom_point(aes(y = 0, colour = "")) +
  theme(legend.position = "right") +
  #scale_color_identity(guide = "legend") +
  scale_colour_manual(
    name = "Lines",
  values = .color,
    labels = .label
    )


dat <- as.data.frame(exampleAB_add)

col <- c("wellbeing", "cigarrets")

ggplot(dat, aes(x = day, group = phase)) +
  theme_void() +
  geom_line(aes(y = !!sym(col[1]), colour = col[1])) +
  geom_line(aes(y = !!sym(col[2]), colour = col[2])) +
  geom_point(aes(y = !!sym(col[2])), colour = "green") +
  facet_grid(as.factor(case) ~ .) +
  scale_colour_manual(
    name = "Lines",
    values = c(cigarrets = "black", wellbeing = "blue", "red")
  )#+
  #scale_color_identity(guide = "legend")

  #add_legend()


# Right -> inside the plot area
p <- scplot(exampleAB_add) %>%
  set_xaxis(increment = 4) %>%
  add_dataline("depression") %>%
  print()

p + theme(
  legend.position = "right",#c(.95, .95),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

