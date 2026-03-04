

add_staircase <- function(
    p, x_pos,
    color = "red",
    linewidth = 1.2,
    linetype = 1,
    y_h = 0.0, # vertical position of the horizontal connector inside the gap (0..1 npc)
    ypos = c(0,1)) {

  p <- p + coord_cartesian(clip = "off")

  g  <- ggplot2::ggplotGrob(p)
  pb <- ggplot2::ggplot_build(p)

  gp_v <- grid::gpar(lty = linetype, lwd = linewidth, col = color)
  gp_h <- grid::gpar(lty = linetype, lwd = linewidth, col = color)

  # assumes fixed x scale across facets
  xr <- pb$layout$panel_params[[1]]$x.range

  # panels in reading order (top -> bottom, left -> right)
  panel_ids <- grep("^panel", g$layout$name)
  panels <- g$layout[panel_ids, c("t","b","l","r")]
  panels <- panels[order(panels$t, panels$l), ]
  n_panels <- nrow(panels)

  # helper: nearest strip row above a panel (same columns), if present
  strip_ids <- grep("^strip-t", g$layout$name)
  strips <- if (length(strip_ids) > 0) g$layout[strip_ids, c("t","b","l","r")] else NULL

  strip_top_row <- function(panel_row) {
    if (is.null(strips)) return(panel_row$t)
    cand <- strips[strips$l == panel_row$l & strips$r == panel_row$r & strips$b <= panel_row$t, , drop = FALSE]
    if (nrow(cand) == 0) panel_row$t else min(panel_row$t, max(cand$t))
  }
  strip_bottom_row <- function(panel_row) {
    if (is.null(strips)) return(panel_row$b)
    cand <- strips[strips$l == panel_row$l & strips$r == panel_row$r & strips$b <= panel_row$t, , drop = FALSE]
    if (nrow(cand) == 0) panel_row$b else max(panel_row$b, max(cand$b))
  }

  # compute for each facet:
  # - top row (including strip)
  # - bottom row for vertical segment (up to just before next facet begins)
  top_row  <- integer(n_panels)
  bot_row  <- integer(n_panels)

  for (i in seq_len(n_panels)) {
    top_row[i] <- strip_top_row(panels[i, ])
  }
  for (i in seq_len(n_panels - 1)) {
    next_top <- strip_top_row(panels[i + 1, ])
    bot_row[i] <- next_top - 1
  }
  bot_row[n_panels] <- max(g$layout$b[panel_ids])

  grid::grid.newpage()
  grid::grid.draw(g)

  # align drawing with the ggplot gtable
  grid::pushViewport(grid::viewport(
    layout = grid::grid.layout(
      nrow = nrow(g), ncol = ncol(g),
      widths = g$widths, heights = g$heights
    )
  ))

  # 1) vertical lines per facet (spanning facet + inter-panel space below)
  for (case in 1:length((x_pos))) {

    for (i in seq_len(n_panels)) {
      grid::pushViewport(grid::viewport(
        layout.pos.row = top_row[i]:bot_row[i],
        layout.pos.col = panels[i, "l"]:panels[i, "r"],
        xscale = xr,
        yscale = c(0, 1),
        clip = "off"
      ))
      grid::grid.lines(
        x = grid::unit(x_pos[[case]][i], "native"),
        y = grid::unit(ypos, "npc"),
        gp = gp_v
      )
      grid::upViewport()
    }


    # 2) horizontal connectors in the gap above facet i+1, from x_pos[i] to x_pos[i+1]
    for (i in seq_len(n_panels - 1)) {
      prev_bottom <- strip_bottom_row(panels[i, ])
      next_top    <- strip_top_row(panels[i + 1, ])

      gap_rows <- (prev_bottom + 1):(next_top - 1)
      if (length(gap_rows) < 1) next  # if panels touch, skip connector

      grid::pushViewport(grid::viewport(
        layout.pos.row = gap_rows,
        layout.pos.col = panels[i + 1, "l"]:panels[i + 1, "r"],
        xscale = xr, yscale = c(0, 1),
        clip = "off"
      ))
      grid::grid.lines(
        x = grid::unit(c(x_pos[[case]][i], x_pos[[case]][i + 1]), "native"),
        y = grid::unit(y_h, "npc"),
        gp = gp_h
      )
      grid::upViewport()
    }
  }
  grid::upViewport()
  invisible(g)
}
