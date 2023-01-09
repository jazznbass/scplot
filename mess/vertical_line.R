library(scplot)

library(gtable)
library(grid)

data2npc <- function(x) {
  range_x <- pb$layout$panel_params[[1]][["x.range"]]
  scales::rescale(c(range_x, x), c(0,1))[-c(1,2)]
}

add_line <- function(pg, pb, start, end) {
  range_x <- pb$layout$panel_params[[1]][["x.range"]]
  start[1] <-  scales::rescale(c(range_x, start[1]), c(0,1))[-c(1,2)]
  end[1] <-  scales::rescale(c(range_x, end[1]), c(0,1))[-c(1,2)]

  pg <- gtable_add_grob(
    pg,
    segmentsGrob(
      x0=start[1], x1=end[1], y0=1, y1= 1 - (1/length(scdf)), gp=gpar(lty=1)
    ),
    t=7, b=11, l=5
  )
  pg

}


scdf <- exampleABC

p<- scplot(scdf) |>
  as_ggplot()



pb <- ggplot_build(p)
pg <- ggplot_gtable(pb)

ps <- lapply(scdf, scan:::.phasestructure, pvar = "phase")

grid.newpage()
grid.draw(pg)



data2npc <- function(x, y, panel) {
  range_x <- pb$layout$panel_params[[panel]][["x.range"]]
  range_y <- pb$layout$panel_params[[panel]][["y.range"]]
  c(scales::rescale(c(range_x, x), c(0,1))[-c(1,2)],
    scales::rescale(c(range_y, y), c(0,1))[-c(1,2)])
}


start <- data2npc(5.5, 90, 1)
end <- data2npc(6, 70, 3)
#pg <- gtable_add_grob(pg, segmentsGrob(x0=start[1], x1=end[1], y0=start[2], y1=end[2], gp=gpar(lty=1)),
#                      t=7, b=11, l=5)

pg <- gtable_add_grob(pg,
  segmentsGrob(x0=start[1], x1=end[1], y0=1, y1= 1 - (1/length(scdf)), gp=gpar(lty=1)),
  t=7, b=11, l=5
)

grid.newpage()
grid.draw(pg)
###


# ggplot2 doesn't use native units in data space
# instead, the data is rescaled to npc, i.e from 0 to 1
# so we need to use the build info to convert from data to [0,1]

df <- data.frame(y=c(1,2,3),x=1,Set=LETTERS[1:3])
p <- ggplot(df,aes(x,y)) +
  theme_bw() + theme(legend.position=c(.01,.99),legend.justification=c(0,1)) +
  geom_point(aes(fill=Set),color="black",shape=21,size=3) +
  facet_grid(cols = vars(Set)) +
  xlim(1,5)
gb <- ggplot_build(p)
g <- ggplot_gtable(gb)
ranges <- gb$layout$panel_params
data2npc <- function(x, range) scales::rescale(c(range, x), c(0,1))[-c(1,2)]


start <- c(data2npc(1, ranges[[1]][["x.range"]]),
           data2npc(3, ranges[[1]][["y.range"]]))

end <- c(data2npc(5, ranges[[3]][["x.range"]]),
         data2npc(1, ranges[[3]][["y.range"]]))

pg <- gtable_add_grob(pg, segmentsGrob(x0=start[1], x1=start[1], y0=0, y1=1, gp=gpar(lty=2)), t=7, b=9, l=5)

grid.newpage()
grid.draw(pg)



####
library(ggplot2)
library(gtable)
library(grid)

dat <- data.frame(x=rep(1:10,2),y=1:20+rnorm(20),z=c(rep("A",10),rep("B",10)))

p <- ggplot(dat,aes(x,y)) + geom_point() + facet_grid(z~.) + xlim(0,10)
pb <- ggplot_build(p)
pg <- ggplot_gtable(pb)


data2npc <- function(x, panel = 1L, axis = "x") {
  range <- pb$layout$panel_params[[panel]][[paste0(axis,".range")]]
  scales::rescale(c(range, x), c(0,1))[-c(1,2)]
}


start <- sapply(c(4,8), data2npc, panel=1, axis="x")

pg <- gtable_add_grob(pg, segmentsGrob(x0=start, x1=start, y0=0, y1=1, gp=gpar(lty=2)), t=7, b=9, l=5)

grid.newpage()
grid.draw(pg)
