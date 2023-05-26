
library(scplot)

filter <- c(1:15, 20:30)

p1 <- exampleABC %>%
  #subset(mt %in% filter) %>%
  scplot() %>%
  as_ggplot()
p1
library(ggbreak)

p1


squash_axis <- function(from, to, factor) {

  trans <- function(x) {
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to

    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)

    return(x)
  }

  inv <- function(x) {

    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor
    ito <- x >= from + (to - from)/factor

    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))

    return(x)
  }

  # return the transformation
  return(scales::trans_new("squash_axis", trans, inv))
}

p1 + scale_y_continuous(trans = squash_axis(60,80, 3))
from = 60
to = 80
factor <- 3



