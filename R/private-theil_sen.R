theil_sen <- function(formula, data) {

  y <- model.frame(formula, data)[[1]]
  x <- model.frame(formula, data)[[2]]

  # Pairwise slopes (exclude ties in x)
  idx <- combn(length(y), 2)
  dx <- x[idx[2, ]] - x[idx[1, ]]
  dy <- y[idx[2, ]] - y[idx[1, ]]
  keep <- dx != 0

  slope <- median(dy[keep] / dx[keep])
  intercept <- median(y - slope * x)

  out <- list(
    intercept = intercept,
    slope = slope,
    coefficients = c(intercept, slope),
    fitted.values = intercept + slope * x
  )

  out
}
