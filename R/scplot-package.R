#' Single-Case Data Plots
#'
#' A collection of procedures for visualizing single-case data. It is an
#' add-on package for the `scan` package.
#'
#' @name scplot-package
#' @docType package
#' @author Juergen Wilbert \[aut, cre\]
#' @keywords package
#' @import scan
#' @import ggplot2
#' @import stats
#' @import utils
#' @importFrom mblm mblm
NULL

utils::globalVariables(c(
  "mt", "phase", "case", "x0", "y0", "y1", "y",
  "by_class", "by_call", "not","within","one_of","has_length","is_true"
))

