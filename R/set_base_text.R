#' Set base text characteristics of an scplot
#'
#' @inheritParams .inherit_scplot
#' @export
set_base_text <- function(object, ...) {

  args <- do.call("element_text", list(...))
  object$theme$text <- merge_element(args, object$theme$text)

  object
}
