#' Not in operator
#'
#' A negation of the \code{\%in\%} operator.
#'
#' @param x vector or list
#' @param y vector or list
#'
#' @return A logical vector indicating if elements of \code{x}
#'   are NOT contained in \code{y}.
#'
#' @examples
#' 1:5 %!in% c(1, 3, 5)
#'
#' @export
'%!in%' <- function(x, y) {
  !(x %in% y)
}
