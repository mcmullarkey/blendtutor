#' @importFrom cli cli_abort
NULL

#' Null-default operator
#'
#' Returns `lhs` if non-NULL, otherwise `rhs`. Provides compatibility
#' with R versions prior to 4.4 which lack a built-in `%||%` operator.
#'
#' @param lhs Left-hand side value
#' @param rhs Right-hand side default value
#' @return `lhs` if non-NULL, otherwise `rhs`
#' @keywords internal
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}
