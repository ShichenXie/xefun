#' Maxima and Minima
#'
#' Returns the (regular or parallel) maxima and minima of the input values. For numeric NAs, it returns NA instead of Inf or -Inf.
#'
#' @param ... numeric or character arguments
#' @param na.rm a logical indicating whether missing values should be removed.
#'
#' @examples
#' max2(c(NA), na.rm=TRUE)
#' max(c(NA), na.rm=TRUE)
#'
#' min2(c(NA), na.rm=TRUE)
#' min(c(NA), na.rm=TRUE)
#'
#' @export
max2 = function(..., na.rm=FALSE) {
  x = suppressWarnings(max(..., na.rm=na.rm))
  ifelse(is.infinite(x), NA_real_, x)
}


#' @export
#' @rdname max2
min2 = function(..., na.rm=FALSE) {
  x = suppressWarnings(min(..., na.rm=na.rm))
  ifelse(is.infinite(x), NA_real_, x)
}
