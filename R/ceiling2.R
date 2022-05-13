#' rounding of numbers
#'
#' The ceiling2 is ceiling of numeric values by digits. The floor2 is floor of numeric values by digits.
#'
#' @param x a numeric vector.
#' @param digits integer indicating the number of significant digits.
#'
#' @return
#' ceiling2 rounds the elements in x to the specified number of significant digits that is the smallest number not less than the corresponding elements.
#'
#' floor2 rounds the elements in x to the specified number of significant digits that is the largest number not greater than the corresponding elements.
#'
#' @examples
#' x = c(12345, 54.321)
#'
#' ceiling2(x)
#' ceiling2(x, 2)
#' ceiling2(x, 3)
#'
#' floor2(x)
#' floor2(x, 2)
#' floor2(x, 3)
#'
#' @export
ceiling2 = function(x, digits=1) {
  if (digits < 0) stop('The argument digits should be nonnegative vlaues.')
  x_sci = format(x, scientific = TRUE, digits=digits+1)
  z = ceiling(as.numeric(sub('e.+$', '', x_sci)) * 10^(digits-1))/10^(digits-1)
  e = sub('.+(e.+)$', '\\1', x_sci)
  as.numeric(paste0(z, e))
}

#' @export
#' @rdname ceiling2
floor2 = function(x, digits=1) {
  if (digits < 0) stop('The argument digits should be nonnegative vlaues.')
  x_sci = format(x, scientific = TRUE, digits=digits+1)
  z = floor(as.numeric(sub('e.+$', '', x_sci)) * 10^(digits-1))/10^(digits-1)
  e = sub('.+(e.+)$', '\\1', x_sci)
  as.numeric(paste0(z, e))
}

