#' continuous counting
#'
#' It counts the number of continuous identical values.
#'
#' @param x a vector or data frame.
#' @param cnt whether to count the number rows in each continuous groups.
#' @param ... ignored
#'
#' @return A integer vector indicating the number of continuous identical elements in x.
#'
#' @examples
#' # example I
#' x1 = c(0,0,0, 1,1,1)
#' conticnt(x1)
#' conticnt(x1, cnt=TRUE)
#'
#' x2 = c(1, 2,2, 3,3,3)
#' conticnt(x2)
#' conticnt(x2, cnt=TRUE)
#'
#' x3 = c('c','c','c', 'b','b', 'a')
#' conticnt(x3)
#' conticnt(x3, cnt=TRUE)
#'
#' # example II
#' dt = data.frame(c1=x1, c2=x2, c3=x3)
#' conticnt(dt, col=c('c1', 'c2'))
#' conticnt(dt, col=c('c1', 'c2'), cnt = TRUE)
#'
#' @import data.table
#' @importFrom stats setNames
#' @export
conticnt = function(x, cnt=FALSE, ...) {
  UseMethod('conticnt')
}

#' @export
conticnt.character = function(x, cnt=FALSE, ...) {
  setNames(conticnt1(x, cnt), x)
}

#' @export
conticnt.numeric = function(x, cnt=FALSE, ...) {
  setNames(conticnt1(x, cnt), x)
}

#' @export
conticnt.data.frame = function(x, cnt=FALSE, ...) {

  col = list(...)$col
  dtconti = setDT(x)[,(paste0('conti_', col)) := lapply(.SD, function(xi) conticnt1(xi,cnt)), .SDcols = col]

  return(dtconti[])
}

conticnt1 = function(x, cnt=FALSE) {
  conti = v1 = NULL

  dt = data.table(
    v1 = x
  )[, conti := as.integer(v1 != shift(v1, type='lag'))
  ][!(conti %in% 0), conti := 1
  ][, conti := cumsum(conti)]

  if (cnt) dt = dt[, conti := seq(.N), by = conti][]

  return(dt$conti)
}

