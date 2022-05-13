#' char repetition rate
#'
#' reprate estimates the max rate of character repetition.
#'
#' @param x a character vector or a data frame.
#' @param col a character column name.
#'
#' @return a numeric vector indicating the max rate of character repetition in the corresponding elements in argument x vector.
#'
#' @examples
#' x = c('a', 'aa', 'ab', 'aab', 'aaab')
#' reprate(x)
#'
#' reprate(data.frame(x=x), 'x')
#'
#' @export
reprate = function(x, col) {
  UseMethod('reprate')
}

#' @export
reprate.character = function(x, ...) {
  reprate.data.frame(data.table(V1=x), 'V1')[['reprate_V1']]
}

#' @export
reprate.data.frame = function(x, col) {
  num = repn = rid = NULL

  dat = setDT(copy(x))[, rid := .I]

  dat2 = dat[!is.na(get(col)),c('rid', col),with=FALSE
  ][, strsplit(get(col),''), by = 'rid'
  ][, num := .N, by = 'rid'
  ][, repn := .N, by = c('rid', 'V1')
  ][][, max(repn/num), by = 'rid'][]
  setnames(dat2, 'V1', paste0('reprate_', col))

  dat3 = merge(dat, dat2, by = 'rid', all = TRUE)[, rid := NULL]
  return(dat3[])
}
