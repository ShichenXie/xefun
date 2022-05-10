#' char repetition rate
#'
#' reprate estimate the max rate of character repetition.
#'
#' @param dt a data frame or vector.
#' @param col a character column name.
#'
#' @examples
#' x = c('a', 'aa', 'ab', 'aab', 'aaab')
#' reprate(x)
#'
#' reprate(data.frame(x=x), 'x')
#'
#' @export
reprate = function(dt, col) {
  UseMethod('reprate')
}

#' @export
reprate.character = function(dt, ...) {
  reprate.data.frame(data.table(V1=dt), 'V1')[['reprate_V1']]
}

#' @export
reprate.data.frame = function(dt, col) {
  num = repn = rid = NULL

  dat = setDT(copy(dt))[, rid := .I]

  dat2 = dat[!is.na(get(col)),c('rid', col),with=FALSE
  ][, strsplit(get(col),''), by = 'rid'
  ][, num := .N, by = 'rid'
  ][, repn := .N, by = c('rid', 'V1')
  ][][, max(repn/num), by = 'rid'][]
  setnames(dat2, 'V1', paste0('reprate_', col))

  dat3 = merge(dat, dat2, by = 'rid', all = TRUE)[, rid := NULL]
  return(dat3[])
}
