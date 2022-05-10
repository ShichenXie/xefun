#' continuous counting
#'
#' It counts the number of values continuous comparing with a threshold.
#'
#' @param dt a data frame or vector.
#' @param col a numeric column name.
#' @param opr relational operators, defaults to ==.
#' @param threshold threshold value, defaults to 0.
#' @param cnt whether to count the number rows in each continuous groups.
#'
#' @examples
#' # example I
#' dt1 = c(0,0,0, 1,1,1, 0,0, 1,1)
#' conticnt(dt1)
#' conticnt(dt1, cnt = TRUE)
#' conticnt(dt1, opr = '<=')
#'
#' # example II
#' dt2 = data.frame(x = c(0,0,0, 1,1,1, 0,0, 1,1))
#' conticnt(dt2, col='x')
#' conticnt(dt2, col='x', cnt=TRUE)
#' conticnt(dt2, col='x', opr='<=')
#'
#' library(data.table)
#' setDT(dt2)[, ct_N := conticnt(x)][]
#'
#' # example III
#' # number of growth values
#' dt3 = data.frame(x = c(0,1,2, 3,2,1, 0,1, 2,1))
#' dt3 = setDT(dt3)[, diff := x - shift(x,type='lag',fill = 0)]
#' conticnt(dt3, col='diff', opr = '>=')
#'
#' @import data.table
#' @importFrom stats setNames
#' @export
conticnt = function(dt, col, opr = '==', threshold = 0, cnt = FALSE) {
  UseMethod('conticnt')
}

#' @export
conticnt.numeric = function(dt, col=NULL, opr = '==', threshold = 0, cnt = FALSE) {
  datCnt = conticnt.data.frame(data.table(V1 = dt), col = 'V1', opr = opr, threshold = threshold, cnt = cnt)

  ret = setNames(datCnt$ct_N, datCnt$ct_oprth)
  return(ret)
}

#' @export
conticnt.data.frame = function(dt, col, opr = '==', threshold = 0, cnt = FALSE) {
  V1=comp=contigp=contix=ct_N=ct_group=ct_oprth=op=opgp=NULL

  relopr = data.table(
    op = c('<', '>=', '>', '<=', '==', '!='),
    opgp = rep(1:3, each = 2),
    key = 'op'
  )

  dt = setDT(copy(dt))
  setnames(dt, col, 'V1')

  datCnt = copy(dt)[, comp := do.call(opr, list(V1,threshold))
                  ][comp == TRUE, ct_oprth := paste0(opr, threshold)
                  ][comp == FALSE, ct_oprth := paste0(relopr[opgp==relopr[opr,opgp], setdiff(op, opr) ], threshold)
                  ][, contix := as.integer(comp)
                  ][, contigp := contix - shift(contix, type='lag')
                  ][!(contigp %in% 0), contigp := 1
                  ][, ct_group := cumsum(contigp)
                  ]

  bykeys = c('ct_group', 'ct_oprth')
  if (cnt) {
    ret = datCnt[, ct_N := .N, by = bykeys][]
  } else {
    ret = datCnt[, ct_N := seq(.N), by = bykeys][]
  }
  ret = ret[ct_oprth != paste0(opr, threshold), ct_N := -ct_N][, (c('comp', 'contix', 'contigp')) := NULL]
  setnames(ret, 'V1', col)

  return(ret[])
}

