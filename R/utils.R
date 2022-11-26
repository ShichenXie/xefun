# vector to list
c_list = function(x, name=TRUE) {
    lst = as.list(x)
    if (isTRUE(name)) lst = setNames(lst, x)
    return(lst)
}

#' columns by type
#'
#' The columns name of a data frame by given data types.
#'
#' @param dt a data frame.
#' @param type a string of data types, available values including character, numeric, double, integer, logical, factor, datetime.
#'
#' @examples
#' dt = data.frame(a = sample(0:9, 6), b = sample(letters, 6),
#'                 c = Sys.Date()-1:6, d = Sys.time() - 1:6)
#' dt
#' # numeric columns
#' cols_type(dt, 'numeric')
#' # or
#' cols_type(dt, 'n')
#'
#' # numeric and character columns
#' cols_type(dt, c('character', 'numeric'))
#' # or
#' cols_type(dt, c('c', 'n'))
#'
#' # date time columns
#' cols_type(dt, 'datetime')
#'
#' @export
cols_type = function(dt, type) UseMethod('cols_type')

#' @export
cols_type.data.frame = function(dt, type) {
  type = sapply(type,
                function(x) match.arg(x, c('character', 'numeric', 'double', 'integer', 'logical', 'factor', 'datetime')),
                USE.NAMES = FALSE)

  as.vector(unlist(sapply(
    type,
    function(t) names(which(setDT(dt)[, sapply(.SD, paste0('is.',t))])),
    USE.NAMES = FALSE
  )))
}

#' constant columns
#'
#' The columns name of a data frame with constant value.
#'
#' @param dt a data frame.
#'
#' @examples
#' dt = data.frame(a = sample(0:9, 6), b = sample(letters, 6),
#'                 c = rep(1, 6), d = rep('a', 6))
#' dt
#' cols_const(dt)
#'
#' @export
cols_const = function(dt) UseMethod('cols_const')

#' @export
cols_const.data.frame = function(dt) {
  names(which(setDT(dt)[,sapply(.SD, function(x) length(unique(x))==1)]))
}
