#' vector to list
#'
#' Converting a vector to a list with names specified.
#'
#' @param x a vector.
#' @param name specify the names of list. Setting the names of list as x by default.
#' @param ... Additional parameters provided in the as.list function.
#'
#' @examples
#' as.list2(c('a', 'b'))
#'
#' as.list2(c('a', 'b'), name = FALSE)
#'
#' as.list2(c('a', 'b'), name = c('c', 'd'))
#'
#' @export
as.list2 = function(x, name=TRUE, ...) {
    lst = as.list(x, ...)

    if (isTRUE(name)) {
      lst = setNames(lst, x)
    } else if (length(name) == length(x)) {
      lst = setNames(lst, name)
    }
    return(lst)
}

c_list = function(x, name=TRUE, ...) {
  as.list2(x, name, ...)
}

#' merge data.frames list
#'
#' Merge a list of data.frames by common columns or row names.
#'
#' @param datlst a list of data.frames.
#' @param by A vector of shared column names in x and y to merge on. This defaults to the shared key columns between the two tables. If y has no key columns, this defaults to the key of x.
#' @param all logical; all = TRUE is shorthand to save setting both all.x = TRUE and all.y = TRUE.
#' @param ... Additional parameters provided in the merge function.
#'
#' @export
merge2 = function(datlst, by = NULL, all = TRUE, ...) {
  Reduce(function(x,y) merge(setDT(x), setDT(y), by = by, all=all, ...), datlst )
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
