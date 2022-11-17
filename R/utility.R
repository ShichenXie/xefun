# vector to list
c_list = function(x, name=TRUE) {
    lst = as.list(x)
    if (isTRUE(name)) lst = setNames(lst, x)
    return(lst)
}

# columns name by type
cols_name = function(dt, type) {
  dt = setDT(dt)
  type = match.arg(type, c('character', 'numeric', 'double', 'integer', 'logical', 'factor'))

  names(which(dt[, sapply(.SD, paste0('is.',type))]))
}
