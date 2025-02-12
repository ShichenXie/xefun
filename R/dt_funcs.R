dat_filter = function(dat, date_range, timestamp, timesubmit) {
  if (!is.null(date_range) && !is.na(date_range)) {
    timestamp_col = timesubmit_col = NULL

    timecols = c(timestamp, timesubmit)
    dat = copy(dat)[, (c('timestamp_col', 'timesubmit_col')) := lapply(.SD, as_date), .SDcols = timecols]

    dat = dat[
      timestamp_col >= date_from(date_range, timesubmit_col) &
        timestamp_col < timesubmit_col
    ][, (c('timestamp_col', 'timesubmit_col')) := NULL]

  }

  return(dat)
}

dat_expand = function(dat, keys) {
  setDT(dat)

  dtexp = merge(
    dat,
    setDT(do.call(expand.grid, args = lapply(dat[, keys, with=FALSE], unique))),
    by = keys,
    all = TRUE
  )

  return(dtexp)
}
