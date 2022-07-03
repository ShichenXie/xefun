as_date = function(x, format = NULL) {
  d = try(as.Date(x), silent = TRUE)

  if (inherits(d, 'try-error')) {
    if (is.null(format)) {
      if (all(nchar(x) == 8)) format = '%Y%m%d'
    }
    d = as.Date(x, format = format)
  }

  return(d)
}

#' start/end date by period
#'
#' The date of bop (beginning of period) or eop (end of period).
#'
#' @param freq the frequency of period. It supports weekly, monthly, quarterly and yearly.
#' @param x a date
#' @param workday logical, whether to return the latest workday
#'
#' @return
#' date_bop returns the beginning date of period of corresponding x by frequency.
#'
#' date_eop returns the end date of period of corresponding x by frequency.
#'
#' @examples
#' date_bop('weekly', Sys.Date())
#' date_eop('weekly', Sys.Date())
#'
#' date_bop('monthly', Sys.Date())
#' date_eop('monthly', Sys.Date())
#'
#' @export
date_bop = function(freq, x, workday = FALSE) {
  bop_mthday = NULL

  freq = match.arg(freq, c('daily', 'weekly', 'monthly', 'quarterly', 'yearly'))
  x = as_date(x)

  monthday = data.table(
    m = 1:12,
    bop_mthday = sprintf('-%02i-01', 1:12),
    key = 'm'
  )

  if (freq == 'yearly') {
    x = as_date(sub('-[0-9]{2}-[0-9]{2}$', '-01-01', x))
  } else if (freq == 'quarterly') {
    x = monthday[(quarter(x)-1)*3+1, as_date(paste0(year(x),bop_mthday))]
  } else if (freq == 'monthly') {
    x = as_date(sub('[0-9]{2}$', '01', x))
  } else if (freq == 'weekly') {
    x = x - wday(x) + 1
  } else if (freq == 'daily') {
    return(x)
  }

  if (workday) x = date_lwd(-1, x)
  return(x)
}


#' @export
#' @rdname date_bop
date_eop = function(freq, x, workday = FALSE) {
  freq = match.arg(freq, c('daily', 'weekly', 'monthly', 'quarterly', 'yearly'))
  if (inherits(x, 'character')) x = as_date(x)

  if (freq == 'yearly') {
    x = as_date(sub('-[0-9]{2}-[0-9]{2}$', '-12-31', x))
  } else if (freq == 'quarterly') {
    x = date_eop(sprintf('%s-%s-01', year(x), quarter(x)*3), freq = 'monthly')
  } else if (freq == 'monthly') {
    x = date_bop(date_bop(x, freq='monthly') + 45, freq='monthly') - 1
  } else if (freq == 'weekly') {
    x = x - wday(x) + 7
  } else if (freq == 'daily') {
    return(x)
  }

  if (workday) x = date_lwd(1, x)
  return(x)
}


#' start date by range
#'
#' The date before a specified date by date_range.
#'
#' @param date_range date range, available value including nd, nm, mtd, qtd, ytd, ny, max.
#' @param to a date, default is current system date.
#' @param default_from the default date when date_range is sett to max
#'
#' @return It returns the start date of a date_range with a specified end date.
#'
#' @examples
#' date_from(3)
#' date_from('3d')
#'
#' date_from('3m')
#' date_from('3q')
#' date_from('3y')
#'
#' date_from('mtd')
#' date_from('qtd')
#' date_from('ytd')
#'
#' @export
date_from = function(date_range, to=Sys.Date(), default_from='1000-01-01') {
  UseMethod('date_from')
}

# last calendar day by xtd (ytd/qtd/mtd)
date_from_xtd = function(date_range, to = Sys.Date()) {
  if (inherits(to, 'character')) to = as_date(to)
  to_year = year(to)
  to_quarter = quarter(to)
  to_month = month(to)

  if (date_range == 'ytd') {
    from_month = '01'
  } else if (date_range == 'qtd') {
    from_month = to_quarter*3-2
  } else if (date_range == 'mtd') {
    from_month = to_month
  }
  from = as_date(sprintf('%s-%s-01', to_year, from_month))

  return(from)
}
date_from_xm = function(date_range, to = Sys.Date()) {
  if (inherits(to, 'character')) to = as_date(to)
  to_year = year(to)
  to_month = month(to)
  to_day = mday(to)

  xm = as.integer(sub("m","",date_range))
  rng_year = floor(xm / 12)
  rng_month = xm %% 12

  if (to_month <= rng_month) {
    rng_year = rng_year + 1
    rng_month = rng_month - 12
  }

  from = as_date(sprintf('%s-%s-%s', to_year-rng_year, to_month-rng_month, to_day))

  return(from)
}
date_from_ym = function(date_range, to = Sys.Date()) {
  from_year = year(to) - as.integer(sub("y","",date_range))
  from = as_date(sub("^[0-9]{4}", from_year, to))
  return(from)
}

# the date from date_range before (calendar day)
#' @export
date_from.character = function(date_range, to = Sys.Date(), default_from='1000-01-01') {
  to = as_date(to)

  if (grepl("[yqm]td", date_range)) {
    from = date_from_xtd(date_range, to)
  } else if (grepl("[1-9][0-9]*d", date_range)) {
    from = to - as.integer(sub("d","",date_range))
  } else if (grepl("[1-9][0-9]*w", date_range)) {
    from = to - as.integer(sub("w","",date_range))*7
  } else if (grepl("[1-9][0-9]*m", date_range)) {
    for (i in c(0, 1, -1, 2, -2)) {
      from = try(date_from_xm(date_range, to+i), silent = TRUE)
      if (!inherits(from, 'try-error')) {
        if (i != 0) from = from - i/abs(i)
        break
      }
    }
  } else if (grepl("[1-9][0-9]*y", date_range)) {
    for (i in c(0, 1, -1, 2, -2)) {
      from = try(date_from_ym(date_range, to+i), silent = TRUE)
      if (!inherits(from, 'try-error')) {
        if (i != 0) from = from - i/abs(i)
        break
      }
    }
  } else if (date_range == 'max') {
    from = as_date(default_from)
  } else {
    from = NULL
  }
  return(from)
}
#' @export
date_from.numeric <- function(date_range, to = Sys.Date(), ...) {
  ft = NULL
  # , tz = Sys.timezone()
  to = as_date(to)
  from = to - date_range

  return(from)
}


#' latest workday
#'
#' The latest workday date of n days before a specified date.
#'
#' @param n number of days
#' @param to a date, default is current system date.
#'
#' @return It returns the latest workday date that is n days before a specified date.
#'
#' @examples
#' date_lwd(5)
#' date_lwd(3, "2016-01-01")
#' date_lwd(3, "20160101")
#'
#' @export
date_lwd = function(n, to = Sys.Date()) {
  ft = NULL

  to = as_date(to)
  n2 = abs(n) + ceiling(abs(n)/7)*2

  from = sapply(
    to,
    function(x) {
      data.table(
        ft = seq(x, x - sign(n) * n2,  by = -sign(n))
      )[wday(ft) %in% 2:6
      ][abs(n), as.character(ft)]
    }
  )

  return(as_date(from))
}


is_datetime = function(x) {
  inherits(x, c("Date","POSIXlt","POSIXct","POSIXt"))
}

