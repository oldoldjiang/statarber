#' Trading calendar
#'
#' An integer vector of trading days
#' @export
print(getwd())
trading.calendar <- as.integer(readLines('inst/calendar.csv'))

#' Check trading day
#' @param x integer date
#' @export
#' @examples
#' isTradingDay(20160106)
#' isTradingDay(c(20160106, 20160205))
isTradingDay <- function(x) {
  calendar_contains(trading.calendar, x)
}

#' Get trading days in a range
#' @param from starting date
#' @param to ending date
#' @export
#' @examples
#' getTradingDayRange(20160101, 20160131)
getTradingDayRange <- function(from, to) {
  trading.calendar[which(trading.calendar >= as.integer(from) & trading.calendar <= as.integer(to))]
}

#' Get previous trading day
#' @inheritParams isTradingDay
#' @export
#' @examples
#' prevTradingDay(20160106)
#' prevTradingDay(c(20160106, 20160205))
prevTradingDay <- function(x) {
  calendar_lt(trading.calendar, x)
}

#' Get next trading day
#' @inheritParams isTradingDay
#' @export
#' @examples
#' nextTradingDay(20160106)
#' nextTradingDay(c(20160106, 20160205))
nextTradingDay <- function(x) {
  calendar_gt(trading.calendar, x)
}

#' Get greater-or-equal trading day
#' @inheritParams isTradingDay
#' @export
#' @examples
#' getTradingDayGte(20160102)
#' getTradingDayGte(20160105)
getTradingDayGte <- function(x, n = 0) {
  calendar_gte(trading.calendar, x, n)
}

#' Get less-or-equal trading day
#' @inheritParams isTradingDay
#' @export
#' @examples
#' getTradingDayLte(20160102)
#' getTradingDayLte(20160105)
getTradingDayLte <- function(x, n = 0) {
  calendar_lte(trading.calendar, x, n)
}
getTradingDay <- getTradingDayLte
