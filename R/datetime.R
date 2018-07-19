#' Format date in integer representation
#' @param date Date represented as integer
#' @return character
#' @export
#' @examples
#' dtFormatDate(20160115)
#' dtFormatDate(c(20160115, 20160320))
dtFormatDate <- function(date) {
  date <- sprintf("%08d", date)
  sprintf("%s-%s-%s", substr(date, 1L, 4L), substr(date, 5L, 6L), substr(date, 7L, 8L))
}

#' Format time in integer representation
#' @param time Time represented as integer
#' @return character
#' @export
#' @examples
#' dtFormatTime(92351)
#' dtFormatTime(152032)
#' dtFormatTime(c(92351, 152032))
dtFormatTime <- function(time) {
  time <- sprintf("%06d", time)
  sprintf("%s:%s:%s", substr(time, 1L, 2L), substr(time, 3L, 4L), substr(time, 5L, 6L))
}

#' Convert string date into integer date
#' @param date Date string
#' @export
#' @examples
#' dtIntDate("2016-01-05")
#' dtIntDate(c("2016-01-05", "2016-01-06"))
dtIntDate <- function(date) {
  as.integer(gsub("-", "", date, fixed = TRUE))
}

#' Convert string time into integer time
#' @param time Time string
#' @export
#' @examples
#' dtIntTime("09:25:30")
#' dtIntTime(c("15:10:30", "9:45:21"))
dtIntTime <- function(time) {
  as.integer(gsub(":", "", time, fixed = TRUE))
}

#' Get the number of seconds relative to midnight
#' @param hms a numeric time
#' @export
#' @examples
#' dtGetSec(93000)
#' dtGetSec(0)
#' dtGetSec(235930)
#' dtGetSec(235959)
dtGetSec <- function(hms) {
  as.integer((3600L * floor(hms / 10000L) +
                60L * floor((hms %% 10000L) / 100L) +
                (hms %% 100L)) - (hms > 200000L) * 86400L)
}
getExpiryDays <- function(codes, currentDate, useTradingDay = FALSE){
  expiryDate <- as.Date(as.character(getExpiryDate(codes)), '%Y%m%d')
  currentDate <- as.Date(gsub('-','',currentDate),'%Y%m%d')
  if(useTradingDay==TRUE){
    tradingDay <- getTradingDay(currentDate)
    duration <- unlist(sapply(1:length(codes), function(i){
      dr <- get_trading_day_range(as.integer(gsub('-','',currentDate)), as.integer(gsub('-','',expiryDate)))
      length(dr) - 1
    }, USE.NAMES = FALSE))
  }else{
    duration <- as.integer(expiryDate - currentDate)
  }
  return(duration)
}
#' temp get expiry date function
#'
#' @param codes
#'
#' @return
#' @export
#' @importFrom timeDate timeNthNdayInMonth
#' @examples
getExpiryDate <- function(codes){
  shf <- c('CU','AL','PB','ZN','SN','NI','RU','BU','AU','AG','RB','HC','WR') # 'FU'
  dce <- c('A', 'C', 'CS','I', 'J', 'JM','L', 'M', 'P', 'PP','V', 'Y', 'BB','FB','B') # 'JD'
  czc <- c('CF','MA','FG','OI','RM','SR','TA','WH','WS','ME','RO','ER','JR','LR','PM','RI','RS','SF','SM') # 'TC' 'ZC'
  indexFutures <- c('IF','IC','IH')
  bondFutures  <- c('TF', 'T')
  instrument <- str_extract(codes, '\\w{1,2}\\d{3,4}')
  product <- gsub('\\d','',instrument)
  product <- toupper(product)
  date <- str_pad(gsub('[A-z]','',instrument), width = 4, pad = 1)
  date <- paste0('20',date)
  firstDate <- paste0(date,'01')
  sd <- as.Date(paste0(min(date),'01'), '%Y%m%d')
  sd <- sd + 30
  ed <- as.Date(paste0(max(date),'01'), '%Y%m%d')
  ed <- ed + 30
  dateRange <- get_trading_day_range(dt_int_date(sd),dt_int_date(ed))

  expiryDate <- sapply(1:length(product), function(i){
    product1 <- product[i]
    date1 <- date[i]
    firstDate1 <- firstDate[i]
    if(product1 %in% shf){
      tmp <- as.integer(paste0(date1,'15'))
      return(calendar[calendar>=tmp][1])
    }else if(product1 %in% c(dce,czc)){
      return(calendar[calendar>=as.integer(firstDate1)][10])
    }else if(product1 %in% c('TC','ZC')){
      return(calendar[calendar>=as.integer(firstDate1)][5])
    }else if(product1 == 'JD'){
      if(date1>=201703){
        lastDate1 <- paste0(date1,'31')
        return(tail(calendar[calendar<=as.integer(lastDate1)],4)[1])
      }else{
        return(calendar[calendar>=as.integer(firstDate1)][10])
      }
    }else if(product1 == 'FU'){
      return(tail(calendar[calendar < as.integer(firstDate1)], 1))
    }else if(product1 == 'WT'){
      return(tail(calendar[calendar < as.integer(firstDate1)], 7)[1])
    }else{
      firstDate1.Date <- as.Date(firstDate1, '%Y%m%d')
      if(product1 %in% indexFutures){
        tmp <- timeNthNdayInMonth(firstDate1.Date, nday = 5, nth = 3, format = '%Y-%m-%d')
        return(dt_int_date(tmp))
      }else if(product1 %in% bondFutures){
        tmp <- timeNthNdayInMonth(firstDate1.Date, nday = 5, nth = 2, format = '%Y-%m-%d')
        return(dt_int_date(tmp))
      }else{
        return(NA)
      }
    }
  },USE.NAMES = FALSE)
  return(unlist(expiryDate))
}

