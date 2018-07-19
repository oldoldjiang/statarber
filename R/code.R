name2value <- function(string = 'fwd.Ret.DAILY.5'){
  str.part <- strsplit(string,'[.]')[[1]]
  if(str.part[1] == 'fwd'){
    n <- as.integer(str.part[4])
  }else if(str.part[1] == 'bwd'){
    n <- -as.integer(str.part[4])
  }else{
    stop('unsupported format')
  }
  return(list(y      = str.part[2],
              freq   = str.part[3],
              n      = n))
}
value2name <- function(y = 'Ret', freq = 'DAILY', n = 5){
  x <- ifelse(n>=0, 'fwd', 'bwd')
  paste(x, y, freq, abs(n), sep = '.')
}

#' append exchange to stock code
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
#' code_stock_add_exchange(c('302309.SZ','sh009234','602312'))
code_stock_add_exchange <- function(code){
  num <- gsub('\\D+','',code)
  stopifnot(all(nchar(num)==6))
  out <- rep(NA,length(code))
  is_sh <- grepl('^6',num)
  is_sz <- grepl('^0|^3',num)
  out[is_sz] <- paste0(num[is_sz],'.SZ')
  out[is_sh] <- paste0(num[is_sh],'.SH')
  out

}
#' pad stock code
#' @description when reading from csv, stock code may coerece to number,
#' use code_pad to get code
#'
#' @param code
#'
#' @return
#' @export
#' @importFrom stringi stri_pad_left
#' @examples
#' code_stock_pad(c('403','2','43','000403'))
code_stock_pad <- function(code){
  stri_pad_left(code,width = 6, pad = 0)
}

#' pad future code for CZC
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
#' code_future_pad(c('CF709.CZC'))
code_future_pad <- function(code){
  num <- gsub('\\D+','',code)
  num <- stri_pad_left(num,width = 4,pad = 1)
  gsub('\\d+',num,code)
}
#' extract exchange from code
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
#' code_get_exchange(c('IC1709.CFE','600446.SH'))
code_get_exchange <- function(code){
  gsub('.{1,6}[.]','',code)
}
#' get product from code
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
#' code_future_get_product(c('IF1709','CF709.ZCZ','i1709'))
code_future_get_product <- function(code){
  remove_exchange <- gsub('[.].+','',code)
  product <- gsub('\\d+','',remove_exchange)
  product[nchar(product)==0] <- NA
  product
}
#' change futures product/contract case
#' @description
#' different exchagne may have different code case, like IF1709 and i1709
#' this function change the case of product or contract
#' @param code
#'
#' @return
#' @export
#' @examples
#' code_future_case(c('IF1709','I1709','cf709.CZC'))
code_future_case <- function(code){
  with_exchange <- grepl('[.]',code)
  exchange <- gsub('.*[.]','',code)
  contract <- gsub('[.]\\w{2,3}','',code)
  product <- gsub('\\d{3,4}','',contract)
  upper_case <- toupper(product) %in% c('IF','IC','IH','TF','T',
                                        'WT','PM','ZC','CF','FG','SR','TC',
                                        'TA','WH','RI','RM','RS','MA','OI',
                                        'ME')
  contract[upper_case] <- toupper(contract[upper_case])
  contract[!upper_case] <- tolower(contract[!upper_case])
  contract[with_exchange] <- paste0(contract[with_exchange],'.',exchange[with_exchange])
  contract
}

getExchangeStock <- function(codes, tolower = FALSE){
  exchange <- rep(NA, length(codes))
  cleanCodes <- substr(gsub('\\D','',codes), 1, 6)
  exchange[grepl('^[0|3]', cleanCodes)] <- 'SZ'
  exchange[grepl('^[6]', cleanCodes)] <- 'SH'
  if(tolower == TRUE){
    exchange <- tolower(exchange)
  }
  exchange
}
