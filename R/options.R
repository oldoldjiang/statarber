extract.product.option <- function(codes){
  codes <- gsub('-','',codes)
  product <-gsub('(\\w{1,2})(\\d{3,4})([P|C])(\\d+)','\\1',codes)
  product[nchar(product)==0] <- NA
  product
}
extract.expiryYM.option <- function(codes,add1 = TRUE){
  codes <- gsub('-','',codes)
  expiryYM <- gsub('(\\w{1,2})(\\d{3,4})([P|C])(\\d+)','\\2',codes)
  expiryYM[nchar(expiryYM)==0] <- NA
  if(add1){
    expiryYM <- ifelse(nchar(expiryYM)==3, paste0('1',expiryYM),expiryYM)
  }
  expiryYM
}
extract.putcall.option <- function(codes){
  codes <- gsub('-','',codes)
  putcall <- gsub('(\\w{1,2})(\\d{3,4})([P|C])(\\d+)','\\3',codes)
  putcall[nchar(putcall)==0] <- NA
  putcall
}
extract.strike.option <- function(codes){
  codes <- gsub('-','',codes)
  strike <- gsub('(\\w{1,2})(\\d{3,4})([P|C])(\\d+)','\\4',codes)
  strike[nchar(strike)==0] <- NA
  strike <- as.numeric(strike)
  strike
}

# currently only future options are available
get.exchange.option <- function(codes){
  product <- extract.product.option(codes)
  get.exchange.future(product)
}
