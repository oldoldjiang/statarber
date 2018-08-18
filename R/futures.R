is.realcontract <- function(contracts){
  c1 <- grepl('^\\w{1,2}\\d{3,4}[.]\\w{3,6}', contracts) ## remove simulated contract
  c2 <- !grepl('^IM|^TM', contracts) ## remove simulated contract of CFE like IM1709
  return(c1 & c2)
}
#' extract product name from futures codes
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
#' extract.product(c('IF1705','CS709'))
extract.product <- function(codes){
  remove_exchange <- gsub('[.].+','',codes)
  product <- gsub('\\d+','',remove_exchange)
  product[nchar(product)==0] <- NA
  product
}
