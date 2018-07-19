isRealContract <- function(contracts){
  c1 <- grepl('^\\w{1,2}\\d{3,4}[.]\\w{3,6}', contracts) ## remove simulated contract
  c2 <- !grepl('^IM|^TM', contracts) ## remove simulated contract of CFE like IM1709
  return(c1 & c2)
}
getProduct <- function(code){
  remove_exchange <- gsub('[.].+','',code)
  product <- gsub('\\d+','',remove_exchange)
  product[nchar(product)==0] <- NA
  product
}
