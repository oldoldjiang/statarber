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
extract.product.future <- function(codes){
  remove_exchange <- gsub('[.].+','',codes)
  product <- gsub('\\d+','',remove_exchange)
  product[nchar(product)==0] <- NA
  product
}

PRODUCTS <- yaml.load_file('inst/products.yml')
PRODUCTS <- rbindlist(lapply(PRODUCTS, function(r) as.data.table(r)),fill = TRUE)

get.exchange.future <- function(instrument_id){
  product <- extract.product.future(instrument_id)
  product <- data.table(product_id = product)
  # merge function will reorder the row
  df <- join(product, PRODUCTS[,c('product_id','exchange'),with = FALSE],by = 'product_id')
  return(df[['exchange']])
}
get.multiplier.future <- function(product_id){
  if(any(nchar(product_id)>2)) stop('Invalid product_id')
  product <- data.table(product_id = product_id)
  df <- join(product, PRODUCTS[,c('product_id','volume_multiple'),with = FALSE],by = 'product_id')
  return(df[['volume_multiple']])
}
