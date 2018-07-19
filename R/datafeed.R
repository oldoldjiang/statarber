#' fread version to read .csv.bz2
#'
#' @param file
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fread.bz2 <- function(file,...){
  fread(sprintf("bzcat %s | tr -d '\\000'", file), integer64 = 'numeric', ...)
}

queryStockWindTick <- function(dateRange, stockCodes, cols = c('iVolume','nPrice'),
                               returnDataTable = TRUE, verbose = TRUE){
  cleanStockCodes <- substr(gsub('\\D','',stockCodes), 1, 6)
  exchanges <- getExchangeStock(cleanStockCodes)
  root.dir <- ''
  headers <- c('chWindCode','chCode', 'nDate', 'iVolume', 'iTurover', 'nMatchItems',
               'nInterest', 'chTradeFlag', 'chBSFlag', 'iAccVolume', 'iAccTurover', 'nHigh',
               'nLow', 'nPrice', 'nOpen', 'nPreClose', paste0('nAskPrice_',0:9), paste0('nAskVolume_',0:9),
               paste0('nBidPrice_',0:9),paste0('nBidVolume_',0:9), 'nAskAvPrice','nBidAvPrice',
               'iTotalAskVolume','iTotalBidVolume')
  if(!is.null(cols)){
    if(any(!cols %in% headers)) stop(paste('cols should be NULL or some of',paste(headers, collapse = ',')))
    colss <- union(cols, c('chWindCode', 'nDate','nTime'))
    idx <- which(headers %in% cols)
    headers <- headers[idx]
  }else{
    idx <- 1:length(headers)
  }

  res <- rbindlist(lapply(1:length(exchanges), function(i){
    path <- file.path(root.dir,exchanges[i])
    rbindlist(lapply(dateRange, function(d){
      file <- file.path(path, d, paste0(cleanStockCodes[i],'.csv.bz2'))
      if(verbose == TRUE) print(paste('reading', file))
      df <- fread.bz2(file)
      df[,idx,with = FALSE]
    }))
  }))

  colnames(res) <- headers
  res <- res[nTime>93000000]
  if('chCode' %in% headers){
    res[, chCode:=as.character(chCode)]
    res[, chCode:=code_stock_pad(chCode), by = chCode]###########
  }
  reorderHeader <- c('nDate','nTime',headers[!headers %in% c('nDate','nTime')])
  res <- res[,reorderHeader,with=FALSE]
  setorderv(res, c('nDate','nTime'))
  if(!returnDataTable){
    res <- setDF(res)
  }
  res
}
