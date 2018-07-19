null.replace <- function(var, replace){
  ifelse(is.null(var), replace, var)
}
head.na <- function(x){
  xx <- x[!is.na(x)]
  if(length(xx)==0L){as(NA,Class = class(x))}else{head(xx,1)}
}
tail.na <- function(x){
  xx <- x[!is.na(x)]
  if(length(xx)==0L){as(NA,Class = class(x))}else{tail(xx,1)}
}
max.na <- function(x){
  if(all(is.na(x))){
    return(NA)
  }else{
    return(max(x, na.rm = TRUE))
  }
}
min.na <- function(x){
  if(all(is.na(x))){
    return(NA)
  }else{
    return(min(x, na.rm = TRUE))
  }
}
get.exchange.stock <- function(codes){
  exchange <- vector(mode = 'character', length = length(codes))
  exchange[grepl('^[0|3]',codes)] <- 'SZ'
  exchange[grepl('^[6]',codes)] <- 'SH'
  exchange
}

path.helper <- function(dtype = 'md',mkt = 'CHINA_STOCK',freq = 'DAILY',
                        ver = 'tushare', root.dir = '~/ics', path.check = TRUE){
    comb  <- CJ(dtype, mkt, freq, ver)
    valid <- lapply(1:nrow(comb), function(i){
      p <- paste(comb[i,], collapse = '/')
      p <- file.path(root.dir, p)
      if(path.check && !file.exists(p)){
        return(NULL)
      }else{
        return(p)
      }
    })
    valid <- valid[!unlist(lapply(valid,is.null))]
    return(valid)
}
