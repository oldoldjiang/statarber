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

#' generate pathes based rule
#'
#' @param dtype
#' @param mkt
#' @param freq
#' @param ver
#' @param root.dir
#' @param path.check logical
#'
#' @return
#' @export
#' @importFrom data.table CJ
#'
#' @examples
#' path.helper(freq = c('DAILY','M1'),dtype = c('md','fdd'),path.check = FALSE)
path.helper <- function(dtype = 'md',mkt = 'CHINA_STOCK',freq = 'DAILY',
                        ver = 'test', root.dir = '~/data', path.check = TRUE){
    comb  <- CJ(root.dir, dtype, mkt, freq, ver)
    valid <- lapply(1:nrow(comb), function(i){
      p <- paste(comb[i,], collapse = '/')
      if(path.check && !file.exists(p)){
        print(paste(p,'not exists'))
        return(NULL)
      }else{
        return(p)
      }
    })
    valid <- unlist(valid[!unlist(lapply(valid,is.null))])
    return(valid)
}

endswith <- function(str, matchstr){
  substr(str, nchar(str)-nchar(matchstr)+1, nchar(str)) == matchstr
}
startswith <- function(str, matchstr){
  substr(str, 1, nchar(str)) == matchstr
}

