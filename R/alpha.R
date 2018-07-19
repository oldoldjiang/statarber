filte.univ <- function(data, univ, by = NULL){

  if(is.array(univ)){
    data <- data[get(SYMBOL) %chin% univ]
  }else if(inherits(univ, 'data.table')){
    if(is.null(by)){
      if(all(c('K','D','T') %in% colnames(data) &
             c('K','D','T') %in% colnames(univ))){
        by <- c('K', 'D', 'T')
      }else{
        by <- c(SYMBOL, DATE, TIME)
      }
    }
    univ.name <- colnames(univ)[!colnames(univ) %in% by]
    data <- merge(data, univ, by = by)
    data <- data[get(univ.name)==1,][,-which(univ.name == colnames(data)),with = FALSE]

  }
}

winsorize <- function(x, p = 0.05){
  thres <- quantile(x,probs = c(p,1-p), na.rm = TRUE)
  x[x<thres[1]] <- thres[1]
  x[x>thres[2]] <- thres[2]
  x
}
