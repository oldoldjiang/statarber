## sampling data is convenient to analyse sub group
## sampling is different from groupping because sampling may only take a subset

## TODO list:
## 1. batch mode
## 2. return has NA???
group.by.all  <- function(data){
  return(list(data))
}
group.by.stock <- function(data, by){
  if(is.null(by)){
    warning('by is NULL, try to use default stock colname')
    if('K' %in% colnames(data)) by = 'K'
  }
  if(inherits(data, "data.table")){
    res <- split(data, by = by)
    return(res)
  }else{
    stop('only data.table is supportted')
  }

}
group.by.flag  <- function(data, by, keep.name = NULL){
  if(is.null(by)) stop('by should be a character in group.by.flag()')
  if(inherits(x, "data.table")){
    res <- split(data,by = by, keep.by = FALSE)
    if(!is.null(keep.name)){
      res <- res[keep.name]
    }
    return(res)
  }else{
    stop('only data.table is supportted in groupping')
  }
}
#' only take
#'
#' @param data
#' @param range
#' @param fwd.var
#'
#' @return
#' @export
#'
#' @examples
sample.quantile <- function(data, range, fwd.var){
  nms <- names(data)
  range <- sort(range)
  if(length(range)!=2) stop('range should be a length-2 numeric vector')
  data <- lapply(names(data), function(x){
    res <- data[get(fwd.var)>=range[1] & get(fwd.var)<=range[2]]
    return(res)
  })
  names(data) <- nms
  data
}
#' wapper function for sampling function
#'
#' @param data
#' @param by         character, one of colnames of data
#' @param group.type character, 'ALL' 'EACH' or others
#' @param group.name character vector, do subsetting
#' @param demean
#'
#' @return
#' @export
#'
#' @examples
sample.factory <- function(data,
                           by = NULL,
                           group.type = 'ALL',
                           group.name = NULL){
  if(group.type == 'ALL'){
    res <- group.by.all(data)
  }else if(group.type == 'EACH'){
    res <- group.by.stock(data,by = by)
  }else{
    res <- group.by.flag(data, by = by, keep.by = group.name)
  }
  res
}
#' sampling for each day
#' @description read alphas as x and read forward return as y, do grouping and take subset
#' @param day
#'
#' @return
#' @export
#'
#' @examples
sample.gen.day <- function(day,
                           alpha.dir,
                           ret.dir,
                           univ.dir = NULL,
                           group.type = 'ALL',
                           group.name = NULL,
                           by = NULL){
  alpha  <- readicsdata(day, alpha.dir, des.format = 'data.table')
  ret    <- readicsdata(day, ret.dir  , des.format = 'data.table')
  data   <- merge(alpha,ret, all.x = TRUE)

  if(!is.null(univ.dir)){
    univ <- readicsdata(day, univ.dir, des.format = 'data.table')
    data <- filte.univ(data, univ)
  }
  if(group.type == 'INDUSTRY'){

  }
  res <- sample.factory(data,
                        group.type = group.type,
                        group.name = group.name,
                        by = by)

}
#' wrapper function for sampling
#'
#' @param dates
#' @param alpha.dir
#' @param ret.dir
#' @param univ.dir
#' @param group.type
#' @param group.name
#' @param by
#' @param quantile.range
#' @param cache.dir
#' @param use.cache logical, if TRUE, will read cache if exists
#' @param error.tolerant
#' @param cores
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
sample.gen.multiday <- function(dates,
                                alpha.dir,
                                ret.dir,
                                univ.dir = NULL,
                                group.type = 'ALL',
                                group.name = NULL,
                                by = NULL,
                                quantile.range = c(0,1),
                                cache.dir = NULL,
                                use.cache = FALSE,
                                error.tolerant = FALSE,
                                cores = 1,
                                verbose = TRUE){
  ply.func <- ifelse(is.null(cache.dir), llply, l_ply)
  res <- ply.func(dates, function(day){

    out.file <- file.path(cache.dir, paste0('RAW.',day,'.rds'))
    if(use.cache && file.exists(out.file)){ ## lazy evaluation
      sampled <- sample.read(out.file)
    }else{
      sampled <- sample.gen.day(day,
                                alpha.dir, ret.dir, univ.dir,
                                group.type,group.name,by)
      if(!(identical(quantile.range, c(0, 1)) ||
         identical(quantile.range, c(1, 0)))){
        sampled <- sample.quantile(sampled,
                                   range = quantile.range,
                                   fwd.var = )
      }

      if(!is.null(sampled)){
        if(!is.null(cache.dir)){
          if(verbose == TRUE){
            print(paste('Writing ', out.file))
          }
          sample.save(sampled, out.file)
          return(NULL)
        }else{
          return(sampled)
        }
      }else if(error.tolerant == FALSE){
        stop(paste('sampling error on',d))
      }
    }
  }, .parallel = cores > 1)

  if(!is.null(res)){ ## llply
    if(length(res)==1){
      names <- names(res[[1]])
    }else{
      names <- Reduce(union,lapply(res, names))
    }

    res <- lapply(names,function(g){
      rbindlist(lapply(res, function(d) d[[g]]))
    })
    names(res) <- names
  }
  return(res)

  ## TODO batch mode save
}

sample.save <- function(data, file, overwrite = FALSE){
  dir.create(dirname(file), FALSE, TRUE)
  if(file.exists(file) & overwrite == FALSE){
    stop('sampled data exits, set overwrite = TRUE')
  }else{
    saveRDS(data, file)
  }

}
sample.read <- function(file, ignore.empty.file = FALSE){
  if(!file.exists(file)){
    if(ignore.empty.file == TRUE){
      return(NULL)
    }else{
      stop('sampled data does not exits')
    }
  }else{
    readRDS(file)
  }

}
