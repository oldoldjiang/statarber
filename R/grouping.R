## sampling data is convenient to analyse sub group

## TODO list:
## 1. batch mode
## 2. return has NA???

group.by.all  <- function(data){
  res <- list(data)
  names(res) <- 'ALL'
  return(res)
}
#' grouping functions
#'
#' @param data a data.frame
#' @param by
#' @param group.name character vector, subset of `sby` column
#'
#' @return
#' @export
#'
#' @examples
#' library(data.table)
#' df <- data.table(stock=c(letters), forward_return = runif(26), date = 20180101, time = '15:00:00.000',industrygroup = c('xxx','ooo'), stringsAsFactors = FALSE)
#' group.by.instrument(df,by = 'stock')
#' group.by.flag(df, by = 'industrygroup')
#' group.by.flag(df, by = 'industrygroup', group.name = 'ooo')
group.by.flag  <- function(data, by, group.name = NULL, keep.by = TRUE){
  if(is.null(by)) stop('`by` should be a character')
  if(!inherits(data, "data.table") && inherits(data, "data.frame")) setDT(data) # convert to data.table to use split function
  if(inherits(data, "data.table")){
    stopifnot(by %in% colnames(data))
    res <- split(data,by = by, keep.by = keep.by)
    if(!is.null(group.name)){
      res <- res[group.name]
    }
    return(res)
  }else{
    stop('only data.table is supportted in groupping')
  }
}

sampling.quantile <- function(data, range, var){
  nms <- names(data)
  range <- sort(range)
  if(length(range)!=2) stop('range should be a length-2 numeric vector')
  data <- lapply(names(data), function(x){
    res <- data[get(var)>=range[1] & get(var)<=range[2]]
    return(res)
  })
  names(data) <- nms
  data
}
#' wapper function for grouping function
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
group.factory <- function(data,
                           by = NULL,
                           group.type = 'ALL',
                           group.name = NULL){
  if(group.type == 'ALL'){
    res <- group.by.all(data)
  }else if(group.type == 'EACH'){
    by = null.replace(by,DNAMES$SYMBOL)
    res <- group.by.flag(data,by = by, group.name = group.name)
  }else{
    res <- group.by.flag(data, by = by, group.name = group.name, keep.by = FALSE)
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
#' alpha.dir <- path.helper(dtype = 'alpha', ver = 'alphaHL')
#' ret.dir <- path.helper(dtype = 'ret')
#' r1 <- sampling.everyday(20170502, alpha.dir, ret.dir)
sampling.everyday <- function(date, alpha.dir, ret.dir, univ.dir = NULL, ind.dir = NULL,
                              univ.set = NULL, time.set = NULL,
                           group.type = 'ALL',
                           group.name = NULL,
                           group.by = NULL){

  data <- get.mergeddata(dates = date, alpha.dir, ret.dir, univ.dir, ind.dir, univ.set, time.set)
  res <- group.factory(data,
                        group.type = group.type,
                        group.name = group.name,
                        by = group.by)
  return(res)

}

#' convert to one clean data.frame
#'
#' @param dates
#' @param alpha.dir
#' @param ret.dir
#' @param univ.dir
#' @param ind.dir
#'
#' @return
#' @export
#'
#' @examples
#' df <- .get.mergeddata(20170502, alpha.dir = path.helper(dtype = 'alpha',ver = 'alphaHL'),
#'                                   ret.dir = path.helper(dtype = 'ret'))
get.mergeddata <- function(dates, alpha.dir, ret.dir, univ.dir = NULL, ind.dir = NULL,
                           univ.set = NULL, time.set = NULL){
  alpha  <- data.read(dates, alpha.dir, des.format = 'data.table')
  if(!is.null(univ.set)){
    alpha <- alpha[get(DNAMES$SYMBOL) %in% univ.set]
  }
  if(!is.null(time.set)){
    alpha <- alpha[get(DNAMES$TIME) %in% time.set]
  }
  ret    <- data.read(dates, ret.dir  , des.format = 'data.table')
  data   <- merge(alpha,ret, all.x = TRUE)
  if(!is.null(univ.dir)){
    univ <- data.read(dates, univ.dir, des.format = 'data.table')
    data <- filte.univ(data, univ)
  }
  data
}

#' wrapper function for sampling
#'
#' @param dates
#' @param alpha.dir
#' @param ret.dir
#' @param univ.dir
#' @param group.by
#' @param quantile.range
#' @param cache.dir
#' @param error.tolerant
#' @param cores
#' @param verbose
#' @param group.type
#' @param group.name
#' @param quantile.var
#'
#' @return
#' @export
#'
#' @examples
#' alpha.dir <- path.helper(dtype = 'alpha', ver = 'alphaHL')
#' ret.dir <- path.helper(dtype = 'ret')
#' sampling.multiday(c(20170502,20170503), alpha.dir, ret.dir, cache.dir = '~/')
sampling.multiday <- function(dates,
                                alpha.dir,
                                ret.dir,
                                univ.dir = NULL,
                                univ.set = NULL, # only take instrument that is in this set, more convinient than univ.dir
                                time.set = NULL, # only focus part of the data
                                group.type = 'ALL',
                                group.name = NULL,
                                group.by = NULL,
                                quantile.range = c(0,1),
                                quantile.var = NULL,
                                cache.dir = NULL,
                                error.tolerant = FALSE,
                                cores = 1,
                                verbose = TRUE){
  if(!is.null(cache.dir)){
    ply.func <- l_ply # no return, save to disk instead
    dir.create(cache.dir, FALSE, TRUE)
  }else{
    ply.func <- llply
  }
  res <- ply.func(dates, function(day){

    out.file <- file.path(cache.dir, paste0('RAW.',day,'.rds'))
    if(!is.null(cache.dir) && file.exists(out.file)){ ## lazy evaluation
      sampled <- sample.read(out.file)
    }else{
      sampled <- sampling.everyday(day,
                                alpha.dir, ret.dir, univ.dir, univ.set, time.set,
                                group.type = group.type,
                                group.name = group.name,
                                group.by   = group.by)
      if(!is.null(quantile.range) && !is.null(quantile.var)){
        if(!(identical(sort(quantile.range), c(0, 1)))){
          sampled <- sample.quantile(sampled,
                                     range = quantile.range,
                                     var   = quantile.var)
        }
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

sample.save <- function(data, file, overwrite = FALSE, verbose = TRUE){
  if(verbose) print(paste('saving sample file:', file))
  dir.create(dirname(file), FALSE, TRUE)
  if(file.exists(file) & overwrite == FALSE){
    stop('sampled data exits, set overwrite = TRUE')
  }else{
    saveRDS(data, file)
  }

}
sample.read <- function(file, ignore.empty.file = FALSE, verbose = TRUE){
  if(verbose) print(paste('reading sample file:', file))
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
