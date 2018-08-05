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
#' @param names2keep character vector, subset of `by` column
#'
#' @return
#' @export
#'
#' @examples
#' library(data.table)
#' df <- data.table(stock=c(letters), forward_return = runif(26), date = 20180101, time = '15:00:00.000',industrygroup = c('xxx','ooo'), stringsAsFactors = FALSE)
#' group.by.instrument(df,by = 'stock')
#' group.by.flag(df, by = 'industrygroup')
#' group.by.flag(df, by = 'industrygroup', names2keep = 'ooo')
group.by.flag  <- function(data, by, names2keep = NULL, keep.by = TRUE){
  if(is.null(by)) stop('`by` should be a character')
  if(!inherits(data, "data.table") && inherits(data, "data.frame")) setDT(data) # convert to data.table to use split function
  if(inherits(data, "data.table")){
    stopifnot(by %in% colnames(data))
    res <- split(data,by = by, keep.by = keep.by)
    if(!is.null(names2keep)){
      res <- res[names2keep]
    }
    return(res)
  }else{
    stop('only data.table is supportted in groupping')
  }
}

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
                           grouptype = 'ALL',
                           names2keep = NULL){
  if(grouptype == 'ALL'){
    res <- group.by.all(data)
  }else if(grouptype == 'EACH'){
    by = null.replace(by,DNAMES$SYMBOL)
    res <- group.by.flag(data,by = by, names2keep = names2keep)
  }else{
    res <- group.by.flag(data, by = by, names2keep = names2keep, keep.by = FALSE)
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
                           grouptype = 'ALL',
                           names2keep = NULL,
                           by = NULL){

  data <- get.mergeddata(dates = date, alpha.dir, ret.dir, univ.dir, ind.dir)
  res <- group.factory(data,
                        grouptype = grouptype,
                        names2keep = names2keep,
                        by = by)
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
get.mergeddata <- function(dates, alpha.dir, ret.dir, univ.dir = NULL, ind.dir = NULL){
  alpha  <- data.read(dates, alpha.dir, des.format = 'data.table')
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
#' @param by
#' @param quantile.range
#' @param cache.dir
#' @param error.tolerant
#' @param cores
#' @param verbose
#' @param grouptype
#' @param name2keep
#' @param fwd.var
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
                                group.type = 'ALL',
                                group.name = NULL,
                                by = NULL,
                                quantile.range = c(0,1),
                                fwd.var = NULL,
                                cache.dir = NULL,
                                error.tolerant = FALSE,
                                cores = 1,
                                verbose = TRUE){
  ply.func <- ifelse(is.null(cache.dir), llply, l_ply)
  res <- ply.func(dates, function(day){

    out.file <- file.path(cache.dir, paste0('RAW.',day,'.rds'))
    if(!is.null(cache.dir) && file.exists(out.file)){ ## lazy evaluation
      sampled <- sample.read(out.file)
    }else{
      sampled <- sampling.everyday(day,
                                alpha.dir, ret.dir, univ.dir,
                                grouptype = group.type,names2keep = group.name,by = by)
      if(!(identical(quantile.range, c(0, 1)) ||
         identical(quantile.range, c(1, 0)))){
        sampled <- sample.quantile(sampled,
                                   range = quantile.range,
                                   fwd.var = fwd.var)
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
