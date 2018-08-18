
#' read n-dim array from h5 file
#'
#' @param file
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#' a <- panel.read('~/data/md/CHINA_STOCK/DAILY/test/20130403.h5')
panel.read <- function(file, verbose = TRUE, checkname = TRUE){
  if(verbose) print(paste('Reading:',file))
  f <- h5file(file,'r')
  names <- h5attr(f, 'names')
  dimnames <- list()
  for(i in seq_len(length(names))){
    dimnames[[names[i]]] <- readDataSet(f[[paste0('dimnames_',i)]])
  }
  if(checkname){
    if(basename(dirname(file))!=dimnames[[length(names)]]) warning(paste('data in',file,'is not the same as the folder'))
  }
  # for(i in seq_len(length(dim))){
  #   dimnames[[names[i]]] <- h5attr(f,paste0('dimnames_',i))
  # }
  a <- readDataSet(f[['ndarray']])
  dim(a) <- f[['ndarray']]$dims
  dimnames(a) <- dimnames
  h5close(f)
  return(a)
}

#' write n-dim array to h5 file
#'
#' @param data
#' @param file
#' @param key
#' @param dircreate
#' @param verbose
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
#' a <- array(1:12,dim = c(1,4,1,3), dimnames = list(S = 'a', D = 20180103:20180106,T='15:00:00.000',C=c('x','y','z')))
#' panel.write(a, '~/test.h5')
panel.write <- function(data, file, key = 'ndarray', dircreate = FALSE, verbose = TRUE, overwrite = FALSE){
  mode <- ifelse(overwrite, 'w', 'w-')
  if(dircreate == TRUE) dir.create(dirname(file), FALSE, TRUE)
  if(verbose) print(paste('Writing:', file))
  if(file.exists(file) && overwrite == FALSE) stop('Error: file exist, please set overwrite = TRUE')

  #if(mode == 'w' && file.exists(file)) file.remove(file) ## I don't known why 'w' break down, fix this issue

  f <- h5file(file, mode)
  f[[key]] <- data
  h5attr(f, 'names') <- names(dimnames(data))

  for(i in seq_len(length(dim(data)))){
   f[[paste0('dimnames_',i)]] <- dimnames(data)[[i]]
  }
  # for(i in seq_len(length(dim(data)))){
  #   h5attr(f,paste0('dimnames_',i)) <- dimnames(data)[[i]]
  # }

  h5close(f)
  invisible(TRUE)
}

#' add 1 dim without add more data
#'
#' @param panel
#' @param label
#' @param dnames
#'
#' @return
#' @export
#'
#' @examples
#' a <- array(1:12,dim = c(1,4,1,3), dimnames = list(S = 'a', D = 20180103:20180106,T='15:00:00.000',C=c('x','y','z')))
#' a <- panel.add.dim(a, label = 'O', dnames = 'G1')
#' dim(a)
#' dimnames(a)
panel.add.dim <- function(panel, label, dnames){
  stopifnot(length(label)==1)
  stopifnot(length(dnames)>0)
  dnames.extend <- dimnames(panel)
  dim(panel) <- c(dim(panel), length(dnames))
  dnames.extend[[label]] <- dnames ## append at the end
  dimnames(panel) <- dnames.extend
  panel
}

#' combine multi n-dim array into 1 n-dim array
#'
#' @param panel.list a list of n-dim array
#' @param default
#'
#' @return
#' @export
#'
#' @examples
#' a1 <- array(1:12,dim = c(1,4,1,3), dimnames = list(S = 'a', D = 20180103:20180106,T='15:00:00.000',C=c('x','y','z')))
#' a2 <- array(13:24,dim = c(1,4,1,3), dimnames = list(S = 'b', D = 20180103:20180106,T='15:00:00.000',C=c('x','y','z')))
#' b <- panel.combine(list(a1,a2))
#' dim(b)
#' dimnames(b)
panel.combine <- function(panel.list, default = NA){
  if(!is.list(panel.list)){
    stop('should pass a list of panel into this functions')
  }
  panel.list <- panel.list[!unlist(lapply(panel.list, is.null))]

  if(length(panel.list)==1){ ## to avoid error in Reduce when only given x
    return(panel.list[[1]])
  }else{
    names <- Reduce(function(x,y) union(names(dimnames(x)), names(dimnames(y))), panel.list)
    dnames <- lapply(names, function(x){
      Reduce(union,lapply(panel.list,function(panel) dimnames(panel)[[x]]))
    })
    names(dnames) <- names
    out <- array(default, dim = sapply(dnames, length),
                 dimnames = dnames)
    for(panel in panel.list){
      if(length(dimnames(panel))==4){ # faster for 4-dim, which is the most common situation
        out[dimnames(panel)[[1]],
              dimnames(panel)[[2]],
              dimnames(panel)[[3]],
              dimnames(panel)[[4]]] <- panel
      }else{
        idx <- as.matrix(do.call(CJ, dimnames(panel)))
        out[idx] <- panel[idx]
      }
    }

    return(out)
  }

}

#' comvert n-dim array to data.frame by specific rule
#' @description
#' @param panel
#'
#' @return
#' @export
#'
#' @examples
#' a <- array(1:12,dim = c(1,4,1,3), dimnames = list(S = 'a', D = 20180103:20180106,T='15:00:00.000',C=c('x','y','z')))
#' df <- panel2df(a)
panel2df <- function(panel){
  dim <- dim(panel)
  dim.len <- length(dim)
  dnames <- dimnames(panel)
  dim(panel) <- c(prod(dim[-dim.len]),tail(dim,1))
  panel <- as.data.table(panel)
  setnames(panel,1:ncol(panel),dnames[[dim.len]])
  nrow <- nrow(panel)
  for(i in seq_len(dim.len-1)){
    panel[,(names(dnames)[i]):=rep(dnames[[i]],
                                      length.out = nrow,
                                      each  = ifelse(i==1,1,prod(dim[1:(i-1)])))]
  }
  panel[,c(names(dnames)[-dim.len], dnames[[dim.len]]), with = FALSE]
}

#' main API for reading clean data
#'
#' @param dates
#' @param pathes
#' @param src.format
#' @param des.format
#' @param verbose
#' @param missing.allow
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' pathes <- path.helper(dtype = c('md','ret'),ver = 'test')
#' a <- data.read(20170502,pathes)
data.read <- function(dates, pathes,
                        src.format = c('panel.h5','data.frame.h5','rds','csv'),
                        des.format = c('panel','data.table'),
                        verbose = TRUE,
                        missing.allow = FALSE,
                        ...){
  src.format <- match.arg(src.format)
  des.format <- match.arg(des.format)

  ext.name <- switch(src.format,
                        panel.h5 = 'h5',
                        data.table.h5 = 'h5',
                        rds = 'rds',
                        csv = 'csv')
  format.pair <- paste0(src.format,'.',des.format)
  read.func <- switch(format.pair,
                        panel.h5.panel = panel.read,
                        panel.h5.data.table = function(file, verbose = TRUE){
                          panel <- panel.read(file, verbose)
                          panel2df(panel)},
                        data.table.h5.data.table = stop,
                        rds.data.table = readRDS,
                        csv.data.table = fread)
  if(des.format=='panel' && src.format != 'panel.h5') stop('source format should be panel.h5')

  if(des.format == 'panel'){

    files <- paste0(c(outer(pathes, dates, file.path)),'.h5')
    panel.combine(lapply(files, function(f){
      if(file.exists(f)){
        panel.read(f, verbose = verbose)
      }else if(missing.allow){
        NULL
      }else{
        stop(paste(f,'not found'))
      }

    }))
  }else if(des.format == 'data.table'){
    mergeWrap <- function(x,y){
      merge(x,y,all = TRUE)
    }
    rbindlist(lapply(dates, function(d){
      Reduce(mergeWrap,lapply(pathes,function(p){ # merge by DNAMES$SYMBOL, DNAMES$DATE, DNAMES$TIME
        file <- file.path(p, paste0(d,'.',ext.name))
        tmp <- read.func(file,...)
      }))

    }))
  }else{
    stop('unsupported destination format')
  }
}

panel.write.daily <- function(data, file, key = 'ndarray',
                              dircreate = FALSE,
                              verbose = TRUE,
                              overwrite = FALSE,
                              na.rm = TRUE){
  stopifnot(!is.null(dimnames(data)$D)) ## have date dim
  for(d in dimnames(data)$D){
    subdata <- data[,d,,,drop = FALSE]
    if(na.rm){
      flag <- aaply(subdata, 1, function(m){
        ifelse(all(is.na(m)), TRUE, FALSE)
      })
      flag <- c(flag)
      subdata <- subdata[!flag,,,, drop = FALSE]
    }
    panel.write(subdata, file = file, key = key, dircreate = dircreate,
                verbose = verbose, overwrite = overwrite)
  }
  return(TRUE)
}
