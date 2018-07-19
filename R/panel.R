
panel.read <- function(file, verbose = TRUE){
  if(verbose) print(paste('Reading:',file))
  f <- h5file(file,'r')
  dim <- h5attr(f,'dim')
  names <- h5attr(f, 'names')
  dimnames <- list()
  for(i in seq_len(length(dim))){
    dimnames[[names[i]]] <- f[paste0('dimnames_',i)][]
  }

  # for(i in seq_len(length(dim))){
  #   dimnames[[names[i]]] <- h5attr(f,paste0('dimnames_',i))
  # }
  a <- f['ndarray'][]
  dimnames(a) <- dimnames
  h5close(f)
  return(a)
}

panel.write <- function(data, file, key = 'ndarray', dircreate = FALSE, verbose = TRUE, overwrite = FALSE){
  mode <- ifelse(overwrite, 'w', 'w-')
  if(dircreate == TRUE) dir.create(dirname(file), FALSE, TRUE)
  if(verbose) print(paste('Writing:', file))
  if(file.exists(file) && overwrite == FALSE) stop('Error: file exist, please set overwrite = TRUE')

  f <- h5file(file, mode)
  f[key] <- data
  h5attr(f, 'dim') <- dim(data)
  h5attr(f, 'names') <- names(dimnames(data))

  for(i in seq_len(length(dim(data)))){
   f[paste0('dimnames_',i)] <- dimnames(data)[[i]]
  }
  # for(i in seq_len(length(dim(data)))){
  #   h5attr(f,paste0('dimnames_',i)) <- dimnames(data)[[i]]
  # }

  h5close(f)
  invisible(TRUE)
}

panel.add.dim <- function(panel, label, dnames){
  stopifnot(length(label)==1)
  stopifnot(length(dnames)>0)
  dnames.extend <- dimnames(panel)
  dim(panel) <- c(dim(panel), length(dnames))
  dnames.extend[[label]] <- dnames ## append at the end
  dimnames(panel) <- dnames.extend
  panel
}

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
      if(length(dimnames(panel))==4){
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
panel.read.data.table <- function(file, verbose = TRUE){
   panel <- panel.read(file, verbose)
   panel2df(panel)
}
readicsdata <- function(dates, pathes,
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
                        panel.h5.data.table = panel.read.data.table,
                        data.table.h5.data.table = stop,
                        rds.data.table = readRDS,
                        csv.data.table = fread)
  if(des.format=='panel' && src.format != 'panel.h5') stop('source format should be panel.h5')

  if(des.format == 'panel'){

    files <- paste0(c(outer(pathes, dates, file.path)),'.h5')
    panel.combine(lapply(files, function(f){
      if(file.exists(f) || missing.allow){
        panel.read(f, verbose = verbose)
      }

    }))
  }else if(des.format == 'data.table'){
    rbindlist(lapply(dates, function(d){
      rbindlist(lapply(pathes,function(p){
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
