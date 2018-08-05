#' save data.frame to h5
#'
#' @param data data.frame or data.table format
#' @param file
#' @param verbose
#' @param overwrite
#' @param dircreate logical, if FALSE, error raised if the direcotory of file doesnot exits.
#' @param format
#'
#' @return
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1,2), b = c('x','y'))
#' df.write(df, '~/dftest.h5', overwrite = TRUE)
df.write <- function(data, file, verbose = TRUE, overwrite = FALSE, dircreate = FALSE, format = 'h5'){
  if(format == 'h5'){
    stopifnot(class(data) %in% c('data.table','data.frame'))
    mode <- ifelse(overwrite, 'w', 'w-')
    if(dircreate == TRUE) dir.create(dirname(file), FALSE, TRUE)
    if(verbose) print(paste('Writing:', file))
    if(file.exists(file) && overwrite == FALSE) stop('Error: file exist, please set overwrite = TRUE')

    types <- unlist(lapply(data, class))
    if(any(types %in% 'factor')){
      idx <- which(types == 'factor')
      if(verbose) print(paste('column ', idx, 'are factors, will set to chararcter'))
      for(i in idx){
        data[[i]] <- as.character(data[[i]])
      }
    }

    f <- h5file(file, mode)
    cnames <- colnames(data)
    h5attr(f, 'dim') <- dim(data)
    h5attr(f, 'colnames') <- cnames
    for(name in cnames){
      f[name] <- data[[name]]
    }
    h5close(f)
    invisible(TRUE)
  }

}


#' read data.frame in h5 file
#'
#' @param file
#' @param verbose bool
#' @param format
#' @return
#' @export
#'
#' @examples
#' #' df <- data.frame(a = c(1,2), b = c('x','y'))
#' df.write(df, '~/dftest.h5')
#' df2 <- df.read('~/dftest.h5')
df.read <- function(file, verbose = TRUE, format = 'h5'){

  if(format == 'h5'){
    if(verbose) print(paste('Reading:',file))
    f <- h5file(file,'r')
    cnames <- h5attr(f,'colnames')
    raw <- lapply(cnames,function(name){
      f[name][]
    })
    names(raw) <- cnames
    res <- as.data.frame(raw,stringsAsFactors = FALSE)
    h5close(f)
    return(res)
  }

}
