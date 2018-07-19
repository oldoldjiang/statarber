savedftoH5 <- function(data, file,verbose = TRUE, overwrite = FALSE, dircreate = FALSE){
  stopifnot(class(data) %in% c('data.table','data.frame'))
  mode <- ifelse(overwrite, 'w', 'w-')
  if(dircreate == TRUE) dir.create(dirname(file), FALSE, TRUE)
  if(verbose) print(paste('Writing:', file))
  if(file.exists(file) && overwrite == FALSE) stop('Error: file exist, please set overwrite = TRUE')
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
readdffromH5 <- function(file, verbose = TRUE){
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
