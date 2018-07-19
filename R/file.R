gen.term.file <- function(date, gen.term.func, use.cache = TRUE, time.ver = NULL, root.dir = '~/', ...){

  date <- getTradingDay(date)
  out.file <- gen.term.func(d, root.dir = root.dir, names.only = TRUE, ...)
  print(out.file)
  if(use.cache && file.exists(out.file)){
    print(paste('Exists', out.file))
    return(out.file)
  }

  terms <- gen.term.func(d, root.dir = root.dir, names.only = FALSE, ...)
  print('Done calculation')

  dir.create(dirname(out.file), FALSE, TRUE)
  if(!is.null(time.ver)){
    tmp.out.file <- paste0(out.file, '.', time.ver)
    panel.write(terms, tmp.out.file, overwrite = TRUE)
    print(paste0('Linking ', tmp.out.file, ' to ', out.file))
    if(file.exists(out.file)) file.remove(out.file)
    file.symlink(tmp.out.file, out.file)
  }else{
    panel.write(terms, out.file, overwrite = TRUE)
  }
  return(out.file)
}

gen.alp.helper <- function(d, coef.file, model.name, alpha.name = model.name, term.path,
                           groupType = 'ALL', root.dir = '~/', names.only = FALSE){
  alpha.path <- file.path(root.dir, 'alpha', mkt, freq, alpha.name, paste0(d,'.h5'))
  if(names.only) return(alpha.path)

  coef <- panel.read(coef.file)
  if( !model.name %in% dimnames(coef)[[5]]) stop(paste0('coef model not match ', model.name))
  coef <- coef[,,,model.name, drop = FALSE]

  alp.v <- gen.alp.on.coef(sdate = d, edate = d, alp.only = TRUE, use.cache = FALSE, coef = coef,
                           term.path = term.path, model = model.name, alpha.name = alpha.name,
                           grp.name = groupType, mkt = mkt, alpha.path = alpha.path,
                           cores = 1, include.days = NULL, verbose = TRUE)
  if(is.null(alp.v)) stop('Alp is NULL')
  alp.v

}



## example usage
## d is date
## offset is shift of day, -1 means prior day
## time.ver is a string version file suffix, e.g. 20180213.h5 -> 20170213.h5.20170214_040000
## model.name, which model to pick from coef
## alpha.name, will be ALPHA.NAME in /rootpath/ics/alpha/CHINA_STOCK/DAILY/ALPHA.NAME/YYYYMMDD.h5
## term.path
## gen.alp.file(d, offset, time.ver = '20170214_030000', coef.file = '/path/to/coef.h5',
##model.name = 'z1_smallUniv', alpha.name = 'z1_smallUniv')
gen.alp.file <- function(d, ...){
  gen.term.file(d, gen.term.func = gen.alp.helper, ...)
}

gen.term.dates <- function(sdate = 20100101, edate = 20141231, gen.func, cores = 7, ...){
  library(doMC)
  doMC::registerDoMC(cores = cores)

  dates <- getTradingDayRange(sdate, edate)
  outfiles <- llply(dates, function(d){
    tryCatch(gen.func(d, ...), error = function(e){
      return(NULL)
    })
  },.parallel = cores>1)
}
