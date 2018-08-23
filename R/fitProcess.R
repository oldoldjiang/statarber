### fit process ###

## usage case:
# 1. take terms and fit coef/select term and eval result
# 2. compare different version of alphas. use alpEval directly
# 3. try different sampling method and compare result

## Input
# 1. univ
# 2. days (INS/OOS)
# 3. different sampling/grouping
# 4. different fit: term set OR fitting techniques

fit.process <- function(cfg,
                        ver = 'v1',
                        report.name = 'test.pdf',
                        use.cache = TRUE,
                        cores = 1,
                        save.result = FALSE,
                        verbose = TRUE,
                        coef.lag = NULL,
                        error.tolerant = FALSE,    # if file missing is allowed
                        auto.per.term.fit = FALSE, # if alpha is fitted or not,
                        only.auto.nz.term = TRUE,
                        assume.coef.stable = TRUE,
                        coef.max = Inf,
                        coef.min = -Inf){

  if(cores>1){
    library(doMC)
    doMC::registerDoMC(cores = cores)
  }

  # three pathes:
  # 1. root.dir: where the market data, return data are stored
  # 2. report.dir: where to save the report and the cache data
  # 3. dir.list: dir.list in sampler.list is the path where the alpha data is in

  root.dir   <- cfg$root.dir
  univ       <- cfg$univ
  mkt        <- cfg$mkt
  freq       <- cfg$freq
  periods    <- sort(cfg$periods)
  report.dir <- cfg$report.dir
  group.type <- null.replace(cfg$group.type, 'ALL')
  group.name <- null.replace(cfg$group.name, 'ALL')
  focus.period <- null.replace(cfg$focus.period, periods[1])
  coef.lag   <- null.replace(coef.lag,max(6, focus.period + 1))
  fwd        <- sapply(periods, function(p) sub('N',p,cfg$rt.pattern))

  upath   <- path.helper(dtype = 'univ', mkt = mkt, freq = 'DAILY', ver = univ, root.dir = root.dir)
  rpathes <- path.helper(dtype = 'fwd',  mkt = mkt, freq = 'DAILY', ver = fwd,  root.dir = root.dir)

  ## in the sample dates
  INS.days <- date.helper(sdate = cfg$dates$sdate,
                          edate = cfg$dates$edate,
                          omit.date  = cfg$dates$omit.days,
                          date.file  = cfg$dates$ins.rule.file,
                          date.group = cfg$dates$ins.group)

  ## out of the sample dates
  OOS.days <- date.helper(sdate = cfg$dates$os.sdate,
                          edate = cfg$dates$os.edate,
                          omit.date  = cfg$dates$omit.days,
                          date.file  = cfg$dates$ins.rule.file,
                          date.group = cfg$dates$oos.group)

  sampler.list <- cfg$sampler.list
  fitter.list  <- cfg$fitter.list
  model.list   <- cfg$model.list

  alpha.vers = c()
  for(fitname in names(cfg$fit.list)){
    fitcfg <- cfg$fit.list[[fitname]]

    # get sampler
    smplname <- fitcfg$sampler
    sampler.cfg <- sampler.list[[smplname]]

    if(is.null(sampler.cfg)) stop("Invalid sampler")

    fittername <- fitcfg$fitter
    fitter.cfg <- fitter.list[[fittername]]

    if(is.null(fitter.cfg)) stop("Invalid fitter")

    # model can be from model.cfg or sampler.cfg$dir.list
    modelname <- fitcfg$model
    model.cfg <- list(model.list[[modelname]])

    if(is.null(model.list[[modelname]])){
      model.cfg <- list()
      model.cfg[[modelname]] <- modelname
    }

    alphaname <- fitname
    names(model.cfg) <- alphaname

    periods <- periods[periods >0]

    dir.list <- sampler.cfg$dir.list

    sampler.dir = file.path(report.dir,"sample",smplname)
    if(!file.exists(sampler.dir))dir.create(sampler.dir,FALSE,TRUE)

    # fit
    fit.para <- fitter.cfg
    fit.out.dir <- file.path(report.dir,"fit",fitname)

    coef.Ver <- sprintf("ins%s.%d.to.%d",paste(cfg$dates$ins.group,collapse = "_"),cfg$dates$sdate,cfg$dates$edate)
    if(!file.exists(fit.out.dir))dir.create(fit.out.dir,FALSE,TRUE)
    coef.out.file <- file.path(fit.out.dir, paste("coef.",coef.Ver,".h5",sep=""))
    cache.coef.ptn <- file.path(fit.out.dir,paste("coef.YYYYMMDD.h5",sep=""))

    if(use.cache && file.exists(coef.out.file)){
      coef <- panel.read(coef.out.file)
    }else{

      INS.days <- .checkFileDate(INS.days, dir.list)

      sampling.multiday(dates = INS.days,
                        alpha.dir = dir.list,
                        ret.dir  = rpathes,
                        univ.dir = upath,
                        univ.set = sampler.cfg$univ.set,
                        time.set = sampler.cfg$time.set,
                        group.type = sampler.cfg$group.type,
                        group.name = sampler.cfg$group.name,
                        group.by = sampler.cfg$group.by,
                        quantile.range = sampler.cfg$quantile.range,
                        quantile.var = sampler.cfg$quantile.var,
                        cache.dir = sampler.dir,
                        error.tolerant = error.tolerant,
                        cores = cores,
                        verbose = verbose)
      print("Done gen sampled")
      gc()

      coef <- fitBeta(INS.days, sampler.dir = sampler.dir,model.cfg = model.cfg,
                      mode = fit.para$mode, y.vars = fwd, wgt.var = NULL,
                      ignore.empty.file = error.tolerant,
                      fit.func = fit.para$fit.func, fit.para = list(),
                      use.cache = use.cache, cache.coef.ptn = cache.coef.ptn,
                      coef.max=Inf, coef.min=-Inf,cores=cores,verbose=verbose)

      # save coef
      panel.write(coef,coef.out.file,overwrite=TRUE)
    }

    coeflabels<-names(dimnames(coef))
    if(auto.per.term.fit){
      coefnames <- dimnames(coef)['X'][-1]
      if(only.auto.nz.term){
        tmpcoef <- adrop(coef[dim(coef)[1],1,-1,1,1,drop=FALSE],c(1,2,4))
        coefnames <- dimnames(tmpcoef[abs(tmpcoef[,1])>1e-10,,drop=FALSE])[[1]]
      }

      if(!all(coefnames %in% dimnames(coef)[['M']])){
        per.term.model.cfg <- llply(coefnames,function(x)x)
        names(per.term.model.cfg) <- coefnames

        per.term.coef <- fitBeta(INS.days, sampler.dir = sampler.dir,model.cfg = per.term.model.cfg,
                                 mode = fit.para$mode, y.vars = fwd, wgt.var = NULL,
                                 ignore.empty.file = error.tolerant,
                                 fit.func = fit.para$fit.func, fit.para = list(),
                                 use.cache = use.cache, cache.coef.ptn = cache.coef.ptn,
                                 coef.max=Inf, coef.min=-Inf,cores=cores,verbose=verbose)

        coef <- panel.combine(list(coef,per.term.coef),0)
        names(dimnames(coef)) <- coeflabels
        panel.write(coef,coef.out.file,overwrite=TRUE)
      }
    }

    print(paste("Non-zero coef # ", mean(aaply(coef[,,,,1,drop=FALSE],c(1:2,4:5),function(x)sum(abs(x)>1e-10),.drop=FALSE)),sep=""))
    midx <- if(auto.per.term.fit){1:dim(coef)[5]}else{1}
    tmpcoef <- adrop(coef[dim(coef)[1],1,,1,midx,drop=FALSE],c(1,2,4))
    print(tmpcoef[abs(tmpcoef[,1])>1e-10,,drop=FALSE])

    gc()

    ## generate alpha
    anames <- ifelse(auto.per.term.fit,dimnames(coef)[['M']],dimnames(coef)[['M']][1])
    for(aname in anames){
      anameVer <- ifelse(assume.coef.stable,aname,paste0(aname,sprintf(".coef.lag%d.%s",coef.lag,coef.Ver)))
      alpha.path <- file.path(root.dir,"alpha",mkt,freq,anameVer,"YYYYMMDD.h5")
      alpha.names <- gen.alp.on.coef(stDate = cfg$date$os.sdate, edDate = cfg$dates$os.edate, coef=coef[,,,,aname,drop=FALSE],
                                    term.path=dir.list,model=aname,grp.name=groupType,alphaname=aname,mkt=cfg$mkt,lag=coef.lag,
                                    fit.para=fit.para,alpha.path=alpha.path,cores = cores, include.days=OOS.days,
                                    use.cache=use.cache,verbose=verbose,onecache.only=onecache.only)
      dates <- getTradingDayRange(from = cfg$date$os.sdate, to = cfg$dates$os.edate)
      alpha.names <- gen.alp.on.coef(dates, coef[,,,,aname,drop=FALSE], term.path = dir.list, model = aname,
                                     group.type = group.type,
                                     alpha.path = alpha.path, alpha.name = aname,
                                     lag = coef.lag, mkt = cfg$mkt, cores = cores,
                                     alpha.only = FALSE, verbose = verbose, use.cache = use.cache,
                                     fit.para = fit.para)

      alpha.vers <- union(alpha.vers,anameVer)
      all.alpha.names <- union(all.alpha.names,alpha.names)
      gc()
    }
  }

  # eval result
  full.report.name <- file.path(report.dir,"rpt",report.name)
  dir.create(dirname(full.report.name))
  result=NULL
  if(!use.cache || file.exists(full.rpt.name)){
    print(paste("To generate rpt ", full.rpt.name))
    result <- genReport(cfg$dates$os.sdate,max.rpt.date,period = prt.periods,focus.period=focus.period,include.days=OOS.days,
                       alpha.vers=alpha.vers,alpha.dir=root.dir,alpha.paths=alpha.paths,cores=cores,univ=univ,mkt=mkt,
                       freq=freq,alphaNames=c(extra.alpha.names,all.alpha.names),reportName=full.rpt.name,
                       quickTest=quickRpt,vers=md.ver,univ.vers=univ.ver,verbose=verbose,alpha.method=alpha.method,
                       use.cache=use.cache,groupType=groupType,groupName=groupName, ... )


    result <- genAlphaReport(file    = full.report.name,
                             dates   = OOS.days,
                             apathes = dir.list,
                             rpathes = rpathes,
                             upath   = upath,
                             focus.period = focus.period,
                             quicktest    = TRUE)

    if(save.result){
      saveRDS(result,sub(".pdf$",".rds",full.rpt.name))
    }
  }
  invisible(result)

}
.checkFileDate <- function(dates, dir.list){
  dates.list <- lapply(dir.list,function(dir){
    file.dates <- substr(list.files(dir, pattern = '^\\d*.h5$'),1,8)
    if(setequal(file.dates, dates)){
      warning(paste(setdiff(dates, file.dates),'in',dir,'is not found'))
    }
    file.dates
  })

  if(length(dates.list)==1){
    file.dates <- dates.list[[1]]
  }else{
    file.dates <- Reduce(intersect,dates.list)
  }

  res <- intersect(file.dates, dates)
  return(res)
}
