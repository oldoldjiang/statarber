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
                        rpt.name = 'fitReport.pdf',
                        rpt.periods = c(-2,1,2), ##  will sort it
                        use.cache = TRUE,
                        cores = 1,
                        alpha.paths = NULL,
                        extra.alpha.names = NULL,
                        save.result = FALSE,
                        verbose = FALSE,
                        oos.use.all = FALSE,
                        os.sdate = NULL, ## overlap with cfg
                        os.edate = NULL, ## overlap with cfg
                        focus.period = cfg$periods[1],
                        coef.lag = max(6, focus.period + 1),
                        error.torrent = FALSE,
                        auto.per.term.fit = FALSE,
                        only.auto.nz.term = TRUE,
                        assume.coef.stable = TRUE,
                        coef.max = Inf,
                        coef.min = -Inf){

  if(cores>1){
    library(doMC)
    doMC::registerDoMC(cores = cores)
  }


  univ     <- cfg$univ
  md.ver   <- null.replace(cfg$md.ver, 'windzf')
  univ.ver <- null.replace(cfg$univ.ver, md.ver)
  mkt      <- cfg$mkt
  freq     <- cfg$freq

  ## 1. why change root.dir. 2. use file.path
  root.dir <- paste(cfg$root.dir, ver, sep = '/')
  periods  <- null.replace(cfg$periods, 5)
  group.type <- null.replace(cfg$group.type, 'ALL')
  group.name <- null.replace(cfg$group.name, 'ALL')

  ## TODO: only use 1 par, either in cfg or in func par
  rpt.periods <- sort(union(rpt.periods, periods))

  ## get days
  cfg$dates$ins.rule.file <- null.replace(cfg$dates$ins.rule.file,
                                          file.path(Sys.getenv('ICS_DATA_ROOT',
                                                               '/mnt/analysis/ics'),
                                                    'dategroup',
                                                    'default.ins.2017'))
  cfg$dates$ins.group <- null.replace(cfg$dates$ins.group, 'INS1')

  ## get days by multi rules
  INS.days <- fp.get.days(cfg$dates$sdate,
                          cfg$dates$edate,
                          cfg$dates$omit.days,
                          ins.rule.file = cfg$dates$ins.rule.file,
                          days.group = cfg$dates$ins.group)


  cfg$dates$os.sdate <- null.replace(os.sdate, null.replace(cfg$dates$os.sdate,
                                                            cfg$dates$sdate))
  cfg$dates$os.edate <- null.replace(os.edate, null.replace(cfg$dates$os.edate,
                                                            cfg$dates$edate))
  cfg$dates$oos.group  <- null.replace(cfg$dates$oos.group, 'INS2')

  max.rpt.date <- null.replace(cfg$dates$max.rpt.date, cfg$dates$os.edate)

  if(oos.use.all){
    cfg$dates$oos.group = NULL
  }

  ## get days by multi rules
  OOS.days <- fp.get.days(cfg$dates$os.sdate,
                          cfg$dates$os.edate,
                          cfg$dates$omit.days,
                          ins.rule.file = cfg$dates$ins.rule.file,
                          days.group = cfg$dates$oos.group)

  ## get sampler definitions
  sampler.list <- cfg$sampler.list
  fitter.list  <- cfg$fitter.list
  model.list   <- cfg$model.list

  fwd.var = 'Ret' ## TODO: pass as par
  all.alpha.names <- c()

  ## for each sampler/fit combination instance, do sample/fit
  alpha.vers = c()
  for(fitname in names(cfg$fit.list)){
    fitcfg = cfg$fit.list[[fitname]]

    # get sampler
    smplname = fitcfg$sampler
    sampler.cfg = sampler.list[[smplname]]

    if(is.null(sampler.cfg)) stop("Invalid sampler")

    fittername = fitcfg$fitter
    fitter.cfg = fitter.list[[fittername]]

    if(is.null(fitter.cfg)) stop("Invalid fitter")

    modelname = fitcfg$model
    model.cfg = list(model.list[[modelname]])

    if(is.null(model.list[[modelname]])){model.cfg = list();model.cfg[[modelname]] = modelname}

    alphaname = fitname
    names(model.cfg) = alphaname

    periods <- periods[periods >0]

    groupType <- sampler.cfg$group.type
    KRange <- sampler.cfg$KRange
    timeRange <- sampler.cfg$timeRange
    var.pattern <- null.replace(sampler.cfg$var.pattern,"_ALL")
    dir.list = sampler.cfg$dir.list
    quantileRange = sampler.cfg$quangileRange
    if(is.null(quantileRange)) quantileRange = c(0,1)
    if(length(quantileRange)!=2 | any(quantileRange>1) | any(quantileRange<0))stop('error format of quantile range')
    sampler.dir = file.path(root.dir,"sample",smplname)
    if(!file.exists(sampler.dir))dir.create(sampler.dir,FALSE,TRUE)

    # fit
    fit.para = fitter.cfg
    fit.out.dir = file.path(root.dir,"fit",fitname)

    coef.Ver = sprintf("ins%s.%d.to.%d",paste(cfg$dates$ins.group,collapse = "_"),cfg$dates$sdate,cfg$dates$edate)
    if(!file.exists(fit.out.dir))dir.create(fit.out.dir,FALSE,TRUE)
    coef.out.file = file.path(fit.out.dir, paste("coef.",coef.Ver,".h5",sep=""))
    cache.coef.ptn <- file.path(fit.out.dir,paste("coef.YYYYMMDD.h5",sep=""))

    if(use.cache && file.exists(coef.out.file)){
      coef = panel.read(coef.out.file)
    }else{
      fileDay.list <- lapply(dir.list,function(dir){
        files <- list.files(sub("YYYYMMDD.h5",'',dir),pattern = '^\\d*.h5$')
        fileDay <- as.integer(str_sub(files,1,8))
      })

      if(length(fileDay.list)==1){
        fileDay = fileDay.list[[1]]
      }else{
        fileDay = Reduce(intersect,fileDay.list)
      }
      INS.days = intersect(INS.days,fileDay)


      if(fit.para$mode!="ALL") onecache.only=FALSE
      genMultidaySample(INS.days=INS.days,periods=periods,groupType=groupType,KRange=KRange,timeRange=timeRange,
                        fwd.var=fwd.var,var.pattern=var.pattern,dir.list=dir.list,mkt=mkt,freq=freq,
                        sampler.dir=sampler.dir,use.cache=use.cache,verbose=verbose,md.ver=md.ver,univ.ver=univ.ver,
                        cores=cores,error.tolerant=error.tolerant,onecache.only=onecache.only,quantileRange=quantileRange)
      print("Done gen sampled")
      gc()

      coef = fitBeta(stDate = min(INS.days),edDate = max(INS.days),sampler.dir = sampler.dir, model.cfg = model.cfg, ins.days = INS.days,
                     fit.func = fut.para$fit.func, mode = fit.para$mode, fit.para = fit.para, fit.y.vars = value2name(periods,var=fwd.var,frq =freq),
                     ignore.empty.file =error.tolerant, use.cache=use.cache, cache.coef.ptn=cache.coef.ptn,coef.max=coef.max,
                     coef.min=coef.min,cores=cores,verbose=verbose)

      # save coef
      panel.write(coef,coef.out.file,overwrite=TRUE)
    }

    coeflabels=names(dimnames(coef))
    if(auto.per.term.fit){
      coefnames = dimnames(coef)['X'][-1]
      if(only.auto.nz.term){
        tmpcoef = adrop(coef[dim(coef)[1],1,-1,1,1,drop=FALSE],c(1,2,4))
        coefnames = dimnames(tmpcoef[abs(tmpcoef[,1])>1e-10,,drop=FALSE])[[1]]
      }

      if(!all(coefnames %in% dimnames(coef)[['M']])){
        per.term.model.cfg = llply(coefnames,function(x)x)
        names(per.term.model.cfg) = coefnames
        per.term.coef = fitBeta(stDate=min(INS.days),edDate=max(INS.days),sampler.dir=sampler.dir,model.cfg=per.term.model.cfg,ins.days=INS.days,
                                fit.func=fit.para$fit.func,mode=fit.para$mode,fit.para=fit.para,fit.y.vars=value2name(periods,var=fwd.var,freq=freq),
                                cache.coef.ptn=cache.coef.ptn,coef.max=coef.max,coef.min=coef.min,ignore.empty.file=error.tolerant,cores=cores,verbose=verbose)
        coef = panel.combine(list(coef,per.term.coef),0)
        names(dimnames(coef)) = coeflabels
        panel.write(coef,coef.out.file,overwrite=TRUE)
      }
    }

  }

  # eval result
  full.rpt.name = file.path(root.dir,"rpt",rpt.name)
  if(!file.exists(dirname(full.rpt.name)))dir.create(dirname(full.rpt.name))
  result=NULL
  if(!use.cache || file.exists(full.rpt.name)){
    print(paste("To generate rpt ", full.rpt.name))
    result = genReport(cfg$dates$os.sdate,max.rpt.date,period = prt.periods,focus.period=focus.period,include.days=OOS.days,
                       alpha.vers=alpha.vers,alpha.dir=root.dir,alpha.paths=alpha.paths,cores=cores,univ=univ,mkt=mkt,
                       freq=freq,alphaNames=c(extra.alpha.names,all.alpha.names),reportName=full.rpt.name,
                       quickTest=quickRpt,vers=md.ver,univ.vers=univ.ver,verbose=verbose,alpha.method=alpha.method,
                       use.cache=use.cache,groupType=groupType,groupName=groupName, ... )
    if(save.result){
      saveRDS(result,sub(".pdf$",".rds",full.rpt.name))
    }
  }
  invisible(result)

}
