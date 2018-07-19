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
                        core = 1,
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
  library(doMC)
  doMC::registerDoMC(cores = cores)

  univ     <- cfg$univ
  md.ver   <- null.replace(cfg$md.ver, 'windzf')
  univ.ver <- null.replace(cfg$univ.ver, md.ver)
  mkt      <- cfg$mkt
  freq     <- cfg$freq

  ## 1. why change root.dir. 2. use file.path
  root.dir <- paste(cfg$root.dir, ver, sep = '/')
  periods  <- null.replace(cfg$periods, 5)
  grp.type <- null.replace(cfg$grp.type, 'ALL')
  grp.name <- null.replace(cfg$grp.name, 'ALL')

  ## TODO: only use 1 par, either in cfg or in func par
  rpt.periods <- sort(union(rpt.periods, periods))

  ## get days
  cfg$dates$ins.rule.file <- null.replace(cfg$dates$ins.rule.file,
                                          file.path(Sys.getenv('ICS_DATA_ROOT',
                                                               '/mnt/analysis/ics'),
                                                    'dategrp',
                                                    'default.ins.2017'))
  cfg$dates$ins.grp <- null.replace(cfg$dates$ins.grp, 'INS1')

  ## get days by multi rules
  INS.days <- fp.get.days(cfg$dates$sdate,
                          cfg$dates$edate,
                          cfg$dates$omit.days,
                          ins.rule.file = cfg$dates$ins.rule.file,
                          days.grp = cfg$dates$ins.grp)


  cfg$dates$os.sdate <- null.replace(os.sdate, null.replace(cfg$dates$os.sdate,
                                                            cfg$dates$sdate))
  cfg$dates$os.edate <- null.replace(os.edate, null.replace(cfg$dates$os.edate,
                                                            cfg$dates$edate))
  cfg$dates$oos.grp  <- null.replace(cfg$dates$oos.grp, 'INS2')

  max.rpt.date <- null.replace(cfg$dates$max.rpt.date, cfg$dates$os.edate)

  if(oos.use.all){
    cfg$dates$oos.grp = NULL
  }

  ## get days by multi rules
  OOS.days <- fp.get.days(cfg$dates$os.sdate,
                          cfg$dates$os.edate,
                          cfg$dates$omit.days,
                          ins.rule.file = cfg$dates$ins.rule.file,
                          days.grp = cfg$dates$oos.grp)

  ## get sampler definitions
  sampler.list <- cfg$sampler.list
  fitter.list  <- cfg$fitter.list
  model.list   <- cfg$model.list

  fwd.var = 'Ret' ## TODO: pass as par
  all.alpha.names <- c()

  ## for each sampler/fit combination instance, do sample/fit
  alpha.vers = c()
  for(fitname in names(cfg$fit.list)){
    fitcfg <- cfg$fit.list[[fitname]]

    ## get sampler
    samplename  <- fitcfg$sampler
    sampler.cfg <- sampler.list[[samplename]]
    if(is.null(sampler.cfg)) stop('Invalid sampler')

    ## get fitter
    fittername <- fitcfg$fitter
    fitter.cfg <- fitter.list[[fittername]]
    if(is.null(fitter.cfg)) stop('Invalid fitter')

    ## get model
    modelname <- fitcfg$model
    model.cfg <- list(model.list[[modelname]]) ## why add list()???
    if(is.null(model.list[[modelname]])){
      model.cfg <- list()
      model.cfg[[modelname]] <- modelname
    }

    alphaname <- fitname
    names(model.cfg) <- alphaname

    ## TODO: sampler to support more periods types
    periods <- periods[periods > 0]

    group.type <- sampler.cfg$group.type
    range <- null.replace(sampler.cfg$quantile.range, c(0,1))

    sampler.dir <- file.path(root.dir, 'sample', samplename)
    dir.create(sampler.dir, FALSE, TRUE)

    ## fit
    fit.para <- fitter.cfg
    fit.out.dir <- file.path(root.dir, 'fit', fitname)

    coef.ver <- sprintf('ins%s.%d.to%d',
                        paste(cfg$dates$ins.grp, collapse = '_'),
                        cfg$dates$sdate, cfg$dates$edate)
    coef.out.file  <- file.path(file.out.dir, paste0('coef.', coef.ver, '.csv'))
    cache.coef.ptn <- file.path(file.out.dir, 'coef.YYYYMMDD.csv')

    sampled <- sample.gen.multiday(INS.days,
                                   alpha.dir,
                                   ret.dir,
                                   univ.dir,
                                   group.type = group.type,
                                   quantile.range = quantile.range,
                                   cache.dir = NULL,
                                   error.tolerant = FALSE,
                                   cores = cores,
                                   verbose = verbose)
    print('Done gen sampled')
    gc()

    coef = fit.beta()

    ## save coef

    ## generate alpha

  }

  ## eval result
  full.rpt.name <- file.path(root.dir, 'rpt', rpt.name)
  if(!file.exists(dirname(full.rpt.name))) dir.create(dirname(full.rpt.name))

}
