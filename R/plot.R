genAlphaReport <- function(file, dates, apathes, rpathes, upath, focus.period, quicktest = TRUE,
                           na.alpha = c('remove','zero','median')){

  # save image or print out
  if(!(is.null(file) || file=='')){
    if(!endswith(file,'.pdf')) stop('file should be end with .pdf')
    pdf(file)
  }

  print('start generating plot')
  data <- prepareData(dates, apathes, rpathes, upath)
  print('done preparing data')
  DT <- paste0(DNAMES$DATE, DNAMES$TIME)

  anames <- basename(apathes)
  rnames <- basename(rpathes)
  rname  <- rnames[grepl(paste0('*.*.*.',focus.period),rnames)]
  uname  <- basename(upath)

  if(!quicktest){
    # calculate coverage before na is handled
    cv <- calcCoverage(data, anames)
  }
  data[,(uname):=NULL]

  # handle NA in alpha, NA should be in return
  if(na.alpha == 'remove'){
    data <- complete.cases(data)
  }else if(na.alpha == 'zero'){
    for(an in anames){
      data[is.na(get(an)),(an):=0]
    }
  }else if(na.alpha == 'median'){
    dt <- c(DNAMES$DATE,DNAMES$TIME)
    for(an in anames){
      dta <- c(DNAMES$DATE,DNAMES$TIME,an)
      na  <- data[is.na(get(an))][,dt,with = FALSE] # only take subset to merge
      kdta<- c(DNAMES$SYMBOL,DNAMES$DATE,DNAMES$TIME,an)
      na  <- merge(na,data[,kdta,with= FALSE], all.x = TRUE, by = dt)
      med <- na[,median(get(an),na.rm = TRUE),by=dt]$V1
      data[is.na(get(an)),(an):=med]
    }
  }

  alpha <- calcAlpha(data, anames, rnames, method = 'pearson')
  # rename alpha names
  a_rnames <- colnames(alpha)[grepl(paste0('*.*.*.',focus.period),colnames(alpha))]
  print('done calculating alpha')
  alpha[,(DT):=strptime(paste0(get(DNAMES$DATE),get(DNAMES$TIME)), format = '%Y%m%d %H:%M:%OS')]
  alpha <- alpha[,c(DT,a_rnames), with = FALSE]
  setorderv(alpha,DT)
  plotCumAlpha(alpha,dtname = DT)
  print('done plotting cummulative alpha')

  if(!quicktest){
    plotCumAlphaEachYear(alpha,dtname = DT)
    print('done plotting yearly cummulative alpha')
    plotRelativeAlpha(alpha,dtname = DT)
    print('done plotting relative cummulative alpha')
    plotDeltaAlpha(alpha,dtname = DT)
    print('done plotting first order alpha derivative')

    # plot alpha property using spearman corrlation methods
    spalpha <- calcAlpha(data, anames, rnames, method = 'spearman')
    a_rnames <- colnames(spalpha)[grepl(paste0('*.*.*.',focus.period),colnames(spalpha))]
    spalpha[,(DT):=strptime(paste0(get(DNAMES$DATE),get(DNAMES$TIME)), format = '%Y%m%d %H:%M:%OS')]
    spalpha <- alpha[,c(DT,a_rnames), with = FALSE]
    setorderv(spalpha,DT)
    plotCumAlpha(spalpha,dtname = DT)
    plotCumAlphaEachYear(spalpha,dtname = DT)
    plotRelativeAlpha(spalpha,dtname = DT)
    plotDeltaAlpha(spalpha,dtname = DT)

    # plot cummulative return of portfolio with alpha as weight
    ret <- calcRetWithAlphaWeight(data,anames, rnames)
    a_rnames <- colnames(ret)[grepl(paste0('*.*.*.',focus.period),colnames(ret))]
    ret[,(DT):=strptime(paste0(get(DNAMES$DATE),get(DNAMES$TIME)), format = '%Y%m%d %H:%M:%OS')]
    ret <- ret[,c(DT,a_rnames), with = FALSE]
    setorderv(ret,DT)
    plotCumAlpha(ret,dtname = DT, ylab = 'cum ret use alpha as weight')
    plotCumAlphaEachYear(ret,dtname = DT, ylab = 'cum ret use alpha as weight each year')

    avgret <- calcMeanRetQuantile(data, anames, rname, nquantile = 30)
    print('done calculating mean return quantile')
    plotMeanRetQuantile(avgret,aname = 'anames', rname = 'avgret', qname = 'quantile')
    print('done plotting mean return quantile')
    cumdiffqret <- calcCumDeltaMeanRetQuantile(data, anames, rname, dtname, nquantile = 30)
    plotCumDiffMeanRetQuantile(cumdiffqret, aname = 'anames', rname = 'cumdiffret', qname = 'quantile', dtname)

    alphacor <- calcAlphaCor(data, anames)
    print('done calculating alpha correlation')
    corrplot(alphacor,method = 'square', tl.pos = "h",addCoef.col="grey")
    print('done plotting alpha correlation')
  }

  dev.off()
}

.plotCore <- function(alpha, dtname, ylab){
  cnames <- colnames(alpha)
  alpnames <- cnames[!cnames %in% c(DNAMES$D,DNAMES$T,dtname)]
  longdata <- melt(alpha, id = dtname)

  g <- ggplot(longdata, aes(x = get(dtname), y = value, colour = variable, group = variable)) +
    geom_line() +
    scale_x_datetime() +
    xlab('') +
    ylab(ylab) +
    scale_colour_hue("alpha name")
  plot(g)
  invisible(alpha)
}
plotCumAlpha <- function(alpha,anames, dtname='DT',ylab = 'cum alpha'){
  cumalpha <- alpha[,append(list(DT),
                            lapply(anames, function(an){cumsum(get(an))})
                            )]
  setnames(cumalpha, 1:dim(cumalpha)[2], c(dtname, anames))

  .plotCore(cumalpha,dtname = dtname, ylab = ylab)
  invisible(cumalpha)
}
plotCumAlphaEachYear <- function(alpha, anames, dtname = 'DT', ylab = 'cum alpha yearly'){
  yalpha <- alpha[,append(list(DT),
                           lapply(anames, function(an){cumsum(get(an))})),
                   by = year(get(dtname))]
  yalpha[,year:=NULL]
  setnames(yalpha, 1:dim(yalpha)[2],c(dtname, anames))

  .plotCore(yalpha, dtname = dtname, ylab = ylab)
  invisible(yalpha)
}
plotRelativeAlpha <- function(alpha, anames, baname = NULL,dtname = 'DT'){
  if(is.null(baname)) baname <- anames[1] # default benchmark alpha

  relalpha <- alpha[,append(list(DT),
                            lapply(anames, function(an){get(an) - get(baname)}))]
  setnames(relalpha, 1:dim(relalpha)[2],c(dtname, anames))

  plotCore(relalpha, dtname = dtname, ylab = 'cum relative alpha')
  invisible(relalpha)
}
plotDeltaAlpha <- function(alpha,anames, dtname = 'DT',lag = 1){
  dalpha <- alpha[,append(list(DT),
                          lapply(anames, function(an){c(rep(0,lag),diff(get(an),lag = lag))}))]
  setnames(dalpha, 1:dim(dalpha)[2],c(dtname, anames))

  plotCore(dalpha, dtname = dtname, ylab = 'alpha delta')
  invisible(dalpha)
}
plotMeanRetQuantile <- function(avgret, aname, rname, qname){
  cnames <- colnames(avgret)
  colexists <- c(aname, qname, rname) %in% cnames

  g <- ggplot(avgret, aes(x = get(qname), y = get(rname), group = get(aname))) +
    geom_col() +
    xlab('') +
    ylab('') +
    facet_wrap(aname) +
    scale_colour_hue("alpha name")
  plot(g)
}
plotCumDiffMeanRetQuantile <- function(cumdiffavgret, aname, rname, qname, dtname){
  g <- ggplot(cumdiffavgret, aes(x = get(dtname), y = get(rname), colour = as.factor(get(qname)), group = get(qname))) +
    geom_line() +
    scale_x_datetime()+
    xlab('') +
    ylab('') +
    facet_wrap(aname) +
    scale_colour_hue("alpha name",labels=1:max(cumdiffavgret[[qname]]))
  plot(g)
}
