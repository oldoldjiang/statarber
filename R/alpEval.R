prepareData <- function(dates, apathes, rpathes, upath = NULL){
  alpha <- data.read(dates, pathes = apathes, des.format = 'data.table')
  ret   <- data.read(dates, pathes = rpathes, des.format = 'data.table')
  data  <- merge(alpha, ret, all.x = TRUE) # only care about those with alpha

  rnames <- basename(rpathes)
  for(rn in rnames){
    if(anyNA(data[[rn]])){
      stop(paste('NA in',rn))
    }
  }

  if(!is.null(upath)){
    univ <- data.read(dates, pathes = upath, des.format = 'data.table')
    data <- merge(data, univ, all.y = TRUE)
    uname <- basename(upath)
    if(!all(data[[uname]]==1)) stop('not all universe indicators are 1')
  }
  return(data)
}
calcAlpha <- function(data, anames, rnames, method = "pearson"){

  pairs <- c(outer(anames, rnames,paste,sep = '_'))
  pairs <- lapply(pairs, function(p) strsplit(p,'_')[[1]])
  names(pairs) <- sapply(pairs,function(x) paste(x,collapse  = '_'))

  # p[1] is alpha name, p[2] is forward return name
  alpha <- data[,lapply(pairs,function(p){
      cor(.SD[[p[1]]], .SD[[p[2]]],use = "complete.obs", method = method) *
      sd(.SD[[p[2]]], na.rm = TRUE) /
      name2value(p[2])[['n']]
  }),by=c(DNAMES$DATE, DNAMES$TIME)]

  return(alpha)
}
calcRetWithAlphaWeight <- function(data, anames, rnames){

  pairs <- c(outer(anames, rnames,paste,sep = '_'))
  pairs <- lapply(pairs, function(p) strsplit(p,'_')[[1]])
  names(pairs) <- sapply(pairs,function(x) paste(x,collapse  = '_'))

  # p[1] is alpha name, p[2] is forward return name
  ret <- data[,lapply(pairs,function(p){
    sum(.SD[[p[1]]] * .SD[[p[2]]], na.rm = TRUE)
  }),by=c(DNAMES$DATE, DNAMES$TIME)]
  return(ret)

}
calcMeanRetQuantile <- function(data, anames, rname, nquantile = 30){
  avgret <- copy(data)
  for(an in anames){
    # scale (1,N) to (1,nquantile)
    avgret[,(an):=round((order(get(an))-1)*(nquantile-1)/.N+1), by = c(DNAMES$DATE, DNAMES$TIME)]
  }

  new.anames <- paste(anames,rname,sep = '_')
  setnames(avgret,anames,new.anames)
  avgret <- melt(avgret[,c(new.anames,rname), with = FALSE], id = rname)
  avgret <- avgret[,mean(get(rname), na.rm = TRUE),by=c('variable','value')]
  setnames(avgret,1:3,c('anames','quantile','avgret'))
  return(avgret)
}
calcCumDeltaMeanRetQuantile <- function(data, anames, rname, dtname, nquantile = 30, ntop = round(nquantile/6)){
  cnames <- colnames(data)
  if(any(!anames %in% cnames)){
    stop(paste("alpha name not in data:", anames[!anames %in% cnames]))
  }
  if(!rname %in% cnames){
    stop(paste("return name not in data:", rname))
  }
  stopifnot(ntop>=2)

  diffq <- copy(data)
  for(an in anames){
    # scale (1,N) to (1,nquantile)
    diffq[,(an):=round((order(get(an))-1)*(nquantile-1)/.N+1), by = c(DNAMES$DATE, DNAMES$TIME)]
  }

  diffq[,(dtname):=paste(get(DNAMES$DATE),get(DNAMES$TIME))]
  a_rnames <- paste(anames,rname,sep = '_')
  setnames(diffq,anames,a_rnames)

  diffq <- melt(diffq[,c(a_rnames,rname,dtname), with = FALSE], id = c(rname,dtname))
  diffq <- diffq[,mean(get(rname), na.rm = TRUE),by=c('variable','value',dtname)]
  setnames(diffq,1:4,c('anames','quantile',dtname,'avgret'))
  diffq <- diffq[quantile <= ntop]
  diffq[,quantile:=as.integer(quantile)]
  diffq[,`:=`(
    diffret = c(NA,-diff(avgret[order(quantile)])),
    quantile = 0:(ntop-1)
  ),by=c('anames',dtname)]
  diffq <- diffq[quantile!=0] # remove NA
  setorderv(diffq, dtname)
  diffq[,cumdiffret:=cumsum(avgret),by=c('anames','quantile')]
  diffq[,(dtname):=strptime(paste0(get(dtname)), format = '%Y%m%d %H:%M:%OS')]
  diffq <- diffq[,c('anames','quantile',dtname,'cumdiffret'),with = FALSE]
  return(diffq)
}
calcAlphaCor <- function(data, anames, method =  'pearson'){
  alphacor <- cor(alphacor[,anames, with =FALSE],use = 'complete.obs', method = method)
  return(alphacor)
}

calcHitRatio <- function(data, anames, rname){

  hr <- copy(data)

}

calcCoverage <- function(data, anames){
  # usually universe does not change during 1 day
  # data here is not na-handled

  cv <- data[,lapply(anames, function(an){
    1-(sum(is.na(an)) + sum(an==0,na.rm = TRUE))/.N # na value and zero value are invalid
  }),by=c(DNAMES$DATE, DNAMES$TIME)]
  return(cv)
}

calcICDecay <- function(data, anames, rnames){

  pairs <- c(outer(anames, rnames,paste,sep = '_'))
  pairs <- lapply(pairs, function(p) strsplit(p,'_')[[1]])
  names(pairs) <- sapply(pairs,function(x) paste(x,collapse  = '_'))

  # p[1] is alpha name, p[2] is forward return name
  icdecay <- data[,lapply(pairs,function(p){
    cor(.SD[[p[1]]], .SD[[p[2]]],use = "complete.obs", method = 'pearson')
  }),by=c(DNAMES$DATE, DNAMES$TIME)]
  icdecay <- colMeans(icdecay[,-c(1,2),with = FALSE],na.rm = TRUE)
  return(icdecay)

}

calcAlphaAutoCor <- function(data, anames){

}





calcICMean <- function(mdICDateRange, addZero = TRUE){
  md = adrop(aaply(mdICDateRange, c(1,4), mean, na.rm = TRUE, .drop = FALSE),3)
  # add zero to ic mean
  if( addZero ){
    c1 = md[which(as.integer(dimnames(md)[['K']]) < 0),,drop=FALSE]
    c2 = md[which(as.integer(dimnames(md)[['K']]) > 0),,drop=FALSE]
    mdICMean = rbind(c1,0,c2)
    names(dimnames(mdICMean)) = c("K","V")
    dimnames(mdICMean)[['K']] = c(dimnames(c1)[['K']],c("0"), dimnames(c2)[["K"]])
  }
  else{
    mdICMean = md
    names(dimnames(mdICMean)) = c("K","V")
  }
  return(mdICMean)
}

calcICYears <- function(mdICDateRange){
  dates = dimnames(mdICDateRange)[["D"]]
  minYear = format(as.Date(min(dates),"%Y%m%d"),"%Y")
  maxYear = format(as.Date(max(dates),"%Y%m%d"),"%Y")
  mlist = llply(c("wholePeriod",minYear:maxYear), function(m){
    if(m == "wholePeriod"){tmpIC = mdICDateRange}
    else{
      stDate = as.integer(paste(m,"0101",sep=""))
      edDate = as.integer(paste(m,"1231",sep=""))
      tmpIC = mdICDateRange[,which(as.integer(dates)>=stDate & as.integer(dates) <= edDate),,,drop=FALSE]
    }
    mdmean = calcICMean(tmpIC)
    tmpdata = array(0,dim = c(dim(mdmean)[1],1,dim(mdmean)[2]),dimnames = list(dimnames(mdmean)[["K"]],m,dimnames(mdmean)[['V']]))
    names(dimnames(tmpdata)) = c("K","D","V")
    tmpdata[,1,] = mdmean
    tmpdata
  })
  mdICYears = panel.combine(mlist)
}

# calc IC T space
calcICMean.T <- function(mdICDateRange, focus.period){
  mdIC = adrop(mdICDateRange[as.character(focus.period),,,,drop=FALSE],1)
  md = adrop(aaply(mdIC,c(2,3),mean,na.rm=TRUE,.drop=FALSE),3)
}

# calc monthly effect
calcMonthlyEffect <- function(mdICDateRange, focus.period){
  mdICDateRange = adrop(aaply(mdICDateRange,c(1,2,4),mean,na.rm=TRUE,.drop=FALSE),4)
  mdIC = adrop(mdICDateRange[as.character(focus.period),,,drop=FALSE],1)
  dates = dimnames(mdIC)[['D']]
  dtmths = (as.integer(dates)%/%100)%%100
  umonths = unique(dtmths)
  umonths = umonths[order(umonths)]
  mlist = llply(umonths, function(m){
    a = aaply(mdIC, 2, function(x){mean(x[dtmths==m],na.rm=TRUE)},.drop=FALSE)
    dimnames(a)[[2]]=m
    names(dimnames(a)) = c("V","D")
    a = aperm(a,c(2,1))
  })
  meffect = panel.combine(mlist)
}

# calc ic IR
calcIR <- function(mdICDateRange){
  period = as.integer(dimnames(mdICDateRange)[['K']])
  mdICDateRange = mdICDateRange[which(period >= 0),,,,drop=FALSE]
  mdMean = calcICMean(mdICDateRange)
  tmp = adrop(aaply(mdICDateRange,c(1,4),sd,na.rm=TRUE,.drop = FALSE),3)
  mdStd = rbind(1,tmp)
  names(dimnames(mdStd)) = c("K","V")
  dimnames(mdStd)[["K"]] = c(c("0"), dimnames(tmp)[["K"]])
  mdIR = mdMean / mdStd
  periods = as.integer(dimnames(mdIR)[["K"]])
  for( p in 2:length(periods)){
    mdIR[p,] = mdIR[p,]*sqrt(242)/sqrt(periods[p])
  }
  return(mdIR)
}

calcIRYears <- function(mdICDateRange){
  dates = dimnames(mdICDateRange)[["D"]]
  minYear = format(as.Date(min(dates),"%Y%m%d"),"%Y")
  maxYear = format(as.Date(max(dates),"%Y%m%d"),"%Y")
  mlist = llply(c("wholePeriod",minYear:maxYear),function(m){
    if(m == "wholePeriod"){tmpIC = mdICDateRange}
    else{
      stDate = as.integer(paste(m,"0101",sep=""))
      edDate = as.integer(paste(m,"1231",sep=""))
      tmpIC = mdICDateRange[,which(as.integer(dates)>=stDate & as.integer(dates)<= edDate),,,drop=FALSE]
    }
    mdIR = calcIR(tmpIC)
    tmpdata = array(0,dim = c(dim(mdIR)[1],1,dim(mdIR)[2]),dimnames = list(dimnames(mdIR)[["K"]],m,dimnames(mdIR)[["V"]]))
    names(dimnames(tmpdata)) = c("K","D","V")
    tmpdata[,1,] = mdIR
    tmpdata
  })
  mdIRYears = panel.combine(mlist)
}



###############################################################################################################################

# ICIR           z: whole period + each year     y: ICIR
# mean return    z: whole period + each year     y: mean return   x: period
# return to alpha x:
# alpha coverage x: trading day          y: number of non NA stock compare with univ
# ic decay       x: periods              y: mean()#???
# alpha auto cor x: periods              y: mean(corr time series)
# risk expo1     x: each risk            y: mean(cor with risk expo)  z: each alpha@ret
# risk expo2     x: each alpha@ret       y: mean(cor with risk expo)  z: each risk
# monthly effect x: 12-month           ; y: mean(real return) z: each alpha@ret
# mean return,   x: alpha>0 and alpha<0; y: mean(real return) z: each alpha@ret
# hit ratio,     x: alpha>0 and alpha<0; y: Number of ret with same sign as alpha/all number;
#                z: each alpha@ret pair
