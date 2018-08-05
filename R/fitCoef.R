INTERCEPT <- 'Intercept'

fit.coef.OLS <- function(data, xy.spec, d = '19911231', group.name = '00:00:00.000',
                         pa.list = list()){
  x.vars   <- xy.spec$x.vars
  y.vars   <- xy.spec$y.vars
  wgt.var <- xy.spec$wgt.var

  cnames <- dimnames(data)[[2]]

  ## check names
  if(!all(c(x.vars, y.vars, wgt.var) %in% cnames)){
    stop(paste('fitting vars not cmplete',
               paste(setdiff(c(x.vars, y.vars, wgt.var),cnames),
                     collapse = ',')))
  }

  # whether to set intercept in regression
  intercept <- !!as.integer(null.replace(pa.list$intercept, 0)) ## TRUE or FALSE

  data[is.na(data) | is.infinite(data)] <- 0 ## ???

  ## fit as simple regression
  y <- data[, y.vars, drop = FALSE]
  x <- data[, x.vars, drop = FALSE]
  x <- cbind(rep(1, dim(x)[1]), x)
  colnames(x)[1] <- INTERCEPT
  if(is.null(wgt.var)){
    w <- rep(1, dim(x)[1])
  }else{
    w <- data[, wgt.var, drop = FALSE]
  }

  xx <- crossprod(x, x * w)
  xy <- crossprod(x, y * w)

  nonzero.x <- which(abs(apply(xx, 2, sum, na.rm = TRUE)) > 1e-7)
  ## exclude zero.x to do solve and add back
  nonzero.xx <- xx[nonzero.x, nonzero.x, drop = FALSE]
  nonzero.xy <- xy[nonzero.x,,drop = FALSE]

  ridge <- null.replace(pa.list$RIDGE, 1e-7)
  nonzero.coefs <- crossprod(solve(nonzero.xx + diag(dim(nonzero.xx)[1]) * ridge), nonzero.xy)

  coefs <- xy * 0
  coefs[nonzero.x, ] <- nonzero.coefs

  dim(coefs) <- c(dim(coefs), 1, 1)
  dimnames(coefs) <- list(c(INTERCEPT , x.vars), y.vars, d, group.name)
  coefs <- aperm(coefs, c(3,4,1,2))
  names(dimnames(coefs)) <- c('D','G','X','Y')
  coefs
}

fit.coef.LM <- function(data, xy.spec, d = '19911231', group.name = '00:00:00.000',
                        pa.list = list()){
  x.vars   <- xy.spec$x.vars
  y.vars   <- xy.spec$y.vars
  wgt.var <- xy.spec$wgt.var

  cnames <- dimnames(data)[[2]]

  ## check names
  if(!all(c(x.vars, y.vars, wgt.var) %in% cnames)){
    stop(paste('fitting vars not cmplete',
               paste(setdiff(c(x.vars, y.vars, wgt.var),cnames),
                     collapse = ',')))
  }

  intercept <- !!as.integer(null.replace(pa.list$intercept, 0)) ## TRUE or FALSE

  data[is.na(data) | is.infinite(data)] <- 0 ## ???

  ## fit as simple regression
  y <- data[, y.vars, drop = FALSE]
  x <- data[, x.vars, drop = FALSE]
  x <- cbind(rep(1, dim(x)[1]), x)
  colnames(x)[1] <- INTERCEPT
  if(is.null(wgt.var)){
    w <- rep(1, dim(x)[1])
  }else{
    w <- data[, wgt.var, drop = FALSE]
  }

  coefs <- t(t(coef(lm(y~0+x, weights = w))))
  coefs[is.na(coefs)] <- 0

  dim(coefs) <- c(dim(coefs), 1, 1)
  dimnames(coefs) <- list(c(INTERCEPT, x.vars), y.vars, d, group.name)
  coefs <- aperm(coefs, c(3,4,1,2))
  names(dimnames(coefs)) <- c('D','G','X','Y')
  coefs
}

fit.coef.NNLS <- function(data, xy.spec, d = '19911231', group.name = '00:00:00.000',
                          pa.list = list()){
  x.vars   <- xy.spec$x.vars
  y.vars   <- xy.spec$y.vars
  wgt.var <- xy.spec$wgt.var

  cnames <- dimnames(data)[[2]]

  ## check names
  if(!all(c(x.vars, y.vars, wgt.var) %in% cnames)){
    stop(paste('fitting vars not cmplete',
               paste(setdiff(c(x.vars, y.vars, wgt.var),cnames),
                     collapse = ',')))
  }

  intercept <- !!as.integer(null.replace(pa.list$intercept, 0)) ## TRUE or FALSE

  data[is.na(data) | is.infinite(data)] <- 0 ## ???

  ## fit as simple regression
  y <- data[, y.vars, drop = FALSE]
  x <- data[, x.vars, drop = FALSE]
  x <- cbind(rep(1, dim(x)[1]), x)
  colnames(x)[1] <- INTERCEPT
  if(is.null(wgt.var)){
    w <- rep(1, dim(x)[1])
  }else{
    w <- data[, wgt.var, drop = FALSE]
  }

  coefs <- t(t(coef(nnls(x*w, y*w))))
  dim(coefs) <- c(dim(coefs), 1, 1)
  dimnames(coefs) <- list(c(INTERCEPT, x.vars), y.vars, d, group.name)
  coefs <- aperm(coefs, c(3,4,1,2))
  names(dimnames(coefs)) <- c('D','G','X','Y')
  coefs
}
fit.coef.cvnet <- function(data, xy.spec, d = '19911231', group.name = '00:00:00.000',
                           pa.list = list(MODE = 'FP', IDEAL_FP = 1.02), cores = 1){
  if(cores>1){
    registerDoMC(cores = cores)
  }

  # flexibility rate
  ideal.fr <- as.numeric(null.replace(pa.list$IDEAL_FR, 1.02))
  x.vars   <- xy.spec$x.vars
  y.vars   <- xy.spec$y.vars
  wgt.var <- xy.spec$wgt.var

  cnames <- dimnames(data)[[2]]

  ## check names
  if(!all(c(x.vars, y.vars, wgt.var) %in% cnames)){
    stop(paste('fitting vars not cmplete',
               paste(setdiff(c(x.vars, y.vars, wgt.var),cnames),
                     collapse = ',')))
  }

  intercept <- !!as.integer(null.replace(pa.list$intercept, 0)) ## TRUE or FALSE

  if(length(x.vars) == 1){
    coefs <- fit.coef.OLS(data, xy.spec = xy.spec, d = d, group.name = group.name,
                          pa.list = list(INTERCEPT=intercept))
  }else{
    coefs <- laply(y.vars, function(y.var){
      y <- data[, y.var, drop = FALSE]
      x <- data[, x.vars, drop = FALSE]
      x <- cbind(rep(1, dim(x)[1]), x)
      colnames(x)[1] <- INTERCEPT
      if(is.null(wgt.var)){
        w <- t(t(rep(1, dim(x)[1])))
      }else{
        w <- data[, wgt.var, drop = FALSE]
      }

      x[is.na(x)|is.infinite(x)] <- 0
      y[is.na(x)|is.infinite(x)] <- 0
      w[is.na(x)|is.infinite(x)] <- 0

      cvfit <- cv.glmnet(x = x, y = y, weights = w, parallel = cores > 1, intercept = intercept)
      coefm <- as.matrix(coef(cvfit, s = cvfit$lambda))
      dimnames(coefm)[[1]][1] <- INTERCEPT
      a <- data[, x.vars, drop = FALSE] %*% coefm[-1,]
      fr <- cov(y, a, use = 'pair') / apply(a, 2, var)
      fr.idx <- which.min(abs(fr - ideal.fr))
      lmin.idx <- which.min(abs(cvfit$lambda - cvfit$lambda.min))
      coefm[,min(fr.idx, lmin.idx), drop = FALSE]
    }, .drop = FALSE)

    dimnames(coefs)[[1]] <- y.vars
    dim2 <- dimnames(coefs)[[2]]
    dim(coefs) <- c(dim(coefs),1)
    dimnames(coefs) <- list(y.vars, dim2, d, group.name)
    coefs <- aperm(coefs, c(3,4,2,1))
    names(dimnames(coefs)) <- c('D','G','X','Y')
  }
  coefs
}


fitBeta <- function(dates, sampler.dir,model.cfg=list(ALL="ALL_"),
                    mode="ALL",
                    y.vars = c("fwd.Ret.DAILY.1","fwd.Ret.DAILY.5"),wgt.var=NULL,
                    ignore.empty.file=TRUE,
                    fit.func = fit.coef.cvnet, fit.para = list(), use.cache=TRUE, cache.coef.ptn=NULL,
                    coef.max=Inf, coef.min=-Inf,cores=1,verbose=TRUE){

  mode = match.arg(mode,choices = c('ALL','ROLL'))
  if( is.character(fit.func)){
    fit.func <- toupper(fit.func)
    fit.func = switch(fit.func,
                      CVNET=fit.coef.cvnet,
                      OLS = fit.coef.OLS,
                      LM = fit.coef.LM,
                      NNLS = fit.coef.NNLS,
                      stop(paste("Unsupported fit method name",fut.func)) )
  }else if( !is.function(fit.func)){
    stop("Invalid fit func type!")
  }


  if(cores>1){
    library(doMC)
    doMC::registerDoMC(cores = cores)
  }

  N = length(dates)

  if(mode=="ALL"){

    sampled=NULL
    cache.file = file.path(sampler.dir, paste("cache.RAW.",dates[1],".to.",dates[N],".N",N,".rds",sep=""))
    if(use.cache){
      if(file.exists(paste(cache.file,".end",sep="")) && file.exists(cache.file)){
        sampled=readRDS(cache.file)
      }
    }

    if(is.null(sampled)){
      data.g = llply(dates, function(d){
        sampled = sample.read(file =  file.path(sampler.dir,paste0('RAW.',d,'.rds')),
                              ignore.empty.file = ignore.empty.file,
                              verbose = verbose)
      },.parallel=cores>1)
      which.null = unlist(lapply(data.g, is.null))
      data.g = data.g[!which.null]

      sampled = llply(names(data.g[[1]]), function(g){
        do.call(rbind, lapply(data.g, function(dd) dd[[g]]) )
      })
      names(sampled) = names(data.g[[1]])

      ## save cache
      if(use.cache){
        dir.create(dirname(cache.file),FALSE,TRUE)
        saveRDS(sampled, cache.file)
        file.create(paste(cache.file,".end",sep=""))
      }
    }
    coef.d = panel.combine(llply(names(model.cfg),function(model){
      coefs.g = abind(llply(names(sampled),function(g){
        fitdata = sampled[[g]]
        fitdata = fitdata[,sapply(fitdata, is.numeric),with = FALSE]


        fitdata = as.matrix(fitdata)
        x.vars = model.cfg[[model]]
        print(x.vars)
        if(is.null(x.vars)){
          x.vars = model
        }else if(identical(x.vars,"ALL_")){
          x.vars = setdiff(dimnames(fitdata)[[2]],c(y.vars,wgt.var) )
        }else if(!is.null(x.vars) && length(x.vars)==1 && length(grep("\\*",x.vars))>0){
          x.vars = setdiff(grep(x.vars,dimnames(fitdata)[[2]],value=TRUE),c(y.vars,wgt.var))
        }

        ## handle high order condition
        order = null.replace(fit.para$order,1)
        if(length(x.vars)>1 & order>1) stop("high order fitting is only compatible with 1 independent variable")

        if(order > 1){
          new.fitdata.list <- lapply(2:order,function(o){
            fitdata[,x.vars,drop=FALSE]^o
          })
          new.fitdata <- do.call(cbind,new.fitdata.list)
          colnames(new.fitdata) <- paste0(x.vars,".",2:order)
          fitdata <- cbind(fitdata,new.fitdata)
          x.vars <- c(x.vars,paste0(x.vars,".",2:order))
        }

        xy.spec=list(x.vars = x.vars, y.vars = y.vars)
        coefs = fit.func(fitdata,xy.spec,d="19991231",group.name=g,pa.list=fit.para)
      }),along=2)
      names(dimnames(coefs.g)) = c("D","G","X","Y")
      coefs.g = panel.add.dim(coefs.g,"M",model)
    }),default=0)
  }else if(mode == "ROLL"){
    cat("Fitting")
    cache.set = !is.null(cache.coef.ptn)
    use.cache = use.cache && cache.set

    coef.d = abind(llply(dates, function(d){
      cat(d)
      cat(" ")
      coef.out.file = sub("YYYYMMDD",d,cache.coef.ptn)

      if( use.cache && file.exists(coef.out.file)){
        return(panel.read(coef.out.file, verbose = verbose))
      }

      sampled = sample.read(file = file.path(sampler.dir,paste0('RAW.',d,'.rds')),
                            ignore.empty.file=ignore.empty.file,
                            verbose = verbose)
      coefs.m = panel.combine(llply(names(model.cfg),function(model){
        coefs.g = abind(llply(names(sampled),function(g){
          fitdata = sampled[[g]]
          fitdata = fitdata[,sapply(fitdata,is.numeric),with = FALSE]
          fitdata = as.matrix(fitdata)
          x.vars = model.cfg[[model]]
          if(is.null(x.vars) || identical(x.vars,"ALL_")){
            x.vars = setdiff( dimnames(fitdata)[[2]],c(y.vars,wgt.var))
          }else if(!is.null(x.vars) && length(x.vars)==1 && length(grep("\\*",x.vars))>0) x.vars = setdiff(grep(x.vars,dimnames(fitdata)[[2]],value=TRUE),c(y.vars,wgt.var))
          xy.spec=list(x.vars=x.vars, y.vars = y.vars)
          coefs = fit.func(fitdata,xy.spec,d=d,group.name=g,pa.list=fit.para)
        }),along=2)
        names(dimnames(coefs.g))=c("D","G","X","Y")
        coefs.g = panel.add.dim(coefs.g,"M",model)
      }),default = 0)

      if(cache.set){
        dir.create(dirname(coef.out.file),FALSE,TRUE)
        panel.write(coefs.m, coef.out.file, verbose = verbose)
      }
      coefs.m
    }, .parallel = cores>1),along=1)
    cat("\n")

    if(any(is.na(coef.d))) stop("Coef error")

    if( !is.null(fit.para$WINDOW)){
      W = as.integer(fit.para$WINDOW)
      HL = as.integer(fit.para$HL)
      if(!is.null(HL) & length(HL)!=0){
        W = (0.5)^((1:W)/HL)
        w = w/sum(w)
      }else{
        w = rep(1/W,W)
      }
      print(paste0("smoothing ",W," HL=",HL))
      pad = rep(0,W-1)

      ## apply window average and set leading NA to zero
      coef.d.new = aperm(aaply(coef.d, 2:5, function(x){fiter(c(pad,x),w,sides=1)[-(1:(W-1))]},.drop=FALSE),c(5,1:4))
      dimnames(coef.d.new)[[1]]=dimnames(coef.d)[[1]]
      names(dimnames(coef.d.new)) = names(dimnames(coef.d))
      coef.d = coef.d.new
    }
    if(!is.null(fit.para$NDAY)){
      step = max(1,as.integer(fit.para$NDAY))
      print(paste0("stepping ",step))
      coef.d = coef.d[seq(1,dim(coef.d)[1],by=step),,,,,drop=FALSE]
    }
  }else{
    stop(paste("mode not supported",mode))
  }
  names(dimnames(coef.d)) = c("D","G","X","Y","M")
  coef.d[coef.d>coef.max] = coef.max
  coef.d[coef.d<coef.min] = coef.min
  return(coef.d)
}

#' generate alpha * regression coef
#'
#' @param tradingday
#' @param coef
#' @param term.path
#' @param model
#' @param group.name
#' @param alpha.path
#' @param alpha.name
#' @param lag
#' @param mkt
#' @param cores
#' @param alpha.only
#' @param verbose
#' @param use.cache
#' @param fit.para
#'
#' @return
#' @export
#'
#' @examples
gen.alp.on.coef <- function(dates, coef, term.path, model,
                            group.name, alpha.path, alpha.name = model,
                            lag = 1, mkt = 'CHINA_STOCK', cores = 1,
                            alpha.only = TRUE, verbose = TRUE, use.cache = TRUE,
                            fit.para = list()){
  if(cores>1){
    library(doMC)
    doMC::registerDoMC(cores = cores)
  }


  if(length(group.name)!=1 || length(model)!=1) stop('only support one group or model')
  if(alpha.only && length(dates)>1){
    use.cache = FALSE
    stop('only support one day call when alpha.only = TRUE')
  }

  coef = coef[,,,,model, drop = FALSE] #c("Date","Group","X","Y","Model")

  alpha.path <- sub('MODELNAME',model, alpha.path)
  alpha.path <- sub('GROUPNAME', group.name, alpha.path)

  alpha.names <- sub("(fwd)\\.(Ret)\\.(.*)\\.(.*)",
                    paste0(alpha.name, ".\\3.\\4"),
                    dimnames(coef)[[4]])

  readOneCache <- FALSE

  order <- null.replace(fit.para$order, 1)

  alpha.files <- llply(dates, function(d){
    alpha.file <- sub('YYYYMMDD', d, alpha.path)
    if(verbose) print(paste('gen alpha on coef:',alpha.file))
    if(!alpha.only && use.cache && file.exists(alpha.file)) return(NULL)

    term.file <- sub('YYYYMMDD', d, term.path)

    if(any(!file.exists(term.file))){
      print(paste('WARN: term file does not exist', term.file[!file.exists(term.file)]))
      if(alpha.only) stop('Cannot load term to generate alpha')
      return(NULL)
    }

    terms = panel.read(file = term.file,verbose = verbose)
    terms[is.na(terms)] <- 0

    coefd <- as.integer(dimnames(coef)[[1]])
    coefd <- coefd[max(1, which(coefd <= d) -lag)]
    if(gsub('\\d+','', group.name)=='G'){

    }
    if(order > 1 & length(alpha.name)==1){
      new.term.list = llply(2:order, function(o){
        tmp <- terms^o
        dimnames(tmp)[[4]] <- paste0(dimnames(terms)[[4]],'.',o)
        tmp
      })
      temrs <- panel.combine(append(list(terms), new.term.list))
    }else if(order>1 & length(alpha.name)!=1){
      stop('high order mode is only compatible with 1 dependant varialbe')
    }

    alpha.v <- aperm(aaply(terms, 2:3, function(KV){
      if(is.null(dim(KV))){
        KV <- t(t(KV))
        dimnames(KV)[[2]] <- dimnames(terms)[[4]]
        dimnames(KV)[[1]] <- dimnames(terms)[[1]]
      }

      if(group.name == 'EACH'){
        common.keys <- intersect(dimnames(KV)[[1]], dimnames(coef)[['G']])
        alpha <- abind(llply(dimnames(coef)$Y, function(y){
          alpha.y <- aaply(KV[common.keys, dimnames(coef)$X[-1] * adrop(coef[as.character(coefd), common.keys, -1, y, model, drop = FALSE])])
        }))
      }else if(group.name == 'ALL'){
        beta <- adrop(coef[as.character(coefd), group.name, -1,, model, drop = FALSE], c(1,2,5))
        alpha <- KV[,rownames(beta), drop = FALSE] %*% beta
      }else if(gsub){
        common.keys <- intersect()
      }
    }, .parallel = cores > 1, .drop = FALSE), c(3,1,2,4))

    dimnames(alpha.v)[[4]] <- alpha.names
    names(dimnames(alpha.v)) <- c('K','D','T','V')
    if(alpha.only) return(alpha.v)
    dir.create(dirname(alpha.file), FALSE, TRUE)
    panel.write(alpha.v, alpha.file, verbose = verbose, overwrite = !use.cache)
    alpha.file
  }, .parallel = cores > 1)

  if(alpha.only){
    alpha.files[[1]]
  }else{
    alpha.names
  }
}

#' generate date range for fitting
#'
#' @param sdate start date e.g. 20180101
#' @param edate end date e.g. 20180401
#' @param include.date date vector, date range not in include.date is excluded
#' @param only.include.odd.month logical for convinient to generate include.date, default FALSE
#' @param only.include.even.month logical, default FALSE
#'
#' @return
#' @export
#'
#' @examples
#' dr <- fit.daterange.helper(sdate = 20180101,edate = 20180601)
#' dr <- fit.daterange.helper(sdate = 20180101,edate = 20180601, only.include.odd.month = TRUE)
fit.daterange.helper <- function(sdate, edate, include.date = NULL,
                                 only.include.odd.month     = FALSE,
                                 only.include.even.month    = FALSE){

  if(only.include.odd.month && only.include.even.month)
    stop('only.include.odd.month only.include.even.month can not both be TRUE')

  tradingday <- getTradingDayRange(sdate, edate)
  if(only.include.even.month && !only.include.odd.month){
    include.date <- tradingday[tradingday%/%100%%2==0]
  }else if(!only.include.even.month && only.include.odd.month){
    include.date <- tradingday[tradingday%/%100%%2==1]
  }

  if(!is.null(include.date)) tradingday <- tradingday[tradingday %in% include.date]
  tradingday
}
