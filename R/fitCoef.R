INTERCEPT <- 'Intercept'
fit.coef.OLS <- function(data, xy.spec, pa.list = list()){
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


fitBeta <- function(sdate, edate, sampler.dir, model.cfg,
                    ins.days, mode = 'ALL',
                    y.vars,
                    wgt.var = NULL,
                    ignore.empty.file = TRUE,
                    fit.func = fit.coef.cvnet, fit.para = list(),
                    use.cache = TRUE,
                    cache.coef.ptn = NULL,
                    coef.max = Inf, coef.min = -Inf,
                    cors = 1, verbose = TRUE){
  if(is.character(fit.func)){
    fit.func = switch(fit.func,
                      CVNET = fit.coef.cvnet,
                      OLS   = fit.coef.OLS,
                      LM    = fit.coef.LM,
                      NNLS  = fit.coef.NNLS,
                      stop(paste('unsupported fit function')))

  }else if( !is.function(fit.func)){
    stop('Invalid fit function type')
  }

  library(doMC)
  doMC::registerDoMC(cores = cores)

  tradingDays <- getTradingDayRange(sdate, edate)
  ins.tradingDays <- trading####

  ## TODO: add support for other mode
  if(mode == 'ALL'){
    sampled = NULL
    cache.file <- file.path(sampler.dir, paste0('cache.RAW.', sdate,'.to.',edate,
                                                '.N.',length(ins.tradingDays),
                                                '.rds'))
    if(use.cache){ ### why use cache and readRDS???
      if(file.exists(paste0(cache.file, '.end')) && file.exists(cache.file)){
        sampled <- readRDS(cache.file)
      }
    }

    if(is.null(sampled)){
      data.group <- "" ############################################################
    }
  }
}
gen.alp.on.coef <- function(sdate, edate, coef, term.path, model,
                            group.name, alpha.path, alpha.name = model,
                            lag = 1, mkt = 'CHINA_STOCK', cores = 1,
                            alpha.only = TRUE, verbose = TRUE, use.cache = TRUE,
                            fit.para = list()){
  library(doMC)
  doMC::registerDoMC(cores = cores)

  if(length(group.name)!=1 || length(model)!=1) stop('only support one group or model')
  if(alpha.only && sdate != edate){
    use.cache = FALSE
    stop('only support one day call when alpha.only = TRUE')
  }

  coef = coef[,,,,model, drop = FALSE]

  tradingday <- getTradingDayRange(sdate, edate)
  if(!is.null(include.days)) tradingday <- tradingday[tradingday %in% include.days]

  alpha.path <- sub('MODELNAME',model, alpha.path)
  alpha.path <- sub('GROUPNAME', group.name, alpha.path)

  alpha.name <- sub("(fwd)\\.(Ret)\\.(.*)\\.(.*)",
                    paste0(alpha.name, ".\\3.\\4"),
                    dimnames(coef)[[4]])

  readOneCache <- FALSE

  order <- null.replace(fit.para$order, 1)
  alpha.files <- llply(tradingday, function(d){
    alpha.file <- sub('YYYYMMDD', d, alpha.path)
    if(!alpha.only && use.cache && file.exists(alpha.file)) return(NULL)

    term.file <- sub('YYYYMMDD', d, term.path)

    if(any(!file.exists(term.file))){
      print(paste('WARN: term file does not exist', term.file[!file.exists(term.file)]))
      if(alpha.only) stop('Cannot load term to generate alpha')
      return(NULL)
    }

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
      }else if(gsub){
        common.keys <- intersect()
      }else{
        beta <- adrop(coef[as.character(coefd), group.name, -1,, model, drop = FALSE], c(1,2,5))
        alpha <- KV[,rownames(beta), drop = FALSE] %*% beta
      }
    }, .parallel = cores > 1, .drop = FALSE), c(3,1,2,4))

    dimnames(alpha.v)[[4]] <- alpha.name#s
    names(dimnames(alpha.v)) <- c('K','D','T','V')
    if(alpha.only | onecache.only) return(alpha.v)
    dir.create(dirname(alpha.file), FALSE, TRUE)
    panel.write(alpha.v, alpha.file, verbose = verbose, overwrite = !use.cache)
    alpha.file
  }, .parallel = cores > 1)

  if(alpha.only){
    alpha.files[[1]]
  }else{
    if(onecache.only){
      alpha.combine <- panel.combine(alpha.files)
      dir.create(dirname(alpha.path), FALSE, TRUE)
      panel.write(alpha.combine, file.path(dirname(alpha.path)))
    }
  }
}
