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
      data.group <- "" ###########################################################
    }
  }
}
