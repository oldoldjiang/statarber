#' core function of optimization
#'
#' @param alpha N length numeric vector
#' @param covMatrix NxN matrix
#' @param risk list, each element is list(supThres = , infThres = , exposure = N vector)
#' @param riskLambda numeric
#' @param lastWeight N length numeric vector
#' @param maxWeightBuying non negative number or vector, max weight change when buying specific equity
#' @param maxWeightSelling non negative number or vector, max weight change when selling specific equity
#' @param maxWeightHolding number or vector, maximum weight for holding specific equity
#' @param minWeightHolding number or vector, minimum weight for holding specific equity
#' @param longSideWeight
#' @param shortSideWeight
#' @param optMode 'riskLambda' or 'quantile', if 'riskLambda', use mean-variance model to get the target
#' weight. if 'quantile', pick top (and bottom) x equity and get the averaging weight
#' @param prob numeric, if 0.1 then top (and bottom) 10% percent of equity is selected
#' @param totalTurnover numeric, sum(abs(target weight - last weight)) <= total turnover
#' @param leverageRate
#' @param verbose logical
#'
#' @return
#' @export
#'
#' @examples
optWeight <- function(alpha, covMatrix, risk, riskLambda, lastWeight,
                      maxWeightBuying = Inf, maxWeightSelling = Inf,
                      maxWeightHolding = Inf, minWeightHolding = -Inf,
                      longSideWeight = NULL, shortSideWeight = NULL,
                      optMode = 'riskLambda',prob = 0.1,
                      totalTurnover = NULL, leverageRate = NULL,
                      verbose = TRUE){

  if(optMode == 'riskLambda'){
    alpha   <- matrix(alpha)
    equityN <- dim(alpha)[1] ## ??? multi alpha???

    N <- 3 * equityN # control delta weight
    M <- equityN + 1

    controlLeverage <- shortSideWeight < -1e-7
    if(controlLeverage){
        idxL <- N
        N <- N + 2 * equityN # control abs(weight) need 2 extra variable
        M <- M + equityN
    }

    ## control cov
    if(riskLambda!=0){
      ## if riskLambda == 0, then covMatrix is useless
      which.zero <- diag(covMatrix) < 1e-7
      if(any(which.zero)){
        ## 0 in diag mostly appear when stock is suspended for the whole periods
        print('Trying to handle 0 values in covMatrix')
        ## use the average risk to estimate the suspended stocks
        diag(covMatrix)[which.zero] <- median(diag(covMatrix)[!which.zero]) * 10000
      }
      deCompMatrix <- tryCatch(Matrix::chol(covMatrix), error = function(e){
        print(e)
        NULL
      })

      if(is.null(deCompMatrix)){
        print('Trying to handle non positive definite covariance matrix')
        addon <- diag(diag(covMatrix) * 0.01)
        i <- 1
        while(is.null(deCompMatrix)){
          deCompMatrix <- tryCatch(Matrix::chol(covMatrix + addon * i * i), error = function(e){
            print(e)
            NULL
          })
          i <- i + 1
          stopifnot(i<100)
        }
      }

      EnlargeMatrix <- matrix(0, N, N)
      EnlargeMatrix[1:equityN, 1:equityN] <- deCompMatrix
    }

    ## always control turnover
    ## sum(abs(target weight - last weight)) <= total turnover
    ## target weight = last weight + delta weight
    ## == sum(abs(delta weight)) <= total turnover
    ## == -t <= delta weight <= t and sum(t) = total turnover
    EnlargeUniMatrix <- matrix(0, 1, N)
    EnlargeUniMatrix[1, (equityN + 1):(3 * equityN)] <- 1
    EnlargeUniBound  <- matrix(totalTurnover)

    ## fill alpha
    EnlargeAlpha <- matrix(0, N, 1)
    EnlargeAlpha[1:equityN] <- alpha

    ## control leverage
    if(controlLeverage){
      totalLeverageArray <- matrix(1, 1, N)
      totalLeverageArray[1:(3*equityN)] <- 0
      EnlargeUniMatrix <- rbind(EnlargeUniMatrix, totalLeverageArray)
      EnlargeUniBound  <- rbind(EnlargeUniBound,  leverageRate)
    }

    ## control risk bound
    controlRisk <- !is.null(risk) && is.list(risk)
    if(controlRisk){
      riskN <- length(risk)
      uniMatrix <- matrix(0, riskN * 2, N) # double for sup and inf of risk
      uniBound  <- matrix(0, riskN * 2, 1)
      for(i in 1:riskN){
        uniMatrix[i        , 1:equityN] <- risk[[i]]$exposure
        uniMatrix[i + riskN, 1:equityN] <- 0-risk[[i]]$exposure
        uniBound [i        , 1        ] <- risk[[i]]$supThres
        uniBound [i + riskN, 1        ] <- 0-risk[[i]]$infThres
      }
      EnlargeUniMatrix <- rbind(EnlargeUniMatrix, uniMatrix)
      EnlargeUniBound  <- rbind(EnlargeUniBound , uniBound )
    }


    EnlargeEquaMatrix <- matrix(0, M, N)
    EnlargeEquaArray  <- matrix(0, M, 1)

    ## control total weight
    ## sum(target weight) == total weight
    totalWeight <- longSideWeight + shortSideWeight
    EnlargeEquaMatrix[1, 1:equityN] <- 1
    EnlargeEquaArray[1,1] <- totalWeight

    ## control trading weight
    EnlargeEquaMatrix[1:equityN+1, 1:equityN]                 <- diag(equityN)
    EnlargeEquaMatrix[1:equityN+1, (equityN+1):(2*equityN)]   <- -diag(equityN)
    EnlargeEquaMatrix[1:equityN+1, (2*equityN+1):(3*equityN)] <- diag(equityN)
    EnlargeEquaArray[1:equityN+1] <- lastWeight
    if(controlLeverage){
      EnlargeEquaMatrix[(equityN+2):(2*equityN+1),1:equityN]                 <- diag(equityN)
      EnlargeEquaMatrix[(equityN+2):(2*equityN+1),(3*equityN+1):(4*equityN)] <- -diag(equityN)
      EnlargeEquaMatrix[(equityN+2):(2*equityN+1),(4*equityN+1):(5*equityN)] <- diag(equityN)
    }


    ## control weight bounds
    infWeight <- matrix(pmax(lastWeight - maxWeightSelling, minWeightHolding))
    if(shortSideWeight >= -1e-7){
      infWeight <- pmax(0, infWeight)
    }
    supWeight <- matrix(pmin(lastWeight + maxWeightBuying, maxWeightHolding))
    ## in case of lower bound > upper bound
    ## this may appear when last weight is in range of (min weight holding, max weight holding)
    ## because of problem of execuation in real trading
    infWeight[infWeight > supWeight] <- minWeightHolding[infWeight > supWeight]
    supWeight[infWeight > supWeight] <- maxWeightHolding[infWeight > supWeight]


    EnlargeInfWeight <- matrix(0, N, 1)
    EnlargeInfWeight[1:equityN] <- infWeight
    EnlargeSupWeight <- matrix(Inf, N, 1)
    EnlargeSupWeight[1:equityN] <- supWeight

    if(riskLambda!=0){
      if(verbose) print('using qptogrob')
      prob <- mosek_qptoprob(EnlargeMatrix, -EnlargeAlpha/riskLambda,
                             EnlargeUniMatrix, EnlargeUniBound,
                             EnlargeEquaMatrix,EnlargeEquaArray,
                             EnlargeInfWeight, EnlargeSupWeight)
    }else{
      if(verbose) print('using lptoprob')
      prob <- mosek_lptoprob(-EnlargeAlpha,
                             EnlargeUniMatrix, EnlargeUniBound,
                             EnlargeEquaMatrix,EnlargeEquaArray,
                             EnlargeInfWeight, EnlargeSupWeight)
    }

    result <- mosek(prob, list(verbose = ifelse(verbose, 4,1)))
    weight <- result$sol$itr$xx[1:equityN]
    return(list(
      weight = weight,
      status = result$sol$itr$solsta
    ))
  }else if(optMode == 'quantile'){
    ## long top N (and/ short bottom N)
    ## weight-averaged
    alpha      <- matrix(alpha)
    alphaOrder <- order(alpha)
    equityN    <- dim(alpha)[1]
    holdN      <- floor(equityN * prob) # e.g. top 10% prob = 0.1 under long only mode
    longPos    <- tail(alphaOrder, holdN)
    shortPos   <- head(alphaOrder, holdN)
    weight     <- rep(0, equityN)

    if(shortSideWeight > -1e-7){
      ## long only, just pick top N
      weight[longPos] <- leverageRate / holdN
    }else{
      ## long short, pick top N and bottom N, 2N, but half of their weight
      w <- 0.5 * leverageRate / holdN
      weight[shortPos] <- -w
      weight[longPos]  <- w
    }
    return(list(
      weight = weight,
      status = 'SOLVED'
    ))
  }else{
    stop('unsupported optimization mode')
  }

}
checkOptCfg <- function(cfg){
  cfg$optMode         <- null.replace(cfg$optMode, 'riskLambda')
  ## backward compatibility
  cfg$singleMaxWeight <- null.replace(cfg$singleMaxWeight, Inf)
  cfg$maxWeightHolding<- null.replace(cfg$maxWeightHolding, cfg$singleMaxWeight)
  cfg$minWeightHolding<- null.replace(cfg$minWeightHolding, 0-cfg$singleMaxWeight)
  cfg$totalTurnover   <- null.replace(cfg$totalTurnover, Inf)
  cfg$leverageRate    <- null.replace(cfg$leverageRate, cfg$longSideWeight - cfg$shortSideWeight)
  if(!is.null(cfg$longOnly) && cfg$longOnly == TRUE){
    cfg$longSideWeight
  }
  if(cfg$optMode == 'quantile' && is.null(cfg$prob)){
    stop('use quantile mode, please set prob')
  }
  cfg
}
optProcess <- function(cfg, verbose = TRUE){
  cfg <- checkOptCfg(cfg)
  # in case of no feasible solution, target weight will be the same as last weight
  targetWeight <- cfg$lastWeight

  result <- optWeight(cfg$alpha, cfg$covMatrix, cfg$risk, cfg$riskLambda, cfg$lastWeight,
                      cfg$maxWeightBuying, cfg$maxWeightSelling,
                      cfg$maxWeightHolding, cfg$minWeightHolding,
                      cfg$longSideWeight, cfg$shortSideWeight,
                      cfg$optMode,cfg$prob,
                      cfg$totalTurnover, cfg$leverageRate,
                      verbose)
  if(grepl('INFEASIBLE', result$status)){
    print('Warning: No feasible solution')

    ## try to loose condition and optimize again
    ## first try to loose turnover
    if(is.finite(cfg$totalTurnover)){
      cfg$totalTurnover = cfg$totalTurnover * 10
      print('Trying to loose turnover')
      result <- optWeight(cfg$alpha, cfg$covMatrix, cfg$risk, cfg$riskLambda, cfg$lastWeight,
                          cfg$maxWeightBuying, cfg$maxWeightSelling,
                          cfg$maxWeightHolding, cfg$minWeightHolding,
                          cfg$longSideWeight, cfg$shortSideWeight,
                          cfg$optMode,cfg$prob,
                          cfg$totalTurnover, cfg$leverageRate,
                          verbose)

      if(!grepl('INFEASIBLE', result$status)){
        ## still control the turnover, thus not the optimal solution
        ## but we want to reduce the cost
        targetWeight <- cfg$lastWeight + 0.1*(result$weight - cfg$lastWeight)
      }else{
        ## only loose once
        print('Error: No feasible solution')
      }
    }else{
      stop('set loose condition')
    }
  }else{
    targetWeight <- result$weight
  }
  print(paste('Turnover would be'))
  targetWeight
}
