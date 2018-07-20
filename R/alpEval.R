K = "Symbol"
D = "Date"
T = "Time"

calc.alpha <- function(data, alpha.names, ret.name, method = "pearson"){
  cnames <- colnames(data)
  if(any(!alpha.names %in% cnames)){
    stop(paste("alpha name not in data:", alpha.names[!alpha.names %in% cnames]))
  }
  if(!ret.name %in% cnames){
    stop(paste("return name not in data:", ret.name))
  }

  ret <- data[,lapply(alpha.names,function(an){
    cor(get(an), get(ret.name),use = "complete.obs", method = method) * sd(get(ret.name), na.rm = TRUE)
  }),by=c(D,T)]
  setnames(ret, 3:4, alpha.names)

  return(ret)
}

setFwdRet <- function(data, periods = 0){
  data[,,by = c(K)]
}

gen.report <- function(data = NULL, cfg = NULL){
  if(!is.null(data)){ # use given data

  }else{ # read data from cfg

  }
  focus.period <- cfg$focus.period

}
plot.alpha <- function(alpha.data, id, alpha.names = NULL){

  cnames <- colnames(alpha.data)
  if(is.null(alpha.names)){
    alpha.names <- cnames[!cnames %in% c(K,D,T)]
  }

  plot.data <- melt(alpha.data, id = id)
  g <- ggplot(plot.data, aes(x = as.Date(get(id),format = '%Y%m%d'), y = value, colour = variable, group = variable)) +
    geom_line() +
    scale_x_date() +
    xlab(id) +
    ylab("cummulative alpha") +
    scale_colour_hue("alpha name")
  plot(g)
}

# cumulative alpha = cumsum(std * ic)
# cumulative alpha each year
# relativea alpha
# alpha delta
# cumulative alpha use alpha as weight

# mean return quantile  x: each alpha quantile   y: mean return   z: each alpha@ret

# alpha correlation matrix: mean(cor(alpha1, alpha2) for each day) OR cor(alpha1 of whole history,
#                                                                         alpha2 of whole history)
# IC_pearson     x
# IC_spearman
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
