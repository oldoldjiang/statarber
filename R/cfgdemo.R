demo.fit.cfg <- list(
  univ = 'sh',
  mkt  = 'CHINA_STOCK',
  freq = 'DAILY',
  rt.pattern   = 'fwd.Ret.DAILY.N',
  root.dir = '~/data',
  report.dir = '~/report/', # where to save report and fitted alpha
  periods = c(1, 5),
  focus.period = 5,

  dates = list(
    ins.rule.file = '~/data/dates/fitdate',
    ins.group = 'INS1',
    sdate = 20151102,
    edate = 20160201,
    omit.days = NULL,

    os.sdate = 20160202,
    os.edate = 20160301,

    oos.group = c("INS1","INS2","OOS")
  ),
  fitter.list  = list(
    OLS = list(mode="ALL",fit.func="OLS"),
    LM = list(mode="ALL",fit.func="LM"),
    CVNET=list(mode="ALL",fit.func="CVNET"),
    NNLS = list(mode="ALL",fit.func="NNLS"),
    OLSROLL63 = list(mode="ROLL",fit.func="OLS",WINDOW=63),
    CVNETROLL = list(mode = "ROLL", fit.func = "CVNET", WINDOW = 63),
    CVNETROLL100HL30N1=list(mode="ROLL",fit.func="CVNET",WINDOW=100,HL=30,NDAY=1)
  ),
  sampler.list = list(
    ALL=list(group.type="ALL", group.name = 'ALL',
             dir.list = c("~/data/alpha/CHINA_STOCK/DAILY/f1/","~/data/alpha/CHINA_STOCK/DAILY/f2/"),
             quantile.var = NULL, quantile.range = NULL,
             univ.set = NULL, time.set = NULL)
  ),
  model.list   = list(
    ALL = "ALL_",
    test=c("f1","f2")
  ),
  fit.list = list(
    fit.test = list(sampler="ALL",fitter="CVNET",model="test")
  )
)
