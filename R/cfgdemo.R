demo.fit.cfg <- list(
  univ = 'zz500',
  md.ver = 'windzf',
  mkt = 'CHINA_STOCK',
  freq = 'DAILY',
  root.dir = '~/data',
  periods = c(-2, 1, 5),
  univ.ver = '',
  grouptype= 'ALL', ## conflict with sampler group type???
  groupname= 'ALL', ## conflict with sampler group name???
  dates = list(
    ins.rule.file = '~/',
    ins.grp = 'INS1',
    sdate = 20170101,
    edate = 20180101,
    omit.days = NULL,
    max.rpt.date = 20170701,

    os.sdate = 20170901,
    os.edate = 20170801,

    oos.grp = c("INS1","INS2","OOS")
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
    ALL=list(group.type="ALL", dir.list = c("/mnt/analysis/ics/alpha/CHINA_STOCK/DAILY/zfb_smallUniv/YYYYMMDD.h5"))
  ),
  model.list   = list(
    ALL = "ALL_",
    zfb="alphazf2b"
  ),
  fit.list = list(
    alpha = list(sampler="ALL",fitter="CVNET",model="zfb")
  )
)
