fit.cfg <- list(
  univ = 'zz500',
  md.ver = 'windzf',
  mkt = 'CHINA_STOCK',
  freq = 'DAILY',
  root.dir = '~/',
  periods = c(-2, 1, 5),
  grp.type= 'ALL', ## conflict with sampler group type???
  grp.name= 'ALL', ## conflict with sampler group name???
  dates = list(
    ins.rule.file = '~/',
    ins.grp = 'INS1',
    sdate = 20170101,
    edate = 20180101,
    omit.days = NULL,
    max.rpt.date = 20170701,

    os.sdate = 20170901,
    os.edate = 20170801,

    oos.grp = 'INS2'
  ),
  fitter.list  = list(

  ),
  sampler.list = list(
    s1 = ""
  ),
  model.list   = list(

  ),
  fit.list = list(

  )
)
