# statarber

[![github-ci](https://github.com/oldoldjiang/statarber/badges/master/build.svg)](https://github.com/oldoldjiang/statarber/builds)
[![codecov](https://codecov.io/gl/oldoldjiang/statarber/branch/HEAD/graph/badge.svg?token=iyITozaXcF)](https://codecov.io/gl/oldoldjiang/statarber)

Statistic Arbitrage Strategy Research Toolbox

## Install


```r
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
install.packages("devtools")
devtools::install_git("https://github.com/oldoldjiang/statarber.git", 
  credentials = git2r::cred_user_pass("username", "password"))
```
ls::build()
```
