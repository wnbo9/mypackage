# mypackage
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/wnbo9/package/workflows/R-CMD-check/badge.svg)](https://github.com/wnbo9/package/actions)
  [![codecov](https://codecov.io/gh/wnbo9/mypackage/branch/main/graph/badge.svg?token=uuvdpeN6mN)](https://codecov.io/gh/wnbo9/mypackage)
  <!-- badges: end -->
  
## Overview

`mypackage` is an R package that helps users to fit linear models. It can be used to carry out linear regression and output the details of the fitting result. Furthermore, it can carry out analysis of covariance (ANOVA) and there is also a function `lm_anova_fit` to facilitate ANOVA. In `mypackage`, there is also an internal dataset `mt_cars` to help illustrate to usage of `mypackage`.

## Installation

To get a bug fix or to use a feature from the development version, you can install the development version of `mypackage` from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("wnbo9/mypackage")
```

## Usage

 ``` r
# library the mypackage
library(mypackage)

# load internal dataset
dat = mt_cars

# conduct linear regression
m1 <- lm_fit(mpg ~ cyl + disp, data = mt_cars)

# output the details of the linear regression result
lm_detail(m1)

# output the analysis of covariance results of the linear regression result
lm_anova(m1)

# an example to show how `lm_anova_fit()` works
X <- m1[["X"]]
Y <- m1[["Y"]]
adata <- cbind(Y, X)
ax = adata[,1:3]
lm_anova_fit(ax)
```

## Getting Help
If you encounter a clear bug, or you have questions and other discussion, please feel free to contact me (wnbo@umich.edu).
