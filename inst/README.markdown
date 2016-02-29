
# parr

> Easy parallel programming in R

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Linux Build Status](https://travis-ci.org/gaborcsardi/parr.svg?branch=master)](https://travis-ci.org/gaborcsardi/parr)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/gaborcsardi/parr?svg=true)](https://ci.appveyor.com/project/gaborcsardi/parr)
[![](http://www.r-pkg.org/badges/version/parr)](http://www.r-pkg.org/pkg/parr)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/parr)](http://www.r-pkg.org/pkg/parr)
[![Coverage Status](https://img.shields.io/codecov/c/github/gaborcsardi/parr/master.svg)](https://codecov.io/github/gaborcsardi/parr?branch=master)

Evaluate the specified expressions in parallel, using a very easy API.

## Installation

```r
devtools::install_github("gaborcsardi/parr")
```

## Usage

```r
library(parr)
```

In the simplest form, just list your expressions to run in parallel,
and assign their results to variables. The listed expressions must
be function calls.

```r
parallel(
  result1 <- task1(param11, param12, ...),
  result2 <- task2(param21, param22, ...),
  ...
)
```

### Collecting the output

The output printed on the screen by the expressions can be collected
from the output of the `parallel()` call itself:

```r
myfun <- function() { print("hello"); 1:10 }
output <- parallel(
  result1 <- myfun(),
  result2 <- paste(letters[1:5], 1:5)
)
result1
result2
output$output
```

## License

MIT © [Gábor Csárdi](https://github.com/gaborcsardi)
