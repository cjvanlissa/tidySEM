
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidySEM <a href='https://github.com/cjvanlissa/tidySEM'><img src='https://github.com/cjvanlissa/tidySEM/raw/master/docs/badge.png' align="right" height="139" /></a>

[![CRAN
status](https://www.r-pkg.org/badges/version/tidySEM)](https://cran.r-project.org/package=tidySEM)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/tidySEM?color=blue)](https://r-pkg.org/pkg/tidySEM)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![R-CMD-check](https://github.com/cjvanlissa/tidySEM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cjvanlissa/tidySEM/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/cjvanlissa/tidySEM/branch/master/graph/badge.svg)](https://app.codecov.io/gh/cjvanlissa/tidySEM?branch=master)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](https://www.contributor-covenant.org/version/2/0/code_of_conduct.html)
<!--[![Code of Merit](https://img.shields.io/badge/Code%20%20of%20Merit-adopted-ff69b4.svg)](https://codeofmerit.org/CODE_OF_CONDUCT.md)-->

The package `tidySEM` provides a ‘tidy’ workflow for conducting,
reporting, and plotting structural equation modeling analyses. It does
not perform model estimation, but instead allows users to estimate
models in a software-agnostic way, using either the free open source R
packages `lavaan` or `OpenMx`, or the commercial closed-source program
`Mplus` (controlled through the R package `MplusAutomation`). The aim of
`tidySEM` is to provide three specific functions:

1.  Generate model syntax in a top-down, tidy way,
    - With particular attention to specifying mixture models in `OpenMx`
2.  Tabulate model output in a publication-ready, uniform manner,
3.  Make easily customizable graphs for SEM-models.

These functions are designed with the [*tidy tools manifesto* (Wickham,
last updated
23-11-2019)](https://tidyverse.tidyverse.org/articles/manifesto.html) in
mind, and interface with the existing suite of packages in the
[`tidyverse`](https://tidyverse.tidyverse.org/).

## Installation

You can install the public release of `tidySEM` from CRAN with:

``` r
install.packages("tidySEM")
```

If you want access to the latest developer version, including features
that may not yet be finalized, you can install the development version
of `tidySEM` from R-universe with:

``` r
install.packages('tidySEM',
                 repos = c('https://cjvanlissa.r-universe.dev',
                           'https://cloud.r-project.org'))
```

And if you are a developer and want to contribute to `tidySEM`, or want
to install a specific historical version of the package, you can install
directly from GitHub with:

``` r
install.packages("remotes")
remotes::install_github('cjvanlissa/tidySEM')
```

## Documentation

Every user-facing function in the package is documented, and the
documentation can be accessed by running `?function_name` in the R
console, e.g., `?graph_sem`.

Furthermore, there are three main vignettes, describing the three main
tracks of `tidySEM` functions:

1.  A [vignette about generating syntax and estimating
    models](https://cjvanlissa.github.io/tidySEM/articles/Generating_syntax.html)
2.  A [vignette about tabulating
    results](https://cjvanlissa.github.io/tidySEM/articles/Tabulating_results.html)
3.  A [vignette about making
    graphs](https://cjvanlissa.github.io/tidySEM/articles/Plotting_graphs.html)
    - An additional vignette describes the [graphing
      conventions](https://cjvanlissa.github.io/tidySEM/articles/sem_graph.html)
      for structural equation models.

## Citing tidySEM

You can cite the R-package with the following citation:

> Van Lissa, C. J., (2019). *tidySEM: Tidy structural equation
> modeling.* R package version 0.2.1.
> <https://github.com/cjvanlissa/tidySEM/>

## Contributing and Contact Information

If you have ideas, please get involved. You can contribute by opening an
issue on ‘GitHub’, or sending a pull request with proposed features (see
further instructions below).

- File a ‘GitHub’ issue [here](https://github.com/cjvanlissa/tidySEM)
- Make a pull request
  [here](https://github.com/cjvanlissa/tidySEM/pulls)

By participating in this project, you agree to abide by the [Contributor
Covenant](https://www.contributor-covenant.org/version/2/0/code_of_conduct.html).

### Pull requests

- Please always contribute via pull request instead of committing to
  master, so I can review the contribution.
  - You can either fork the repository to your own account and then
    submit a pull request, or make minor changes directly on this
    repository, and choose to commit them to a different branch and send
    a pull request when you save those changes
- If you fix an issue, in your Commit message, please write “closes
  \#issuenumber” so we can trace which commits fixed which issues
- Please add a line to NEWS.md for each bug fix or feature contribution
- Please increment the minor version number in DESCRIPTION, e.g.:
  0.2.3.1 -\> 0.2.3.2
