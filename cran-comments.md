# tidySEM 0.2.1

* Fix CRAN check errors related to OpenMx
* Add mx_lca(), a dedicated function for estimating mixture models with
  ordinal/binary data that avoids convergence problems when using mx_mixture()
  with those data types
* Add table_prob() to tabulate category probabilities for models with ordinal
  indicators
* Add plot_prob() to plot category probabilities for models with ordinal
  indicators

## Test environments

* local Windows 10 install, R 4.1.2
* GitHub Actions (r-lib): windows-latest (release)
* GitHub Actions (r-lib): macOS-latest (release)
* GitHub Actions (r-lib): ubuntu-20.04 (release)
* GitHub Actions (r-lib): ubuntu-20.04 (devel)
* win-builder: release: R version 4.1.2 (2021-11-01)
* win-builder: oldrelease: R version 4.0.5 (2021-03-31)
* win-builder: R Under development (unstable) (2022-02-04 r81654 ucrt)
* Rhub Fedora Linux, R-devel, clang, gfortran
* Rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Windows Server 2022, R-devel, 64 bit

## R CMD check results

1 NOTE: New submission; Package was archived on CRAN
