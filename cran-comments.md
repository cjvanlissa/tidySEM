# tidySEM 0.2.10

* Fix GitGub issue #116: lavaan is deprecating lavaan:::vnames, replaced with
  lavaan::lavNames()
* Fix GitHub issue #115: Wald tests resulted in empty data.frame when progress
  was not loaded.
* Vignettes should show that OpenMx must be loaded in order to run functions
  that depend on it (i.e., latent class analyses)

## Test environments

* local Windows 10 x64, R 4.4.3
* local Ubuntu 22.2, R 4.5.0
* GitHub Actions (r-lib): windows-latest (release)
* GitHub Actions (r-lib): macOS-latest (release)
* GitHub Actions (r-lib): ubuntu-20.04 (release)
* GitHub Actions (r-lib): ubuntu-20.04 (devel)
* win-builder: release
* win-builder: oldrelease
* win-builder: development
* Rhub Fedora Linux, R-devel, clang, gfortran
* Rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Windows Server 2022, R-devel, 64 bit

## R CMD check results

0 errors | 0 warnings | 0 notes
