# tidySEM 0.2.4

* Fixed bug in plot_profiles() when covariances are free
* Improved starting values for mixture models with free covariances (check and 
  fix non-positive definite vcov matrices)
* Documentation incorrectly referred to data_mixture_ordinal, instead of
  data_mix_ordinal
* Added legend to plot_probs()

## Test environments

* local Windows 10 install, R 4.1.2
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

OK
