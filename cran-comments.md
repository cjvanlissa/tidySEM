# tidySEM 0.2.4

* Update maintainer email address
* Fixed bug in `plot_profiles()` when covariances are free
* Improved starting values for latent class analysis
* Documentation incorrectly referred to `data_mixture_ordinal`, instead of
  `data_mix_ordinal`
* Added legend to `plot_probs()`
* Added `np_ratio` and `np_local` to `table_fit()` output for mixture models
* Reimplement `BLRT()` to avoid slow OpenMx implementation
* Add functions `lr_test()` and `lr_lmr()`

## Test environments

* local Windows 10 install, R 4.1.2
* local Windows 11 install, R 4.2.2
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

1 NOTE:

* New maintainer: Caspar J. van Lissa <c.j.vanlissa@tilburguniversity.edu>
  + I have updated my email address because my affiliation changed.
