# tidySEM 0.1.9

* Removed LazyData entry from DESCRIPTION (no data included in package)
* Add methods for mplusObjects
* Fixed bug where nodes from multilevel models were incorrectly merged, due to
  lack of within-level intercepts
* Fixed bug where get_layout did not work for multilevel models because it used
  table_results() instead of get_edges()
* Fixed bug in create_scales
  
## Test environments

* local Windows 10 install, R 4.0.3
* GitHub Actions (r-lib): windows-latest (release)
* GitHub Actions (r-lib): macOS-latest (release)
* GitHub Actions (r-lib): ubuntu-20.04 (release)
* GitHub Actions (r-lib): ubuntu-20.04 (devel)
* win-builder: release, R version 4.0.4 (2021-02-15)
* win-builder: oldrelease: R version 3.6.3 (2020-02-29)
* win-builder: R Under development (unstable) (2021-03-05 r80073)
* rhub check: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* rhub check: Fedora Linux, R-devel, clang, gfortran
* rhub check: Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results

0 errors | 0 warnings | 0 notes
