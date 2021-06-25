# tidySEM 0.1.10

* Apologies for the brief interval between submissions.
  There was a remaining bug in the new functions:
  if_edit() would fail when target column is available in nodes
  or edges, but not in both
  
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
