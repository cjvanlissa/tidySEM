# tidySEM 0.2.5

* Add reference to best practices paper, <DOI:10.1080/10705511.2023.2250920>
* Include vignettes corresponding to the appendices of that paper
* Correct implementation of `lr_lmr()` using `nonnest2::vuongtest()`

## Test environments

* local Windows 11 install, R 4.3.0
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
