# Version 0.1.1

This resubmission addresses the comment by CRAN-maintainer Martina Schmirl:

* It seeems like you write to the home filespace in the vignette and maybe also
  in examples. Please check.
  + Response:
    Several vignettes use the external program 'Mplus', and these indeed wrote
    files to the user's home filespace when the documentation was built.
    I have disabled all of these calls by including an if() statement that is
    set to FALSE. There are no examples that write to the home filespace.

## Test environments

* local Windows 10 install, R 4.0.0
* win-builder: release: R version 4.0.0 (2020-04-24)
* win-builder: oldrelease: R version 3.6.3 (2020-02-29)
* win-builder: devel: R Under development (unstable) (2020-05-15 r78473)
* rhub check: Fedora Linux, R-devel, clang, gfortran
* Travis Ubuntu 16.04.6 LTS, R release

## R CMD check results

0 errors | 0 warnings | 1 notes

NOTE: New submission
