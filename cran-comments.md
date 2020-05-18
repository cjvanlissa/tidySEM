# Version 0.1.0

This resubmission addresses the comment by CRAN-maintainer Uwe Ligges:

* Please single quote software names in the Description field of the  
  DESCRIPTION file as in 'Mplus'.
    + We have done as requested; we had already done this in all R/Rd files,  
      but forgot the DESCRIPTION file. Apologies!
* Is there some reference about the method you can add in the Description  
  field in the form Authors (year) <doi:.....>?
    + There is no reference as of yet. After publishing to CRAN, the next step
      will be to write a tutorial paper.

## Test environments

* local Windows 10 install, R 4.0.0
* win-builder: release: R version 4.0.0 (2020-04-24)
* win-builder: oldrelease: R version 3.6.3 (2020-02-29)
* win-builder: devel: R Under development (unstable) (2020-05-15 r78473)
* rhub check: Fedora Linux, R-devel, clang, gfortran
* rhub check: Ubuntu Linux 16.04 LTS, R-release, GCC
* Travis Ubuntu 16.04.6 LTS, R release

## R CMD check results

0 errors | 0 warnings | 1 notes

NOTE: New submission
