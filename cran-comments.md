# Version 0.1.1

This resubmission addresses the comment by CRAN-maintainer Jelena Saf:

* Please make sure that you do not change the user's options, par or working
  directory. If you really have to do so, please ensure with an *immediate*
  call of on.exit()
  + I have done as requested; line 73 of R/syntax-estimate.R now reads:
    on.exit(setwd(oldwd))
  + Unfortunately it is necessary to change the working directory, because 
    the R-package 'MplusAutomation' did not respect file paths until I reported
    this as a bug.

## Test environments

* local Windows 10 install, R 4.0.0
* win-builder: release: R version 4.0.0 (2020-04-24)
* win-builder: oldrelease: R version 3.6.3 (2020-02-29)
* win-builder: devel: R Under development (unstable) (2020-05-15 r78473)
* rhub check: RHub non-responsive
* Travis Ubuntu 16.04.6 LTS, R release

## R CMD check results

0 errors | 0 warnings | 1 notes

NOTE: New submission
