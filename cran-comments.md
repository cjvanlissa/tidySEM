# tidySEM 0.2.3

* Fix Issue #45 , argument `label` was not passed correctly to `get_nodes()` and
  `get_edges()` in `graph_sem()` and `prepare_graph()`

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
