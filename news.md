# tidySEM 0.1.2

* create_scales() no longer writes .csv files
* add_paths() now relies on lavaanify(lavParseModelString()), uses the lavaan
  parameter table, and accepts all arguments of lavaanify(), with default
  arguments same as sem() and cfa()
* measurement() uses add_paths() for consistency
* `graph_sem.lavaan()` and `graph_sem.mplus.model()` gain an argument
  `label = "est_sig"`, so users can easily select custom labels.
* New vignette about SEM plotting conventions.
* `graph_sem()` adds empty labels for nodes without a mean; in version 0.1.1,
  such nodes were displayed without any node label.

# tidySEM 0.1.1

* Addressed comment by CRAN maintainer Jelena Saf
* Minor bug fixes

# tidySEM 0.1.0

* First CRAN release.
