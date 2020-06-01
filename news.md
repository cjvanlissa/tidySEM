# tidySEM 0.1.2

* `graph_sem.lavaan()` and `graph_sem.mplus.model()` gain an argument
  `label = "est_sig"`, so users can easily select custom labels.
* New vignette about SEM plotting conventions.
TODO: * `add_paths()` by default sets variance for variables so the model can be
  estimated in lavaan
TODO: * `graph_sem()` adds empty labels for nodes without a mean; in version 0.1.1,
  such nodes were displayed without any node label.

# tidySEM 0.1.1

* Addressed comment by CRAN maintainer Jelena Saf
* Minor bug fixes

# tidySEM 0.1.0

* First CRAN release.
