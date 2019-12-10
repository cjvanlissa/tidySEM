#' @importFrom utils getFromNamespace
#' @importFrom lavaan parametertable lavInspect standardizedsolution
lav_getParameterLabels <-
  getFromNamespace("getParameterLabels", "lavaan")

table_results_lavaan <- function(x, standardize, retain_which = c("~", "~~", "=~")){

  # Prepare output skeleton -------------------------------------------------
  num_groups <- lavInspect(x, what = "ngroups")

  # Get estimates -----------------------------------------------------------

  unst_pars <- parametertable(x)
  between_group_constraints <- FALSE

  if(any(unst_pars$op == "==")){ # Maybe use op == "=="
    constraints <- unst_pars[unst_pars$op == "==", ]
    unst_pars <- unst_pars[!unst_pars$op == "==", ]
  } else {
    constraints <- NULL
  }

  parameter_table <- cbind(unst_pars, standardizedsolution(x))

  ## Only the free parameters or parameters explicitly defined by user.
  #parameter_table <- parameter_table[parameter_table$free > 0 & !parameter_table$plabel == "", ]

  #parameter_table$bain_label <- parameter_table$parameter_label <- lav_getParameterLabels(partable = parameter_table)
  if(num_groups > 1){
    # Label group params ------------------------------------------------------

    group_labels <- lavInspect(x, what = "group.label")

    parameter_table$lhs <- paste0(parameter_table$lhs, group_labels[parameter_table$group])
    parameter_table$rhs <- paste0(parameter_table$rhs, group_labels[parameter_table$group])

  }

  # Retain only requested parameters
  parameter_table <- parameter_table[parameter_table$op %in% retain_which, ]

  # Remove duplicates
  parameter_table <- parameter_table[!duplicated(do.call(paste0, parameter_table[, c("lhs", "op", "rhs")])), ]

  parameter_table
}
