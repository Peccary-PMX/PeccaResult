#' SetClass S4 run Object
#' Author: Thibaud Derippe
#'
#' @slot name name of the run (usefull for plots title for instance)
#' @slot path root of the run, such as with name + path we can have the complete pathway of the run
#' @slot status  usefull only for NONMEM: is the run successfully converged or did it failed (and a key work of the nature of the error)
#' @slot FO : objective function value
#' @slot Estimation : parameter estimation values with RSE
#' @slot OBS : every observation
#' @slot IPRED : every estimation
#' @slot cov : ID every covariable concerning each ID
#' @slot randomEffect : random effect values
#' @slot residus : residual values
#' @slot administration : sampling schedule
#' @slot loq : loq values
#' @slot software : NONMEM or Monolix
#' @slot date : date of creation


setClass(Class = "run",
  representation = representation("name" = "character",
                                  "path" = "character",
                                  "status" = "character",
                                  "FO" = "numeric",
                                  "Score" = "data.frame",
                                  "estimation" = "data.frame",
                                  "OBS" = "data.frame",
                                  "IPRED" = "data.frame",
                                  "cov" = "data.frame",
                                  "randomEffect" = "data.frame",
                                  "individualValues" = "data.frame",
                                  "residus" = "data.frame",
                                  "administration" = "data.frame",
                                  "software" = "character",
                                  "dataset" = "list",
                                  "path_source" = "character",
                                  "date" = "character"
                                  )
)












