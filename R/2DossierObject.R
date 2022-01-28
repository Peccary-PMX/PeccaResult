#' SetClass S4 Dossier Object
#' Author: Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @slot RunsStorage list of runs included in the dossier object
#' @slot Summary summary of every runs the dossier will handle
#' @slot racine root of the folder
#' @slot software wether it's NONMEM or Monolix
#' @slot lastSelected see how CreateDossier function works


setClass(Class = "folder",
         representation = representation( "RunsStorage" = "list",
                                          "summary" = "data.frame",
                                          "racine" = "character",
                                          "software" = "character",
                                          "lastSelected" = "numeric"
         )
)


