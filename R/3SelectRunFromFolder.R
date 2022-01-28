#' @export
setGeneric("select_run",
           function(object, value = 0){standardGeneric("select_run")}
)

#' From a folder give the run
#' Author: Thibaud Derippe
#' Arguments: object, the dossier object
#' Arguments: value, the run you want to create from
#' Output: a run object
#' @export

# object  <- createFolder("file:///D:/Peccary/Exemple_demo")(folder = T)
#
# value = 15
# dossier() %>%
#   select(-files)
# dossier(15)
# results(test(1:2))
# object <- test(1:2)
# dossier
# object <- createFolder("file:///C:/Users/titi7/lixoft/monolix/monolix2019R2/demos/2.models_for_continuous_outcomes/2.1.residual_error_model/warfarinPK_project")
# object <- object(folder = T); value = 1
setMethod(f = "select_run",
          signature = "folder",
          definition = function(object, value = 0){


            # Search if the run have already been created
            test <- try(object@RunsStorage[[value]], silent = T)
            if( class(test) == "run"){

              return( test )

            }else{

            # If value equal 0, we launch the most recent one
            if(value == 0){
           temp <-  object@summary %>%
              arrange(desc(Date)) %>%
             slice(1)
           file <-  paste0(object@racine,"/",temp$files)

            }else{
  print("la")
              #else the name
              file <- paste0(object@racine,"/", subset(object@summary, number == value)$files)

              object@summary %>%
                filter(number == value) %>% pull(software) -> software

              if(software == "NLMIXR") file <- paste0(file, ".nlmixr")
              if(software == "NONMEM") file <- paste0(file, ".res")
              if(software == "ADAPT") file <- paste0(file, ".run")
            }

          run_output <- createRun(file)

            return(run_output)

            }
          })
# select_run(object, 1)
