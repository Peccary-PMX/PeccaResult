
#' @export
setGeneric("results",
           function(object, plot = F){standardGeneric("results")}
)

#' Create a df with every run selected
#' @Author: Thibaud Derippe
#' Arguments: object, the dossier bobject
#' Arguments: double ?
#' Arguments: plot: if it's a data.frame or a ggplot output
#' Arguments: forcenumbers, to have numeric type of atomic vector
#'
#' Output : see plot argument
#'
#' @param run
#'
#' @export
setMethod(f = "results",
          signature = "run",
          definition = function(object, plot = F){


          if(nrow( object@Score )>0){
            object@Score %>%
              mutate(Value = if_else(Score != "FO", round(Value), Value)) %>%
              rename(Parameter = Score) %>%
              mutate(Value = as.character(Value)) %>%
              add_row(Parameter = "n_subj", Value = as.character(length(unique(gsub("OCC.+", "",object@OBS$ID)))), .before = 1) %>%
              add_row(Parameter = "n_obs", Value = as.character(length(object@OBS$ID)), .before = 2) %>%
              add_row(Parameter = "n_par", Value = as.character(nrow(object@estimation %>% filter(Value != "0"))), .before = 3) %>%
              bind_rows(object@estimation %>%
                          mutate(Value = as.character(Value)) )  -> res; res
          }else{

            object@estimation %>%
              mutate(Value = as.character(Value)) -> res; res
          }


            if(object@software == "NONMEM"){

              res <- res %>%
                add_row(Parameter = "Status", Value = object@status, .before = 7)

            }

            res <- map_dfr(res, function(x){

              x[x == "" | is.na(x)] <- "-"
              x

            })


            if(plot == T){
              ggplot()+
                annotation_custom(tableGrob(res))+
                ggtitle(object@name) %>%
                return()
            }else{

              return(res)
            }


          }
)


#' @export
setMethod(f = "results",
          signature = "folder",
          definition = function(object, plot = F){




            tibble(n = object@lastSelected) %>%
              mutate(runs = map(n, function(x) results(select_run(object,x)))) %>%
              # {.[[1, "runs"]][[1]]} ->runs
              mutate(runs_mdf = map2(n, runs, function(n, runs){



                if("p_value" %in% names(runs)){
                  output <- runs %>%
                    select(1, 2, 4, 5) %>%
                    mutate(p_value = if_else(p_value == "-", "", paste0("p:",p_value,", "))) %>%
                    mutate(Value = paste0(Value," (", p_value, round(as.double( `RSE(%)`), 0),"%)")) %>%
                    select(-  `RSE(%)`, - p_value)
                }else{
                  output <- runs %>%
                    select(Parameter, Value, `RSE(%)`) %>%
                    mutate(Value = paste0(Value," (", round(as.double(gsub("%", "",`RSE(%)`)), 0),"%)") %>%
                             gsub(pattern = " \\(NA%\\)", replacement = "")) %>%
                    select(- `RSE(%)`)
                }

                output <- map_dfr(output, function(x){

                  gsub(" \\(.?NA%.?\\)", "", x)

                }) %>%
                  add_row(Parameter = "run_n", Value = as.character(n), .before = 1)

                names(output)[[2]] <- object@summary$run[object@summary$number == n]

                return(output)

              })) %>%
              {reduce(.$runs_mdf, full_join)} -> results



            # Then we change the na value
            results[] <- lapply(results, function(x){
              x[is.na(x)] <- "-"
              x <- gsub("(.?NA%.?)", "", x)
              x
            })



            if(plot == F){

              return(results)

            }else{

              ggplot()+
                annotation_custom(tableGrob(results))

            }


          }
)



