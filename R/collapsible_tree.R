#' @export
# @details plot_pred() has been developped to have a quick view of individual profiles predictions superposed to observation. It is ...
setGeneric("pecc_workflow",
           function(object, wd = ""){standardGeneric("pecc_workflow")}
)

############################################## R U N #########################################
#' @export
setMethod(f = "pecc_workflow",
          signature = "folder",
          definition = function(object, wd = ""){



              tibble(path = str_split(object@summary$files, "/")) %>%
              # {.[[1,1]]} -> one_run
              mutate(df = map(path, function(one_run){


                temp <- tibble(Parent = NA, Son = one_run[2])


                  for(x  in 2:length(one_run)){

                    temp <- temp %>% bind_rows(tibble(Parent = one_run[x-1], Son = one_run[x]))

                  }
                return(temp)
              })) %>%
              unnest(df) %>%
              mutate(Son = if_else(is.na(Parent), "", Son)) %>%
              distinct() ->network

print(        map_dfr(network, function(x){

  tibble(run = x) %>%
    left_join(object@summary %>%
                distinct(run, number, FOs)) %>%
    mutate(output =  if_else(!is.na(run), paste0(run,if_else(is.na(FOs), "", paste0(" (FO=", FOs, ", run " , number,")"))), run)) %>%
    pull(output)

}) %>%
  mutate(col = map_dbl(Son, function(x) length(grep("(WINNER)|(FINAL)", toupper(x) )))) %>%
  mutate(coltest = col) %>%
  mutate(col = if_else(col ==1, "black", "white")) %>%
  mutate(col = if_else(Son %in% unique(network$Parent), "grey", col)) %>%
  mutate(col = if_else(Son == gsub(".+/", "", wd), "red", col)))

            map_dfr(network, function(x){

              tibble(run = x) %>%
                left_join(object@summary %>%
                            distinct(run, number, FOs)) %>%
                mutate(output =  if_else(!is.na(run), paste0(run,if_else(is.na(FOs), "", paste0(" (FO=", FOs, ", run " , number,")"))), run)) %>%
                pull(output)

            }) %>%
              mutate(col = map_dbl(Son, function(x) length(grep("(WINNER)|(FINAL)", toupper(x) )))) %>%
              mutate(col = if_else(col ==1, "black", "white")) %>%
              mutate(col = if_else(Son %in% unique(network$Parent), "grey", col)) %>%
              mutate(col = if_else(Son == gsub(".+/", "", wd), "red", col)) %>%
            # filter(col == "white")
              collapsibleTreeNetwork(inputId = "inputcollapsibletree", fill = "col")



          })
