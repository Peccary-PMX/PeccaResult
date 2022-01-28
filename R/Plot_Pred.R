#' Prediction Plots
#' @export
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @description Display individual and population predictions (IPRED and PRED) profiles versus observation dots over time for a given number of subjects (sampled or selected through filters).
#' @param object peccary object, namely a run or a folder, generally through a folder function (with one or several run numbers).
#' @param filterr string or NSE, directly through a R code (for instance \emph{"IPRED > 3 & dose \%in\%  c(100, 200)"}, with or without quotes). Works with IPRED, PRED, OBS, TIME, YTYPE and any covariate (included ID).
#'  This filter is secondly reworked so too complex filters can lead to errors - notably filters like (A & B) | (C & D)...
#' @param covs string, name of the covariate the user want to discriminate trough different colors. Works only for a single run (method not implemented for folders).
#' @param n numeric, number of maximal plots represented. If the number of possible ID is above this value, a sampling process is applied.
#' @param pred bool, display (or not) prediction profiles in dashed lines.
#' @param ylog bool, if true (by default) a logarithmic y axis is applied, if false an ordinary one.
#' @param xlog bool, if true a logarithmic x axis is applied, if false (by default) an ordinary one.
#' @param freescale bool, if true every ID will have its personnal axis, if false shared axis will be used.
#' @param plotly bool, if true toggle a dynamic plot, if false (by default) display a static plot.
#' @param LOQ numeric, manually provide a LLOQ value. Every point below or equal to this value will be displayed with a different shape. A horitonzal dashed line will also be printed.
#' @return Returns a ggplot object (which can be reworked, see example)
#'
#' @examples
#' \dontrun{
#'
#' # folder creation
#' folder <- createFolder("path_to_initial_folder")
#'
#' # Basic plot_pred
#' plot_pred(folder(1), covs = "Dose", n = 25, LOQ = 10 )
#'
#' # Applying a filter for a console peccary use:
#' plot_pred(folder(3:4),  IPRED > 0.1 & x_cov == "y")
#'
#' # Equivalent to (for shiny app purpose mostly):
#' plot_pred(folder(3:4),  "IPRED > 0.1 & x_cov == \"y\"")
#'
#' # Created plot can be manually modified:
#'
#'  plot_pred(folder(1)) +
#'   ggtitle("Peccary is a beautifull animal") +
#'   theme_dark() +
#'   labs(x = "region", y = "peccary fertility")
#'
#' }
#' @export
# @details plot_pred() has been developped to have a quick view of individual profiles predictions superposed to observation. It is ...
setGeneric("plot_pred",
           function(object, filterr = "", covs = F, n = 20, pred = T,  ylog = T, xlog = F, freeScale = F,  plotly = F, LOQ = "", lowerlimit = NA, upperlimit = NA, valuelimit = F){standardGeneric("plot_pred")}
)
# lowerlimit = 0.0001
############################################## R U N #########################################
#' @export
setMethod(f = "plot_pred",
          signature = "run",
          definition = function(object, filterr, covs = F,  n = 20, pred = T, ylog = T, xlog = F, freeScale = F,  plotly = F,  LOQ,  lowerlimit = NA, upperlimit = NA, valuelimit = F){


          ######################## D A T A     S E T S     L O A D I N G ###################

            tibble(name = c("IPRED", "OBS", "COV", "ADM"),
              datasets = list(object@IPRED %>% left_join(object@cov),  object@OBS %>% left_join(object@cov), object@cov, object@administration)) -> dfs



# Filters -----------------------------------------------------------------


          ###  ###  ### ### ### ### ### ### F I L T E R S ### ### ### ### ### ### ### ### ### ##
          ### I ) Manage the filter input
          ### Filter can be either in a NSE (for console users) or in quotes


            test_filter <- try(typeof(filterr) == "character", silent = T)

            if(class(test_filter) == "try-error"){

              filterr <- deparse(substitute(filterr))

            }

            # filterr = "YTYPE == 0 & OBS > 2"
            filterr_dec <- filter_decomposer(filterr)



            ## ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
            ### II ) Apply Trying filters on every dataset
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

            dfs %>%
              mutate(df_filterd = map(datasets, function(x){

                for(a in filterr_dec$group){

                  test <- try(x %>%
                                filter_(a), silent = T)
                  # print(test)
                  if(class(test)[1] != "try-error") x <- test

                }

                x


              })) %>%
              select(-datasets) -> dfs
            # dfs[[1,2]]

# randomize ID ------------------------------------------------------------


            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
            ###  III) Randoming ids  (if there is no ID filter )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

            if(nrow(filterr_dec %>%
                    filter(type == "ID")) == 0){


              numbers <- double()

              if(class(object@OBS$ID) == "factor")  {

                numbers <- character()
              }

              # If one covs is selected, we will sample patient such as all groups are represented
              if(covs != F){

                npgroup <- dfs[[2]][[1]] %>% # number of groups and patients in eac
                  group_by_(covs) %>%
                  summarise(n = length(unique(ID))) %>%
                  arrange(n)


                n_group <- nrow(npgroup) # number of subgroups
                pgroup <- floor(n/n_group) # theoretical subject per subgroup



                # For each subgroup we sample patient (all patients if inferior to "pgroup", or "pgroup")
                for (a in npgroup[[covs]]){

                  n2 <- min(c(pgroup, npgroup$n[npgroup[[covs]] == a]))
                  new <- sample(dfs[[2]][[1]] $ID[dfs[[2]][[1]] [[covs]] == a],n2)
                  if(class(new) == "factor") new <- as.character(new)


                  numbers <- c(numbers,new)

                }
              }

              # At the end of cov selection, we fill the rest by random patient
              while(length(numbers) < min(c(n, length(unique(dfs[[2]][[1]]$ID))))){

                new <-  sample(dfs[[2]][[1]]$ID,1)
                if(class(new) == "factor") new <- as.character(new)

                if(!(new %in% numbers)){

                  numbers <- c(numbers, new)
                }
              }

              ###### A P P L Y I N G  ID    F I L T E R
              dfs %>%
                mutate(df_filterd = map(df_filterd, function(x){

                 test <-  try(x %>%
                    filter(ID %in% numbers))
                 if(class(test)[1] != "try-error") x <- test
                 x

                }))  -> dfs
            }


            # if manual input of LOQ
            if(LOQ != ""){


              dfs[[2]][[2]] <- dfs[[2]][[2]] %>%
                mutate(LLOQ = if_else(OBS <= LOQ, LOQ, 0)) %>%
                mutate(OBS = if_else(OBS <= LOQ, LOQ, OBS)) %>%
                mutate(BLQ = if_else(OBS <= LOQ, "Yes", "No"))


            }

            # if automatique BLQ values
            if(length(unique(dfs[[2]][[2]]$BLQ))>0){

              dfs[[2]][[2]] <- dfs[[2]][[2]] %>%
                mutate(BLQ = if_else(BLQ == 1, "Yes", "No"))
            }

            # print(dfs[[2]][[2]])
            #### Is there BLQ  which need to be handled?
            if(suppressMessages(length(unique(dfs[[2]][[2]]$BLQ))) <1){
              BLQ <- F
            }else{
              BLQ <- T
            }

            ########## Additional  handling
            ### 1) Gathering IPRED and pred

            dfs[[2]][[1]] <- dfs[[2]][[1]] %>%
              gather(IPRED, PRED, key = "Lty", value = "value")

            ### 2) Removing PRED if asked by the user

            if(pred == F){

              dfs[[2]][[1]]  <- dfs[[2]][[1]]  %>%
                filter(Lty == "IPRED")
            }

            ### Adding a fake YTYPE if there isn't

            if(! ("YTYPE" %in% names(dfs[[2]][[1]] ))){

              dfs[[2]][[1]] <- dfs[[2]][[1]] %>%
                mutate(YTYPE =  1)

              dfs[[2]][[2]] <- dfs[[2]][[2]] %>%
                mutate(YTYPE =  1)

            }


# Lower and upper limit ---------------------------------------------------

if(!is.na(lowerlimit)){

  # dfs[[2]][[1]] <-
    dfs[[2]][[1]] %>%
    mutate(lowerlimitbool = if_else(value <= lowerlimit, 1, 0)) %>%
     filter(Lty == "IPRED") %>%
    group_by(ID, YTYPE) %>%
    nest() %>%
    # filter(ID == 212) %>% {.[[1,3]]} -> temp
    mutate(mins = map(data, function(temp){

      bind_rows(  temp %>%
                    mutate(lead = lead(lowerlimitbool)) %>%
                    slice(1) %>% filter(lowerlimitbool == 1),
                  temp %>%
                    mutate(lead = lead(lowerlimitbool)) %>%
                    filter(lowerlimitbool != lead)) %>%
        mutate(TIME2 = lead(TIME)) %>%
        mutate(testlastlin = temp %>% slice(nrow(temp)) %>% pull(lowerlimitbool)) %>%
        filter(! (is.na(TIME2) & testlastlin == 0)) %>%
        filter(lead == 1) %>%
        mutate(TIME2 = if_else(is.na(TIME2),max(temp$TIME), TIME2)) %>%
        # filter()
        mutate(min = map2_dbl(TIME, TIME2, function(x, y){

          temp %>%
            filter(between(TIME, x, y)) %>%
            summarise(min = min(value, na.rm = T)) %>% pull(min)  }))

    }))    -> temp


    temp %>% select(-data) %>%
      unnest -> minsUnderlowerValue


    dfs[[2]][[1]] <-  dfs[[2]][[1]] %>%
      mutate(value = if_else(value <= lowerlimit, as.double(lowerlimit), as.double(value)))


}

if(!is.na(upperlimit)){


              dfs[[2]][[1]] %>%
                mutate(upperlimitbool = if_else(value >= upperlimit, 1, 0)) %>%
                group_by(ID, YTYPE) %>%
                nest() %>%
                # filter(ID == 226) %>%  {.[[1,3]]} -> temp
                mutate(mins = map(data, function(temp){

                  bind_rows(  temp %>%
                                mutate(lead = lead(upperlimitbool)) %>%
                                slice(1) %>% filter(upperlimitbool == 1),
                              temp %>%
                                mutate(lead = lead(upperlimitbool)) %>%
                                filter(upperlimitbool != lead)) %>%
                    filter(Lty == "IPRED") %>%
                    mutate(TIME2 = lead(TIME)) %>%
                    filter(lead == 1) %>%
                    mutate(TIME2 = if_else(is.na(TIME2),max(temp$TIME), TIME2)) %>%
                    mutate(min = map2_dbl(TIME, TIME2, function(x, y){

                      temp %>%
                        filter(between(TIME, x, y)) %>%
                        summarise(max = max(value, na.rm = T)) %>% pull(max)  }))

                }))    -> temp


              temp %>% select(-data) %>%
                unnest -> maxsUnderlowerValue


              dfs[[2]][[1]] <-  dfs[[2]][[1]] %>%
                mutate(value = if_else(value >= upperlimit, as.double(upperlimit), as.double(value)))


            }


# PLot creation -----------------------------------------------------------




            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            ### ### ### ### ### ### ### ### ### ## P L O T S  C R E A T I O N ### ### ### ##
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            ### How many YTYPE we have? Used only for ggtitle
            n_YTYPE <- length(unique(dfs[[2]][[1]]$YTYPE))

            dfs[[2]][[1]] %>%
              ##### one plot per YTYPE
              group_by(YTYPE) %>%
              nest(.key = "PRED") %>%
              left_join(

                dfs[[2]][[2]] %>%
                  group_by(YTYPE) %>%
                  nest(.key = "OBS")

              ) %>%
              arrange(YTYPE) %>%
              # filter(ID == 210)
              # {.[[1,3]] } -> b
              # {.[[1,2]] } -> a
              # {.[[1,1]] } -> c
              mutate(plots = pmap(list(PRED, OBS, YTYPE), function(a, b, c){




               ### Choosing the right aes according covs & BLQ

                if(covs != F  & BLQ == T){

                  aest <- list(geom_line(data = a, aes_string("TIME", "value",  lty = "Lty", col = paste0("factor(",covs,")"))),
                               geom_point(data = b, aes_string("TIME", "OBS", shape = "factor(BLQ)")),
                               labs(col = covs, shape = "BLQ"),
                               geom_hline( data =b,  aes(yintercept = LLOQ), lty = 3),
                               scale_shape_manual(values = c(19,8)[1:length(unique(b$BLQ))]))

                if("LLOQ" %in% names(dfs[[2]][[1]]))  aest[[5]] <- geom_hline( data =b,  aes(yintercept = LLOQ), lty = 3)


                }else if(covs != F  & BLQ == F){

                  aest <- list(geom_line(data = a, aes_string("TIME", "value",  lty = "Lty", col = paste0("factor(",covs,")"))),
                               geom_point(data = b, aes_string("TIME", "OBS", col = paste0("factor(",covs,")"))),
                               labs(col = covs))


                  } else if(covs == F & BLQ == T){


                  aest <- list(geom_line(data = a, aes_string("TIME", "value",  lty = "Lty")),
                               geom_point(data = b, aes_string("TIME", "OBS", shape = "factor(BLQ)")),
                               labs(shape = "BLQ"),
                               # geom_hline( data =b,  aes(yintercept = LLOQ), lty = 3),
                               scale_shape_manual(values = c(19,8)[1:length(unique(b$BLQ))]))

                  if("LLOQ" %in% names(dfs[[2]][[1]]))  aest[[6]] <- geom_hline( data =b,  aes(yintercept = LLOQ), lty = 3)

                  if("BLQ" %in% names(dfs[[2]][[2]])){

                    datablq <- b %>%
                      filter(BLQ == "Yes") %>%
                      distinct(ID, OBS) %>%
                      group_by(ID) %>%
                      mutate(t = length(unique(OBS))) %>%
                      filter(t == 1)
                    aest[[6]] <- geom_hline( data =datablq,  aes(yintercept = OBS), lty = 3)

                  }
                     } else if(covs == F  & BLQ == F){

                  aest <- list(geom_line(data = a, aes_string("TIME", "value",  lty = "Lty")),
                               geom_point(data = b, aes_string("TIME", "OBS")))
                 }



                ### problem: if there is one point per ID, we can't do a geom_line -> search those ID for a geom_point!
                ### Solution: add it !
                try({ a %>%
                  group_by(ID) %>%
                  nest() %>%
                  mutate(n = map_dbl(data, function(x) nrow(x %>% distinct(TIME)))) %>%
                  filter( n == 1 ) %>%
                  unnest(data) %>%
                  filter(Lty == "IPRED")-> singlepoint

                if(nrow(singlepoint) >= 1) aest <- list(aest, geom_point(data = singlepoint, aes_string("TIME", "value"), col = "red"))

                })
                ##### define the aestecic according covariable and/or BLQ


                ggplot()+
                  aest+
                  theme_bw() -> output


                ### remove a guides if not needed
                if(pred == F){

                  output <- output + guides(lty = F)
                }


                ## Handling logarithmic axis and freeScale
                if(ylog == T & nrow(b %>% filter(OBS < 0)) ==0 & plotly == F) output <- output + scale_y_log10(breaks = breaks_log, labels = labels_log)
                if(ylog == T & nrow(b %>% filter(OBS < 0)) ==0 & plotly == T) output <- output + scale_y_log10()
                if(xlog == T & nrow(b %>% filter(TIME < 0)) ==0 & plotly == F) output <- output + scale_x_log10(breaks = breaks_log, labels = labels_log)
                if(xlog == T & nrow(b %>% filter(TIME < 0)) ==0 & plotly == T) output <- output + scale_x_log10()

                if(freeScale == F){
                  output <- output + facet_wrap(~ID, labeller = label_both )
                }else{
                  output <- output + facet_wrap(~ID, labeller = label_both, scales = "free" )
                }

                ### mins

                if(!is.na(lowerlimit) & valuelimit == T){


                 try( output <-   output+
                    geom_text(data =   minsUnderlowerValue %>%
                                filter(YTYPE == c) %>%
                                mutate(min = as.character(signif(min,0))) %>%
                                mutate(x = (TIME + TIME2)/2) %>%
                                mutate(min = gsub(".*e", "e", min)),
                              mapping = aes(x = x,y = lowerlimit, label = min), nudge_y = 0.2)
                 )
                }

                if(!is.na(upperlimit) & valuelimit == T){

                  #
                  output <-
                    output+
                    geom_text(data =   maxsUnderlowerValue %>%
                                filter(YTYPE == c) %>%
                                mutate(min = as.character(signif(min,2))) %>%
                                mutate(x = (TIME + TIME2)/2) %>%
                                mutate(min = gsub(".*e", "e", min)),
                              mapping = aes(x = x,y = upperlimit, label = min), nudge_y = 0.2)

                }

               ### T I M E S    O F     A D M I N I S T R A T I O N S
                # If we have information on time of adminstration

                if(length(dfs[[2]][[4]]) > 1){



                  if(length(object@cov) > 0) {

                    dfs[[2]][[4]] <- dfs[[2]][[4]] %>%
                      left_join(object@cov)

                  }


                 output + geom_vline(data = dfs[[2]][[4]], aes(xintercept = TIME), lty = 3, col = "gray20" )


                }

                #### GGTITLE

                if(n_YTYPE > 1 ){

                  output <- output+
                  ggtitle(paste0("YTYPE " ,c))+
                  theme(plot.title = element_text(hjust = 0.5))
                }


                if(filterr != ""){


                  gsub("==", "=", filterr) %>%
                  {gsub("&", ", ",.)} %>%
                  {gsub("\\|","or", .)} %>%
                  {gsub("\"", "", . )} %>%
                  {   output + ggtitle(label = "", subtitle = .)+
                      theme(plot.subtitle = element_text(hjust = 0.5))} -> output



                }





                ### OUTPUT
                return(output)

              }
              )) -> output



            if(plotly == F & nrow(output) > 1){
              return( invoke(plot_grid, output$plots))
            }else if (plotly == F & nrow(output) == 1){
              return(output$plots[[1]])
            }else{
              return(ggplotly(output$plots[[1]]))

            }


          })

# plot_pred(object, filterr  = "YTYPE == 3", lowerlimit = 0.000001, upperlimit = 10,  freeScale = T, valuelimit = T)
############################################## F O L D E R #########################################

#' @export
setMethod(f = "plot_pred",
          signature = "folder",
          definition = function(object, filterr, covs = F, n = 20, pred = T, ylog = T, xlog = F, freeScale = F, plotly = F, LOQ,  lowerlimit = NA, upperlimit = NA, valuelimit = F){



            numbers <- object@lastSelected
            names <- object@summary$run[numbers]




        ####### SELECT ALL DATA

            tibble(names, numbers) %>%
                            mutate(run = map(numbers,function(x) select_run(object, x))) %>%
                            mutate(IPRED = map(run, function(x) x@IPRED %>% left_join(x@cov) )) %>%
                            mutate(OBS = map(run, function(x) x@OBS %>% left_join(x@cov))) %>%
                            mutate(ADMIN = map(run, function(x) x@administration)) %>%
                            gather(IPRED, OBS, ADMIN,  key = "key", value = "datasets") -> dfs


# filters -----------------------------------------------------------------


            ### ### ### ### ### ### ### # F I L T E R S ### ### ### ### ### ### ### ### ### ##
            ### I ) Manage the filter input
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            ### Filter can be either in a NSE (for console users) or in quotes


            test_filter <- try(typeof(filterr) == "character", silent = T)

            if(class(test_filter) == "try-error"){

              filterr <- deparse(substitute(filterr))

            }

            # filterr = "YTYPE == 0 & OBS > 2"
            filterr_dec <- filter_decomposer(filterr)
            # print(filterr_dec)

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            ### II ) Apply Trying filters on every dataset
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

            dfs %>%
              mutate(df_filterd = map(datasets, function(x){

                for(a in filterr_dec$group){

                  test <- try(x %>%
                                filter_(a), silent = T)
                  # print(test)
                  if(class(test)[1] != "try-error") x <- test

                }

                x


              })) %>%
              select(-datasets) -> dfs





# randoming patients ------------------------------------------------------


            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            ### III ) Randoming Patient If needed
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
try({
            if(nrow(filterr_dec %>%
                    filter(type == "ID")) == 0){

              ids <- double()
              x  <- 1
              while(length(ids) == 0){

              ids <- unique(dfs[["df_filterd"]][[x]]$ID)
              if(length(ids) <n){n_id <-  ids} else {n_id <-  sample(ids, n)}
              x <- x+1

              }

              dfs %>%
                mutate(df_filterd = map(df_filterd, function(x){

                  x %>%
                    filter(ID %in% n_id)


                })) -> dfs

            }
})
# Final datasets ----------------------------------------------------------


            ################# Final datasets

           temp_IPRED <-  dfs %>%
              filter(key == "IPRED") %>%
              select(names, numbers, df_filterd) %>%
              unnest() %>%
              gather(IPRED, PRED, key = "Lty", value = "value")


           if(pred == F){

             temp_IPRED <- temp_IPRED %>%
               filter(Lty == "IPRED")
           }

            if(!is.na(lowerlimit)){


              temp_IPRED <- temp_IPRED %>%
                mutate(value = if_else(value < lowerlimit, as.double(lowerlimit),as.double( value)))

            }

            if(!is.na(upperlimit)){


              temp_IPRED <- temp_IPRED %>%
                mutate(value = if_else(value > upperlimit, as.double(upperlimit), as.double(value)))

            }


           temp_OBS <-  dfs %>%
             filter(key == "OBS") %>%
             select(names, numbers, df_filterd) %>%
             unnest()

           if(LOQ != ""){

             temp_OBS <- temp_OBS %>%
               mutate(LLOQ =  LOQ) %>%
               mutate(BLQ = if_else(OBS <= LLOQ, "Yes", "No"))


           }

           temp_ADM <- try({dfs %>%
             filter(key == "ADMIN") %>%
             select(names, numbers, df_filterd) %>%
             unnest()%>%
             distinct(ID,  TIME)}, silent = T)

            if(class(temp_ADM) == "try-error") temp_ADM = tibble(ID = character(), TIME = double())

           if(!("YTYPE" %in% names(temp_IPRED))){

             temp_IPRED <- temp_IPRED %>%
               mutate(YTYPE = 1)

             temp_OBS <- temp_OBS %>%
               mutate(YTYPE = 1)

           }



# plopt creation ----------------------------------------------------------


        tibble(YTYPE = unique(temp_IPRED$YTYPE))  %>%
          arrange(YTYPE) %>%
          mutate(plots = map(YTYPE, function(x){


            if("BLQ" %in% names(temp_OBS)){




              ggplot()+
                geom_line(data = temp_IPRED %>% filter(YTYPE == x), aes(TIME, value, col = names, lty = Lty))+
                geom_point(data = temp_OBS %>% filter(YTYPE == x), aes(TIME, OBS, shape = factor(BLQ)))+
                theme_bw()+
                scale_shape_manual(values = c(19,8)[1:length(unique(temp_OBS$BLQ[temp_OBS$YTYPE ==x]))]) +
                labs(shape = "BLQ") -> result

              if("LOQ" %in% names(temp_OBS)) result <- result +
                geom_hline(data= temp_OBS, aes(yintercept = LOQ), lty = 2)

              temp_OBS %>%
                filter(BLQ == 1) %>%
                distinct(ID, OBS) %>%
                group_by(ID) %>%
                 mutate(test = length(unique(OBS))) %>%
                filter(test == 1) -> blqs

              result <-  result +
                geom_hline(data= blqs, aes(yintercept = OBS), lty = 2)

            }else{

            ggplot()+
              geom_line(data = temp_IPRED %>% filter(YTYPE == x), aes(TIME, value, col = names, lty = Lty))+
              geom_point(data = temp_OBS %>% filter(YTYPE == x), aes(TIME, OBS))+
              theme_bw() -> result

            }

            if(length(unique(temp_IPRED$ID)) > 1 & freeScale == T ){ #& covs == F
              result <- result +   facet_wrap(~ID, scales = "free", labeller = label_both)
            }else if(length(unique(temp_IPRED$ID)) > 1 & freeScale == F){ # & covs == F
              result <- result +   facet_wrap(~ID, labeller = label_both)
            }


#& nrow(temp_IPRED %>% filter(value < 0 )) == 0
            if(ylog == T & plotly == F ) result <- result + scale_y_log10(breaks = breaks_log, labels = labels_log)
            if(ylog == T & plotly == T ) result <- result + scale_y_log10()
            if(xlog == T & plotly == F ) result <- result + scale_x_log10(breaks = breaks_log, labels = labels_log)
            if(xlog == T & plotly == T) result <- result + scale_x_log10()


            if(length(unique(temp_IPRED$YTYPE)) > 1 ){

              result <- result+
                ggtitle(paste0("YTYPE " ,x))+
                theme(plot.title = element_text(hjust = 0.5))
            }


            ### T I M E S    O F     A D M I N I S T R A T I O N S
            # If we have information on time of adminstration

            if(nrow(temp_ADM) > 0 & length(temp_ADM) > 1){


              result <- result + geom_vline(data = temp_ADM, aes(xintercept = TIME), lty = 3, col = "gray20" )


            }



            if(filterr != ""){


              gsub("==", "=", filterr) %>%
              {gsub("&", ", ",.)} %>%
              {gsub("\\|","or", .)} %>%
              {gsub("\"", "", . )} %>%
              {   result + ggtitle(label = "", subtitle = .)+
                  theme(plot.subtitle = element_text(hjust = 0.5))} -> result



            }


            return(result)

          })) -> output




        if(plotly == F & nrow(output) > 1){
          return( invoke(plot_grid, output$plots))
        }else if (plotly == F & nrow(output) == 1){
          return(output$plots[[1]])
        }else{
          return(ggplotly(output$plots[[1]]))

        }



          })


# plot_pred( crhistelle(1:2), ID == 15, freeScale = T)
#
# plot_pred(blaise(1), ID == 75016,  freeScale = T)
# plot_pred(blaise(1:2), freeScale = T)
# plot_pred(a(c(2,4)))
