#' Goodness of Fits Plots
#' @export
#' @author Thibaud Derippe \email{thibaud.derippe@@gmail.com}
#' @description Create goodness of fits plots for one or several runs, eventually spread by a covariate.
#' @param object either a run or a folder
#' @param plots_to_keep by default  create every possible GOF. User can select only plots of interest either by numbers or by strings
#' @param cov_to_use string of the covariate header the user want to split GOFS
#' @param col_cov bool, if true make one color per covariate on a single plot, if F make one layer per covariate value
#' @param gather_by_cov bool, wether the user want plots gathered by covariate or by type of GOFS
#' @param plotly bool, if true transforms static plots into plotly dynamic plots, usefull to detect outliers
#' @param filter a filter can contains IPRED/PRED/OBS/TIME/NPDE/YTYPE... and any covariable (included ID) that you want. Can be quoted or not.
#' @param table_output TRUE or FALSE, either to directly print the plots or to return the table
#' @details plot_GOF() search of every possible goodness of fit plots and create them individually. If a proporitional error is found, according plots will
#' be in logarithmic scales. User can filter as it wants the dataset. If several runs is given in input, through a folder, it will give back the table of every plots doable. User can then
#' filter and arrange as it wants and then use the plot_invoke() function

setGeneric("plot_GOF",
           function(object, plots_to_keep = F, cov_to_use =  "", col_cov = F,  gather_by_cov = F , plotly = F , filter = F, table_output = F, xlog = "", ylog = "", units_obs = "", units_time = "", method_smooth = "lm", removeBLQ = T){standardGeneric("plot_GOF")}
)



#' @export
setMethod(f = "plot_GOF",
          signature = "run",
          definition =  function(object, plots_to_keep = "", cov_to_use , col_cov = F, gather_by_cov = F , plotly = F , filter = "", table_output = F, xlog = "", ylog = "", units_obs = "", units_time = "", method_smooth = "lm",  removeBLQ = T){


  ### cov_to_use can be either in a NSE (for console users) or in quotes
  ## usefull for Shiny App

  test_cov_to_use <- try(typeof(cov_to_use) == "character", silent = T)

  if(class(test_cov_to_use) == "try-error"){

    cov_to_use <- deparse(substitute(cov_to_use))

  }


  test_filter <- try(typeof(filter) == "character", silent = T)

  if(class(test_filter) == "try-error"){

    filter <- deparse(substitute(filter))

  }


  if(plotly == F){
    output_funct <- plot_grid
  }else{
    output_funct <- subplot
  }


  test_prop <- function(object){
    if(object@software == "Monolix"){

      if(length(which(object@estimation$Parameter == "b")) == 1){
      return(1)
      }else{
        return(0)
      }

    } else{

      if( length(grep("PROP", toupper(as.character(object@estimation$Parameter)))) >=1){
        return(1)
      }else{
        return(0)
      }

      }


    } # test si le mod?le est proportionnelle, pour ajuster les log
  prop <- test_prop(object)
  # prop <- 1

# filter <- quo(ID != 1001001 )
### Dataset loading

dataset <-
  suppressMessages(object@residus %>%
    left_join(object@IPRED %>% filter(TIME %in% object@OBS$TIME)) %>%
    left_join(object@OBS) %>%
    left_join(object@cov) %>%
    filter(is.na(OBS) == F) %>%
    filter(is.na(PRED) == F ) %>%
    mutate(nameRun = object@name) )

if(!"BLQ" %in% names(dataset)){

  dataset <- dataset %>% mutate(BLQ = 0 )

}

if(filter != "") dataset <- dataset %>% filter_(filter)

if(removeBLQ == T & "BLQ" %in% names(dataset)) dataset <- dataset %>% filter(BLQ == 0)

# plots_to_keep <- "OBSvsIPRED"
plots <- c("OBSvsIPRED", "OBSvsPRED", "CWRESvsTIME", "CWRESvsPRED", "IWRESvsIPRED", "IWRESvsTIME", "NPDEvsTIME", "NPDEvsPRED", "StandResvsIPRED","StandResvsTIME")
############## Main function: from a dataset, give you a tibble containing x, y, and ggplot

#### test if there is some negative IPRED or OBS value (in case of log transformation for example)
test_neg <- if_else(nrow(dataset %>% filter(IPRED < 0 | OBS < 0 )) > 0 , 1, 0 )


GOF <- function(dataset){

    temp <- tibble(name = plots, x = gsub(".+vs", "", plots), y = gsub("vs.+","",  plots))%>%
    filter(x %in% names(dataset) & y %in% names(dataset))  ##### Remove graphs which can not be made because columns does not exist

    # If the user don't want all available GOF but only a special one.
    if(typeof(plots_to_keep) == "double" & length(plots_to_keep) > 0 ){

      temp <- temp %>% slice(plots_to_keep)

    } else if (typeof(plots_to_keep) == "character"){

      if(plots_to_keep != "") temp <- temp %>% filter(name %in% plots_to_keep)
    }

if(! "YTYPE" %in% names(dataset)) dataset$YTYPE <- ""

    temp   %>%
    mutate(xlog =  if_else((x != "TIME" & prop == 1 & test_neg == 0 & xlog != F) | (xlog ==T &  x != "TIME") , 1, 0)) %>%
    mutate(ylog = if_else( (y == "OBS" & prop == 1 & test_neg == 0 & xlog != F) | (ylog == T & y == "OBS"), 1, 0 )) %>%
    mutate(theo_line = if_else(y == "OBS", 1, 0)) %>%
    mutate(xscale = case_when( xlog == 1 & plotly == F ~ list(scale_x_log10(breaks = breaks_log, labels = labels_log)),
                               xlog == 1 & plotly == T ~ list(scale_x_log10()),
                               TRUE ~ list(scale_x_continuous())))%>%
    mutate(yscale = case_when( ylog == 1 & plotly == F ~ list(scale_y_log10(breaks = breaks_log, labels = labels_log)),
                               ylog == 1 & plotly == T ~ list(scale_y_log10()),
                               TRUE ~ list(scale_y_continuous())))%>%
      crossing(YTYPE = unique(dataset$YTYPE)) %>%
    mutate(plot = pmap(list(x, y, x, theo_line, xscale, yscale, YTYPE), function(a, b, c, d, e, f, YTYPE2){


      if(col_cov == T & method_smooth != "" & method_smooth != "none" ){
       geom <- list(geom_point(aes_string(x = a, y = b, group = "ID", col = paste0("factor(", cov_to_use, ")"))),
             geom_smooth(method = method_smooth, formula = y ~ x, aes_string(x = a, y = b,col = paste0("factor(", cov_to_use, ")")), se = F), labs(col = ""))
      }else if(col_cov == T & method_smooth == ""){

        geom <- list(geom_point(aes_string(x = a, y = b, group = "ID", col = paste0("factor(", cov_to_use, ")"))))
      }else if(method_smooth != ""){

        geom <- list(geom_point(aes_string(x = a, y = b, group = "ID")),
            geom_smooth(method = method_smooth, formula = y ~ x, aes_string(x = a, y = b), se = F, col = "red"))
      }else{

        geom <- list(geom_point(aes_string(x = a, y = b, group = "ID")))

      }


    datasettemp <- dataset %>% filter(YTYPE == YTYPE2)
     temp <- ggplot(data = datasettemp)+
        geom_abline(aes(intercept = 0, slope = d))+
        geom+
        theme_bw()+
        facet_wrap(~paste0(unique(datasettemp$nameRun),"\n", b, " vs ", a))+
        e + f


      labsx <- a
      labsy <- b


      if(length( unique(dataset$YTYPE)) & a %in% c("OBS", "IPRED", "PRED", "IWRES"))
        labsx <- paste0(labsx, " ", YTYPE2)

      if(length( unique(dataset$YTYPE)) & b %in% c("OBS", "IPRED", "PRED","IWRES"))
        labsy <- paste0(labsy, " ", YTYPE2)

     if(units_obs != "" & a %in% c("OBS", "IPRED", "PRED"))
       labsx <-  paste0(labsx, " (", units_obs, ")")

     if(units_obs != "" & b %in% c("OBS", "IPRED", "PRED"))
       labsy <-  paste0(labsy, " (", units_obs, ")")

     if(units_time != "" & a %in% c("TIME"))
       labsx <- paste0(labsx, " (", units_time, ")")

     if(units_time != "" & b %in% c("TIME"))
       labsy <- paste0(labsy, " (", units_time, ")")


      temp <- temp + labs(x = labsx)
      temp <- temp + labs(y = labsy)


     return(temp)
      # if(plotly == T) {return(ggplotly(output))} else {return(output)}

    })) %>%
      select(name, x, y, plot, YTYPE)%>%
      mutate(run = unique(dataset$nameRun))

  }



# GOF(dataset)
#

# If it's plotly return the plotly set

if(col_cov == T){
  out <- GOF(dataset)$plot
if(table_output == T) return(GOF(dataset))
if(plotly == F) return(invoke(plot_grid, out))

}

if(plotly == T){

  out <- GOF(dataset)$plot
  if(length(out)>1){
    return(invoke(subplot, out, nrows = 2,  margin = 0.07, shareX = F, shareY = F, titleX = T, titleY = T))
  }else{
    return(ggplotly(out[[1]]))
  }
}


# If it's not plotly and no cov output we can return what we want
############# IF NO COV output, return, ends here
if(cov_to_use == ""  ){
  if(table_output == F) return(invoke(plot_grid,   GOF(dataset)$plot))
  if(table_output == T) return(GOF(dataset))
}


## IF COV output, need to do the work for every cov
  # cov_to_use <- quo(DOSE_COV)
#
# ###### DIVIDED per COV

#
plots_per_cov <-   dataset %>%
    # group_by(DOSE_COV) %>%
    group_by_(cov_to_use) %>% #!!cov_to_use
    nest() %>%
    mutate(plots = map(data, GOF)) %>%
    unnest(plots)

    names(plots_per_cov)[names(plots_per_cov) == cov_to_use] <- "COV"

    plots_per_cov <- plots_per_cov %>%
    mutate(plot = pmap(list(COV, name, run, plot), function(a, b, c, d){

      d + facet_wrap(~ paste0(c,"\n",b," - ",  cov_to_use, " : ",  a))

    }))

    # names(plots_per_cov)[names(plots_per_cov) == "COV" ] <- cov_to_use
#
#
if (gather_by_cov == T & table_output == F){

  return(
  plots_per_cov %>%
    # group_by(DOSE_COV) %>%
    arrange(COV) %>%
    group_by(COV) %>%
    nest() %>%
    mutate(plots = map2(data, COV, function(x, y){

      invoke(plot_grid, x$plot, labels = c(y, rep("",length(x$plot) - 1)))

    })) %>%
    pluck("plots")
)





}else if (table_output == F){

return(
  plots_per_cov %>%
    # mutate(plot = pmap(list(!!cov_to_use, plot, run), function(a, b, c){
    #
    #   b + facet_wrap(paste0(c) ~ paste0(deparse(cov_to_use), " :",  a))})) %>%
    arrange(COV) %>%
    group_by(name) %>%
    nest() %>%
    mutate(plots = map2(data, name, function(x, y){


      invoke(plot_grid, x$plot)

    })) %>%
    pluck("plots")

)
} else{

  return(plots_per_cov)
}



})

# plot_GOF(object, xlog = T, ylog =T)

#' @export
setMethod(f = "plot_GOF",
          signature = "folder",
          definition =  function(object, plots_to_keep = "", cov_to_use , col_cov = F, gather_by_cov = F , plotly = F , filter = "", table_output = F, xlog = "", ylog ="", removeBLQ = T){

            test_cov_to_use <- try(typeof(cov_to_use) == "character", silent = T)

            if(class(test_cov_to_use) == "try-error"){

              cov_to_use <- deparse(substitute(cov_to_use))

            }



            object@summary %>%
              slice(object@lastSelected) %>%
              select(number) %>%
              rename(nrun = number) %>%
              mutate(plot_per_run = map(nrun, function(x){
                plot_GOF(select_run(object,  x),  plots_to_keep = plots_to_keep, cov_to_use =  !!cov_to_use , gather_by_cov = F , plotly = plotly , filter = filter, table_output = T, xlog = xlog, ylog = ylog, removeBLQ = removeBLQ)
              })) %>%
              as_tibble %>%
              {invoke(bind_rows, .$plot_per_run)} %>%
              # unnest(plot_per_run) %>%
              select(run, everything())-> table


        # table %>%
        #   group_by(name) %>%
        #   nest() %>%
        #   mutate(plot = map(data, function(x){
        #     x %>%
        #       arrange(!!cov_to_use) %>%
        #       {invoke(plot_grid, .$plot)}
        #
        #   }))
        #

        print("Use tidyverse function like %>% filter(...) %>% arrange(...) then create the plots with %>% plot_invoke ")

        return(table)
        #     summary(table)
        #
        #
        # function(filter = F, arrange = F){
        #
        #   filter <- enquo(filter)
        #   arrange <- enquo(arrange)
        #
        #   temp <- table
        #
        #   if(deparse(filter) != "~F") temp <- temp %>% filter(!!filter)
        #   if(deparse(arrange) != "~F") temp <- temp %>% arrange(!!arrange)
        #
        #   invoke(plot_grid, temp$plot)
        #
        # }



          }


)


