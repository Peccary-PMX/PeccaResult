#' Plot forest_run
#'
#' @author Thibaud Derippe
#'
#' @return
#' @export
#'
#' @examples
#' @export
# @details plot_pred() has been developped to have a quick view of individual profiles predictions superposed to observation. It is ...
setGeneric("plot_forest_run",
           function(object,  n = 1000, returnExpr = F, onlytable = F){standardGeneric("plot_forest_run")}
)
# lowerlimit = 0.0001
############################################## R U N #########################################
#' @export
#'
#'
# test <- createFolder("file:///D:/Peccary/Exemple_demo/2_DoseEffect")
# table <- tibble(Parameter = "Cl", cov = c("beta_Cl_DOSE_CAT_2_100, beta_Cl_DOSE_CAT_3_150"))
# object <- createRun("file:///D:/Peccary/Exemple_demo/2_DoseEffect/run3_Dose_CAT_Cl_V1")
# parameter = "Cl"
# plot_forest_run(test(1))
# object <- test(1)
setMethod(f = "plot_forest_run",
          signature = "run",
          definition = function(object,  n = 1000, returnExpr = F, onlytable = F){


  if(!onlytable) return(plot_forest(plot_forest_run(object,  n = n, returnExpr =returnExpr, onlytable = T)))



  # object@software
methods <- c("itself", "cat_fold", "cat_log_fold","cont_linear",  "cont_ref", "IIV_lognormal")

  estimates <- object@estimation %>%
    mutate(Parameter = gsub("_pop$", "", Parameter))

  covs <- object@cov
#
  outputt  <- tibble(param = character(), cov = character())
  covmodel <- estimates$Parameter[grep("(beta_)|(cov)", estimates$Parameter)]
  otherparam <- estimates$Parameter[!estimates$Parameter %in% covmodel & !grepl("omega_", estimates$Parameter)]




  map(names(covs)[-1], function(x){


    if(sum(grepl(x, estimates$Parameter)) >0){

      full <- estimates$Parameter[grepl(x, estimates$Parameter)]

      if(typeof(x) == "character" | length(unique(covs[[x]])) == 2){ # for categorical if character (monolix) or boolean (others)

        tibble(parameter = x, full = full) %>%
          mutate(cont_indiv = "", cont_ref = "", paramcov = full, full = gsub(paste0("(beta_?)|(", x,")"),"",full)) %>%
          mutate(analysis = map(full,function(y){

            temp <- str_split(y, "_")[[1]]

            on <- temp[map_lgl(temp, ~ sum(.x == estimates$Parameter) %>% as_logical)]
            temp <- temp[temp != on & temp != ""]
            tibble(method = factor("cat_log_fold", methods),  on = on, label = paste0(paste0( temp, collapse = "_")))

            } )) %>%
          unnest()

      }else{ # else its categorical

        tibble(parameter = x, full = full) %>%
          mutate(paramcov = full,  full = gsub(paste0("(beta_?)|(", x,")"),"",full)) %>%
          mutate(analysis = map(full,function(y){

            temp <- str_split(y, "_")[[1]]

            on <- temp[map_lgl(temp, ~ sum(.x == estimates$Parameter) %>% as_logical)]

            covs[x] %>%
              summarise(min = min(!!parse_expr(x)), max = max(!!parse_expr(x)), cont_ref = median(!!parse_expr(x))) %>%
              mutate(on = on) %>%
              gather(min, max, key = "key", value = "cont_ind") %>%
              mutate(label = paste0(cont_ind), method = factor("itself", methods)) %>%
              select(-key)

          } )) %>%
          unnest()


        }

    }else{

      tibble()

    }





  }) %>%
    bind_rows() -> temp

  temp %>%
    group_by(on) %>%
    slice(1) %>%
    mutate(parameter = on, paramcov = on, label = "Uncertainty", method = factor("itself", methods) ) -> uncertainty

  IIV <- temp %>%
    group_by(on) %>%
    slice(1) %>%
    mutate(parameter = on, paramcov = paste0("omega_",on), label = "IIV", method = factor("IIV_lognormal", methods) )

bind_rows(temp, uncertainty, IIV) %>%
    left_join(estimates %>% rename(paramcov = Parameter)) %>%
    rename( value = Value, RSE = `RSE(%)`) %>%
    select(label, parameter, value, RSE, on, method, cont_indiv, cont_ref) %>%
    mutate(use = T, delete = F) %>%
  mutate(RSE = as.double(RSE), value  = as.double(value))

 } )



# plot_forest_run(object)

# datainput <- plot_forest_run(object) %>%  filter(on == "Cl")
#' Plot forest
#'
#' @author Thibaud Derippe
#'
#' @return
#' @export
#'
#' @examples
#' @export
plot_forest <- function(datainput,  n = 1000, returnExpr = F){





  datainput <- map_if(datainput, is.factor, ~ as.character(.x)) %>% as_tibble
  datainput$value <- as.double(datainput$value)


   expr(tibble(!!!map(datainput, ~ .x)) %>%
           mutate(samples =  map2(value, RSE, ~ rnorm(mean = .x, sd = .y * abs(.x) / 100, n = !!n))) %>%
           {sampling <<- .} %>%
           left_join(sampling %>%
              filter(method == "itself") %>%
              select(parameter, samples) %>%
              rename(sample_ref  = samples, on = parameter)) %>%
  mutate(samples = pmap(list(method, cont_indiv, cont_ref, samples, sample_ref, value), function(method, cont_indiv, cont_ref, samples, sample_ref, value){

        if(method == "itself"){

            return(samples/median(samples))

        }else if(method == "cat_fold"){


        return(samples * sample_ref / median(sample_ref))

        }else if(method == "cat_log_fold"){

          return(exp(samples) * sample_ref / median(sample_ref))

        }else if(method == "cont_ref"){


          return( (cont_indiv / cont_ref)^samples * sample_ref / median(sample_ref))

        }else if(method == "IIV_lognormal"){


          return(exp(rnorm(!!n, mean = 0, sd = value)))

        }else if(method == "cont_linear"){

          return( cont_indiv * samples / median(sample_ref))

        }

  })) %>%
  mutate(mid = map_dbl(samples, ~ quantile(.x, 0.5, na.rm = T)),
         lower = map_dbl(samples, ~ quantile(.x, 0.05, na.rm = T)),
         upper = map_dbl(samples, ~ quantile(.x, 0.95, na.rm = T))) %>%
  rename( covname = parameter, paramname = on)  %>%
  mutate(LABEL =  paste0(format(round(mid,2), nsmall = 2),
                         " [", format(round(lower,2), nsmall = 2), "-",
                         format(round(upper,2), nsmall = 2), "]")) %>%
coveffectsplot::forest_plot(ref_area = c(0.8, 1/0.8),
                            x_facet_text_size = 13,
                            y_facet_text_size = 13,
                            interval_legend_text = "Median (points)\n90% CI (horizontal lines)",
                            ref_legend_text = "Reference (vertical line)\n+/- 20% ratios (gray area)",
                            area_legend_text = "Reference (vertical line)\n+/- 20% ratios (gray area)",
                            xlabel = "Fold Change Relative to Parameter",
                            facet_formula = "covname~.",
                            facet_switch = "both",
                            facet_scales = "free",
                            facet_space = "fixed",
                            paramname_shape = TRUE,
                            show_table_facet_strip = "none",
                            table_position = "right",
                            table_text_size=4,
                            plot_table_ratio = 4,
                            legend_space_x_mult = 0.5,
                            return_list = FALSE)

) -> exprl


   if(returnExpr == T){

     # return(expr({!!!c(ex, ex2, expr(ind_param))}))
     return(exprl)

   } else{


     return(eval(exprl))

   }

}

#
