#' @export
setGeneric("plot_random_effect_matrix",
           function(object, table = F, filterr = ""){standardGeneric("plot_random_effect_matrix")}
)

#' Random Effect Matrix
#' Author: Thibaud Derippe
#' Arguments: object, a run
#' Output: random effect matrix plots
#' @export
setMethod(f = "plot_random_effect_matrix",
          signature = "run",
          definition =  function(object, table = F, filterr = "") {


            test_filterr <- try(typeof(filterr) == "character", silent = T)

            if(class(test_filterr) == "try-error"){

              filterr <- deparse(substitute(filterr))

            }

            datatemp <- object@randomEffect %>%
              select(-ID)

           # a <-  try(library(GGally));a
           #
           #  if(class(a) != "try_error"){
           #
           #    return(
           #  ggpairs(datatemp, lower=list(continuous="smooth"))+
           #    theme_bw()
           #    )
           #  }else{



            sapply(datatemp, function(x){
              length(unique(x))
            }) -> verif

            datatemp <- datatemp[ , verif != 1]

            matchs <- crossing(y = names(datatemp),x = names(datatemp)) %>%
              mutate(test = 1)


            for (a in 1:length(matchs$y)){

              test <- paste0(matchs$y[a],matchs$x[a]) %in%  paste0(matchs$x[1:(a-1)],matchs$y[1:(a-1)])

              if(test == FALSE){
                matchs$test[a] <- 0
              }

              if(matchs$y[[a]] ==  matchs$x[[a]]){

                matchs$test[a] <- 1
              }
            }



            result <- list()


          lapply(1:nrow(matchs), function(x){

              if(matchs$test[[x]] == 0 & table == F ){

                ncol1 <- matchs$y[[x]]
                ncol2 <- matchs$x[[x]]

                ncol12 <- gsub("eta_","", ncol1)
                ncol12 <- gsub("RE_","", ncol12)
                ncol12 <- gsub("_mode","", ncol12)

                ncol22 <- gsub("eta_","", ncol2)
                ncol22 <- gsub("RE_","", ncol22)
                ncol22 <- gsub("_mode","", ncol22)

                lm <- lm(datatemp[[ncol2]] ~ datatemp[[ncol1]])
                # summary(lm)$coefficients
                # str(summary(lm))

                if(summary(lm)$coefficients[8] > 0.05){
                base <- ggdraw() +
                  draw_label(x = 0.5, y= 0.8, paste0(ncol12,  " ~ ", ncol22), fontface='bold', size = 10)
                } else {
                  base <- ggdraw() +
                    draw_label(x = 0.5, y= 0.8, paste0(ncol12,  " ~ ", ncol22), fontface='bold', size = 10, color =   "red")



                }
                ### Adding slope in red or black according the value of the test
                if(summary(lm)$coefficients[8] > 0.05){
              base <-   base + draw_label(x = 0.5, y= 0.5,
                             paste0("Slope: ", signif(summary(lm)$coefficients[2],3)), size = 10)+
                            draw_label(x = 0.5, y= 0.4, paste0(
                                    "Pr(>|t|) = ", signif(summary(lm)$coefficients[8],2)), size = 10)
                }else{
                  base <-   base + draw_label(x = 0.5, y= 0.5,
                                              paste0("Slope: ", signif(summary(lm)$coefficients[2],3)), size = 10, color = "red")+
                    draw_label(x = 0.5, y= 0.4, paste0(
                      "Pr(>|t|) = ", signif(summary(lm)$coefficients[8],2)), size = 10, color = "red")
                }
                ### Adding intercept in red or black according the value of the test
                # if(summary(lm)$coefficients[7] > 0.05){
                #   base <-   base + draw_label(x = 0.5, y= 0.4,
                #                               paste0("Intercept: ", round(summary(lm)$coefficients[7],4)), size = 10)+
                #     draw_label(x = 0.5, y= 0.3, paste0(
                #       "Pr(>|t|) = ", round(summary(lm)$coefficients[7],2),"."), size = 10)
                # }else{
                #   base <-   base + draw_label(x = 0.5, y= 0.4,
                #                               paste0("Intercept: ", round(summary(lm)$coefficients[7],4)), size = 10, col = "red")+
                #     draw_label(x = 0.5, y= 0.3, paste0(
                #       "Pr(>|t|) = ", round(summary(lm)$coefficients[7],2),"."), size = 10, col = "red")
                # }
                #
              temp <- base


              }else{


                      ncol1 <- matchs$y[[x]]
                      ncol2 <- matchs$x[[x]]

                      if(ncol1 != ncol2){

                        lm <- lm(datatemp[[ncol1]] ~ datatemp[[ncol2]])
                        testslope <- round(summary(lm)$coefficients[8],3)
                        label <- paste0("Slope: ", round(coef(lm)[[2]],2), "\n ",
                                        "t.test: ", testslope*100,"%" )
                        ggplot()+
                          geom_point(aes(datatemp[[ncol2]], datatemp[[ncol1]]))+
                          geom_hline(yintercept = 0, lty = 3)+
                          # geom_vline(xintercept = 0, lty = 3)+
                          labs(x = ncol2, y = ncol1)+
                          theme_light() -> temp

                        if(testslope < 0.05){
                        temp +
                            # geom_text(aes(x = max(datatemp[[ncol1]])*0.7, y = max( datatemp[[ncol2]])*0.8, label = label), size = 1, col = "red")+
                            geom_abline(intercept = coef(lm)[[1]], slope = coef(lm)[[2]], col = "red")
                        }else{

                          temp +
                            # geom_text(aes(x = max(datatemp[[ncol1]])*0.7, y = max( datatemp[[ncol2]])*0.8, label = label), size = 1)+
                          geom_abline(intercept = coef(lm)[[1]], slope = coef(lm)[[2]], col = "blue")

                        }
                      }else{

                        x <-  seq(min(datatemp[[ncol1]]), max(datatemp[[ncol1]]), 0.01)
                        y <-  dnorm(x,0, sd(datatemp[[ncol1]]))

                        ggplot()+

                          geom_histogram(aes(x = datatemp[[ncol1]], y = ..density..), fill = "grey", col = "grey",alpha=0.3)+
                          geom_density(aes(x=datatemp[[ncol1]]), col = "darkgrey", size = 1.5)+
                          geom_density(aes(x=datatemp[[ncol1]]), col = "black", size = 0.5)+
                          labs(x = ncol1, y= ncol1)+
                          geom_vline(xintercept = 0, col = "steelblue")+
                          theme_light()+
                          geom_density(x = y, col = "black", size = 1.5)


                      }

                    }}
                    ) -> plots




          if(table == T){


            temp <-        matchs %>%
              mutate(plot = map(plots, function(x){
                x + ggtitle(object@name) + theme(plot.title = element_text(hjust = 0.5))
              })) %>%
              mutate(type = if_else(y == x, "Histogram", "Regression")) %>%
              mutate(name = if_else(y == x, gsub("eta_","",gsub("_mode", "", x)),gsub("eta_","",gsub("_mode", "",paste0(x,"vs",y))))) %>%
              mutate(y = gsub("eta_","",gsub("_mode", "", y))) %>%
              mutate(x = gsub("eta_","",gsub("_mode", "", x))) %>%
              select(name, x, y,  type, plot) %>%
              mutate(y = if_else(type == "Histogram", "-",y)) %>%
              mutate(run = object@name)

            if(filterr != ""){

              temp <- temp %>%
                filter_(filterr)

            }

            return(temp)

          }


          #
            n <- length(plots)
          #
            n2 <- sqrt(n)
           y <-  seq(1, n , n2)[- length(seq(1, n , n2))]
          xy <-   seq(1, n , n2)[length(seq(1, n , n2))]
          X <- seq(xy, n, 1)[-1]

          rel <- 1.3
          rel_widths <- rep(1,n)
          rel_heights <- rep(1,n)
          #remove all
          for(a in (1:n)[-c(X,y,xy)]){
            plots[[a]] <- plots[[a]] +
              theme(axis.text.x=element_blank(),
                    axis.ticks.x =element_blank(),
                    axis.title.x =element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks.y =element_blank(),
                    axis.title.y =element_blank())

          }
          #remove x
          for(a in y){

            plots[[a]] <- plots[[a]] +
              theme(axis.text.x=element_blank(),
                    axis.ticks.x =element_blank(),
                    axis.title.x =element_blank()
           )

          }


          for(a in X){

            plots[[a]] <- plots[[a]] +
              theme(       axis.text.y=element_blank(),
                           axis.ticks.y =element_blank(),
                           axis.title.y =element_blank()
              )

          }

          for(a in 1:n){

            plots[[a]] <- plots[[a]] + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

          }

          rel_widths[c(y, xy)] <- rel
          rel_heights[c(X, xy)] <- 10

          result <- suppressMessages(do.call(plot_grid, c(plots, rel_heights = list( rel_heights), rel_widths = list(rel_widths) ) ))

            return(result)
          }
        #  }

)

#' @export
setMethod(f = "plot_random_effect_matrix",
          signature = "folder",
          definition =  function(object, table = T, filterr = "") {

            print("Use tidyverse function like %>% filter(...) %>% arrange(...) then create the plots with %>% plot_iinvoke ")

            test_filterr <- try(typeof(filterr) == "character", silent = T)

            if(class(test_filterr) == "try-error"){

              filterr <- deparse(substitute(filterr))

            }


            tibble(n_run = object@lastSelected) %>%
              mutate(run = map(n_run, function(x) select_run(object, x))) %>%
              mutate(ind = map(run, function(x) plot_random_effect_matrix(x, table = T, filterr = filterr))) %>%
              select(-run) %>%
              unnest(ind)



          })



