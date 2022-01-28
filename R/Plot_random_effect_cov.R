#' @export
setGeneric("plot_random_effect_cov",
           function(object, covPrim = "none", covSec ="none", output = "grid", boxplotPoint = F, boxplotest = "anova",  ...){standardGeneric("plot_random_effect_cov")}
)


#' BoxPlot according covariabe
#' Author: Thibaud Derippe
#' Arguments: object, a run or severals through a dossier
#' Arguments: covPrim : wich cov to use in x axis
#' Arguments: CovSec: ggplot color separation
#' ARguments: output : grid always (removed?)
#' Arguments: boxplotPoint if you want to add the observation point
#' Output: IndPred plots
#' @export

setMethod(f = "plot_random_effect_cov",
          signature = "run",
          definition =  function(object, covPrim = "none", covSec ="none", output = "grid", boxplotPoint = F, boxplotest = "anova") {

            if(covPrim == "none") covPrim <- names(object@cov)[-1] # par d?faut on prend tout

            if(covSec == "none"){ #on prend ou pas en compte la covariable "secondaire"
              covSec <- ""
            datacov <- object@cov[c("ID", covPrim)]
            }else{
              datacov <- object@cov[c("ID", covPrim, covSec)]
            }

            datatemp <- suppressMessages(object@randomEffect %>%
              left_join( datacov) %>%
              select(-ID) %>%
              gather( starts_with("ET") , key = "eta", value = "value"))

            ## on retir les eta nulles
            datatemp %>%
              filter(value < -0.005 | value >0.005) %>%
              group_by(eta) %>%
              summarise(length(value)) -> testlongueur

            datatemp %>%
              filter(eta %in% testlongueur$eta) -> datatemp





            plots <- list(); n <- compteur()
            for(a in covPrim[covPrim != covSec]){ # pour chaque cov primaire

              # Categorielle ou continue?
              datatemp %>%
                filter(eta == datatemp$eta[[1]]) -> catorcont

              catorconttest <- length(unique(catorcont[[a]]))/length(catorcont[[a]])
              if(length(unique(catorcont[[a]]))>8) catorconttest <- 1
              # On r?cupere les valeure utiles

              if(covSec == ""){
              datatemp[c(a, "eta", "value")] -> datatemp2
              }else{
                datatemp[c(a, "eta", "value", covSec)] -> datatemp2
              }
              # If categorial covariate
              if(catorconttest < 0.75 | is.na(catorconttest) == T ){

                datatemp2[[1]] <- factor(datatemp2[[1]]) # on factorise la covariable

            ggplot(data = datatemp2 %>% mutate(label = paste0(gsub("_mode$","",eta)," vs ",a)))+
              geom_boxplot(aes_string(x = a,  y = "value"))+
              stat_compare_means(aes_string(x = a,  y = "value"), method= boxplotest)+
              facet_wrap(~label, scales = "free")+
              geom_hline(yintercept = 0, lty = 2)+
              theme_bw()+
              theme(legend.position="bottom")+
              labs(x = names(datatemp2)[1]) -> temp





            if(boxplotPoint == T) temp <- temp + geom_point(aes_string(x = a,  y = "value"))

            if(covSec == ""){
            temp -> plots[[n()]]#-> result
            } else {


            ggplot(data = datatemp2)+
              geom_boxplot(aes_string(x = a,  y = "value", col = paste0("as.factor(",covSec,")")))+
                stat_compare_means(aes_string(x = a,  y = "value"), method= boxplotest)+
              facet_wrap(~eta, scales = "free")+
              geom_hline(yintercept = 0, lty = 2)+
              theme_bw()+
              labs(x = names(datatemp2)[1])+
              theme(legend.position="bottom") -> temp

              if(boxplotPoint == T) temp <- temp + geom_point(position = position_dodge(width=0.75), aes_string(x = a,  y = "value", fill = paste0("as.factor(",covSec,")")),pch =21, colour="black" )
              temp -> plots[[n()]]
              # plots <- plots[[length(plots)]]
            }
              }else{

                # inspired by     https://stackoverflow.com/questions/19699858/ggplot-adding-regression-line-equation-and-r2-with-facet

                # to take the result string
                lm_eqn = function(datatemp2){
                  m = lm(as.formula(paste0("value ~ ", a)), datatemp2);
                  eq <- substitute(italic(y) == a + b %.% italic(x)*","~ ttest == pvalue,  #~italic(r)^2~"="~r2 *
                                   list(a = format(coef(m)[1], digits = 2),
                                        b = format(coef(m)[2], digits = 2),
                                        r2 = format(summary(m)$r.squared, digits = 3),
                                        pvalue = format(summary(m)$coefficients[8], digits = 3)))
                  as.character(as.expression(eq))
                }

                red <- function(datatemp2){
                  m = lm(as.formula(paste0("value ~ ", a)), datatemp2);
                  if(summary(m)$coefficients[8] > 0.05){
                    return("black")
                  } else{
                    return("red")
                  }
                }



                #to create the rigth dataset
                eq <- data.frame(eta = character(), V1 = character(), red = character())
                for(b in unique(datatemp2$eta)){

                  eq <- rbind(eq, data.frame(eta = b, V1= lm_eqn(datatemp2[datatemp2$eta == b, ]),
                                             red = red(datatemp2[datatemp2$eta == b, ] )))

                }


                # to know where to put the result string
                suppressMessages(datatemp2 %>%
                group_by(eta) %>%
                  summarise_(ymax = "max(value)", ymin =  "min(value)", xmax = paste0("max(",a,",na.rm = T)"), xmin = paste0("min(",a,",na.rm = T)")) %>%
                  mutate(xmean = mean(c(xmin, xmax))) %>%
                  left_join(eq))  -> analyse



                ggplot(data = datatemp2)+
                  geom_point(aes_string(x = names(datatemp2)[1],  y = "value"))+
                  facet_wrap(~eta, scales = "free")+
                  geom_hline(yintercept = 0, lty = 2)+
                  geom_smooth(aes_string(x = a,  y = "value"), method = "lm", se=FALSE, color="red", formula = y ~ x)+
                  geom_text(data=analyse[analyse$red == "black",], aes(x = xmean, y = 0.9* ymax,label=V1), parse = TRUE, size = 2, inherit.aes=FALSE)+
                  geom_text(data=analyse[analyse$red == "red",], aes(x = xmean, y = 0.9* ymax,label=V1), col = "red", parse = TRUE, size = 2, inherit.aes=FALSE)+
                  theme_bw()+
                  labs(x = names(datatemp2)[1]) -> plots[[n()]]#-> result

            }

            }
            # the ouput of the function, depending on the according parameter
            if(output == "grid"){
            return(do.call("plot_grid", c(plots, list(ncol = 1))))
            } else if(output == "pdf_ind"){
              setwd(object@path)
              pdf("eta_cov_ind.pdf", width = 15,height = 9, pointsize = 50)
              invisible(lapply(plots[], print))
              dev.off()
              shell.exec("eta_cov_ind.pdf")
              print(Sys.time() -start_time)
            }else if(output == "plotly"){

              plots <- lapply(plots, function(x){
                x <- ggplotly(x)
                x
              })
              return(plots)
            }


          }
)


#' @export
setMethod(f = "plot_random_effect_cov",
          signature = "folder",
          definition =  function(object, covPrim = "none",covSec = "none", output = 1,  boxplotPoint = F){

            numbers <- object@lastSelected
            names <- object@summary$run[numbers]

            plots <- list(); n <- compteur()
        if(output == 0){
            for(a in numbers){

              title <- ggdraw() + draw_label(names[a], size=10, fontface="bold")
              plot <- plot_random_effect_cov(select_run(object,a ),covPrim = covPrim, covSec = covSec, boxplotPoint = boxplotPoint)
              plot_grid(title, plot, ncol=1, rel_heights=c(0.1, 1))  ->  plots[[n()]]

            }

            plots[[n()]] <- do.call("plot_grid", plots)
            return(plots)
        }else if(output == 1){

            b = 1
            etas <- list()
            #create a list of all etas
            for (a in numbers){

              runTemp <-  select_run(object,a )
              runTemp@randomEffect %>% mutate(Run = names[b]) -> etatemp

              if(covPrim == "none" | covPrim ==""){
                datacov <-  runTemp@cov
              }else{
                datacov <-  runTemp@cov
                datacov <- datacov[c("ID", covPrim)]

              }

              suppressMessages(etatemp %>%
                left_join(datacov) %>%
                select(-ID))-> etas[[b]]
              b <- b + 1
            }

            #and bind them
            # In case they don't have the same cov ( for "none" setup)


            # namesFirst <- names(etas[[1]])
            # etas <-  lapply(etas, function(x){
            #
            #   x %>%
            #     select( which(names(etas[[1]]) %in% namesFirst))
            #
            # })
            #
            # dfetas <-  do.call("bind_rows", etas)
            #

            dfetas <- etas[[1]]
            if(length(etas) >1){
            for (a in 2:length(etas)){

              dfetas <- suppressMessages(dfetas %>%
                full_join(etas[[a]]))

            }
            }

            datatemp <- dfetas %>%
              gather( starts_with("ET") , key = "eta", value = "value")

            datatemp %>%
              filter(value < -0.05 | value >0.05) %>%
              group_by(eta) %>%
              summarise(length(value)) -> testlongueur

            datatemp %>%
              filter(eta %in% testlongueur$eta) -> datatemp

            namesdatatemp <- names(datatemp)
            covfound <- namesdatatemp[- c(1, length(namesdatatemp), length(namesdatatemp)-1)]

            plots <- list(); n <- compteur()
            for(a in covfound){

              # Categorielle ou continue?
              datatemp %>%
                filter(eta == datatemp$eta[[1]]) -> catorcont

              catorconttest <- length(unique(catorcont[[a]]))/length(catorcont[[a]])
              if(length(unique(catorcont[[a]]))>8) catorconttest <- 1


              datatemp %>%
                select_("Run",a, "eta", "value") %>%
                filter_(paste0("is.na(",a,") ==F"))-> datatemp2



              if((catorconttest < 0.75 | is.na(catorconttest) == T) & length(unique(datatemp2$Run)) >1 ){

                datatemp2[[2]] <- factor(datatemp2[[2]])
              plottemp <-   ggplot(data = datatemp2)+
                  geom_boxplot(aes_string(x = names(datatemp2)[2],  y = "value", col ="Run"))+
                  facet_wrap(~eta, scales = "free")+
                  geom_hline(yintercept = 0, lty = 2)+
                  theme_bw()+
                   # theme(legend.position="bottom", legend.direction="vertical")+
                  labs(x = names(datatemp2)[2])

              if(boxplotPoint == T){
                plottemp <-  plottemp + geom_point(position=position_dodge(width=0.75), aes_string(x = a,  y = "value", fill = paste0("as.factor(","Run",")")),pch =21, colour="black" )+
                  guides(fill = F)
              }
              plottemp -> plots[[n()]]#-> result

              }else{
                # lm <- lm(datatemp2[["value"]] ~ datatemp2[[1]])

                # ggplot(data = datatemp2)+
                #   geom_point(aes_string(x = names(datatemp2)[2],  y = "value", col ="Run"))+
                #   facet_wrap(~eta, scales = "free")+
                #   geom_hline(yintercept = 0, lty = 2)+
                #   # theme(legend.position="bottom", legend.direction="vertical")+
                #   # geom_abline(intercept = coef(lm)[1], slope = coef(lm)[2])+
                #   theme_bw()+
                #   labs(x = names(datatemp2)[2]) -> plots[[n()]]#-> result

              }


            }
        }

          if(length(plots) == 2) return(invoke("plot_grid", plots, ncol = 1))

            return(invoke("plot_grid", plots))
          }
)

