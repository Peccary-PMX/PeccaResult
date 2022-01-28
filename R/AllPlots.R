#'
#' Output: a pdf with lots of plots
#' @export
setGeneric("AllPlots",
           function(object, pdf = T, pdfall = F, covPrim = "none", covSec = "none", runDataDes = 0,  addFunction = "", sensitivity =""){standardGeneric("AllPlots")}
)
# dossier(1:3)
#'
#' All plot single run
#' @export
setMethod(f = "AllPlots",
          signature = "run",
          definition = function(object, pdf = T, pdfall = F, covPrim = "none", covSec = "none", addFunction = ""){

            plots <- list(); n <- compteur()
            plots2 <- list(); n2 <- compteur()
            object2 <- object
            a <- try(results(object, plot = T)); if(class(a) != "try-error")  plots[[n()]]  <- a
            a <- try(plot_random_effect_matrix(object)); if(class(a) != "try-error")  plots[[n()]]  <- a
            a <- try(plot_random_effect_cov(object, covPrim = covPrim, covSec = covSec)); if(class(a) != "try-error")  plots[[n()]]  <- a
            a <- try(plot_GOF(object, method_smooth = "lm" )); if(class(a) != "try-error")  plots[[n()]]  <- a
            a <- try( plot_pred(object)); if(class(a) != "try-error")  plots[[n()]]  <- a
            # a <- try(plot_vpc(object)); if(class(a) != "try-error" & length(a) > 0)  plots[[n()]]  <- a
            print("Plot_grid")
            plots[[n()]] <- do.call("plot_grid", c(plots, labels = list(1:length(plots))))



            if(addFunction == "" & object@software == "NONMEM"){
              pathtemp <-  gsub(str_split(object@path, "/")[[1]][length(str_split(object@path, "/")[[1]])],"",object@path)
            } else{
              pathtemp <- object@path
            }
            searchfiles <-  grep("AdditionalPlots.R", list.files(pathtemp))

            # If the user want to adds functions
            if(addFunction != "" | length(searchfiles) > 0){

              # If it's a character and not "", it means it's the patway
              if(class(addFunction) == "character" & addFunction != ""){
                source(addFunction)
                addFunction <- functs
              }

              if(addFunction == "" & length(searchfiles) > 0){
                print("Automatic Integration of AdditionalPlots.R files")
               a <- try(source(paste0(pathtemp, "/AdditionalPlots.R")))

                addFunction <- functs
                functs
              }


            for(a in 1:length(addFunction)){
              # print("rerzerezrez")
              # print(object2)
            plots2[[n2()]] <- addFunction[[a]](input = object)
            }
              plots2[[n()]] <- do.call("plot_grid", c(plots2, labels = list(1:length(plots2))))

            }

            # If there is a creation of PDF and its monolix
            if(pdf == T & object@software == "Monolix"){

              setwd(object@path)
              if(pdfall == F){
              ggsave("Summary.pdf", plot =  plots[[length(plots)]], width = 70, height = 40 , dpi = 310, units = "cm", limitsize = F)
              shell.exec(paste0(object@path,"/Summary.pdf"))
              }
            # If there is a creation PDF and its nonmem file
            }else if(pdf == T ){ # & object@software == "NONMEM" (28/04)
              name_file <-str_split(gsub("\\\\","/",object@path), "/")[[1]]  [length( str_split(gsub("\\\\","/",object@path), "/")[[1]] )]
              # show(name_file)
              # show( paste0(gsub(name_file,"", object@path)))

              setwd(paste0(gsub(paste0(name_file,"$"),"", object@path)))
              name <- paste0(gsub(".+/", "", object@path),".pdf")
              #one bye one
              if(pdfall == F){
              ggsave(filename = name, plot =  plots[[length(plots)]], width = 70, height = 40 , dpi = 310, units = "cm", limitsize = F)

                pdf(file = name, width = 30,height = 18, pointsize = 50)
                invisible(print(plots[[length(plots)]]))
                if(length(plots2)>0){
                invisible(print(plots2[[length(plots2)]]))
                }
                dev.off()
                shell.exec(name)

             shell.exec(name)
              }
              #if on the countraty there is no pdf output (e.g. for dossier allplots)
               }else if (pdf == 0) {


                 if(addFunction[1] == ""){
                   return(plots[length(plots)])
                 }else{
                   return(list(plots[length(plots)], plots2[length(plots2)]))
                 }




               }

            else{
             # show( plots[[length(plots)]] )
              print("only pdf is doable for now")
               }

               if(pdfall == T){
               pdf("all.pdf", width = 15,height = 10, pointsize = 20)
               invisible(lapply(plots[-length(plots)], print))
               invisible(lapply(plot2s[-length(plots2)], print))
               dev.off()
               shell.exec("all.pdf")
               }

                if(addFunction == ""){
                 return("End")
                }else{
                  return("End")
                }

          }
          )

# object <- dossier(1:3)
# covPrim <- c("DOSE_COV", "STUDYID")
# pdf = T
# pdfall = F
# # covPrim = "none"
# covSec = "none"
# runDataDes = 0
# addFunction = ""
# sensitivity =""
# Addition arguments
# runDataDes: number of the run you want to use for exploratory analysis first page
# sensitivity: experimental for the moment (not important)
#'
#' All plot folder
#' @export
setMethod(f = "AllPlots",
          signature = "folder",
          definition = function(object, pdf = T, pdfall = F, covPrim = "none", covSec = "none", runDataDes = 0,  addFunction = "", sensitivity =""){

            print(covPrim)
            print(covSec)

            numbers <- object@lastSelected
            df <- object@summary


            numbersend <- numbers

            df %>%
              arrange(files) %>%
              mutate(files2 = files) %>%
              separate(files2,into = c("base2","Base"), sep = "/") -> df


            if(numbers != "all"){
              df %>%
                filter(number %in% numbers) -> df

            }


            plots <- list(); n <- compteur()


            sousgroupe = unique(df$Base)
            if(length(unique(df$Base)) == length( df$run) ) sousgroupe <- object@racine


            avancementmax <- length(df$run)
            avancement <- compteur()
            start_time <- Sys.time()
            base_length <- length(sousgroupe)
            base_n <- compteur()


            # if several subgroup, add at the beggining the comparisn of all runs
            if(length(sousgroupe) > 1){
              print("Comparative Table All Subgroups")
              results(object, type) ->  plots[[n()]]

              if(runDataDes > 0 ){
                print("Dataset Presentation")
                test <- try(plot_data(object@RunsStorage[[runDataDes]], table = T))
                # print(test)
                if(class(test) != "try-error"){
                  test -> plots[[n()]]
                }
              }

            }





            for (a in sousgroupe){



              names <- df$files[df$Base == a ]
              numbers <-  df$number[df$Base == a ]

              if(length(names) == 0 ) names <- df$run
              if(length(numbers) == 0 ) numbers <- df$number

              base_n2 <- base_n()
              print(paste0("Comparative Table (", base_n2,"/",base_length,")"))
              print( (Sys.time() -start_time))
              results(object, plot = T) +
                ggtitle(a)+
                theme(plot.title = element_text(size = 40))->  plots[[n()]]

              if(length(sousgroupe) == 1){

                if(runDataDes > 0 ){
                  print("Dataset Presentation")
                  test <- try(plot_data(object@RunsStorage[[runDataDes]], table = T))
                  # print(test)
                  if(class(test) != "try-error"){
                    test -> plots[[n()]]
                  }
                }

              }


              for(b in numbers){

                print(paste0("Run ", avancement(), "/", avancementmax))
                print((Sys.time() -start_time))
                run <- select_run(object,b )
                AllPlots(run, pdf = 0, covPrim = covPrim, covSec = covSec) -> temp
                for(a in 1:length(temp)){
                  temp[[a]]  ->  plots[[n()]]
                }


              }
              plots2 <- list(); n2 <- compteur()

              print(paste0("plot_pred (", base_n2,"/",base_length,")"))
              print( (Sys.time() -start_time))
              c <- try(plot_pred(object, ylog = T)); if(class(c) != "try-error"){
                # plots2[[n2()]]  <- c
                plots2[[n2()]]  <- c
              }

              print(paste0("plot_random_effect_cov (", base_n2,"/",base_length,")"))
              print( (Sys.time() -start_time))
              c <- try(plot_random_effect_cov(object, covPrim = covPrim))
              if(class(c) != "try-error"){

                plots2[[n2()]]  <- c
              }

              # print(paste0("plot_vpc (", base_n2,"/",base_length,")"))
              # print( (Sys.time() -start_time))
              # c <- try(plot_vpc(object, numbers = numbers))
              # if(class(c) != "try-error"){
              #   # plots2[[n2()]]  <- c
              # }
             if(length(plots2)>0)
               do.call(plot_grid, plots2) ->  plots[[n()]]
              # do.call(plot_grid, c(plots2, ncol = as.list(1))) ->  plots[[n()]]
            }# end of per subgroup
            if(length(sousgroupe) > 1){

              print("Comparative Plots All Subgroups")
              plots3 <- list(); n2 <- compteur()
              numbers <- numbersend
              print( (Sys.time() -start_time))
              c <- try(plot_pred(object, ylog = T)); if(class(c) != "try-error"){
                # plots3[[n2()]]  <- c
                plots3[[n2()]]  <- c
              }

              print( (Sys.time() -start_time))
              c <- try(plot_random_effect_cov(object, covPrim = covPrim))
              if(class(c) != "try-error"){
                plots3[[n2()]]  <- c
              }

              # print( (Sys.time() -start_time))
              # c <- try(plot_vpc(object, numbers = numbers))
              # if(class(c) != "try-error"){
              #   plots3[[n2()]]  <- c
              # }

              if(length(plots3) == 2){
                invoke(plot_grid, plots3, ncol = 1 ) ->  plots[[n()]]

              }else{
                do.call(plot_grid, plots3) ->  plots[[n()]]

              }

            }


            # pdf creation
            name <- str_split(object@racine,"/")[[1]][str_split(object@racine,"/")[[1]] != ""]
            pdfname <- paste0(name[length(name)],".pdf")
            setwd(object@racine)
            setwd('..')
            pdf(pdfname, width = 30,height = 18, pointsize = 50)
            invisible(lapply(plots[], print))
            dev.off()
            shell.exec(pdfname)
            print(Sys.time() -start_time)
            return("")
          })




#### Abandonned functions

#
# setMethod(f = "AllPlots",
#           signature = "character",
#           definition = function(object, numbers , last = F, sensitivity ="",  covprim = "all", covsec = ""){
#
#             if(last == T){
#               run <- createDossier(object)
#               run@summary %>%
#                 arrange(desc(Date)) %>%
#                 slice(1) -> mostresecent
#               run <- createRun(paste0(object, mostresecent$files))
#               AllPlots(run,  covPrim = covprim, covSec = covsec )
#             }else{
#
#
#               if(file.exists(paste0(object,"/finegrid.txt")) == T | file.exists(paste0(object, ".tab")) == T ){
#                 run <- createRun(object)
#                 print("You must use covprim and covsec without maj ")
#
#                 AllPlots(run,  covPrim = covprim, covSec = covsec )
#               } else{
#                 run <- createDossier(object)
#                 print("You must use covprim and covsec without maj ")
#                 if(class(run)=="run"){
#                   AllPlots2(run,  covPrim = covprim, covSec = covsec )
#                 }
#               }
#             }
#             return(run)
#           })


#+theme(axis.text.y=element_blank(),strip.text.x = element_text(size = 8))
#
# setMethod(f = "AllPlots2",
#           signature = "dossier",
#           definition = function(object, sensitivity ="", covPrim = "all", covSec = "", numbers = "all"){
#
#             numbers <- object@lastSelected
#             df <- object@summary
#
#
#             numbersend <- numbers
#
#             df %>%
#               arrange(files) %>%
#               mutate(files2 = files) %>%
#               separate(files2,into = c("base2","Base"), sep = "/") -> df
#
#
#             if(numbers != "all"){
#               df %>%
#                 filter(number %in% numbers) -> df
#
#             }
#
#
#             plots <- list(); n <- compteur()
#
#
#             sousgroupe = unique(df$Base)
#             if(length(unique(df$Base)) == length( df$run) ) sousgroupe <- object@racine
#
#
#             avancementmax <- length(df$run)
#             avancement <- compteur()
#             start_time <- Sys.time()
#             base_length <- length(sousgroupe)
#             base_n <- compteur()
#
#
#             # if several subgroup, add at the beggining the comparisn of all runs
#             if(length(sousgroupe) > 1){
#               print("Comparative Table All Subgroups")
#               plot_text(object, numbers = numbers, type = 1) ->  plots[[n()]]
#             }
#
#
#
#
#             for (a in sousgroupe){
#
#               names <- df$files[df$Base == a ]
#               numbers <-  df$number[df$Base == a ]
#
#               if(length(names) == 0 ) names <- df$run
#               if(length(numbers) == 0 ) numbers <- df$number
#
#               base_n2 <- base_n()
#               print(paste0("Comparative Table (", base_n2,"/",base_length,")"))
#               print( (Sys.time() -start_time))
#               plot_text(object, type = 1) +
#                 ggtitle(a)+
#                 theme(plot.title = element_text(size = 40))->  plots[[n()]]
#
#               for(b in numbers){
#
#                 print(paste0("Run ", avancement(), "/", avancementmax))
#                 print((Sys.time() -start_time))
#                 run <- select_run(object,b )
#                 AllPlots(run, pdf = 0, covPrim = covPrim, covSec = covSec) -> temp
#                 for(a in 1:length(temp)){
#                   temp[[a]]  ->  plots[[n()]]
#                 }
#
#
#               }
#               plots2 <- list(); n2 <- compteur()
#
#               print(paste0("plot_pred (", base_n2,"/",base_length,")"))
#               print( (Sys.time() -start_time))
#               c <- try(plot_pred(object)); if(class(c) != "try-error"){
#                 # plots2[[n2()]]  <- c
#                 plots2[[n2()]]  <- c + scale_y_log10(labels = labels_log, breaks = breaks_log)
#               }
#
#               print(paste0("plot_random_effect_cov (", base_n2,"/",base_length,")"))
#               print( (Sys.time() -start_time))
#               c <- try(plot_random_effect_cov(object, type = 1, covPrim = covPrim))
#               if(class(c) != "try-error"){
#                 plots2[[n2()]]  <- c
#               }
#
#               # print(paste0("plot_vpc (", base_n2,"/",base_length,")"))
#               # print( (Sys.time() -start_time))
#               # c <- try(plot_vpc(object, numbers = numbers))
#               # if(class(c) != "try-error"){
#               #   # plots2[[n2()]]  <- c
#               # }
#               do.call(plot_grid, plots2) ->  plots[[n()]]
#               # do.call(plot_grid, c(plots2, ncol = as.list(1))) ->  plots[[n()]]
#             }# end of per subgroup
#             if(length(sousgroupe) > 1){
#
#               print("Comparative Plots All Subgroups")
#               plots3 <- list(); n2 <- compteur()
#               numbers <- numbersend
#               print( (Sys.time() -start_time))
#               c <- try(plot_pred(object)); if(class(c) != "try-error"){
#                 # plots3[[n2()]]  <- c
#                 plots3[[n2()]]  <- c + scale_y_log10(labels = labels_log, breaks = breaks_log)
#               }
#
#               print( (Sys.time() -start_time))
#               c <- try(plot_random_effect_cov(object, type = 1, covPrim = covPrim))
#               if(class(c) != "try-error"){
#                 plots3[[n2()]]  <- c
#               }
#
#               # print( (Sys.time() -start_time))
#               # c <- try(plot_vpc(object, numbers = numbers))
#               # if(class(c) != "try-error"){
#               #   plots3[[n2()]]  <- c
#               # }
#               do.call(plot_grid, plots3) ->  plots[[n()]]
#             }
#
#
#             # pdf creation
#             name <- str_split(object@racine,"/")[[1]][str_split(object@racine,"/")[[1]] != ""]
#             pdfname <- paste0(name[length(name)],".pdf")
#             setwd(object@racine)
#             setwd('..')
#             pdf(pdfname, width = 30,height = 18, pointsize = 50)
#             invisible(lapply(plots[], print))
#             dev.off()
#             shell.exec(pdfname)
#             print(Sys.time() -start_time)
#             return("")
#           })
#
#
#
#
