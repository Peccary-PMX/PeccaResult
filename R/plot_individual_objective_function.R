#' @export
setGeneric("plot_individual_objective_function",
           function(object, histogram = F, cov = F){standardGeneric("plot_individual_objective_function")}
)

#' @export
setMethod(f = "plot_individual_objective_function",
          signature = "run",
          definition =  function(object, histogram = F, cov = F) {


            if(object@software == "NONMEM"){

              dataset <- suppressMessages(readTablePerso(paste0(object@path,".phi"), skip = 1, header = T) %>%
                select(ID, OBJ) %>%
                left_join(object@cov) %>%
                mutate(ID = factor(ID)))

              label <- "Individual Objective Function"

            }else{

              dataset <- suppressMessages( readTablePerso(file.path(object@path,"individualContributionToLL.txt"),  skip = 1, header = F)  %>%
                                             rename(ID = V1 ) %>%
                                             rename(OBJ = V2) %>%
                mutate(ID = as.character(ID)) %>%
                # rename(OBJ = Linearization) %>%
                left_join(object@cov) %>%
                mutate(ID = factor(ID))) %>%
                mutate(OBJ = -2 * OBJ)


              label <- "Individual Objective Function"
            }



        if((cov == F | cov == "None")  & histogram == F){

          output <-  ggplot(data = dataset) +
            geom_bar(aes(fct_reorder(ID,OBJ),OBJ , group = ID ),col = "black", stat = "identity")+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 60, hjust = 1))+
            labs(x = "ID", y = label)

        }else if((cov != F & cov != "None") & histogram == F){

          output <-  ggplot(data = dataset) +
            geom_bar(aes_string("fct_reorder(ID,OBJ)","OBJ" , fill = paste0("factor(",cov,")"), group = "ID" ), col = "black", stat = "identity")+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 60, hjust = 1))+
            labs(fill = cov,x = "ID", y = label)

        }else if((cov == F | cov == "None") & histogram == T){

          output <-  ggplot(data = dataset) +
            geom_histogram(aes(OBJ), col = "black")+
            theme_bw() +
            labs(x = label)

        }else{


          output <-  ggplot(data = dataset) +
            geom_histogram(aes_string("OBJ", fill = paste0("factor(",cov,")")), col = "black")+
            theme_bw()+
            labs(fill = cov, x = label)
        }



            return(output)
          })



#' @export
setMethod(f = "plot_individual_objective_function",
          signature = "folder",
          definition =  function(object,  histogram = F, cov = F) {



            n <- object@lastSelected[1:2]
            n1 <- select_run(object,n[1])@name
            n2 <- select_run(object,n[2])@name

            if(object@software == "NONMEM"){

              temp <- tibble(n_run = n) %>%
                mutate(run = map(n_run, function(x)readTablePerso(paste0(select_run(object,x)@path,".phi"), skip = 1, header = T) %>% select(ID, OBJ) %>%
                  mutate(ID = factor(ID)) %>%
                  mutate(run = select_run(object,x)@name) %>%
                  left_join(select_run(object,x)@cov %>% mutate(ID = factor(ID)))  )) %>%
                unnest

              label <- "Difference Individual Objective Function"

            }else{

              temp <- suppressMessages(

                tibble(n_run = n) %>%
                mutate(run = map(n_run, function(x) readTablePerso(paste0(select_run(object,x)@path,"/individualContributionToLL.txt"),  skip = 1, header = F)  %>%
                                   rename(ID = V1 ) %>%
                                   rename(OBJ = V2) %>%
                                   mutate(ID = factor(ID)) %>%
                                   left_join(select_run(object,x)@cov) %>%
                                   mutate(run = select_run(object,x)@name))) %>%
                unnest) %>%
                mutate(OBJ = -2 * OBJ)

              label <- "Difference Individual Objective Function"

            }

            temp %>%
              group_by(run ) %>%
              summarise(sum(OBJ))


            if((cov == F | cov == "None") & histogram == F){
              output <-    temp %>%
                select(- run) %>%
                spread(key = "n_run",  value = "OBJ") %>%
                mutate_(diff = paste0("`",n[[1]],"` - `",n[[2]],"`")) %>%
                ggplot() +
                geom_bar(aes(fct_reorder(factor(ID),diff),diff , group = ID ),col = "black", stat = "identity")+
                geom_vline(xintercept = 0, lty = 2) +
                theme_bw()+
                facet_wrap(~paste0("Ind. Obj. Func.: ", n1, " minus ", n2))+
                theme(axis.text.x = element_text(angle = 60, hjust = 1))+
                labs(x = "ID", y = label)

            }else if((cov != F & cov != "None") & histogram == F){

              output <-    temp %>%
                select(- run) %>%
                spread(key = "n_run",  value = "OBJ") %>%
                mutate_(diff = paste0("`",n[[1]],"` - `",n[[2]],"`")) %>%
                ggplot() +
                geom_bar(aes_string("fct_reorder(factor(ID),diff)","diff" , fill = paste0("factor(",cov,")"), group = "ID" ), col = "black", stat = "identity")+
                geom_vline(xintercept = 0, lty = 2) +
                theme_bw()+
                facet_wrap(~paste0("Ind. Obj. Func.: ", n1, " minus ", n2))+
                theme(axis.text.x = element_text(angle = 60, hjust = 1))+
                labs(x = "ID", y = label,fill = cov)


              temp %>%
                filter(ID == 45794)


            }else if((cov == F | cov == "None") & histogram == T){

              output <-   temp %>%
                select(- run) %>%
                spread(key = "n_run",  value = "OBJ") %>%
                mutate_(diff = paste0("`",n[[1]],"` - `",n[[2]],"`")) %>%
                ggplot() +
                geom_histogram(aes(diff), col = "black")+
                geom_vline(xintercept = 0, lty = 2) +
                theme_bw()+
                facet_wrap(~paste0("Ind. Obj. Func.: ", n1, " minus ", n2))+
                labs(x = label)

            }else{

              output <-  temp %>%
                select(- run) %>%
                spread(key = "n_run",  value = "OBJ") %>%
                mutate_(diff = paste0("`",n[[1]],"` - `",n[[2]],"`")) %>%
                ggplot() +
                geom_histogram(aes_string("diff", fill = paste0("factor(",cov,")")), col = "black")+
                geom_vline(xintercept = 0, lty = 2) +
                theme_bw()+
                facet_wrap(~paste0("Ind. Obj. Func.: ", n1, " minus ", n2))+
                labs(x = label, fill = cov)


            }


          return(output)



          })
