#' @export
setGeneric("run_cov_summary",
           function(object,  covPrim = "", covSec = ""){standardGeneric("run_cov_summary")}
)

# Very new and experimental, not important for the moment
#' @export
setMethod(f = "run_cov_summary",
          signature = "run",
          definition = function(object, covPrim = "", covSec = ""){



            if(covPrim != "" & covSec != ""){
         test <-  object@cov %>%
            group_by_(covPrim, covSec) %>%
            summarise(n = length(ID))

         temp <- as.data.frame(test) %>%
            spread(key = covSec , value = n)

         temp <-  temp %>%
           mutate(Total =  rowSums(temp[, -1], na.rm = T))

         a <- 1
         temp <-data.frame(lapply(temp, function(x){
           if(a == 1){
             x <- c(x, "Total")
           }else{
           x <- c(x, sum(x, na.rm = T))
           }
           a <<- a + 1
           x
         }))


         temp[] <- lapply(temp, function(x){
           x[is.na(x)] <- "-"
           x
         })

         # plots[[n()]] <-  ggplot()+
         #   annotation_custom(tableGrob(temp))
            } else {



              map_dfr(object@cov[-1],  function(x){


                if(typeof(x) == "character" | length(unique(x)) <= 8) x <- as.factor(x)
                x

              }) %>%
                summary


            }




          })

