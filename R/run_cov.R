#' @export
setGeneric("run_cov",
           function(object){standardGeneric("run_cov")}
)

#' @export
setMethod(f = "run_cov",
          signature = "run",
          definition =  function(object){

            return(object@cov)
          })
