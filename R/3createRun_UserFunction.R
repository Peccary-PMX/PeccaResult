#' Create a run object with the pathway
#' @Author: Thibaud Derippe
#' @Arguments: path of the run
#' @Names: don't remenber, I thing it's an old residu that can be removed?
#' @Output: a run object
#' @export
createRun <- function(path){


  output <-  new("run")

  # guess the software
  blocs <- str_split(gsub("\\\\", "\\/", path), pattern = "/")[[1]]
  lastbloc <- blocs[length(blocs)]


  # Is it NONMEM?
  # reading configuration files
  config <- readLines(file.path(find.package("peccary"),  "Peccary_configuration.R"))
  # to extract atomic vector of possible nonmem extention result files
  eval(parse_expr(config[grep("ext_nm_res_file", config)]))
  pre_keyword_NONMEM <- paste0("\\", ext_nm_res_file, "$")
  # pre_keyword_NONMEM <- c("\\.res$","\\.lst$")
  keyword_NONMEM <- paste0("(", pre_keyword_NONMEM, ")", collapse = "|")
  if(grepl(paste0("(\\.clf)|(\\.ext)|(\\.cpu)|", keyword_NONMEM),lastbloc)){

    setPath_NONMEM(output) <- path

  # Is it nlmixr?
  }else if(grepl("\\.nlmixr",lastbloc)){

    setPath_NLMIXR(output) <- path

    # else it's (for now) Monolix (2019 or 2016)
  }else if(grepl("\\.run",lastbloc)){

    setPath_ADAPT(output) <- path

    # else it's (for now) Monolix (2019 or 2016)
  }else{

    # try if monolix 2019
   suppressWarnings(test <- try(readLines(paste0(path,"/IndividualParameters/estimatedIndividualParameters.txt"), skipNul = T, n = 1), silent = T))

   if(class(test) != "try-error"){

      ## Monolix 2019
      # print("Monolix 2019!!")
      setPath_MONOLIX2019(output) <- path


    }else{

      # Monolix2016
      try(setPath_MONOLIX(output) <- path)

    }



  }


  return(output)
}



# createRun_NONMEM <- function(path_to_run, names = 2){
#
#   output <-  new("run")
#   setPath_NONMEM(output, names) <- path_to_run
#
#   return(output)
# }
#
#
#
#
# createRun_MONOLIX <- function(path){
#
#   output <-  new("run")
#
#
#   setPath_MONOLIX(output) <- path
#
#   return(output)
# }
