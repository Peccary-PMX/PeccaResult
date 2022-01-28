
############## run individu

#' runIndivi
#' To transform a string of numbers into numbers (only used in Shiny app)
#' @author Thibaud Derippe
#' @param numbers a string
#' @return la fonction renvoie la moyenne d'un vecteur
#' @export
runIndivi <- function(numbers = "1 2 5:10 - 6:7 9"){
  split <-  str_split(numbers, " ")[[1]][str_split(numbers, " ")[[1]] != ""]
  if(length(grep("i", split))>0){
    value <- split[grep("i", split)][[1]]
    return( as.numeric(gsub("i","",value)))
  }else{

    return(stringToNumbers(numbers)[[1]])
  }


}



########## filter decomposer
#' Filter decomposition
#' @author Thibaud Derippe
#' @export
filter_decomposer <- function(filterr){

  tibble(group = strsplit(filterr, "&")[[1]]) %>%
    mutate(type = map_chr(group, function(x){

      case_when(
        length(grep("PRED", x)) > 0   ~ "PRED",
        length(grep("^ID$", x)) > 0   ~ "ID",
        length(grep("TIME", x)) > 0   ~ "TIME",
        length(grep("OBS", x)) > 0   ~ "OBS",
        length(grep("YTYPE", x)) > 0   ~ "YTYPE",
        T ~ "COV"
      )


    }))

}
