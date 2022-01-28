#' Giving a path of a folder and the software, it search every single runs in the folder
#' Author: Thibaud Derippe
#' Arguments: path, the pathway to begin the researches of runs
#' Output: a tibble containing the name of the runs and the complete pathway
#' @export
find_runs <- function(path){

  path <- gsub("file:///", "", path)
   path <- gsub("file:", "", path) # For link like that "file://emsw0136/_cinetiq/CINETIQ/2_Scientifique/Software/R/Package_Thibaud/Exemple_demo"


  # Determine the tag to search depending on the software
  # (for Nonmem, several possible extension, noted in configuration file)

   config <- file.path(find.package("peccary"),  "Peccary_configuration.R")
   config <- readLines(config)  # keyword_NONMEM from configuration file
   eval(parse_expr(config[grep("ext_nm_res_file", config)]))
   pre_keyword_NONMEM <- paste0("\\", ext_nm_res_file, "$")
   # pre_keyword_NONMEM <- c("\\.res$","\\.lst$")
   keyword_NONMEM <- paste0("(", pre_keyword_NONMEM, ")", collapse = "|")

   keyword_MONOLIX  = "pop_parameters\\.txt"
   keyword_MONOLIX2019  = "IndividualParameters"
   keyword_nlmixr <- "\\.nlmixr$"
   keyword_adapt <- "\\.aci$"
  # Initiation of the process
  dossier <- ""
  name_res <- character()
  path_res <- character()
  software <- character()
  # while loop
  while (length(dossier) > 0){

    # list all files of the first unexplored yet folder
    files <- list.files(file.path(path,dossier[1]))

    # We 1) search for the keyword and 2) remove the keyword of the found runs. Stored in the new vector.
    keywordFound_NONMEM <- files[grep( keyword_NONMEM, files )]
    new_name_res_NONMEM <- gsub( keyword_NONMEM,"", keywordFound_NONMEM)

    keywordFound_MONOLIX <- files[grep( keyword_MONOLIX, files )]
    new_name_res_MONOLIX <- gsub( keyword_MONOLIX,"", keywordFound_MONOLIX)

    new_name_res_MONOLIX2019 <- files[grep( keyword_MONOLIX2019, files )]

    keywordFound_nlmixr <- files[grep( keyword_nlmixr, files )]
    new_name_res_nlmixr <- gsub( keyword_nlmixr,"", keywordFound_nlmixr)


    keywordFound_adapt <- files[grep( keyword_adapt, files )]
    new_name_res_adapt <- gsub( keyword_adapt,"", keywordFound_adapt)

    if(length(new_name_res_MONOLIX2019) > 0) new_name_res_MONOLIX2019 <- dossier[1]
    # new_name_res_MONOLIX2019 <- gsub( keyword_MONOLIX2019,"", keywordFound_MONOLIX)

    # In case of monolix use, the keyword is "finegrid.txt", so the name of the run, if it has been found, is actually the name of the folder
    if(length(new_name_res_MONOLIX) > 0) new_name_res_MONOLIX <- dossier[1]
    # To be sure we just obtain the name run (and not the entire pathway). str_split create a list (hence the [[1]]) Useless?

    new_name_res <- c(new_name_res_MONOLIX, new_name_res_NONMEM, new_name_res_MONOLIX2019, new_name_res_nlmixr, new_name_res_adapt)

    software <- c(software, rep("MONOLIX", length(c(new_name_res_MONOLIX, new_name_res_MONOLIX2019))), rep("NONMEM", length(new_name_res_NONMEM)),
                  rep("NLMIXR", length(new_name_res_nlmixr)), rep("ADAPT", length(new_name_res_adapt)))

    if(length(new_name_res)>0 ){
    for(a in 1:length(new_name_res) )
    new_name_res[a] <-  str_split(new_name_res[a], "/")[[1]][length( str_split(new_name_res[a], "/")[[1]])]
    }
    # Add the new found runs in the name_res atomic vector
    name_res <- c(name_res,new_name_res)

    # Add the complete pathway of the run by concatenate the observed data and the file names
    for (a in new_name_res_MONOLIX){
      path_res <- c(path_res, dossier[1])
    }

    for (a in new_name_res_NONMEM){
      path_res <- c(path_res, file.path(dossier[1], a))
    }

    for (a in new_name_res_MONOLIX2019){
      path_res <- c(path_res, dossier[1])
    }

    for (a in new_name_res_nlmixr){
      path_res <- c(path_res, file.path(dossier[1], a))
    }

    for (a in new_name_res_adapt){
      path_res <- c(path_res, file.path(dossier[1], a))
    }



    # Initiae the new_folder vector
    new_dossier <- character()

    # Find new folders as previously and increment the new_dossier vector
    new_doss <- gsub(".+\\..+","", files)[ gsub(".+\\..+","", files) != ""]
    for (b in new_doss) new_dossier <- c(new_dossier, paste(dossier[1], b,sep = .Platform$file.sep))

    # Concatenate the dossier with new_dossier vector, and remove the first element (the one we just analyzed)
    dossier <- c(dossier, new_dossier)[-1]; dossier[1]; length(dossier)
  }

  # Once every folders have been studied, create the tibble of every run names and complete pathway
  return(
  tibble(run = name_res,files =  path_res, software ) %>%
    ## remove monolix2019subfolder
    mutate(toremove = map_dbl(files, ~ length(grep("(\\/ChartsData$)|(\\/IndividualParameters$)|(\\/Tests$)", .x)))) %>%
    filter(toremove != 1) %>%
    select(-toremove)

  )
}


# find_runs("file:///D:/Peccary/Exemple_demo/run_adapat")

