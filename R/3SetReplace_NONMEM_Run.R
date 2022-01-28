#' setPath_NONMEM
#' Author: Thibaud Derippe
setGeneric("setPath_NONMEM<-",
           function(object, value){standardGeneric("setPath_NONMEM<-")}
)

# value <- "Z:/ETUDES/S95005/CS_95005/ANACIN/PK/1806_WORK/3_NONMEM/STD8_FTD/NM_VIEWER/STD8_GROUP1_DAY1/STD8_GROUP1_DAY1.cpu"
# value <- "D:/Peccary/Exemple_demo/run_nonmem/2_4_StandardErrorSansW12.res"
# value <- "file:///D:/Peccary/Exemple_demo/run_nonmem/test_blaise_etaproblem.res"
# value <- "file:///D:/Peccary/Exemple_demo/run_nonmem/1_comp.res"
# value <- "D:/Peccary/Exemple_demo/run_nonmem/run_xposeformat/run001.res"
#' SetReplaceMethod to fill a run object with the corresponding patwhay
#' Author: Thibaud Derippe
#' Arguments: object, the run object we want to fill the attribute
#' Arguments: value, pathway to the run
#' Output: not concerned, SetReplacedMethod
#' @export
setReplaceMethod(
  f="setPath_NONMEM",
  signature = "run",
  definition = function(object, value){

    print("Creation of a new run")

    ################################### P A T H     C L E A N E R #########################
    # Objective: improve the toleratibility of the input patway

    # Allowing copypast with RStudio
    path_to_run <- gsub("^file:///", "", value)
    ## For instance for "file://emsw0136/" wher we absolutely need the "//" beginning
    path_to_run <- gsub("^file:", "", path_to_run)
    # And pasting every file (.res, .tab, .cfl...)
    path_to_run <- gsub("\\..{3}$", "", path_to_run)
    # Standardisation by replacing backslash
    path_to_run <- gsub("\\\\", "\\/", path_to_run)

    ################################### F I L E S     R E A D I N G  #########################
    # Objective: read once for all every files containing data

    # reading configuration files
    config <- readLines(file.path(find.package("peccary"),  "Peccary_configuration.R"))
    # to extract atomic vector of possible nonmem extention result files
    eval(parse_expr(config[grep("ext_nm_res_file", config)]))

    # so here are all possibilies for result files
    possibilities <- paste0(path_to_run, ext_nm_res_file)
    # find the proper extension
    extension <- ext_nm_res_file[file.exists(possibilities)][[1]]

    # to read result fie
    dot_res_file <- readLines(paste0(path_to_run, extension), skipNul = T)

    # find table # UPDATE 12/01/2021: can be several tables, in that case need to merge them first
    temp <- dot_res_file[grep("\\$TABLE", dot_res_file)[[1]]:grep("NM-?TRAN MESSAGES", dot_res_file)] #[[1]]]

    # Find all tables names (# New from 12/01/2021)
    str_split( paste0(temp, collapse =  " "), pattern =  "\\$TABLE")[[1]] %>%
      map(function(temp){

        temp[grep("FILE *=", temp)] %>%
          gsub(pattern = ".+(FILE)|(file) *\\= *", replacement = "") %>%
          gsub(pattern = "=", replacement = "") %>%
          gsub(pattern = " .*", replacement = "")

      }) %>%
      reduce(c) -> namesoutputtables

    # read table
    pathtemp <- str_split(path_to_run, pattern = "/")[[1]]
    root <- paste0(pathtemp[- length(pathtemp)], collapse = "/")
    # # New from 12/01/2021 if Xpose , replace final X by the number
    # does the file exist ? If not replace final X -> loots of room to improvement here
    if(!file.exists(file.path(root,namesoutputtables[[1]] ))){

      numero_run <-
        gsub("^.+\\/", "", path_to_run) %>%
        gsub(pattern = "[[:alpha:]]|[[:punct:]]", replacement = "")

      namesoutputtables <- gsub("X$", numero_run, namesoutputtables)
    }

    # print("namesoutputtables")
    # print(namesoutputtables)
    # take the first table
    first <-  namesoutputtables[[1]]
    # print(file.path(root, first))
    dot_tab_df <- as_tibble(read.table(file.path(root, first), header = T, skip = 1))

    # print("dot_tab_df first")
    # print(dot_tab_df)
    # and merge the eventual others ones
    for(a in namesoutputtables[-1] ){

      temp <- try(as_tibble(read.table(file.path(root, a), header = T, skip = 1)) %>%
        distinct())

      if(class(temp)[[1]] != "try-error")  dot_tab_df <- dot_tab_df %>% left_join(temp)

    }

    # print("dot_tab_df second")
    # print(dot_tab_df)
    # From the .res. file, we need to remove, if several estimation method have been used, all the intermediare one...


    balise_multiple_method <- grep("#TBLN:    ", dot_res_file)
    if(length(balise_multiple_method) > 1){

      dot_res_file <- dot_res_file[-(balise_multiple_method[[1]]:balise_multiple_method[[length(balise_multiple_method)]])]

    }

    ################################### F U N C T I O N S #########################
    # Objective: by using the inputs, extract the information and standardise it

    # Find the name of the run
    name_research <- function(path = path_to_run){

      return(gsub("^.+\\/", "", path))

    }#; name_research()

    # Find statut of the run (sussessful, terminating due to rounding error...)
    status_research <- function(res_file = dot_res_file){

      if(length(grep("DUE TO ROUNDING ERRORS", res_file)) > 0){
        return("Rounding Error")
      }else if (length(grep("MINIMIZATION SUCCESSFUL", res_file)) > 0){
        return("Successful")
      }else if (length(grep("MINIMIZATION TERMINATED", res_file)) > 0){
        return("Terminated")
      }else{
        return("Terminated")
      }

    }#; status_research()


    # Search the objective function of the run
    FO_research_NONMEM <- function(res_file = dot_res_file){

      # look for the objective function
      line <-  unlist(res_file[grep('#OBJV:*', res_file)])
      result <- as.double(gsub("[^0-9,.]", "", line))
      result <- result[length(result)] # if multiple method, we chose the last one.

      # Problem: negative objective function were positive (minus sign not captured)
      # Solved by the following line:
      if(length(grep(paste0("-",result), line )) > 0 ) result <- - result


      return(result)
    }#; FO_research_NONMEM()







    ### E T A --- E T A ---  E T A --- E T A --- E T A --- E T A --- E T A --- E T A ---
    #Three steps:
    #1 - find the number and the names of the etas
    #2 - select matrix of estimation and (if existe) of standard error (and compute RSE in that case)
    #3 - have a table output
    # Find eta Name, very similar to previous function

    # Step 1
    eta_name <- function(res_file = dot_res_file){

      #### F I R S T   M E T H O D:   A T   T I M E    O F    D E C L A R A T I O N
      eta_name_1st_mtd <- character()
      a <- 1
      temp <- "initiation"
      res_file_commentless <- gsub(";.+", "", res_file)


      while(class(temp) != "try-error"){
        temp <- try(res_file_commentless[grep(paste0("[^H]ETA(.",a,")"), toupper(res_file_commentless))[[1]]], silent = T)
        value <-  try(gsub(" ", "",str_split(temp, "=")[[1]][1]), silent = T)
        if(class(temp) != "try-error"){
          eta_name_1st_mtd<- c(eta_name_1st_mtd, value)
        }
        a <- a + 1
      }

      for (a in eta_name_1st_mtd){
        if(length(eta_name_1st_mtd[eta_name_1st_mtd == a]) > 1){
          eta_name_1st_mtd[eta_name_1st_mtd == a] <- ""
        }
      }
      ### S E C O N D   M E T H O D:  C O M M E N T S   N A M E

      eta_begin <- grep("^[[:blank:]]?\\$OMEGA", toupper(res_file))[[1]]
      eta_end <- grep("^[[:blank:]]?\\$SIGMA", toupper(res_file))[[1]]
      res_file_shorten <- res_file[eta_begin:(eta_end-1)]
      res_file_shorten <- res_file_shorten[ - (toupper(res_file_shorten) == "$OMEGA")]
      res_file_shorten <- res_file_shorten[-grep("^[[:space:]]*;", res_file_shorten)]
      res_file_shorten <- res_file_shorten[res_file_shorten != ""]
      res_file_shorten <- gsub(".+;", "", res_file_shorten)
      res_file_shorten <- gsub("^[[:space:]]*", "", res_file_shorten)
      res_file_shorten <- gsub("[[:space:]].*","",res_file_shorten)

      for (a in res_file_shorten){
        if(length(res_file_shorten[res_file_shorten == a]) > 1){
          res_file_shorten[res_file_shorten == a] <- ""
        }
      }

      ### C O M P A R I S O N    A N D    F I N A L     I N P U T

      eta_name <- character()
      for (a in 1:length(eta_name_1st_mtd)){


        test1 <- toupper(eta_name_1st_mtd[a])
        test2 <- toupper(res_file_shorten[a])

        if(is.na(test1) == T) test1 <- ""
        if(is.na(test2) == T) test2  <- ""

        if(test1 == test2){
          if(test1 == "" & test2 ==""){
            eta_name <- c(eta_name, a)
          }else{
          eta_name <- c(eta_name, eta_name_1st_mtd[a])
          }
        }else if(test1 == "" | test2 ==""){

            eta_name <- c(eta_name, paste0(test1,test2))

        }else{

          eta_name <- c(eta_name, paste0(test1, "/",test2))
        }


      }
      return(paste0("eta_",eta_name))

    }
    eta_names <- eta_name()#; eta_names

    # Step 2
    eta_matrixs <- function(res_file = dot_res_file){
      # Search the section containing the eta values

      omega_balises <- grep("OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS", res_file)#;omega_balises
      sigma_balises <- grep("SIGMA - COV MATRIX FOR RANDOM EFFECTS", res_file)#;sigma_balises
      # corr_balises <- grep("CORR MATRIX FOR RANDOM EFFECTS - ETAS  ", res_file); corr_balises

      tibble(balise1 = omega_balises, balise2 = sigma_balises) %>%
        # mutate(balise2 = if_else(length(sigma_balises) == length(omega_balises), 1:2, 2)) %>%
        mutate(bloc = map2(balise1, balise2, function(x, y) gsub("\\.\\.\\.\\.\\.\\.\\.\\.\\.", "0E+0",res_file[x:y]))) %>%
        # {.$bloc[[2]]} -> x ; x # for test purpose
        mutate(matrix = map(bloc, function(x){


          strsplit(paste(x, collapse = " "), " ")[[1]] %>%
          {as.double(.[grep("E[\\+-]", .)])} -> values #; values


          return(
            tibble(n = 1:length(eta_name())) %>%
              mutate(line = map_chr(n, function(x){

                y <- values[1:x];y
                values <<- values[-(1:x)]; values
                return(paste(y, collapse = " "))

              })) %>%
              {str_split(.$line, " ", simplify = T)} %>%
              {matrix(as.numeric(.), nrow = length(eta_name()))}
          )


        })) %>%
        select(matrix) -> matrixs #; matrixs

      matrixs %>%
        mutate(name = c("estimation", "se")[1:nrow(matrixs)]) %>%
        select(name, matrix) -> matrixs; matrixs

      if(nrow(matrixs) == 2){

        matrixs <- matrixs %>%
          bind_rows(tibble(name = "rse", matrix = list(round(matrixs[[2]][[2]] / matrixs[[2]][[1]] * 100, 2)))); matrixs

      }

      return(matrixs)
    }#; eta_matrixs()



    # Step3
    eta_table_output <- function( res_file = dot_res_file, matrixs = eta_matrixs()){


      crossing(eta_names = eta_names, eta_names1 =  eta_names) %>%
      # crossing(col1 = tibble(eta_names), col2 = tibble(eta_names)) %>%
        mutate("estimation" =   as.double(t(matrixs[["matrix"]][[1]]))) -> temp

      if(nrow(matrixs) == 3){

        temp %>%
          mutate("se" =  as.double(t(matrixs[["matrix"]][[2]])) ) %>%
          mutate("rse" = as.double(t(matrixs[["matrix"]][[3]]))) -> temp

      }


      ## Calculating shrinkage
      ETAshrink_balise <- grep("ETAshrink", res_file)
      EBVshrink_balise <- grep("EBVshrink", res_file)
      EPSshrink_balise <- grep("EPSshrink", res_file)

      if(length(ETAshrink_balise) >0){
      shrinkage_line <- res_file[ETAshrink_balise:(EBVshrink_balise - 1)] %>%
      {invoke(paste, .)}


        ETAshrinkage <- str_split(shrinkage_line, "  ")[[1]][-1] %>%
                 as.double() %>%
                 {.[!is.na(.)]} %>%
                 {paste0(round(.,1),"%")}

      EBVshrinkage_line <- res_file[EBVshrink_balise:(EPSshrink_balise - 1)] %>%
        {invoke(paste, .)}


        EVBshrinkage <- str_split(EBVshrinkage_line, "  ")[[1]][-1] %>%
          as.double() %>%
          {.[!is.na(.)]} %>%
          {paste0(round(.,1),"%")}




      ### Final output
       eta_diag <- temp %>%
          filter(eta_names1 == eta_names) %>%
          mutate(ETAshrink = ETAshrinkage) %>%
         mutate(EBVshrink = EVBshrinkage)

       eta_cov <- temp %>%
         filter(is.na(estimation) == F & estimation > 0 & eta_names != eta_names1) %>%
         mutate(ETAshrink = NA) %>%
         mutate(EBVshrink = NA)
      }else{

        eta_diag <- temp %>%
          filter(eta_names1 == eta_names)

        eta_cov <- temp %>%
          filter(is.na(estimation) == F & estimation > 0 & eta_names != eta_names1)


      }

       bind_rows(eta_diag, eta_cov) %>%
         filter(estimation > 0) %>%
         mutate(parameter = case_when(eta_names != eta_names1 ~ paste0("cov_", gsub("eta_","", eta_names), gsub("eta_","", eta_names1)),
                                      TRUE ~ eta_names)) %>%
         mutate(type = if_else(eta_names != eta_names1, "cov", "eta")) %>% # I want cov to be after eta
         arrange(type) %>%
         select( -eta_names, - eta_names1, -type) %>%
         select(parameter, everything())




    }#; eta_table_output()





    ### T H E T A --- T H E T A ---  T H E T A --- T H E T A --- T H E T A --- T H E T A --- T H E T A --- T H E T A ---

    # Step1: Find theta Name
    # Difficulties: two possibilites: either in the declaration line, either in comments...
    theta_name <- function(res_file = dot_res_file){

      #### F I R S T   M E T H O D:   A T   T I M E    O F    D E C L A R A T I O N
      theta_name_1st_mtd <- character()
      a <- 1
      temp <- "initiation"
      res_file_commentless <- gsub(";.+", "", res_file)


      while(class(temp) != "try-error"){
        temp <- try(res_file_commentless[grep(paste0("THETA\\( *",a," *\\)"), toupper(res_file_commentless))[[1]]], silent = T)
        # Problem if for instance we have  "$THETA 10 FIX " in first estimation
        if(length(grep("^\\$THETA", temp)) > 0){

          temp <- try(ejrzerjezlckjlefserlksejresres) # create an error
        }
        value <-  try(gsub(" ", "",str_split(temp, "=")[[1]][1]), silent = T)

        if(length(grep("MU_", value) >0)){

          temp <-  res_file_commentless[grep(paste0(".+ = .+?",value), toupper(res_file_commentless))]
          value <-  try(gsub(" ", "",str_split(temp, "=")[[1]][1]), silent = T)
        }

        if(class(temp) != "try-error"){
          theta_name_1st_mtd<- c(theta_name_1st_mtd, value)
        }
        a <- a + 1
      }

      for (a in theta_name_1st_mtd){
        if(length(theta_name_1st_mtd[theta_name_1st_mtd == a]) > 1){
          theta_name_1st_mtd[theta_name_1st_mtd == a] <- ""
        }
      }
      ### S E C O N D   M E T H O D:  C O M M E N T S   N A M E

      theta_begin <- grep("^[[:blank:]]?\\$THETA", toupper(res_file))[[1]]
      theta_end <- grep("^[[:blank:]]?\\$OMEGA", toupper(res_file))[[1]]
      res_file_shorten <- res_file[theta_begin:(theta_end-1)]

      res_file_shorten <- gsub(" *\\$THETA *", "", res_file_shorten)
      res_file_shorten <- res_file_shorten[-grep("^[[:space:]]*;", res_file_shorten)]
      res_file_shorten <- res_file_shorten[res_file_shorten != ""]
      res_file_shorten <- gsub(".+;", "", res_file_shorten)
      res_file_shorten <- gsub("^[[:space:]]*", "", res_file_shorten)

      # then it is either the first word either if 1 = Cl the afther =
      res_file_shorten_test <- gsub("[[:space:]].*","",res_file_shorten)

      if(sum(!is.na(as.double(res_file_shorten_test))) == length(res_file_shorten)){

        res_file_shorten <- gsub(" *[[:digit:]] *=? *", "", res_file_shorten)

      }else{

        res_file_shorten <- res_file_shorten_test
      }



      for (a in res_file_shorten){
        if(length(res_file_shorten[res_file_shorten == a]) > 1){
          res_file_shorten[res_file_shorten == a] <- ""
        }
      }

      ### C O M P A R I S O N    A N D    F I N A L     I N P U T

      theta_name <- character()
      for (a in 1:length(theta_name_1st_mtd)){

        test1 <- toupper(theta_name_1st_mtd[a])
        test2 <- toupper(res_file_shorten[a])

        if(is.na(test1) == T) test1 <- ""
        if(is.na(test2) == T) test2  <- ""

        if(test1 == test2){
          if(theta_name_1st_mtd[a] == "" & test2 ==""){
            theta_name <- c(theta_name, paste0("THETA_",a))
          }else{
            theta_name <- c(theta_name, test1)
          }
        }else if(theta_name_1st_mtd[a] == "" | test2 ==""){

          theta_name <- c(theta_name, paste0(test1, test2))

        }else{

          theta_name <- c(theta_name, paste0(test1, "/",test2))
        }


      }
      return(theta_name)

    }
    theta_names <- theta_name(); theta_names
    # Step2: find estimaton and (if possible) the standard error (if so compute RSE)
    theta_table_output <- function(res_file = dot_res_file){

      theta_balise <- grep("THETA - VECTOR OF FIXED EFFECTS PARAMETERS", res_file)
      omega_balise <- grep("OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS", res_file)

      tibble(balise1 = theta_balise, balise2 = omega_balise) %>%
        mutate(bloc = map2(balise1, balise2, function(x, y){

          gsub("\\.\\.\\.\\.\\.\\.\\.\\.\\.", "0E+0",res_file[x:y]) %>%
            {strsplit(paste(., collapse = " "), " ")[[1]]} %>%
            {as.double(.[grep("E[\\+-]", .)])}


        })) %>%
        select(bloc)-> temp;



    tibble(parameter = theta_names) %>%  # previous bug: if we have let's say 10 $omega X but only 8 theta(X), then
      mutate(estimation = temp[[1]][[1]][1:length(theta_names)]) -> output  # theta_names and temp[[1,1]] will not have the same length...

    if(nrow(temp) == 2){

      output <- output %>%
        mutate(se = temp[[1]][[2]]) %>%
        mutate(rse = se / estimation * 100)

    }



    return(output)
    }#; theta_table_output()

### S I G M A--- S I G M A---  S I G M A--- S I G M A--- S I G M A--- S I G M A--- S I G M A--- S I G M A---


    sigma_table_output <- function(res_file = dot_res_file){


      sigma_balise <- grep("SIGMA - COV MATRIX FOR RANDOM EFFECTS", res_file)
      omega_balise <- grep("OMEGA - CORR MATRIX FOR RANDOM", res_file)


      tibble(balise1 = sigma_balise, balise2 = omega_balise) %>%
        mutate(bloc = map2(balise1, balise2, function(x, y){

          gsub("\\.\\.\\.\\.\\.\\.\\.\\.\\.", "0E+0",res_file[x:y]) %>%
          {strsplit(paste(., collapse = " "), " ")[[1]]} %>%
          {as.double(.[grep("E[\\+-]", .)])}


        })) %>%
        select(bloc)-> temp;



      tibble(parameter = paste0("sigma_",1:length(temp[[1]][[1]]))) %>%
        mutate(estimation = temp[[1]][[1]]) -> output; output

      if(nrow(temp) == 2){

        output <- output %>%
          mutate(se = temp[[1]][[2]]) %>%
          mutate(rse = se / estimation * 100)

      }

      return(output)
    }#; sigma_table_output()

### F I N A L    P A R A M E T E R   T A B L E ---   F I N A L    P A R A M E T E R   T A B L E
    # Search the parameter estimation, standard deviation and RSE of the run
    data_result_NONMEM <- function(res_file = dot_res_file){


      # we need to remove the annoying regular expression
      res_file <- gsub('[\r\n\t]', '', res_file )

      # Problem: some text behind comments can be taking into account and create wrong output
      # Solved by the following line
      res_file <- gsub(";.+","",  res_file)

      # Seach the theta values

      theta_table_output() %>%
        bind_rows(eta_table_output()) %>%
        bind_rows(sigma_table_output()) %>%
        # pull(estimation) -> x
        {imap_dfr(., function(x,y){

          if(y != "Value"){
          if(is.numeric(x) == T) x <- map_dbl(x, ~ if_else(.x <1, signif(.x, 2), round(.x, 2)))
          x[x == 0] <- "-"
          }
          x
        })} %>%
        rename(Parameter = parameter) %>%
        rename(Value = estimation) -> output

      if("se" %in% names(output)){
        output <- output %>%
          rename(SE = se) %>%
          rename(`RSE(%)` = rse)

      }else{

        output <- output %>%
          mutate(SE = "-") %>%
          mutate(`RSE(%)` = "-")

      }

      return(output)

    }#; data_result_NONMEM()

    # Only the observation
    observation_research_NONMEM <- function(df = dot_tab_df){
      # Objective: get only the observation value


      # df %>%
      #   select(ID, TIME, DV) %>%
      #   rename ( OBS = DV) %>%
      #   filter(OBS > 0)

      if("EVID" %in% names(df)){

        df <- df %>% filter(EVID == 0) # To remove annoying EVID == 2 !
      }

      df %>%
        select( ID, grep("DV|TIME|YTYPE|FLAG", toupper(names(df)))) %>%
        distinct -> temp


      if("FLAG" %in% names(temp)){

        temp <- temp %>%
          rename(YTYPE = FLAG)
      }


       if(length(grep("^LLOQ$", names(df))) >= 1){

         temp <- temp %>%
           left_join( df %>% select(ID, TIME,DV, LLOQ)) %>%
           mutate(BLQ = if_else(DV <= LLOQ, 1, 0)) %>%
           mutate(DV = if_else(DV < LLOQ,  LLOQ, DV))

       }

       if(length(grep("^YTYPE$", names(df))) >= 1){

         temp <- temp %>%
           left_join( df %>% select(ID, YTYPE))

       }

       return( temp %>% rename(OBS = DV))


    }#;observation_research_NONMEM() #Renvoie les observation

    # Only the prediction
    prediction_research_NONMEM <- function(df = dot_tab_df){


      df %>%
        select( ID, grep("PRED|TIME|YTYPE|FLAG", toupper(names(df)))) %>%
        distinct -> temp


      if("FLAG" %in% names(temp)){

        temp <- temp %>%
          rename(YTYPE = FLAG)
      }

      return(temp)

      }#;prediction_research_NONMEM()


     # Renvoie les PRED et IPRED

    # Covariable of interest
    cov_research_NONMEM <- function(df = dot_tab_df){
      return(
        df %>%
          select( ID, grep("AGE|SEX|SIZE|STUDY|CYCLE|CELL|WEIGHT|DOSE|DRUG|GROUP|COV", toupper(names(df)))) %>%
          distinct()
      )
    }#; cov_research_NONMEM() # Renvoie les covariables

    # Residuals
    residus_research_NONMEM <- function(df = dot_tab_df){

      if("EVID" %in% names(df)){

        df <- df %>% filter(EVID == 0) # To remove annoying EVID == 2 !
      }

      df %>%
        select( ID, grep("RES|TIME|NPDE|YTYPE", toupper(names(df)))) %>%
        distinct() -> temp



      # if("RES"  %in% names(temp)) temp %>% filter(RES !=0) -> temp

      return(temp)
    }#;residus_research_NONMEM() # Renvoie les r?sidus

    # Eta values
    eta_research_NONMEM <- function(df = dot_tab_df, res_file = dot_res_file){


      # Search the eta_name
      eta_names <- eta_name()

      df[c( which(names(df) == "ID"), grep("ETA?[0-9]|YTYPE", names(df)))] %>%
        distinct() -> eta

      for(a in grep("ETA?",names(eta))){
        number <- gsub("ETA?","", names(eta[a]))
        names(eta)[a] <- eta_names[as.integer(number)]
      }


      # Remove if not enough point
      for(a in names(eta)[names(eta) != "ID" & names(eta) !="YTYPE"]){
        test <- length(unique(eta[[a]]))
        if(test == 1){
          eta <- eta[,-which(names(eta) == a)]
        }
      }


      return(eta)
    }#; eta_research_NONMEM() # Renvoie les ?tas

    # Date of creation
    date_creation <- function(path = path_to_run){
      path_temp <- paste0(path,".res")
      return(file.info(path_temp)$mtime)
      }#; date_creation()

    # Protocole, time of administraiton
    protocole <- function(df = dot_tab_df){

      names(df)[toupper(names(df)) == "TIME"] <- "TIME"
      df %>%
        filter(DV == 0 & EVID == 1 ) %>%
        select(ID, TIME) %>%
        distinct

    }#; protocole()


    # individualValues

    individualValues <- function(df = dot_tab_df){

allpossible <- str_split(string = theta_name(), pattern = "/") %>% reduce(c)

df %>%
    select(ID, matches(paste0("^",allpossible, "$"))) %>%
  distinct()

      }

    # Score

    # Search the objective function of the run
    scores_NONMEM <- function(res_file = dot_res_file){

      # look for the objective function
      line <-  unlist(res_file[grep('#OBJV:*', res_file)])
      result <- as.double(gsub("[^0-9,.]", "", line))
      FO <- result[length(result)] # if multiple method, we chose the last one.

      # Problem: negative objective function were positive (minus sign not captured)
      # Solved by the following line:
      if(length(grep(paste0("-",result), line )) > 0 ) FO <- - FO


      nparam <- nrow(data_result_NONMEM() %>% filter(SE != "-"))

      result <- tibble(Score = c("FO", "AIC", "BIC"), Value = c(FO, 2* nparam + FO ,
                                                      FO + nparam * log(nrow(observation_research_NONMEM()))))

      return(result)
    }#; scores_NONMEM()



    ################################### S E T T E R S #########################
    # Objective: store every function output in corresponding run object attribute

    object@software <- "NONMEM"
    # print("Name Research")
    try(object@name <- name_research(), silent = T)
    # print("Statut Research")
    try(object@status <- status_research(), silent = T)
    # print("Path Research")
    try(object@path <- path_to_run, silent = T)
    # print("Estimation Research")
    try(object@estimation <- data_result_NONMEM(), silent = T)
    # print("FO Research")
    try(object@FO <- FO_research_NONMEM(), silent = T)
    # print("Observation Research")
    try(object@OBS <- observation_research_NONMEM(), silent = T)
    # print("Prediction Research")
    try(object@IPRED <- prediction_research_NONMEM(), silent = T)
    # print("COV Research")
    try(object@cov <- cov_research_NONMEM(), silent = T)
    # print("Residus Research")
    try(object@residus <- residus_research_NONMEM(), silent = T)
    # print("Random Effect Research")
    try(object@randomEffect <- eta_research_NONMEM(), silent = T)
    # print("Date Research")
    try(object@date <- as.character(date_creation()), silent = T)
    # print("Administration Research")
    try(object@administration <- protocole(), silent = T)
    try(object@Score <- scores_NONMEM(), silent = T)
    try(object@individualValues <- individualValues(), silent = T)


    return(object)
  }
)
# data()
# rm()
# demo <- createDossier("file://emsw0136/_cinetiq/CINETIQ/2_Scientifique/Software/R/Package_Thibaud")
# demo(13)
