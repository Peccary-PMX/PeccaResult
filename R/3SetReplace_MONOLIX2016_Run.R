#' setPath_MONOLIX
#' Author: Thibaud Derippe
setGeneric("setPath_MONOLIX<-",
           function(object, value){standardGeneric("setPath_MONOLIX<-")}
)

# value <- "file:///D:/Peccary/Exemple_demo/1_LINEAR/run1_3cpt_linear_Cl_comb"
#' SetReplaceMethod to fill a run object with the  corresponding patwhay
#' Author: Thibaud Derippe
#' Arguments: object, the run object we want to fill the attribute
#' Arguments: value, pathway to the run
#' Output: not concerned, SetReplacedMethod
#' @export
setReplaceMethod(
  f="setPath_MONOLIX",
  signature = "run",
  definition = function(object, value){

    print("Creation of a new run ")
    ################################### P A T H     C L E A N E R #########################
    # Objective: improve the toleratibility of the input patway

    # Allowing copypast with RStudio
    path_to_run <- gsub("^file:///", "", value)

    ## For instance for "file://emsw0136/" wher it's in fact the I:drive
    path_to_run <- gsub("^file:", "", path_to_run)
    # Standardisation by replacing backslash
    path_to_run <- gsub("\\\\", "\\/", path_to_run)

    ################################### F I L E S     R E A D I N G  #########################
    # Objective: read once for all every files containing data
    # Difficulties to read data frame in case of multi occasions...

    ## All the available files
    files <- list.files(path_to_run)

    pop_parameters_txt <- try(readLines(paste0(path_to_run,"/pop_parameters.txt"), skipNul = T))


    predictions_txt <-try(

        tibble(files = files[grep("predictions[[:digit:]]?\\.txt", files)]) %>%
          mutate(n = as.double(gsub("\\D", "", files))) %>%
          mutate(predictions = map2(files, n, function(x, y){

            as_tibble(readTablePerso(paste0(path_to_run, "/", x), header = T)) -> temp

            if(is.na(y) == F)  temp <- temp %>%  mutate(YTYPE = y)
            return(temp)


          }  )) %>%
          # slice(1) %>%
          # {.[[1, 3]] } -> x
          mutate(pred = map2(predictions,n ,  function(x, y ){

            x %>%
              rename_("y" = paste0("y", if_else(is.na(y), "", as.character(y)))) %>%
              select(ID, time, y, matches("Pred|Res|NPDE"), contains("YTYPE") )


          })) %>%
          select(-predictions, -files, -n ) %>%
          unnest %>%
          mutate(ID = gsub("#", "_", ID)),
          silent = T)



     # predictions_txt <- try(as_tibble(readTablePerso(paste0(path_to_run,"/predictions.txt"), header = T)), silent = T)
    # finegrid_txt <- try(as_tibble(readTablePerso( paste0(path_to_run,"/finegrid.txt"), header = T)), silent = T)

    finegrid_txt <-try(

      tibble(files = files[grep("finegrid[[:digit:]]?\\.txt", files)]) %>%
      mutate(n = as.double(gsub("\\D", "", files))) %>%
      mutate(predictions = map2(files, n, function(x, y){

        as_tibble(readTablePerso(paste0(path_to_run, "/", x), header = T)) %>%
          mutate(YTYPE = y)

      }  )) %>%
        # slice(1) %>%
      # {.[[1, 3]] } -> x
      mutate(pred = map2(predictions,n ,  function(x, y ){

        x %>%
          select( ID, time, contains("popPred")  	, contains("indPred") ) -> temp

        if(is.na(y) == F)  temp <- temp %>%  mutate(YTYPE = y)

        return(temp)

      })) %>%
      select(-predictions, -files, -n) %>%
      unnest %>%
      mutate(ID = gsub("#", "_", ID)),
      silent = T)




    covariatesSummary_txt <- try(readLines(paste0(path_to_run, "/covariatesSummary.txt")), silent = T)
    indiv_eta_txt <- try(as_tibble(readTablePerso(paste0(path_to_run,"/indiv_eta.txt"), header = T) %>%
                                     mutate(ID = gsub("#", "_", ID))), silent = T)
    # fulltimes_txt <- try(as.tibble(readTablePerso( paste0(path_to_run,"/fulltimes.txt") , header = T)), silent = T)

    fulltimes_txt <-try(

      tibble(files = files[grep("fulltimes[[:digit:]]?\\.txt", files)]) %>%
        mutate(n = as.double(gsub("\\D", "", files))) %>%
        mutate(predictions = map2(files, n, function(x, y){

          as_tibble(readTablePerso(paste0(path_to_run, "/", x), header = T)) %>%
            mutate(YTYPE = y) -> temp

        }  )) %>%
        # slice(1) %>%
        # {.[[1, 3]] } -> x
        mutate(pred = map2(predictions,n ,  function(x, y ){


          x %>%
            select( ID, time, contains("popPred")  	,  contains("indPred")) -> temp

          if(is.na(y) == F)  temp <- temp %>%  mutate(YTYPE = if_else(is.na(y), 1, y))


          return(temp)


        })) %>%
        select(-predictions, -files, -n ) %>%
        unnest %>%
        mutate(ID = gsub("#", "_", ID)),
      silent = T)


    ################################### F U N C T I O N S #########################
    # Objective: by using the inputs, extract the information and standardise it


    FO_research_Monolix <- function(pop_parameters = pop_parameters_txt){

      line_temp  <- unlist(pop_parameters[grep('2 x log-likeliho', pop_parameters)])
      line_temp <- gsub("\\(.+\\)", "", line_temp)
      as.double(strsplit(line_temp, " ")[[1]][length(strsplit(line_temp, " ")[[1]])])

    }






    observation_research_Monolix = function(df = predictions_txt){


      return(
        df %>%
          select(ID, time,  y, contains("YTYPE")) %>%
          rename(TIME = time) %>%
          rename(OBS = y)
        #doesn't work if y1 instead of  y...
        # see "Z:/ETUDES/S95008/CS_95008/ANACIN/PK/MONOLIX/MONOCOMPbis"
      )

    }

    prediction_research_Monolix = function(df1 = finegrid_txt, df2 = predictions_txt, ft = fulltimes_txt){

      if(class(df1)[1] != "try-error"){
       names(df1) <-  gsub("\\*", ".", names(df1))

      df1 %>%
        select(ID, time,  indPred_mean., popPred, contains("YTYPE")) %>%
        rename(TIME = time) %>%
        rename( IPRED = indPred_mean.  ) %>%
        rename(PRED = popPred) -> fingrid

      }else{

        names(ft) <-  gsub("\\*", ".", names(ft))

        ft %>%
          select(ID, time, indPred_mean., popPred, contains("YTYPE")) %>%
          rename(TIME = time) %>%
          rename( IPRED = indPred_mean.  ) %>%
          rename(PRED = popPred) -> fingrid

      }

      names(df2) <-  gsub("\\*", ".", names(df2))

      df2 %>%
        select(ID, time,  indPred_mean., popPred, contains("YTYPE")) %>%
        rename(TIME = time) %>%
        rename( IPRED = indPred_mean.  ) %>%
        rename(PRED = popPred) -> vraivalues

      return(
        rbind(fingrid, vraivalues) %>%
          arrange(ID, TIME) %>%
          filter(TIME >= min(c(vraivalues$TIME,0)))
      )

    }

    cov_research_Monolix = function(df = indiv_eta_txt, covariatesSummary = covariatesSummary_txt){

      ###########Recuperation des NOMS de covariables (permet de remplir le manquant par la suite...)
      rl <- gsub("\\t"," ", covariatesSummary) #retire les backslash p?nibles ? g?rer
      continue <- grep("covariate *mean" ,  rl   ) #balise cov continue
      cat <- grep("Table: Categorical covaria" ,  rl ) #balise cov cat?goriel

      covnames <- character()
      covdetail <- character()
      #ligne cont (s?lecionne les bonnes lignes)
      if(length(continue) > 0 & length(cat) > 0){
        continuel <- rl[continue:cat]
      } else if(length(continue) > 0 & length(cat) == 0){
        continuel <- rl[continue:length(rl)]
      } else{
        continuel <-character()
      }
      if(length(continuel) > 0){

      toselect <- which(continuel == "")
      continuel2 <- continuel[2:(toselect-1)]


      for (a in 1:length(continuel2)){
      names <-  str_split(continuel2[a], pattern = " ")[[1]][ str_split(continuel2[a], pattern = " ")[[1]] != ""][1]
      covnames <- c(covnames, names ) # les noms des cov
      covdetail <- c(covdetail, names) #le d?tail avec les valeurs possibles
      }
      }
      #ligne cat (s?lectionne les bonnes lignes)
      if(length(cat) > 0 ){
        catl <- rl[(cat+2):length(rl)]
        catl <- gsub("#.+|%.+","", catl); catl
        catl <- gsub("[[:space:]]+", " ", catl); catl
        catl <- catl[catl != " "]
        catl <- catl[catl != ""]
        catl <- gsub("^ +","", catl)

        for (a in 1:length(catl)){

          covnames <- c(covnames, str_split(catl, " ")[[a]][1])
        }
        covdetail <- c(covdetail, catl)

      }


      # in case of groups, we remove the non-grouped value
      ntosup <- double()
      for (n in 1:length(covnames)){
        test <- grep(paste0("t",covnames[n]), covnames)
        if(length(test) > 0) ntosup <- c(ntosup, n)
      }

      if(length(ntosup > 0)) covnames <- covnames[-ntosup]
      if(length(ntosup > 0)) covdetail <- covdetail[-ntosup]
      ######### quels sont les collones du df contenant les cov d'interet?

      coldebase <- lapply(as.list(covnames), function(x) grep(x, names(df)))
      coltoselect <- unlist(coldebase)

      # On r?cup?re le dataset d'int?ret
      df %>%
        select( ID,coltoselect) %>%
        distinct()   -> covtemp

      for (temp in 1:length(coldebase)){
        split <-  str_split(covdetail[temp]," ")[[1]] # on dissocier le nom de la cov et les diff?rentes valeurs possibles
        if(length(split) > 1 ){ # si cat?goriel donc

          name <- split[1] # le nom de la  cov
          possible_value <- split[-1] # les valeurs possibles
          possible_value <- possible_value[possible_value != ""]
          # on merge  pour la cov d'int?ret
          covtemp %>%
            gather(starts_with(name),  key = "name", value = "test") -> covtemp


          covtemp$name <- gsub(paste0(name,"_"), "", covtemp$name) # on retire le nom de la covariable (qu'il faut mettre en titre de col.  plutot)
          already_in <- unique(covtemp$name) # on regarde quels sont les cov d?j? int?gr?
          toadd <- possible_value[!(possible_value %in% already_in)] # et donc qu'elle est la valeure manquante

          idalreadyattributed <- covtemp$ID[covtemp$test == 1] # quels sont les IDS  qui ont d?j? eu une ballque
          idtoadd <- unique( covtemp$ID)[!(unique( covtemp$ID) %in% idalreadyattributed)] # et donc quels sont les IDS a qui on doit attribuer la derniere valeure

          for (id in idtoadd){

            covtemp$name[covtemp$ID == id][1] <- toadd
            covtemp$test[covtemp$ID == id][1] <- 1
          }

          names(covtemp)[ names(covtemp) == "name"] <- name
          # covtemp$name[covtemp$ID %in% idtoadd]  <- toadd
          # covtemp$test[covtemp$ID %in% idtoadd] <- 1

          covtemp %>%
            filter(test == 1)%>%
            select(-test) -> covtemp


        }


      }

      return(covtemp)
    }


    eta_research_Monolix<- function(df = indiv_eta_txt){

      df %>%
        select(ID, ends_with("_mode")) %>%
        select(ID, starts_with("ET")) %>%
        distinct() -> output

    if(length(output) == 1 ){

      df %>%
        select(ID, ends_with("_mean")) %>%
        select(ID, starts_with("ET")) %>%
        distinct() -> output

    }
      return(output)

    } # Renvoie les ?tas






    data_result_Monolix = function(pop_parameters = pop_parameters_txt){

      test <- try( pop_parameters[(grep("Estimation of the population paramet", pop_parameters)+3):(grep("orrelation matrix", pop_parameters)-2)], silent = T)
      if (class(test) != "try-error"){
        line_temp <-   test
      } else{
        line_temp <- pop_parameters[(grep("Estimation of the population paramet", pop_parameters)+3):(grep("___________", pop_parameters)[1]-2)]
      }
      line_temp <- line_temp[line_temp != ""]
      results <- tibble(Parameter = character(), Theta = double(), se = double(), rse = double(), p_value = double())
      for(a in line_temp){
        temp <- as.character(unlist(str_split(a, " ")))
        temp[temp == "-"] <- NA
        temp <- temp[temp != "" & temp!= ":"]
        temp2 <- tibble(Parameter = temp[1], Theta = temp[2], se = temp[3], rse = temp[4], p_value = temp[5])
        results <- rbind(results, temp2)
      }
      results[] <- lapply(results, function(x){
        x[is.na(x) == T ] <- "-"
        x[x == "NaN"  ] <- "-"
        x
      })

      if(length(unique(results$p_value)) == 1){
        results <- results %>% select(- p_value)
      }

      ## Shrinkage

      eta_research_Monolix() -> etas



      results %>%
        left_join(
      eta_research_Monolix() %>%
        select(-ID) %>%
        map_dfr(sd) %>%
        rownames_to_column() %>%
        select(-rowname) %>%
        gather(everything(), key = "Parameter", value = "sd") %>%
        mutate(Parameter = gsub("eta", "omega", Parameter)) %>%
        mutate(Parameter = gsub("_mode", "", Parameter)) %>%
        mutate(Parameter = gsub("_mean", "", Parameter))
        ) %>%
        mutate(`Shrink(%)` = 1 - sd/ as.double(Theta)) %>%
        select(-sd) %>%
        mutate(`Shrink(%)` = if_else(is.na(`Shrink(%)`), "-", paste0(round(`Shrink(%)`*100)))) -> results




      return(results %>%
               rename("SE" = se) %>%
               rename("RSE(%)" = rse) %>%
               rename("Value" = Theta))
    }#; data_result_Monolix()

    residus_research_Monolix <- function(predictions = predictions_txt){

      PRED <- suppressMessages(observation_research_Monolix() %>%
        left_join(prediction_research_Monolix()) %>%
        mutate(RES = IPRED - OBS) %>%
        select(ID, RES, TIME))


      output <-  suppressMessages(predictions %>%
        select(matches("NPDE|ID|time|indWRes_mode|indWRes_mean."))  %>%
        rename(TIME = time) %>%
        left_join(PRED))

      test <- grep("indWRes_", names(output))
      if(length(test) ==2){

        output <- output[ , - test[1]]
      }

      names(output)<-  gsub("(_mode)|(_mean\\.)", "", names(output))

      if("indWRes" %in% names(output)){

        output <- output %>%
          rename(IWRES = indWRes)
      }




      return(output)
    } # Renvoie les r?sidus

    date_creation <- function(path = path_to_run){
      return(file.info(path)$mtime)
    }

    protocole <- function(predictions = predictions_txt, fulltimes = fulltimes_txt){

      # Select DV times
      DVTimes <- predictions %>%
        select(ID, time)

      # Select ALL times
      fullTimes <- fulltimes %>%
        select(ID, time)

      # Select the others times = administration times !
      # fullTimes %>%
      #   left_join(DVTimes, by = c("ID"))
      to_remove <- double()
      for (a in unique(fullTimes$ID)){

        to_remove <- c(to_remove, which(fullTimes$ID == a & fullTimes$time %in% DVTimes$time[DVTimes$ID == a]))
      }

      # fullTimes[-to_remove, ] %>%
      #   group_by(time) %>%
      #   summarise(length(time))
      return(
      fullTimes[-to_remove, ] %>%
        rename(TIME = time)
      )
    }

    name_research <- function(path = path_to_run){

      return(gsub(".+\\/","",path))

    }

    score <- function(pop_parameters = pop_parameters_txt){

      AIC_line <- pop_parameters[grep("Akaike", pop_parameters)][[1]]
      gsub("Akaike Information Criteria[[:blank:]]+\\(AIC):[[:blank:]]+","", AIC_line ) %>%
      {gsub("\\(.+\\)", "", .)} %>%
      as.double() -> AIC

      BIC_line <- pop_parameters[grep("(BIC)", pop_parameters)][[1]]
      gsub("Bayesian Information Criteria[[:blank:]]+\\(BIC):[[:blank:]]+","", BIC_line ) %>%
      {gsub("\\(.+\\)", "", .)} %>%
        as.double() -> BIC



      tibble(Score = c("FO", "AIC", "BIC"),
             Value = c(FO_research_Monolix(), AIC, BIC)
      )



    }


    ################################### S E T T E R S #########################
    # Objective: store every function output in corresponding run object attribute

    object@software <- "Monolix"
    # print("Name Research")
    try(object@name <- name_research(), silent = T)
    object@status <- "Not concerned by Monolix Runs"
    # print("Path Research")
    try(object@path <- path_to_run, silent = T)
    # print("Estimation Research")
    try(object@estimation <- data_result_Monolix(), silent = T)
    # print("LOQ Research")
    # try(object@LOQ <- loq())
    # print("FO Research")
    try(object@FO <- FO_research_Monolix(), silent = T)
    # print("Observation Research")
    try(object@OBS <- observation_research_Monolix(), silent = T)
    # print("Prediction Research")
    try(object@IPRED <- prediction_research_Monolix(), silent = T)
    # print("COV Research")
    test <- try(cov_research_Monolix(), silent = T)
    if(class(test) != "try-error"){

      object@cov <- test

    }else{

      if(length(unique(object@OBS$ID)) >0){
      object@cov  <- tibble(ID = unique(object@OBS$ID))
      }
    }
    # print("Residus Research")
    try(object@residus <- residus_research_Monolix(), silent = T)
    # print("Random Effect Research")
    try(object@randomEffect <- eta_research_Monolix(), silent = T)
    # print("Date Research")
    try(object@date <- as.character(date_creation()), silent = T)
    # print("Administration Research")
    try(object@administration <- protocole(), silent = T)
    try(object@Score <- score(), silent = T)




    return(object)
  }
)


