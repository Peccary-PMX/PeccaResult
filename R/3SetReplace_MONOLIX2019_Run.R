#' setPath_MONOLIX
#' Author: Thibaud Derippe
setGeneric("setPath_MONOLIX2019<-",
           function(object, value){standardGeneric("setPath_MONOLIX2019<-")}
)

value <- "file:///D:/Peccary/Exemple_demo/bolusLinear_project"
value <- "D:/these/Pecc_test/3_Models/1_Models/alemtuzumab_final_dataset/2CMT_ful_matrix"
value <- "file:///D:/these/Pecc_test/3_Models/1_Models/000_21_01_11_5ytype/cov_analsysis/Ref_without_cov_no_growth_estimElim_IL7onExpanHillIL7_10HigherEff4_IL750free2"
value <- "file:///D:/Peccary/Exemple_demo/selma_stuff/a0_d2_sex_dtr"
value <- "file:///D:/these/Second_project/BH3m_pecc/3_Models/first_try"
#' SetReplaceMethod to fill a run object with the  corresponding patwhay
#' Author: Thibaud Derippe
#' Arguments: object, the run object we want to fill the attribute
#' Arguments: value, pathway to the run
#' Output: not concerned, SetReplacedMethod
#' @export
setReplaceMethod(
  f="setPath_MONOLIX2019",
  signature = "run",
  definition = function(object, value){

    print("Creation of a new Monolix 2019 run ")
    ################################### P A T H     C L E A N E R #########################
    # Objective: improve the toleratibility of the input patway
# value <- "file:///C:/Users/titi7/lixoft/monolix/monolix2019R2/demos/2.models_for_continuous_outcomes/2.1.residual_error_model/warfarinPK_project"

  # value <- "file:///D:/these/Monolix/3_Models/1_models/tgi/tgi"
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

    pop_parameters_txt <- try(readLines(paste0(path_to_run,"/summary.txt"), skipNul = T))


    predictions_txt <-try(
# careful with YTYPE ! I had once "redictions_y4.txt" vs "redictions_y4_.txt"...
# so lets work with character and not double
        tibble(files = files[grep("predictions.*\\.txt", files)]) %>%
          mutate(YTYPE = gsub("(predictions_y)|(\\.txt)", "", files)) %>%
          mutate(predictions = map(files, function(x){

            as_tibble(read.table(paste0(path_to_run, "/", x), sep = ",", header = T)) -> temp

            names(temp)[3] <- "y"

            temp <- temp %>%
              select(id, time, y, matches("Pred|Res|NPDE") )
            # if(is.na(y) == F)  temp <- temp %>%  mutate(YTYPE = y)
            return(temp)


          }  )) %>%

          select( -files) %>%
          select(predictions, YTYPE) %>%
          unnest %>%
          rename(ID = id) %>%
          rename(OBS = y) %>%
          mutate(ID = gsub("#", "_", ID)),
          silent = T)


  # if("YTYPE" %in% names(predictions_txt))  predictions_txt <- predictions_txt %>%
  #                                                mutate(YTYPE = as.double(gsub("y_", "", YTYPE)))

  if(class(predictions_txt)[1] != "try-error"){

    names(predictions_txt)[names(predictions_txt) == "time"] <- "TIME"

   if(length(unique(predictions_txt$YTYPE)) == 1) predictions_txt <- predictions_txt %>% select(-YTYPE)
  }

     # predictions_txt <- try(as_tibble(readTablePerso(paste0(path_to_run,"/predictions.txt"), header = T)), silent = T)
    # finegrid_txt <- try(as_tibble(readTablePerso( paste0(path_to_run,"/finegrid.txt"), header = T)), silent = T)
#
    finegrid_txt <-try(

      tibble(files =  list.files(paste0(path_to_run, "/ChartsData/IndividualFits/",grep("finegrid[[:digit:]]?\\.txt", files))))%>%
      mutate(test = map_dbl(files,  ~ length(grep("fits", .x)))) %>%
        filter(test == 1) %>% select(-test) %>%
      mutate(YTYPE = gsub("y|(_fits.txt)", "", files)) %>%

      mutate(predictions = map(files, function(x){

        as_tibble(read.table(paste0(path_to_run, "/ChartsData/IndividualFits/", x), header = T, sep = ",")) -> temp


        names(temp)[1] <- "ID"

        temp %>%
          # rename(ID = id) %>%
          select( ID, time, contains("popPred")  	, contains("indivPredMode") )

      }  )) %>%
      select(-files) %>%
      unnest %>%
      mutate(ID = gsub("#", "_", ID)) %>%
        rename(TIME = time),
      silent = T)


    if(class(finegrid_txt)[1] != "try-error"){

      if(length(unique(finegrid_txt$YTYPE)) == 1) finegrid_txt <- finegrid_txt %>% select(-YTYPE)
    }
    # if("YTYPE" %in% names(finegrid_txt)){
    #
    #   if(length(unique(finegrid_txt$YTYPE)) == 1) finegrid_txt <- finegrid_txt %>% select(-YTYPE)
    #
    # }

    etas_txt <- try(read.table(paste0(path_to_run,"/ChartsData/CorrelationBetweenRandomEffects/eta.txt"),header = T,  sep = ",", comment.char = "") %>%
                  as_tibble)

    if(class(etas_txt)[1] != "try-error") names(etas_txt)[1] <- "ID"

    covariatesSummary_txt <- try(read.table(paste0(path_to_run, "/ChartsData/IndividualParametersVsCovariates/covariates.txt"), sep = ",", comment.char = "", header = T), silent = T)


    files <- list.files(paste0(path_to_run, "/ChartsData/IndividualFits"))
    # files <- list.files("D:/these/Pecc_test/3_Models/1_Models/000_20_04_03/model_base_ExpansionCov/ChartsData/IndividualFits")
    obs_txt <-try(


      tibble(files = files[grep("observations\\.txt", files)]) %>%
        # mutate(n = as.double(gsub("\\D", "", files))) %>%
        # {.[[4,1]]} -> x
      mutate(predictions = map(files, function(x){
        # print(x)
        as_tibble(read.table(paste0(path_to_run, "/ChartsData/IndividualFits/", x), header = T,sep = ",", comment.char = "")) %>%
          select(-color) -> temp
        # print(temp)
        # print(temp)
        names(temp)[[1]] <- "ID"
        names(temp)[[3]] -> namesobs
        names(temp)[[3]] <- "OBS"
        # print(names(temp)[[3]])
        temp %>%
          mutate(YTYPE = gsub("^y", "",namesobs)) %>%
          select(ID, time, OBS, censored, YTYPE) %>%
          rename(BLQ = censored)

      }  )) %>%
        unnest %>%
        select(-files) %>%
        rename(TIME = time) %>%
        mutate(ID = gsub("#", "_", ID)),
      silent = T)


    if(class(obs_txt)[1] != "try-error"){

      if(length(unique(obs_txt$YTYPE)) == 1) obs_txt <- obs_txt %>% select(-YTYPE)
    }
    ################################### F U N C T I O N S #########################
    # Objective: by using the inputs, extract the information and standardise it


    FO_research_Monolix <- function(pop_parameters = pop_parameters_txt){

      line_temp  <- unlist(pop_parameters[grep('2 x log-likeliho', pop_parameters)])
      line_temp <- gsub(".+:", "", line_temp)
      return(as.double(line_temp))

    }



    path_dataset_research <- function(path = path_to_run){

      readLines(paste0(path,"/summary.txt")) -> temp

      temp[grep(".mlxtra", temp)] %>%
        gsub(pattern = "\\*| ", replacement = "") -> mlxtran_name


      # if mlxtran on the same folder, return it
      if(file.exists(paste0(path, "/", mlxtran_name))) return(paste0(path, "/", mlxtran_name))

      # else
      str_split(path, "/")[[1]] -> temp
      paste0(c(temp[-length(temp)],mlxtran_name) , collapse = "/") -> output

      if(file.exists(output)) return(output)

    }



    dataset_research <- function(){

      ## copy past of what i already did in monolix_to_desolve function

      path <- path_dataset_research()
      lines <-  readLines(path)

      root <- gsub( gsub(".+/", "", path), "", path)

      file <- lines[grep("file = ", lines)]; file <- file[1] %>%
        gsub(pattern = "(file =)|(')|( )",replacement =  "")

      backwardpath <- sum(str_split(file, pattern = "/")[[1]] == "..")
      bloc_root <- str_split(root, pattern = "/")[[1]]
      bloc_root <- bloc_root[bloc_root != ""]
      root2 <- bloc_root[1:(length(bloc_root) - backwardpath)] %>%
        paste(collapse = "/")

      path_dataset <-  paste(root2, gsub("\\.\\./","", file), sep  = "/")

      delimiter <- gsub("(.+=)| ","",lines[grep("^delimiter *=", lines)])
      if(delimiter == "semicolon")  delimiter = ";"

      return(list(path = path_dataset, sep = delimiter))
    }


    observation_research_Monolix = function(obs = obs_txt, df = predictions_txt){

     if(class(obs)[[1]] != "try-error"){

       return(obs_txt)

     }

        df %>%
          select(ID, TIME, OBS, starts_with("YTYPE")) -> temp

      if("YTYPE" %in% names(temp)) temp$YTYPE <- as.double(temp$YTYPE)

      return(temp)
        #doesn't work if y1 instead of  y...
        # see "Z:/ETUDES/S95008/CS_95008/ANACIN/PK/MONOLIX/MONOCOMPbis"


    }

    prediction_research_Monolix = function(df1 = finegrid_txt, df2 = predictions_txt){


       names(df1) <-  gsub("\\*", ".", names(df1))

      df1 %>%
        select(ID, TIME,  indivPredMode, popPred, contains("YTYPE")) %>%
        rename( IPRED = indivPredMode  ) %>%
        rename(PRED = popPred) -> fingrid



      names(df2) <-  gsub("\\*", ".", names(df2))


      df2 %>%
        select(ID, TIME,  indivPred_mode, popPred, contains("YTYPE")) %>%
        rename( IPRED = indivPred_mode  ) %>%
        rename(PRED = popPred) -> vraivalues

      # to avoid having two value at same time

      fingrid %>%
        left_join(vraivalues %>%
                    select(ID, TIME, contains("YTYPE") ) %>%
                    mutate(testvrai = 1)
        ) %>%
        filter(is.na(testvrai)) -> fingrid

# if("YTYPE" %in% names(vraivalues)) vraivalues$YTYPE <- as.double(vraivalues$YTYPE)

      return(
        bind_rows(fingrid, vraivalues) %>%
          arrange(ID, TIME) %>%
          filter(TIME >= min(c(vraivalues$TIME,0)))
      )

    }

    cov_research_Monolix = function(covariatesSummary = covariatesSummary_txt){

      names(covariatesSummary_txt)[1] <- "ID"

      covariatesSummary_txt %>%
        as_tibble() %>%
        select(-contains("trans_"), - contains("eta_"), - color, - split, - filter) %>%
        mutate(ID = as.character(ID))


    }


    eta_research_Monolix<- function(etas = etas_txt){

      etas %>%
        select(ID, starts_with("eta_")) %>%
        select(ID, ends_with("_mean")) -> temp

      names(temp) <- gsub("(_mean)|(_mode)", "", names(temp))

      return(temp%>%
               mutate(ID = as.character(ID)) %>%
               as_tibble)

    } # Renvoie les ?tas



    individualvalues_Monolix<- function(etas = etas_txt){

      trytest <- try(read.table(paste0(path_to_run,"/IndividualParameters/estimatedIndividualParameters.txt"), header = T, sep = ",") %>%
            as_tibble())

      if(class(trytest)[1] != "try-error"){

        trytest %>%
          select(id, ends_with("mode")) -> temp

        names(temp) <- gsub("_mode$", "", names(temp))

        if(ncol(trytest) == 1){

          trytest %>%
            select(id, ends_with("mean")) -> temp

          names(temp) <- gsub("_mean$", "", names(temp))

        }

        return(temp%>%
                rename(ID = id))

      }



    } # Renvoie les ?tas




    data_result_Monolix = function(){

      read.table(paste0(path_to_run,"/populationParameters.txt"), header = T, sep = ",")-> restemp

      if(length(which(names(restemp) == "se_sa")) > 0 )restemp[-which(names(restemp) == "se_sa")] -> restemp

      tryetas <- try(eta_research_Monolix())

      if(ncol(restemp)>1 & class(tryetas) != "try-error"){

        restemp <- restemp %>%
        left_join(


      eta_research_Monolix() %>%
        select(-ID) %>%
        map_dfr(sd) %>%
        gather(key = "parameter", value = "sd") %>%
        mutate(parameter = (gsub("eta", "omega", parameter))) %>%
        mutate(parameter = (gsub("(_mean$)|(_mode$)", "", parameter)))

        ) %>%
        mutate(`Shrink(%)` = paste0(round(((1 - sd/ as.double(value))*100),1), "%")) %>%
        # mutate(sd = if_else(is.na(sd), "", as.character(sd))) %>%
        select(-sd) %>%
       mutate(`Shrink(%)` = if_else(`Shrink(%)` == "NA%", "", `Shrink(%)`)) -> restemp

      }



      if(!"rse_sa" %in% names(restemp)) restemp$rse_sa <- NA

      restemp <- restemp %>%
        mutate(rse_sa = paste0(round(rse_sa, 1 ), "%")) %>%
        rename("RSE(%)" = rse_sa) %>%
        mutate(`RSE(%)` = if_else(`RSE(%)` == "NA%", "-", `RSE(%)`)) %>%
        rename(Value = value) %>%
        rename(Parameter = parameter) %>%
        mutate(Value = case_when(Value < 100 ~ signif(Value, 4),
                                 T ~ round(Value, 2) ))


      names(restemp)[which(names(restemp) == "value")] <- "Value"
      names(restemp)[which(names(restemp) == "parameter")] <- "Parameter"


return(restemp)

    } #; data_result_Monolix()

    residus_research_Monolix <- function(predictions = predictions_txt){

      PRED <- suppressMessages(
        observation_research_Monolix() %>%
        left_join(prediction_research_Monolix()) %>%
        mutate(RES = IPRED - OBS) %>%
        select(ID, RES, TIME,contains("BLQ"), contains("YTYPE"))
 )


      output <-  suppressMessages(predictions %>%
        select(matches("NPDE|ID|time|indWRes_mode|indWRes_mean|YTYPE"))  %>%
        # rename(TIME = time) %>%
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

    # protocole <- function(predictions = predictions_txt){
    #
    #   # # Select DV times
    #   # DVTimes <- predictions %>%
    #   #   select(ID, time)
    #   #
    #   # # # Select ALL times
    #   # # fullTimes <- fulltimes %>%
    #   # #   select(ID, time)
    #   #
    #   # # Select the others times = administration times !
    #   # # fullTimes %>%
    #   # #   left_join(DVTimes, by = c("ID"))
    #   # to_remove <- double()
    #   # for (a in unique(fullTimes$ID)){
    #   #
    #   #   to_remove <- c(to_remove, which(fullTimes$ID == a & fullTimes$time %in% DVTimes$time[DVTimes$ID == a]))
    #   # }
    #   #
    #   # # fullTimes[-to_remove, ] %>%
    #   # #   group_by(time) %>%
    #   # #   summarise(length(time))
    #   # return(
    #   # fullTimes[-to_remove, ] %>%
    #   #   rename(TIME = time)
    #   # )
    # }

    name_research <- function(path = path_to_run){

      return(gsub(".+\\/","",path))

    }

    score <- function(pop_parameters = pop_parameters_txt){

      AIC_line <- pop_parameters[grep("Akaike", pop_parameters)][[1]]
      gsub(".+:","", AIC_line ) %>%
      # {gsub("\\(.+\\)", "", .)} %>%
      as.double() -> AIC

      BIC_line <- pop_parameters[grep("(BIC)", pop_parameters)][[1]]
      gsub(".+:","", BIC_line ) %>%
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
    try(object@individualValues <- individualvalues_Monolix(), silent = T)
    try(object@path_source <- path_dataset_research(), silent = T)
    try(object@dataset <- dataset_research(), silent = T)


    return(object)
  }
)

# test <- createFolder("file:///D:/these/Pecc_test/3_Models/1_Models/lymphodepletion")
# test <- createRun("file:///D:/these/Pecc_test/3_Models/1_Models/000_20_04_03/model_base")
# test <- createRun("file:///D:/these/Pecc_test/3_Models/1_Models/lymphodepletion/frieb1_elimForced")
# value <- "file:///D:/these/Pecc_test/3_Models/1_Models/lymphodepletion/frieb1_elimForced"
# test@
#   object <- new("run")
#
# object@dataset <- list(a = mtcars, b = "lol")
# #
# createRun("file:///D:/these/Monolix/3_Models/1_models/tgi")
# createRun
