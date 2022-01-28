#' setPath_ADAPT
#' Author: Thibaud Derippe
setGeneric("setPath_ADAPT<-",
           function(object, value){standardGeneric("setPath_ADAPT<-")}
)



# value <- "file:///D:/Peccary/Exemple_demo/run_adapat/SU4_kg/SU4_controlCC2.run"
# value <- "file:///D:/Peccary/Exemple_demo/run_adapat/Cell Cycle Base/SU4_controlCC2.run"
#' SetReplaceMethod to fill a run object with the corresponding patwhay
#' Author: Thibaud Derippe
#' Arguments: object, the run object we want to fill the attribute
#' Arguments: value, pathway to the run
#' Output: not concerned, SetReplacedMethod
#' @export
setReplaceMethod(
  f="setPath_ADAPT",
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


    # to read result fie
    dot_res_file <- readLines(paste0(path_to_run, ".run"), skipNul = T)


    # output -> need to split finegrid from observation
    pathres <- paste0(path_to_run,"PLT.csv")
    reslines <- readLines(pathres)
    firstline <- reslines[[1]]
    str_split(reslines, pattern = ",", simplify = T) %>%
      as.data.frame() %>%
      as_tibble()-> res

    test <- map(res, ~ sum(.x == ""))

    nsplit <- as.double(test[test != 0][1] %>%
      names() %>%
      gsub(pattern = "V",replacement =  ""))

    if(test[length(test)] > test[[nsplit]]) res  <- res[-length(res)]

    grid_table <- res[1:(nsplit-1)] %>%
      map_df(~ gsub(" ", "",.x))
    names(grid_table) <- grid_table %>%
      slice(1) %>%
      gather(key = "key", value = "value") %>%
      pull(value)
    grid_table <- grid_table %>% slice(-1)

    obs_table <- res[nsplit:length(res)] %>%
      map_df(~ gsub(" ", "",.x))
    names(obs_table) <- obs_table %>%
      slice(1) %>%
      gather(key = "key", value = "value") %>%
      pull(value)

    b <- 1
    for(a in grep("-SE$", names(obs_table))){

      names(obs_table)[a + 1] <- paste0(names(obs_table)[a + 1], b)
      names(obs_table)[a + 2] <- paste0(names(obs_table)[a + 2], b)
      b <- b + 1
    }

    obs_table <- obs_table %>% slice(-1)


    ################################### F U N C T I O N S #########################
    # Objective: by using the inputs, extract the information and standardise it

    # Find the name of the run
    name_research <- function(path = path_to_run){

      return(gsub("^.+\\/", "", path))

    }#; name_research()


    # Search the objective function of the run
    FO_research_ADAPT <- function(res_file = dot_res_file){

      grepp <- grep("Negative Log Likelihood",res_file )
      line <- res_file[grepp[length(grepp)]]

      result <- gsub("(.+: *)| *", "", line) %>% as.double

      return(result)
    }#; FO_research_ADAPT()




### F I N A L    P A R A M E T E R   T A B L E ---   F I N A L    P A R A M E T E R   T A B L E
    # Search the parameter estimation, standard deviation and RSE of the run
    data_result_ADAPT <- function(res_file = dot_res_file){

      balise <- grep("Parameter *Value", res_file)
      balise2 <- grep("Correlation Matrix", res_file)

      bloc <- res_file[(balise + 1):(balise2-1)]
      bloc <- gsub("\\( *", "(", bloc)
      bloc <- gsub("\\[  *", "[", bloc)
      bloc <- gsub(" *\\]", "]", bloc)
      bloc <- gsub(" *, *", ",", bloc)
      bloc <- bloc[bloc != ""]
      bloc <- gsub("^ *", "", bloc)
      bloc <- gsub("Not estimated", "NE", bloc)

      str_split(bloc, "  *", simplify = T) %>%
        as.data.frame() %>%
        as_tibble() %>%
        map_df(as.character)-> res

    names(res) <- c("Parameter", "Inivalue", "Value", "RSE(%)", "CI")[1:ncol(res)]

    output <- res %>%
      mutate(Value = if_else(Value == "NE", Inivalue, Value)) %>%
      select(Parameter, Value, `RSE(%)` )

      return(output)

    }#; data_result_ADAPT()

    # Only the observation
    observation_research_ADAPT <- function(df = obs_table){
      # Objective: get only the observation value

     temp <-  df %>%
        select(contains("Time"), contains("Z("))

     names(temp)[grep("Time", names(temp))] <- "TIME"
     names(temp) <- gsub ("(Z\\()|\\)", "", names(temp))

     temp <- temp %>%
       gather(-TIME, key = "YTYPE", value = "OBS")

     if(! "ID" %in% names(temp)) temp$ID <- "1"

     temp <- temp %>%
       select(ID, TIME, OBS, contains("YTYPE"))

     temp <- temp %>%
       mutate(TIME = as.double(TIME), OBS = as.double(OBS)) %>%
       filter(!is.na(OBS)) %>%
       distinct

       return(temp)

    }#;observation_research_ADAPT() #Renvoie les observation

    # Only the prediction
    prediction_research_ADAPT <- function(df = obs_table){


      temp <-  df %>%
        select(contains("Time"), contains("Y("))


      if(sum(grepl("-SE", names(temp))) > 0) temp <- temp[-grep("-SE", names(temp))]

      names(temp)[grep("Time", names(temp))] <- "TIME"
      names(temp) <- gsub ("(Y\\()|\\)", "", names(temp))

      temp <- temp %>%
        gather(-TIME, key = "YTYPE", value = "IPRED")

      if(! "ID" %in% names(temp)){
        temp$ID <- "1"
        temp$PRED <- temp$IPRED
      }

      temp <- temp %>%
        select(ID, TIME, IPRED, PRED, contains("YTYPE")) %>%
        mutate(TIME = as.double(TIME), IPRED = as.double(IPRED), PRED = as.double(PRED))


      return(temp)

      }#;prediction_research_ADAPT()


     # Renvoie les PRED et IPRED

    # Covariable of interest
    cov_research_ADAPT <- function(df = dot_tab_df){
      return(
        prediction_research_ADAPT() %>% distinct(ID)
      )
    }#; cov_research_ADAPT() # Renvoie les covariables

    # Residuals
    residus_research_ADAPT <- function(df = obs_table){

      temp <-  df %>%
        select(contains("Time"), contains("Residual"), contains("Stand.Res."))



      names(temp)[grep("Time", names(temp))] <- "TIME"

      temp %>%
        gather(-TIME, key = "key", value = "value") %>%
        mutate(YTYPE = gsub("(Residual)|(Stand\\.Res\\.)", "", key )) %>%
        mutate(key = if_else(grepl("Residu",key ), "Res", "StandRes")) -> temp

      temp %>%
        filter(key == "Res") %>%
        rowid_to_column() %>%
        select(-key) %>% rename(Res = value) %>%
        left_join(

          temp %>%
            filter(key == "StandRes") %>%
            rowid_to_column() %>%
            select(-key) %>% rename(StandRes = value)
        ) -> temp


      if(! "ID" %in% names(temp)){
        temp$ID <- "1"
      }

      temp <- temp %>%
        select(ID, TIME, Res, StandRes, YTYPE) %>%
        mutate(TIME = as.double(TIME), Res = as.double(Res), StandRes = as.double(StandRes))
      # if("RES"  %in% names(temp)) temp %>% filter(RES !=0) -> temp

      return(temp)
    }#;residus_research_ADAPT() # Renvoie les r?sidus

    # Eta values
    eta_research_ADAPT <- function(df = dot_tab_df, res_file = dot_res_file){
todo
      return(eta)
    }#; eta_research_ADAPT() # Renvoie les ?tas

    # Date of creation
    date_creation <- function(path = path_to_run){
      path_temp <- paste0(path,".run")
      return(file.info(path_temp)$mtime)
      }#; date_creation()

    # Protocole, time of administraiton
    protocole <- function(df = dot_tab_df){

      cov_research_ADAPT()

    }#; protocole()

    # Score

    # Search the objective function of the run
    scores_ADAPT <- function(res_file = dot_res_file){

      # look for the objective function
      AIC <-  res_file[grep("AIC *:", res_file)] %>%
        gsub(pattern = "(.+: *)| *", replacement = "") %>%
        as.double

      BIC <-  res_file[grep("BIC *:", res_file)] %>%
        gsub(pattern = "(.+: *)| *", replacement = "") %>%
        as.double

      result <- tibble(Score = c("FO", "AIC", "BIC"), Value = c(FO_research_ADAPT(), AIC, BIC))

      return(result)
    }#; scores_ADAPT()



    ################################### S E T T E R S #########################
    # Objective: store every function output in corresponding run object attribute

    object@software <- "ADAPT"
    # print("Name Research")
    try(object@name <- name_research(), silent = T)
    # print("Statut Research")
    try(object@status <- status_research(), silent = T)
    # print("Path Research")
    try(object@path <- path_to_run, silent = T)
    # print("Estimation Research")
    try(object@estimation <- data_result_ADAPT(), silent = T)
    # print("FO Research")
    try(object@FO <- FO_research_ADAPT(), silent = T)
    # print("Observation Research")
    try(object@OBS <- observation_research_ADAPT(), silent = T)
    # print("Prediction Research")
    try(object@IPRED <- prediction_research_ADAPT(), silent = T)
    # print("COV Research")
    try(object@cov <- cov_research_ADAPT(), silent = T)
    # print("Residus Research")
    try(object@residus <- residus_research_ADAPT(), silent = T)
    # print("Random Effect Research")
    try(object@randomEffect <- eta_research_ADAPT(), silent = T)
    # print("Date Research")
    try(object@date <- as.character(date_creation()), silent = T)
    # print("Administration Research")
    try(object@administration <- protocole(), silent = T)
    try(object@Score <- scores_ADAPT(), silent = T)


    return(object)
  }
)
# data()
# rm()
# demo <- createDossier("file://emsw0136/_cinetiq/CINETIQ/2_Scientifique/Software/R/Package_Thibaud")
# demo(13)
