#' setPath_NLMIXR
#' Author: Thibaud Derippe
setGeneric("setPath_NLMIXR<-",
           function(object, value){standardGeneric("setPath_NLMIXR<-")}
)

# value <- "D:/Peccary/Exemple_demo/nlmixr_test/model6.nlmixr"
# SetReplaceMethod to fill a run object with the corresponding patwhay
#' Author: Thibaud Derippe
#' Arguments: object, the run object we want to fill the attribute
#' Arguments: value, pathway to the run
#' Output: not concerned, SetReplacedMethod
#' @export
setReplaceMethod(
  f="setPath_NLMIXR",
  signature = "run",
  definition = function(object, value){

    print("Creation of a new run")
    ################################### P A T H     C L E A N E R #########################
    # Objective: improve the toleratibility of the input patway

    # Allowing copypast with RStudio
    path_to_run <- gsub("^file:///", "", value)
    ## For instance for "file://emsw0136/" wher we absolutely need the "//" beginning
    path_to_run <- gsub("^file:", "", path_to_run)
    # Standardisation by replacing backslash
    path_to_run <- gsub("\\\\", "\\/", path_to_run)

    ################################### F I L E S     R E A D I N G  #########################
    # Objective: read once for all every files containing data

    runRDS <- readRDS(path_to_run)

    ################################### F U N C T I O N S #########################
    # Objective: by using the inputs, extract the information and standardise it

    # Find the name of the run
    name_research <- function(path = path_to_run){

      return(gsub("(^.+\\/)|(\\.nlmixr$)", "", path))

    }#; name_research()

    # Find statut of the run (sussessful, terminating due to rounding error...)
    status_research <- function(){

      return("to implement")

    }#; status_research()


    # Search the objective function of the run
    FO_research_NLMIXR <- function(){

      return(runRDS$objf)
    }#; FO_research_NLMIXR()


### F I N A L    P A R A M E T E R   T A B L E ---   F I N A L    P A R A M E T E R   T A B L E
    # Search the parameter estimation, standard deviation and RSE of the run
    data_result_NLMIXR <- function(){

      runRDS$par.fixed %>%
        as.data.frame() %>%
        select(-contains("Parameter")) %>%
        rownames_to_column("Parameter") %>%
        mutate(Parameter = gsub("^l", "", Parameter)) %>%
        rename(Value = `Back-transformed(95%CI)`, `RSE(%)` = `%RSE`)  %>%
        mutate(Value = as.double(gsub(" *\\(.+", "", Value))) %>%
        select(Parameter, Value, SE, `RSE(%)`, `BSV(CV%)`,  `Shrink(SD)%`) -> tempTheta

      tibble(Parameter = rownames(runRDS$omega.R), Value = round(diag(  runRDS$omega.R),3)) %>%
        mutate(Parameter = gsub("eta\\.", "", Parameter)) %>%
        left_join(tempTheta %>%
                    select(Parameter,  `BSV(CV%)`,  `Shrink(SD)%`), by = "Parameter") %>%
        rename(`RSE(%)` =  `BSV(CV%)`, `Shrink(%)` = `Shrink(SD)%`) %>%
        mutate(`Shrink(%)` = gsub("%<", "", `Shrink(%)`)) %>%
        mutate(Parameter = paste0("omega_", Parameter)) %>%
        mutate(SE = "-") -> tempEta

      tempTheta %>%
        select(Parameter, Value, SE, `RSE(%)`) %>%
        mutate(Parameter =  paste0(Parameter, "_pop")) %>%
        mutate(`Shrink(%)` = "-") %>%
        mutate(Parameter = if_else(Parameter == "prop.err_pop", "b", Parameter)) %>%
        bind_rows(tempEta)
    }#; data_result_NLMIXR()

    # Only the observation
    observation_research_NLMIXR <- function(){
      # Objective: get only the observation value
      runRDS %>%
        select(ID, TIME, DV) %>%
        rename(OBS = DV)%>%
        as_tibble

    }#;observation_research_NLMIXR() #Renvoie les observation

    # Only the prediction
    prediction_research_NLMIXR <- function(){


      runRDS %>%
        select(ID, TIME, IPRED, PRED) %>%
        as_tibble


      }#;prediction_research_NLMIXR()


     # Renvoie les PRED et IPRED

    # Covariable of interest
    cov_research_NLMIXR <- function(){

      runRDS %>%
        select(ID)%>%
        as_tibble
    }#; cov_research_NLMIXR() # Renvoie les covariables

    # Residuals
    residus_research_NLMIXR <- function(){

      runRDS %>%
        select(ID, TIME, IRES, IWRES)%>%
        as_tibble

    }#;residus_research_NLMIXR() # Renvoie les r?sidus

    # Eta values
    eta_research_NLMIXR <- function(){
# also in   model$coefficients
      runRDS %>%
        select(ID,starts_with("eta")) %>%
        distinct()%>%
        as_tibble -> temp

      names(temp) <- gsub("\\.", "_", names(temp))
      return(temp)
    }#; eta_research_NLMIXR() # Renvoie les ?tas

    # Date of creation
    date_creation <- function(path = path_to_run){

      return(file.info(path_to_run)$mtime)
      }#; date_creation()

    # Protocole, time of administraiton
    # protocole <- function(df = dot_tab_df){
    #
    #   names(df)[toupper(names(df)) == "TIME"] <- "TIME"
    #   df %>%
    #     filter(DV == 0 & EVID == 1 ) %>%
    #     select(ID, TIME) %>%
    #     distinct
    #
    # }#; protocole()

    # Score

    # Search the objective function of the run
    scores_NLMIXR <- function(){

      tibble(Score = c("FO", "AIC", "BIC"),
                       Value = c(runRDS$objDf[["OBJF"]],runRDS$objDf[["AIC"]] ,runRDS$objDf[["BIC"]]))

    }#; scores_NLMIXR()

    # path source


    ################################### S E T T E R S #########################
    # Objective: store every function output in corresponding run object attribute

    object@software <- "NLMIXR"
    # print("Name Research")
    try(object@name <- name_research(), silent = T)
    # print("Statut Research")
    try(object@status <- status_research(), silent = T)
    # print("Path Research")
    try(object@path <- path_to_run, silent = T)
    # print("Estimation Research")
    try(object@estimation <- data_result_NLMIXR(), silent = T)
    # print("FO Research")
    try(object@FO <- FO_research_NLMIXR(), silent = T)
    # print("Observation Research")
    try(object@OBS <- observation_research_NLMIXR(), silent = T)
    # print("Prediction Research")
    try(object@IPRED <- prediction_research_NLMIXR(), silent = T)
    # print("COV Research")
    try(object@cov <- cov_research_NLMIXR(), silent = T)
    # print("Residus Research")
    try(object@residus <- residus_research_NLMIXR(), silent = T)
    # print("Random Effect Research")
    try(object@randomEffect <- eta_research_NLMIXR(), silent = T)
    # print("Date Research")
    try(object@date <- as.character(date_creation()), silent = T)
    # print("Administration Research")
    try(object@Score <- scores_NLMIXR(), silent = T)
    try(object@path_source <- value)

    return(object)
  }
)
# data()
# rm()
# demo <- createDossier("file://emsw0136/_cinetiq/CINETIQ/2_Scientifique/Software/R/Package_Thibaud")
# demo(13)
