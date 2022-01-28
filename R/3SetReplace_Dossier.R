#' dossier_fill_COMMUN
#' Author: Thibaud Derippe
setGeneric("dossier_fill_COMMUN<-",
           function(object, value){standardGeneric("dossier_fill_COMMUN<-")}
)


#' Fill the dossier with NONMEM or Monolix runs
#' Author: Thibaud Derippe
#' Arguments: object, the dossier object to use the setReplaceMethod
#' Arguments: value, the pathway to = begin the researches
#' Argumets: auto_storage if you want to auto-load runs(useless I think, can be deleted)
#' Output: not appropriate, setReplaceMethod
#' @export
setReplaceMethod(
  f="dossier_fill_COMMUN",
  signature = "folder",
  definition = function(object, value){

    value <- gsub("^file:///", "", value)

    # For link like that "file://emsw0136/_cinetiq/CINETIQ/2_Scientifique/Software/R/Package_Thibaud/Exemple_demo"
    value <- gsub("file:", "", value)

    # First we store the pathway to the racine attribute
    object@racine <- value


    # object@path  <-
   # testtime <-  Sys.time()
     runs <-  find_runs(value)
# print("runs found")
# print( Sys.time() - testtime)

     # in order to eliminate VPC files from NONMEM
     if(length(grep("VPC$", toupper(runs$run))) > 0){
      runs <- runs[-grep("VPC$", toupper(runs$run)), ]
     }


    FOs <- double()
    Time_Creation <- date()
    for(a in  runs$files){
# print(a)
      software <- runs$software[runs$files == a ]

      if(software == "MONOLIX"){


          #rechercheFO
          path_temp <- paste0(value,"/",a, "/pop_parameters.txt")
          t <- try(readLines(path_temp, skipNul = T), silent = T)

          if(class(t) == "try-error") t <- try(readLines(paste0(value,"/",a, "/summary.txt"), skipNul = T))

          if(class(t) != "try-error"){
          line_temp  <- unlist(t[grep('2 x log-likeliho', t)]) %>% {gsub("\\(.+\\)", "", .)}
          result <- try(as.double(strsplit(line_temp, " ")[[1]][length(strsplit(line_temp, " ")[[1]])]), silent = T)


          }


          if(length(line_temp) == 0 | class(result) == "try-error") result = NA
          # print(paste(a, result))
          FOs <- c(FOs, result)
          Time_Creation <-c(Time_Creation,as.character(file.info(paste0(value,"/",a))$mtime))


        }else if(software == "NONMEM"){

      # reading configuration files
      config <- readLines(file.path(find.package("peccary"),  "Peccary_configuration.R"))
      # to extract atomic vector of possible nonmem extention result files
      eval(parse_expr(config[grep("ext_nm_res_file", config)]))
      # so here are all possibilies for result files
      possibilities <- paste0(value, .Platform$file.sep, a, ext_nm_res_file)
      # and here is the one who exist (note: issue if you have several results)
      file <- possibilities[file.exists(possibilities)][[1]]
      #rechercheFO
      t <- try(readLines(file, skipNul = T), silent = T)
      result <- NA
      if(class(t)!= "try-error"){
      line <- unlist(t[grep('#OBJV:*', t)])
      result <- as.double(gsub("[^0-9,.]", "", line))

      if(length(grep("-", line) > 0 )) result <- - result
      }

      FOs <- c(FOs, result[[1]])
      Time_Creation <-c(Time_Creation,as.character(file.info(file)$mtime)[[1]])



      }else if(software == "NLMIXR"){

        file <- paste0(value, "/", a, ".nlmixr")
        t <- try(readRDS(file), silent = T)
        newFOs <- NA
        if(class(t)[[1]]!= "try-error")  try( newFOs <- t$obf, silent = T)
        if(length(newFOs) == 0 ) newFOs <- NA
        FOs <- c(FOs, newFOs)
        Time_Creation <-c(Time_Creation,as.character(file.info(file)$mtime)[[1]])


      }else if(software == "ADAPT"){

        file <- paste0(value, "/", a, ".run")
        line <- readLines(file)
        newFOs <- NA
        try(newFOs <- line[grep("Negative Log", line)[length(grep("Negative Log", line))]] %>%
          gsub(pattern = ".+: *", replacement = "") %>% as.double)

        FOs <- c(FOs, newFOs)
        Time_Creation <-c(Time_Creation,as.character(file.info(file)$mtime)[[1]])


      }
    }

   summary <- data.frame(runs, FOs, Date = Time_Creation[-1])   %>%
     arrange(FOs)  %>%
     mutate(rank = 1:length(FOs)) %>%
     arrange(files) %>%
     mutate(number = 1:length(FOs)) %>%
     select(run, files, FOs,rank, everything())



    if(length(unique(runs$software)) >1){
    object@software <- "Mixte"
    }else{
      object@software <- unique(runs$software)
      # summary <- summary %>%
        # select(-software)
    }

    object@summary <- summary

    methods::show(object@summary %>% select(-files))
    return(object)
  }
)


