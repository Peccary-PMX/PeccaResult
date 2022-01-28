#' Folder Initialization
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @description Create an enclosed folder object containing every found runs, and return a function to manipulate it.
#' @param path string, path of the complete initial folder to scan (RStudio users can directly do a copy-paste from file-explorator, Windows users can also use MAJ-right click -> copy as a Path functionnality).
#' @details createFolder() is the corner-stone function of peccary. From a pathway, it will 1- rework the pathway such as it facilitate user input, 2- find every
#' available runs in the given folder and all its subfolders (no matter the path depth from initial folder), 3- initialize an enclosed peccary folder object
#' with basic informations (among them numeric ID of each run), and 4- return the corresponding closure function to manipulate it (see Value).
#' @return Returns a function related to an enclosed folder. Users can not directly manipulate data. Instead, they use the returned
#' function taking as major  arguments a numeric vector to call desired runs. If no value or 0 is given as run argument, the function print once more the summary of all available runs. A second parameter, folder, is a boolean used to force a single run to have a folder behavior (usefull mostly for programming consideration).
#' Note that folders store runs when they are created (and that run are created when they are called for the first time, not before). Consequently the very first function used after folder creation will take more time than next ones (time for run creations added to intrinsic time for function computation).
#' @examples
#' \dontrun{
#'
#' # folder creation
#' folder <- createFolder("path_to_initial_folder")
#'
#' # print the summary table with run IDs
#' folder() # or folder(0)
#'
#' # create run 1 (first use) and use it
#' a_peccary_function(folder(1))
#'
#' # create run 3 and use both runs 1 and 3
#' a_peccary_function(folder(c(1,3)))
#' }
#' @export
createFolder <- function(path){


  dossier <- new("folder")
  dossier_fill_COMMUN(dossier) <- gsub("\"", "", path)

  function(run = 0, folder = F){


    if(run[[1]] > 0){
      for(nrun in run){
        tryHave <- try(dossier@RunsStorage[[nrun]], silent = T)


        if(class(tryHave) == "try-error" | class(tryHave) == "NULL"){
          tryloadrun <- try(select_run(dossier,nrun), silent = T)
          dossier@RunsStorage[[nrun]] <<- tryloadrun
        }
      }
    }

    if(run[[1]] != 0) dossier@lastSelected <<- run


    # Output (folder or run depending on conditions)
    if(folder == T | length(run) > 1  | run[[1]] == -1 ){

      return(dossier)

    }else if (folder == F & run[[1]] > 0){

      return(dossier@RunsStorage[[run]])

    }else if(run[[1]] == 0){

      return(dossier@summary)

    }
  }

}



# old




