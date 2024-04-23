#' Determine operating system
#'
#' @param os_mode Manual override of which OS to use. Probably only useful for developer.
#'
#' @return What operation system is run
#'
determine_system <- function(os_mode = NULL) {

  if(.Platform$OS.type == "unix") {
    os_mode <- "unix"

    message("You seem to run on unix")
    return(os_mode)
  }
  if(.Platform$OS.type == "windows") {
    os_mode <- "windows"

    message("You seem to run on windows")
    return(os_mode)
  }
  if(is.null(os_mode)) {stop("could not detect operating system. Set manually to \"unix\" or \"windows\"")}

}

#' Locate where the snippet file is hiding, and if it exists
#'
#' @param be_my_guess Looks for user folders on Windows if TRUE. Set to TRUE if no arguments are specified.
#' @param win_user_name The name of window user folder to use.
#' @param rstudio_path Path to windows folder containing the R-studio installation ("c:/user/AppData/Roaming/Rstudio/")
#' @param snippet_path Path pointing to your snippet file ("r.snippets")
#' @param match_argument Not used, but allows you to check for other names of the snippet file (for development use.)
#'
#' @return Path pointing to snippet file if it exists. Otherwise instructions on how to create it.
#' @export
locate_snippet_file <- function(be_my_guess = NULL, win_user_name = NULL, rstudio_path=NULL, snippet_path=NULL, match_argument="r.snippets") {

  null_sum <- as.numeric(!is.null(win_user_name)) + as.numeric(!is.null(rstudio_path)) + as.numeric(!is.null(snippet_path)) + as.numeric(!is.null(be_my_guess))
  if(null_sum > 1) {stop("Please provide only 1 of the arguments 'be_my_guess', 'win_user_name', 'rstudio_path', 'snippet_path'")}

  null_sum_2 <- as.numeric(!is.null(win_user_name)) + as.numeric(!is.null(rstudio_path)) + as.numeric(!is.null(snippet_path))
  if(null_sum_2 == 1) {be_my_guess <- FALSE}

  os_mode <- determine_system()

  if(os_mode == "unix"){
    snippet_path <- "~/.config/rstudio/snippets/r.snippets"
    be_my_guess <- FALSE
    if(file.exists(snippet_path)) {snippet_found <- TRUE} else {snippet_found <- FALSE}
  }

  if(!is.null(snippet_path)){
    be_my_guess <- FALSE
    if(file.exists(snippet_path)) {snippet_found <- TRUE}
  }

  if(is.null(be_my_guess)){
    if(is.null(win_user_name) & is.null(rstudio_path) & is.null(snippet_path)) {be_my_guess <- TRUE}
  }

  if(be_my_guess==T){
    users_folders <- list.dirs("c:/Users/", full.names = F, recursive = F) %>%
      as.data.frame() %>% dplyr::rename("folder" = 1) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      dplyr::relocate(.data$row, .data$folder)

    suggested_folder <- users_folders %>%
      dplyr::filter(grepl(.data$folder, pattern="\\d\\d\\d\\d")) %>%
      dplyr::pull(.data$row)

    suggested_folder <- suggested_folder[1]

    message("The following user folders exists.")
    print(users_folders)

    if(length(suggested_folder > 0)) {
      message(paste0("If you run a computer set up by Stockholm University, you probably want to go for number ", suggested_folder))}

    folder_line <- readline("Select the one you want by entering the line number here: ")

    win_user_name <- users_folders %>%
      dplyr::filter(row == folder_line) %>% dplyr::pull(.data$folder)

    message(paste0("You selected row number ", folder_line, "\nthat means folder ", win_user_name, ". Enjoy."))

  }

  if(!is.null(win_user_name)) {rstudio_path <- paste0("c:/Users/", win_user_name, "/AppData/Roaming/RStudio/")}

  if(is.null(snippet_path) & is.null(rstudio_path)) {
    stop("You have to provide some kind of folder pointing to where to start looking for the snippet file.")
  }

  if(os_mode == "windows"){

    if(is.null(snippet_path)){

  if(!dir.exists(rstudio_path)){
    stop(paste0("provided look-up path (", rstudio_path ,") does not seem to exist on your computer."))
  }}

# Look for snippet file given a folder location:
    if(!is.null(rstudio_path)){
      if(!is.null(snippet_path)) {stop("Please provide only 1 of snippet_path and rstudio_path")}

      message("looking for a folder containing R snippets ...")
      Sys.sleep(1)

      snippet_found <- NULL # dummy value

      snippet_folder <- list.dirs(rstudio_path) %>%
        as.data.frame() %>% dplyr::rename("path" = 1) %>%
        dplyr::filter(grepl(.data$path, pattern="snippet")) %>% dplyr::pull(.data$path)

      if(length(snippet_folder) == 0) {snippet_found <- FALSE} else {if(dir.exists(snippet_folder) == FALSE){snippet_found <- FALSE}}

      if(is.null(snippet_found)) {

      snippet_path <- list.files(snippet_folder, full.names = T, recursive = T) %>% as.data.frame() %>% dplyr::rename("path" = 1) %>%
         dplyr::filter(grepl(.data$path, pattern=match_argument)) %>% dplyr::pull(.data$path)

      if(length(snippet_path) == 0){snippet_found <- FALSE} else {snippet_found <- TRUE}
      }

    }
  } # End windows specific calls

  # Look for snippet file given either provided or derived snippet_path:
  if(!is.null(snippet_path) & snippet_found != FALSE){
    #message(paste0("Checking if snippet file already exists at location: ", snippet_path))

    if(file.exists(snippet_path)){

      message(paste0("Snippet file located at:", snippet_path))

      Sys.sleep(1)

      snippet_found <- TRUE

      return(snippet_path)

    } else { snippet_found <- FALSE}

  }

  if(snippet_found == FALSE){
    message(paste0("Did not find any snippet file at the given location (", snippet_path,")"))
    cat("\nTo 'create' the file, open the menue tools > Global options.\n
        Select 'Code' in the left column and cross 'Enable snippets'.\n
        Add a blank line and save. Close all menues.")
    message("\n\nThen run this function again. Enjoy.")

    return(snippet_found)
  }

}


#' Function adding a snippet to easily editing the snippet file.
#'
#' @param snippet_path The path pointing to your snippet file. Returned by the function 'locate_snippet_file()'
#'
#' @return Checks if a edit_snippet snippet exists. If not, asks if you want to add it automatically or manually.
#' @export
add_snippet_edit_shortcut <- function(snippet_path) {

  if(!file.exists(snippet_path)){stop("You tried to add a snippet shortcut without having a valid path for a snippet file.")}

  if(grepl(pattern="snippet edit_snippets", readLines(snippet_path)) %>% any()){
    message(paste0("\n\n", crayon::underline("You already have a snippet to easily edit your snippets!")))
    message("Make sure that you have snippets activated.\nThen start typing 'edit_snippets' and hit tab when the suggestion is showing. Enjoy.\n\n
                      *** If the snippet does not show, Rstudio has not loaded the updated file. Wait a while or restart Rstudio. ***")
  } else {

    cat("Do you want to try to automatically add an 'edit_snippets' snippet to your snippets?\n")
    answer <- readline("If so, type 'Y' here: ")

    if(tolower(answer) == "y") {

      write("\nsnippet edit_snippets", file = snippet_path, append = TRUE)
      snipett_line <- paste0("\t`r file.edit(\"",snippet_path,"\")`")
      write(snipett_line, file = snippet_path, append = TRUE)
      write("\n", file = snippet_path, append = TRUE)

      message("Now you can try to edit your snippets by typing
              'edit_snippets'")

      message("Make sure that you have snippets activated.\nThen start typing 'edit_snippets' and hit tab when the suggestion is showing. Enjoy.\n\n
                      *** If the snippet does not show, Rstudio has not loaded the updated file. Wait a while or restart Rstudio. ***")

    } else {
      cat(crayon::bgRed$white("Your snippet file was not changed.\n"))

      cat("Do you want to do it manually now?\n")
      answer_2 <- readline("If so, type 'Y' here: ")

      if(tolower(answer_2) == "y"){
        utils::file.edit(snippet_path)

        message("\nYour snippet file is now opened and you can edit it straight away.")
        message("Make a new line and copy and paste the exact text below:\n")

        cat("snippet edit_snippets\n")
        cat(paste0("\t`r file.edit(\"",snippet_path,"\")`"))

        cat("\n")
        cat(crayon::underline("\n(NB! Make sure that the second line is starting with a tab!)"))
        cat("\nAnd then save the file.")
      } else {

        message("To do this manually, run the following code:")
        cat(paste0("file.edit(\"",snippet_path,"\")"))

        message("\nAnd paste:")
        cat("snippet edit_snippets\n")
        cat(paste0("\t`r file.edit(\"",snippet_path,"\")`"))

        cat(crayon::underline("\n(NB! Make sure that the second line is starting with a tab!)"))
        cat("\nAnd then save the file.")
      }
    }
  }

}

#' Wrapper function to locate snippet file and, if it exists, offers to add an 'edit snippet'-snippet.
#'
#' @param be_my_guess Looks for user folders on Windows if TRUE. Set to TRUE if no arguments are specified.
#' @param win_user_name The name of window user folder to use.
#' @param rstudio_path Path to windows folder containing the R-studio installation ("c:/user/AppData/Roaming/Rstudio/")
#' @param snippet_path Path pointing to your snippet file ("r.snippets")
#'
#' @return Checks if the snippet file exists. If it does, checks if a edit_snippet snippet exists. If not, asks if you want to add it automatically or manually.
#' @export
let_there_be_snippets <- function(be_my_guess = NULL, win_user_name = NULL, rstudio_path=NULL, snippet_path=NULL) {

 snippet_path <- locate_snippet_file(be_my_guess, win_user_name, rstudio_path, snippet_path)

 if(snippet_path != FALSE) {
   add_snippet_edit_shortcut(snippet_path)
 }

}




# let_there_be_snippets()
# let_there_be_snippets(win_user_name = "raer1762")
# let_there_be_snippets(snippet_path = "c:/Users/raer1762/AppData/Roaming/RStudio/snippets/r.snippets")
# let_there_be_snippets(rstudio_path = "c:/Users/raer1762/AppData/Roaming/RStudio/")

