# L1-utils.R
# Utility functions, and test code, used exclusively by L1.qmd
# BBL March 2024


#' Scan sub-folders
#'
#' @param root_dir Root directory to start from, character
#' @param file_pattern Regex for files, character
#' @param quiet Be quiet or print diagnostic messages? Logical
#'
#' @return A named list of folder contents; each list object name is the
#' name of the folder, and each object in the list is a vector of
#' fully qualified filenames.
#' @note Does not recurse into sub-folders.
#' @export
#'
#' @examples
#' scan_folders("./")
scan_folders <- function(root_dir, file_pattern = "\\.csv$", quiet = TRUE) {
    if(!dir.exists(root_dir)) {
        stop("Directory doesn't exist!")
    }
    entries <- list.files(root_dir, full.names = TRUE)

    folder_list <- list()
    for(e in entries) {
        if(!quiet) message(e)
        if(dir.exists(e)) { # it's a folder
            files <- list.files(e, pattern = file_pattern, full.names = TRUE)
            if(!quiet) message("\t", length(files), " files")
            if(length(files)) {  # ...with csv files!
                folder_list[[e]] <- files
            }
        }
    }

    return(folder_list)
}
