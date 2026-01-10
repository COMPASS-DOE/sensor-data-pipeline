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

# MAD (median absolute deviation) outlier test, based on how many
# multiples of the MAD a point is from the sample median
# See https://en.wikipedia.org/wiki/Median_absolute_deviation
# and e.g. https://doi.org/10.1016/j.jesp.2013.03.013
# Returns a logical vector of same length as input, with TRUE
# indicating that the corresponding value is an outlier
outliers_MAD <- function(x, threshold = 3, min_n = 100) {
    if(length(x) < min_n) return(rep(FALSE, length(x)))

    md <- mad(x, na.rm = TRUE)

    # return vector of flags, where TRUE signals an outlier
    return(abs(x - median(x, na.rm = TRUE)) > md * threshold)
}

do_outlier_test <- function(x, algorithm, time_grouping, otherparams) {

    # Split data
    message("\t\tTime grouping is ", time_grouping)
    time_groups <- lubridate::round_date(x$TIMESTAMP, time_grouping)
    x_split <- split(x, time_groups)
    message("\t\tData splits: ", length(x_split))

    function_name <- paste0("outliers_", algorithm)
    flag_name <- paste0("F_", algorithm)

    # Construct expression (the call to QAQC_mad) to evaluate
    # The Value column is character but, since we're here, guaranteed
    # to be convertible to numeric
    if(is.na(otherparams)) {
        expr <- paste(function_name, "(as.numeric(x$Value))")
    } else {
        expr <- paste(function_name, "(as.numeric(x$Value),", otherparams, ")")
    }

    # For each group, call the QAQC function
    results <- lapply(x_split, function(x) {
        x[[flag_name]] <- as.integer(eval(parse(text = expr)))
        x
    })

    # Bind and return; we don't do any filtering here
    return(do.call("rbind", results))
}
