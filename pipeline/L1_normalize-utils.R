# L1_normalize-utils.R
# Utility functions, and test code, used exclusively by L1_normalize.qmd
# BBL March 2024

library(lubridate)
library(testthat)

#' Perform unit conversion based on a data frame of data and units table
#'
#' @param dat Data frame with \code{research_name} and \code{value} columns
#' @param ut Data frame with \code{research_name}, \code{conversion}, and \code{new_unit} columns
#' @param quiet Print progress messages?
#'
#' @return The data frame with new columns \code{value_conv} and \code{units}.
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
#' ut <- data.frame(research_name = c("a", "b"), conversion = c("x * 1",
#'  "(x * 2) - 1"), new_unit = c("au", "bu"))
#' unit_conversion(values = 1:3, research_names = c("a", "a", "b"), ut = ut)
unit_conversion <- function(values, research_names, ut, quiet = FALSE) {
    if(!all(c("research_name", "conversion", "new_unit") %in% names(ut))) {
        stop("The units table data frame isn't structured correctly")
    }

    value_conv <- rep(NA_real_, length(values))
    units <- rep(NA_character_, length(values))

    # Isolate the various research_name entries one by one, find corresponding
    # conversion string, evaluate it against the `value` data column
    for(rn in unique(research_names)) {
        rn_entries <- which(research_names == rn)
        x <- values[rn_entries] # this `x` name is crucial; used by conversion strings
        # Isolate conversion string
        which_ut <- which(ut$research_name == rn)
        if(length(which_ut) == 0) {
            conv <- "<not found>"
        } else if(length(which_ut) == 1) {
            conv <- ut$conversion[which_ut]
            # ...and evaluate it
            out <- try(eval(parse(text = conv)))
            if(is.numeric(out) & length(out) == length(x)) {
                value_conv[rn_entries] <- out
            } else {
                stop("Error evaluating conversion string for research_name ", rn)
            }
            units[rn_entries] <- ut$new_unit[which_ut]
        } else {
            stop("Multiple conversions for ", rn)
        }
        if(!quiet) message("\t\tUC: ", rn, " n=", length(rn_entries), ", conv=", conv)
    }
    return(list(value_conv = value_conv, units = units))
}

#test_that("unit_conversion works", {
x <- data.frame(value = 1:3, research_name = c("a", "a", "b"))

# Empty unit conversion table - should be all NA
empty <- data.frame(research_name = "", conversion = "", new_unit = "")
z <- unit_conversion(x$value, x$research_name, empty, quiet = TRUE)
expect_true(all(is.na(z$value_conv)))

# Empty empty
z <- unit_conversion(numeric(0), character(0), empty)
expect_identical(length(z$value_conv), 0L)

# Bad units table
expect_error(unit_conversion(x$value, x$research_name, cars),
             regexp = "isn't structured correctly")

# Multiple conversions for a research_name
y <- data.frame(research_name = c("a", "a"), conversion = c("x * 1", "(x * 2) - 1"), new_unit = "")
expect_error(unit_conversion(x$value, x$research_name, y),
             regexp = "Multiple conversions")

# Respects quiet
y <- data.frame(research_name = c("a", "b"), conversion = c("x * 1", "(x * 2) - 1"), new_unit = "")
expect_silent(unit_conversion(x$value, x$research_name, y, quiet = TRUE))
#})


# We pass an oos_df data frame to oos()
# This is a table of out-of-service windows
# It MUST have `oos_begin` and `oos_end` timestamps; optionally,
# it can have other columns that must be matched as well
# (e.g., site, plot, etc)

# The second thing we pass is the observation data frame

# This function returns a logical vector, of the same length as the
# data_df input, that becomes F_OOS
check_oos <- function(oos_df, data_df) {
    oos_df <- as.data.frame(oos_df)

    # Make sure that any 'extra' condition columns (in addition to the
    # oos window begin and end) are present in the data d.f.
    non_ts_fields <- setdiff(colnames(oos_df), c("oos_begin", "oos_end"))
    if(!all(non_ts_fields %in% colnames(data_df))) {
        stop("Not all out-of-service condition columns are present in data!")
    }
    # For speed, compute the min and max up front
    min_ts <- min(data_df$TIMESTAMP)
    max_ts <- max(data_df$TIMESTAMP)

    oos_final <- rep(FALSE, nrow(data_df))

    for(i in seq_len(nrow(oos_df))) {
        # First quickly check: is there any overlap in timestamps?
        timestamp_overlap <- min_ts <= oos_df$oos_end[i] &&
            max_ts >= oos_df$oos_begin[i]
        #     message("timestamp_overlap = ", timestamp_overlap)
        if(timestamp_overlap) {
            oos <- data_df$TIMESTAMP >= oos_df$oos_begin[i] &
                data_df$TIMESTAMP <= oos_df$oos_end[i]
            # There are timestamp matches, so check other (optional)
            # conditions in the oos_df; they must match exactly
            # For example, if there's a "Site" entry in oos_df then only
            # data_df entries with the same Site qualify to be o.o.s
            for(f in non_ts_fields) {
                matches <- data_df[,f] == oos_df[i,f]
                #               message("f = ", f, " ", oos_df[i,f], ", matches = ", sum(matches))
                oos <- oos & matches
            }

            # The out-of-service flags for this row of the oos_df table
            # are OR'd with the overall flags that will be returned below
            # I.e., if ANY of the oos entries triggers TRUE, then the
            # datum is marked as out of service
            oos_final <- oos_final | oos
        }
    }
    return(oos_final)
}

# Test code for oos()
test_oos <- function() {
    data_df <- data.frame(TIMESTAMP = 1:3, x = letters[1:3], y = 4:6)

    # No other conditions beyond time window
    oos_df <- data.frame(oos_begin = 1, oos_end = 1)
    stopifnot(check_oos(oos_df, data_df) == c(TRUE, FALSE, FALSE))
    oos_df <- data.frame(oos_begin = 4, oos_end = 5)
    stopifnot(check_oos(oos_df, data_df) == c(FALSE, FALSE, FALSE))
    oos_df <- data.frame(oos_begin = 0, oos_end = 2)
    stopifnot(check_oos(oos_df, data_df) == c(TRUE, TRUE, FALSE))
    oos_df <- data.frame(oos_begin = 0, oos_end = 3)
    stopifnot(check_oos(oos_df, data_df) == c(TRUE, TRUE, TRUE))

    # x condition - doesn't match even though timestamp does
    oos_df <- data.frame(oos_begin = 1, oos_end = 1, x = "b")
    stopifnot(check_oos(oos_df, data_df) == c(FALSE, FALSE, FALSE))
    # x condition - matches and timestamp does
    oos_df <- data.frame(oos_begin = 1, oos_end = 1, x = "a")
    stopifnot(check_oos(oos_df, data_df) == c(TRUE, FALSE, FALSE))
    # x condition - some match, some don't
    oos_df <- data.frame(oos_begin = 1, oos_end = 2, x = "b")
    stopifnot(check_oos(oos_df, data_df) == c(FALSE, TRUE, FALSE))
    # x and y condition
    oos_df <- data.frame(oos_begin = 1, oos_end = 2, x = "b", y = 5)
    stopifnot(check_oos(oos_df, data_df) == c(FALSE, TRUE, FALSE))
    oos_df <- data.frame(oos_begin = 1, oos_end = 2, x = "a", y = 5)
    stopifnot(check_oos(oos_df, data_df) == c(FALSE, FALSE, FALSE))

    # Error thrown if condition column(s) not present
    oos_df <- data.frame(oos_begin = 1, oos_end = 2, z = 1)
    out <- try(check_oos(oos_df, data_df), silent = TRUE)
    stopifnot(class(out) == "try-error")
}
test_oos()

# Read all out-of-service files, check their formatting,
# and return as a list of the oos data tibbles
read_oos_data <- function(oos_dir) {
    message("Dir is ", oos_dir)
    oos_files <- list.files(oos_dir, pattern = "\\.csv$", full.names = TRUE)
    message("Files: ", length(oos_files))

    # Read files and check for required columns
    oos_data <- lapply(oos_files, function(f) {
        message(f)
        x <- read_csv(f, col_types = list(oos_begin = col_character(),
                                          oos_end = col_character()))
        required <- c("Site", "oos_begin", "oos_end")

        if(!all(required %in% names(x))) {
            missing <- setdiff(required, names(x))
            stop("Required column(s) ", paste(missing, collapse = ","),
                 " not present in ", f)
        }
        x
    })

    # Make list names
    names(oos_data) <- sapply(oos_files, function(x) {
        gsub(".csv", "", basename(x), fixed = TRUE)
    })
    return(oos_data)
}
